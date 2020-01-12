const std = @import("std");
const allocator = @import("root").allocator;

const Chunk = @import("./chunk.zig").Chunk;

const Value = @import("./value.zig").Value;
const ValueType = @import("./value.zig").ValueType;

const Instance = @import("./compiler.zig").Instance;
const Compiler = @import("./compiler.zig").Compiler;

const Obj = @import("./object.zig").Obj;
const ObjString = @import("./object.zig").ObjString;
const ObjFunction = @import("./object.zig").ObjFunction;
const ObjNative = @import("./object.zig").ObjNative;
const ObjClosure = @import("./object.zig").ObjClosure;
const ObjUpvalue = @import("./object.zig").ObjUpvalue;
const ObjInstance = @import("./object.zig").ObjInstance;
const ObjClass = @import("./object.zig").ObjClass;

const verbose = false;

pub const OpCode = enum(u8) {
    Constant,
    Nil,
    True,
    False,
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Not,
    And,
    Or,
    Print,
    Pop,
    GetLocal,
    SetLocal,
    GetUpvalue,
    SetUpvalue,
    CloseUpvalue,
    GetProperty,
    SetProperty,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    JumpIfFalse,
    Jump,
    Call,
    Closure,
    Invoke,
    Method,
    Class,
    Super,
    Subclass,
    Loop,
    Return,
};

pub const CallFrame = struct {
    closure: *Obj,
    ip: [*]u8,
    slots: u32,

    fn readByte(frame: *CallFrame) u8 {
        const byte = frame.ip[0];
        frame.ip += 1;
        return byte;
    }

    fn readShort(frame: *CallFrame) u16 {
        const value = @intCast(u16, frame.ip[0]) << 8 | frame.ip[1];
        frame.ip += 2;
        return value;
    }

    fn readConstant(frame: *CallFrame) Value {
        const chunk = &frame.closure.data.Closure.function.chunk;
        return chunk.constants.at(frame.readByte());
    }

    fn readString(frame: *CallFrame) *ObjString {
        const chunk = &frame.closure.data.Closure.function.chunk;
        return &chunk.constants.at(frame.readByte()).Obj.data.String;
    }
};

const FRAMES_MAX: u32 = 64;
const STACK_MAX: usize = 1024;

pub const VM = struct {
    instance: Instance,

    frames: [FRAMES_MAX]CallFrame,
    frame_count: u32,
    current_frame_count: u32,

    stack: std.ArrayList(Value),
    strings: std.HashMap([]const u8, *Obj, ObjString.hashFn, ObjString.eqlFn),
    globals: std.HashMap([]const u8, Value, ObjString.hashFn, ObjString.eqlFn),
    openUpvalues: ?*Obj,
    objects: ?*Obj,

    bytesAllocated: usize,
    nextGC: usize,
    grayCount: usize,
    grayStack: ?[]*Obj,

    pub fn create() VM {
        return VM{
            .instance = Instance.create(),
            .frames = undefined,
            .frame_count = 0,
            .current_frame_count = 0,
            .stack = std.ArrayList(Value).initCapacity(allocator, STACK_MAX) catch unreachable,
            .strings = std.HashMap([]const u8, *Obj, ObjString.hashFn, ObjString.eqlFn).init(allocator),
            .globals = std.HashMap([]const u8, Value, ObjString.hashFn, ObjString.eqlFn).init(allocator),
            .openUpvalues = null,
            .objects = null,
            .bytesAllocated = 0,
            .nextGC = 1024 * 1024,
            .grayCount = 0,
            .grayStack = &[_]*Obj{},
        };
    }

    fn interpret(self: *VM, source: []const u8) !void {
        var function = try self.instance.compile(source);
        self.push(function.value());

        const closure = ObjClosure.allocate(&function.data.Function);
        _ = self.pop();
        self.push(closure.value());

        try self.callValue(closure.value(), 0);
        try self.run();
    }

    fn push(self: *VM, value: Value) void {
        self.stack.append(value) catch unreachable;
    }

    fn pop(self: *VM) Value {
        return self.stack.pop();
    }

    fn peek(self: *VM, distance: u32) Value {
        return self.stack.at(self.stack.len - 1 - distance);
    }

    fn slice(self: *VM, distance: u32) []Value {
        return self.stack.items[self.stack.len - 1 - distance ..];
    }

    fn resetStack(self: *VM) void {
        self.stack.resize(0) catch unreachable;
        self.frame_count = 0;
        self.current_frame_count = 0;
        self.openUpvalues = null;
    }

    fn runtimeError(self: *VM, comptime format: []const u8, args: var) void {
        const frame = &self.frames[self.frame_count - 1];
        const instruction = @ptrToInt(frame.ip) - @ptrToInt(frame.closure.data.Closure.function.chunk.ptr());
        const line = frame.closure.data.Closure.function.chunk.getLine(instruction);

        std.debug.warn("Runtime error on line {}: ", .{line});
        std.debug.warn(format, args);
        std.debug.warn("\n", .{});

        var i = self.frame_count - 1;
        while (i > 0) : (i -= 1) {
            const prev_frame = &self.frames[i];
            const function = prev_frame.closure.data.Closure.function;
            const offset = @ptrToInt(prev_frame.ip) - @ptrToInt(function.chunk.ptr());

            std.debug.warn("[line {}] in ", .{function.chunk.getLine(@intCast(usize, offset))});

            if (function.name) |n| {
                std.debug.warn("{}()\n", .{n.data.String.bytes});
            } else {
                std.debug.warn("script\n", .{});
            }
        }

        self.resetStack();
    }

    fn printDebug(self: *VM) void {
        const frame = &self.frames[self.frame_count - 1];
        const instruction = @ptrToInt(frame.ip) - @ptrToInt(frame.closure.data.Closure.function.chunk.ptr());
        for (self.stack.toSlice()) |s, i| {
            std.debug.warn("[{}]\n", .{s.toString()});
        }
        _ = frame.closure.data.Closure.function.chunk.disassembleInstruction(instruction);
        std.debug.warn("\n", .{});
    }

    pub fn print(self: *VM, value: Value) void {
        const string = value.toString();
        std.debug.warn("{}", .{string});
    }

    pub fn puts(self: *VM, string: []const u8) void {
        std.debug.warn("{}", .{string});
    }

    fn callValue(self: *VM, callee: Value, arg_count: u8) !void {
        switch (callee) {
            .Obj => |o| switch (o.data) {
                .Class => |*c| {
                    self.stack.ptrAt(self.stack.len - arg_count - 1).* = ObjInstance.allocate(c).value();
                    return;
                },
                .Closure => return self.call(o, arg_count),
                .Native => |n| {
                    const result = n.function(self.slice(arg_count));
                    self.stack.len -= arg_count;
                    self.push(result);
                    return;
                },
                .Instance, .String, .Upvalue, .Function => {},
            },
            .Bool, .Number, .Nil => {},
        }

        self.runtimeError("Can only call functions and classes.", .{});
        return error.NotCallable;
    }

    pub fn captureUpvalue(self: *VM, local: *Value) *Obj {
        if (self.openUpvalues) |o| {
            var prevUpvalue: ?*Obj = null;
            var upvalue: ?*Obj = o;
            while (upvalue) |u| {
                if (@ptrToInt(u.data.Upvalue.location) > @ptrToInt(local)) {
                    prevUpvalue = u;
                    upvalue = u.next;
                } else {
                    break;
                }
            }

            if (upvalue) |u|
                if (u.data.Upvalue.location == local)
                    return u;

            var createdUpvalue = ObjUpvalue.allocate(local, upvalue);
            if (prevUpvalue) |p| {
                p.next = createdUpvalue;
            } else {
                self.openUpvalues = createdUpvalue;
            }

            return createdUpvalue;
        } else {
            self.openUpvalues = ObjUpvalue.allocate(local, null);
            return self.openUpvalues.?;
        }
    }

    pub fn closeUpvalues(self: *VM, last: *Value) void {
        while (self.openUpvalues) |u| {
            if (@ptrToInt(u.data.Upvalue.location) >= @ptrToInt(last)) {
                u.data.Upvalue.closed = u.data.Upvalue.location.*;
                u.data.Upvalue.location = &u.data.Upvalue.closed;
                self.openUpvalues = u.data.Upvalue.next;
            } else {
                return;
            }
        }
    }

    fn call(self: *VM, closure: *Obj, arg_count: u8) !void {
        if (arg_count != closure.data.Closure.function.arity) {
            self.runtimeError("Expected {} arguments but got {}.", .{
                closure.data.Closure.function.arity,
                arg_count,
            });
            return error.UnexpectedArgs;
        }

        if (self.frame_count == FRAMES_MAX) {
            self.runtimeError("Stack overflow.", .{});
            return error.StackOverflow;
        }

        var frame = &self.frames[self.frame_count];
        frame.closure = closure;
        frame.ip = closure.data.Closure.function.chunk.ptr();
        frame.slots = @intCast(u32, self.stack.len) - arg_count - 1;
        self.frame_count += 1;
    }

    fn defineNative(self: *VM, name: []const u8, function: ObjNative.NativeFn) void {
        self.push(ObjString.copy(name).value());
        self.push(ObjNative.allocate(function).value());
        _ = self.globals.put(self.peek(1).Obj.data.String.bytes, self.peek(0)) catch unreachable;
        _ = self.pop();
        _ = self.pop();
    }

    fn defineMethod(self: *VM, name: *ObjString) void {
        const method = self.peek(0);
        const class = self.peek(1).Obj.data.Class;
        //tableSet(class.methods, name, method);
        _ = self.pop();
    }

    fn createClass(self: *VM, name: *ObjString, superclass: ?*ObjClass) void {
        const class = ObjClass.allocate(name, superclass);
        self.push(class.value());

        // Inherit methods.
        if (superclass) |s| {
            //tableAddAll(s.methods, class.methods);
        }
    }

    fn run(self: *VM) !void {
        var frame = &self.frames[self.frame_count - 1];
        while (true) {
            if (verbose) self.printDebug();
            const instruction = @intToEnum(OpCode, frame.readByte());
            switch (instruction) {
                OpCode.Constant => {
                    const constant = frame.readConstant();
                    self.push(constant);
                },
                OpCode.Nil => self.push(Value.Nil),
                OpCode.True => self.push(Value{ .Bool = true }),
                OpCode.False => self.push(Value{ .Bool = false }),
                OpCode.Pop => _ = self.pop(),
                OpCode.GetLocal => {
                    const slot = frame.readByte();
                    self.push(self.stack.at(frame.slots + slot));
                },
                OpCode.SetLocal => {
                    const slot = frame.readByte();
                    self.stack.at(frame.slots + slot) = self.peek(0);
                },
                OpCode.DefineGlobal => {
                    const name = frame.readString();
                    const value = self.peek(0);
                    _ = try self.globals.put(name.bytes, value);
                    const popped = self.pop();
                },
                OpCode.GetGlobal => {
                    const name = frame.readString();
                    var value = self.globals.get(name.bytes) orelse {
                        self.runtimeError("Undefined variable '{}'.", .{name.bytes});
                        return error.RuntimeError;
                    };
                    self.push(value.value);
                },
                OpCode.SetGlobal => {
                    const name = frame.readString();
                    const value = self.peek(0);
                    _ = self.globals.put(name.bytes, value) catch {
                        self.runtimeError("Undefined variable '{}'.", .{name.bytes});
                        return error.RuntimeError;
                    };
                },
                OpCode.GetUpvalue => {
                    const slot = frame.readByte();
                    self.push(frame.closure.data.Closure.upvalues[slot].data.Upvalue.location.*);
                },
                OpCode.SetUpvalue => {
                    const slot = frame.readByte();
                    frame.closure.data.Closure.upvalues[slot].data.Upvalue.location.* = self.peek(0);
                },
                OpCode.Equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value{ .Bool = a.equals(b) });
                },
                OpCode.Add => {
                    const a = self.peek(0);
                    const b = self.peek(1);
                    switch (a) {
                        .Obj => |o| {
                            switch (o.data) {
                                .String => try self.concatenate(),
                                else => unreachable,
                            }
                        },
                        .Number => {
                            try self.binary(Value.Number, instruction);
                        },
                        else => unreachable,
                    }
                },
                OpCode.Subtract, OpCode.Multiply, OpCode.Divide, OpCode.Greater, OpCode.Less => try self.binary(Value.Number, instruction),
                OpCode.Not => self.push(Value{ .Bool = !self.pop().isTruthy() }),
                OpCode.Negate => {
                    switch (self.peek(0)) {
                        .Number => |x| {
                            const number = self.pop().Number;
                            self.push(Value{ .Number = -number });
                        },
                        else => {
                            self.runtimeError("Operand must be a number.", .{});
                            return error.RuntimeError;
                        },
                    }
                },
                OpCode.Print => {
                    const val = self.pop();
                    self.print(val);
                    self.puts("\n");
                },
                OpCode.JumpIfFalse => {
                    const offset = frame.readShort();
                    if (!self.peek(0).isTruthy()) frame.ip += offset;
                },
                OpCode.Jump => {
                    const offset = frame.readShort();
                    frame.ip += offset;
                },
                OpCode.Loop => {
                    const offset = frame.readShort();
                    frame.ip -= offset;
                },
                OpCode.Call => {
                    const arg_count = frame.readByte();
                    try self.callValue(self.peek(arg_count), arg_count);
                    frame = &self.frames[self.frame_count - 1];
                },
                OpCode.Closure => {
                    const function = frame.readConstant();
                    var closure = ObjClosure.allocate(&function.Obj.data.Function);
                    self.push(closure.value());
                    for (closure.data.Closure.upvalues) |*u| {
                        const isLocal = frame.readByte() != 0;
                        const index = frame.readByte();
                        if (isLocal) {
                            u.* = self.captureUpvalue(self.stack.ptrAt(frame.slots + index));
                        } else {
                            u.* = frame.closure.data.Closure.upvalues[index];
                        }
                    }
                },
                OpCode.CloseUpvalue => {
                    self.closeUpvalues(self.stack.ptrAt(self.stack.len - 2));
                    _ = self.pop();
                },
                OpCode.Return => {
                    const result = self.pop();

                    self.closeUpvalues(self.stack.ptrAt(frame.slots));
                    self.frame_count -= 1;

                    if (self.frame_count == 0) {
                        _ = self.pop();
                        return;
                    }

                    self.stack.len -= self.stack.len - frame.slots;
                    self.push(result);
                    frame = &self.frames[self.frame_count - 1];
                },
                OpCode.Class => {
                    self.createClass(frame.readString(), null);
                },
                OpCode.Subclass => {
                    const superclass = self.peek(0);
                    if (superclass.Obj.data != .Class) {
                        self.runtimeError("Superclass must be a class", .{});
                        return error.RuntimeError;
                    }
                    self.createClass(frame.readString(), &superclass.Obj.data.Class);
                },
                OpCode.Method => {
                    self.defineMethod(frame.readString());
                },
                OpCode.GetProperty => {
                    if (self.peek(0).Obj.data != .Instance) {
                        self.runtimeError("Only instances have properties.", .{});
                        return error.RuntimeError;
                    }

                    const instance = self.peek(0).Obj.data.Instance;
                    const name = frame.readString();

                    const value = instance.fields.get(name.bytes) orelse {
                        self.runtimeError("Undefined property {}.", .{name.bytes});
                        return error.RuntimeError;
                    };

                    _ = self.pop(); // Instance.
                    self.push(value.value);
                },
                OpCode.SetProperty => {
                    if (self.peek(1).Obj.data != .Instance) {
                        self.runtimeError("Only instances have properties.", .{});
                        return error.RuntimeError;
                    }

                    var instance = self.peek(1).Obj.data.Instance;
                    const name = frame.readString();

                    _ = try instance.fields.put(name.bytes, self.peek(0));

                    const value = self.pop();
                    _ = self.pop();
                    self.push(value);
                },
                else => {
                    self.runtimeError("Unknown instruction", .{});
                    return error.CompileError;
                },
            }
        }
    }

    fn binary(self: *VM, value_type: ValueType, operator: OpCode) !void {
        var val: Value = undefined;
        switch (value_type) {
            ValueType.Number => {
                if (self.peek(0) != Value.Number or self.peek(1) != Value.Number) {
                    self.runtimeError("Operands must be numbers", .{});
                    return error.RuntimeError;
                }
                const rhs = self.pop().Number;
                const lhs = self.pop().Number;

                switch (operator) {
                    .Add, .Subtract, .Multiply, .Divide => {
                        const number = switch (operator) {
                            .Add => lhs + rhs,
                            .Subtract => lhs - rhs,
                            .Multiply => lhs * rhs,
                            .Divide => lhs / rhs,
                            else => unreachable,
                        };
                        val = Value{ .Number = number };
                    },
                    .Less, .LessEqual, .Greater, .GreaterEqual => {
                        const result = switch (operator) {
                            .Less => lhs < rhs,
                            .LessEqual => lhs <= rhs,
                            .Greater => lhs > rhs,
                            .GreaterEqual => lhs >= rhs,
                            else => unreachable,
                        };
                        val = Value{ .Bool = result };
                    },
                    else => unreachable,
                }
            },
            ValueType.Bool => {
                if (self.peek(0) != Value.Bool or self.peek(1) != Value.Bool) {
                    self.runtimeError("Operands must be boolean", .{});
                    return error.RuntimeError;
                }
                const rhs = self.pop().Bool;
                const lhs = self.pop().Bool;
                const result = switch (operator) {
                    .And => lhs and rhs,
                    .Or => lhs or rhs,
                    else => unreachable,
                };
                val = Value{ .Bool = result };
            },
            else => unreachable,
        }

        self.push(val);
    }

    fn concatenate(self: *VM) !void {
        const a = self.peek(1).Obj.data.String;
        const b = self.peek(0).Obj.data.String;

        const length = a.bytes.len + b.bytes.len;
        var bytes = try allocator.alloc(u8, length);
        std.mem.copy(u8, bytes[0..a.bytes.len], a.bytes);
        std.mem.copy(u8, bytes[a.bytes.len..], b.bytes);

        const result = ObjString.take(bytes);

        _ = self.pop();
        _ = self.pop();

        self.push(result.value());
    }

    pub fn freeObjects(self: *VM) void {
        var object = self.objects;
        while (object) |o| {
            const next = o.next;
            Obj.free(o);
            object = next;
        }

        allocator.free(self.grayStack.?);
    }

    pub fn destroy(self: *VM) void {
        self.stack.deinit();
        self.globals.deinit();
        self.strings.deinit();
        self.freeObjects();
    }
};
