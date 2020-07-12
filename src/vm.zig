const std = @import("std");
const allocator = @import("root").allocator;

const FixedCapacityStack = @import("./stack.zig").FixedCapacityStack;

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;

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
const ObjBoundMethod = @import("./object.zig").ObjBoundMethod;

const verbose = false;

pub const CallFrame = struct {
    closure: *Obj,
    ip: [*]u8,
    slots: u32,

    pub fn readByte(frame: *CallFrame) u8 {
        const byte = frame.ip[0];
        frame.ip += 1;
        return byte;
    }

    pub fn readShort(frame: *CallFrame) u16 {
        const value = @intCast(u16, frame.ip[0]) << 8 | frame.ip[1];
        frame.ip += 2;
        return value;
    }

    pub fn readConstant(frame: *CallFrame) Value {
        const chunk = &frame.closure.data.Closure.function.chunk;
        return chunk.constants.items[frame.readByte()];
    }

    pub fn readString(frame: *CallFrame) *ObjString {
        const chunk = &frame.closure.data.Closure.function.chunk;
        return &chunk.constants.items[frame.readByte()].Obj.data.String;
    }
};

const FRAMES_MAX: u32 = 64;
const STACK_MAX: usize = 1024;

pub const VM = struct {
    instance: Instance,

    frames: [FRAMES_MAX]CallFrame,
    frame_count: u32,
    current_frame_count: u32,

    stack: FixedCapacityStack(Value),
    strings: std.StringHashMap(*Obj),
    globals: std.StringHashMap(Value),

    openUpvalues: ?*Obj,
    objects: ?*Obj,

    bytesAllocated: usize,
    nextGC: usize,
    grayCount: usize,
    grayStack: ?[]*Obj,

    initString: ?*Obj,

    pub fn create() VM {
        return VM{
            .instance = Instance.create(),
            .frames = undefined,
            .frame_count = 0,
            .current_frame_count = 0,
            .stack = FixedCapacityStack(Value).init(allocator, STACK_MAX) catch unreachable,
            .strings = std.StringHashMap(*Obj).init(allocator),
            .globals = std.StringHashMap(Value).init(allocator),
            .openUpvalues = null,
            .objects = null,
            .bytesAllocated = 0,
            .nextGC = 1024 * 1024,
            .initString = null,
            .grayCount = 0,
            .grayStack = &[_]*Obj{},
        };
    }

    pub fn initialize(self: *VM) !void {
        self.initString = ObjString.copy("init");
    }

    pub fn interpret(self: *VM, source: []const u8) !void {
        // Stack should be empty when we start and when we finish running
        std.debug.assert(self.stack.items.len == 0);
        defer std.debug.assert(self.stack.items.len == 0);
        errdefer self.resetStack();

        var function = try self.instance.compile(source);
        self.push(function.value());

        const closure = ObjClosure.allocate(&function.data.Function);
        _ = self.pop();
        self.push(closure.value());

        try self.callValue(closure.value(), 0);
        try self.run();

        // TODO: Shouldn't be necessary, investigate
        self.resetStack();
    }

    pub fn push(self: *VM, value: Value) void {
        self.stack.append(value);
    }

    pub fn pop(self: *VM) Value {
        return self.stack.pop();
    }

    pub fn peek(self: *VM, distance: u32) Value {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }

    pub fn tail(self: *VM, distance: u32) []Value {
        return self.stack.items[self.stack.items.len - 1 - distance ..];
    }

    fn resetStack(self: *VM) void {
        self.stack.resize(0) catch unreachable;
        self.frame_count = 0;
        self.current_frame_count = 0;
        self.openUpvalues = null;

        // TODO: Dictu
        // self.instance.compiler = null;
    }

    fn runtimeError(self: *VM, comptime format: []const u8, args: var) !void {
        @setCold(true);

        std.debug.warn(format, args);
        std.debug.warn("\n", .{});

        var i = @as(isize, self.frame_count) - 1;
        while (i >= 0) : (i -= 1) {
            const prev_frame = &self.frames[@intCast(usize, i)];
            const function = prev_frame.closure.data.Closure.function;
            const offset = @ptrToInt(prev_frame.ip) - @ptrToInt(function.chunk.ptr());
            const line = function.chunk.getLine(@intCast(usize, offset));
            const name = if (function.name) |str| str.data.String.bytes else "<script>";
            std.debug.warn("[ line {}] in {}\n", .{ line, name });
        }

        self.resetStack();

        return error.RuntimeError;
    }

    fn printDebug(self: *VM) void {
        const frame = &self.frames[self.frame_count - 1];
        const instruction = @ptrToInt(frame.ip) - @ptrToInt(frame.closure.data.Closure.function.chunk.ptr());
        for (self.stack.items) |s, i| {
            std.debug.warn("[ {}: {} ]\n", .{ i, s.toString() });
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
                    self.stack.items[self.stack.items.len - arg_count - 1] = ObjInstance.allocate(c).value();
                    if (c.methods.get(self.initString.?.data.String.bytes)) |init| {
                        return self.call(init.Obj, arg_count);
                    } else if (arg_count != 0) {
                        return self.runtimeError("Can only call functions and classes.", .{});
                    }
                    return;
                },
                .BoundMethod => |m| {
                    self.stack.items[self.stack.items.len - arg_count - 1] = m.receiver;
                    return self.call(o, arg_count);
                },
                .Closure => {
                    return self.call(o, arg_count);
                },
                .Native => |n| {
                    const result = n.function(self.tail(arg_count));
                    self.stack.items.len -= arg_count;
                    self.push(result);
                    return;
                },
                .Instance, .String, .Upvalue, .Function => {},
            },
            .Bool, .Number, .Nil => {},
        }

        return self.runtimeError("Can only call functions and classes.", .{});
    }

    fn invokeFromClass(self: *VM, class: *ObjClass, name: *ObjString, arg_count: u8) !void {
        const method = class.methods.get(name.bytes) orelse {
            return self.runtimeError("Cannot invoke undefined property {} in class {}.", .{ name.bytes, class.name.bytes });
        };

        return self.call(method.Obj, arg_count);
    }

    fn invoke(self: *VM, name: *ObjString, arg_count: u8) !void {
        const receiver = self.peek(arg_count).Obj.data;

        if (receiver != .Instance) {
            return self.runtimeError("Only instances have methods.", .{});
        }

        if (receiver.Instance.fields.get(name.bytes)) |value| {
            self.stack.items[self.stack.items.len - arg_count - 1] = value;
            return self.callValue(value, arg_count);
        }

        return self.invokeFromClass(receiver.Instance.class, name, arg_count);
    }

    fn bindMethod(self: *VM, class: *Obj, name: *ObjString) !void {
        const method = class.data.Class.methods.get(name.bytes) orelse {
            return self.runtimeError("Cannot bind undefined property {} in class {}.", .{ name.bytes, class.data.Class.name.bytes });
        };

        const bound = ObjBoundMethod.allocate(class.value(), &method.Obj.data.Closure);
        _ = self.pop();
        self.push(bound.value());
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
            return self.runtimeError("Expected {} arguments but got {}.", .{
                closure.data.Closure.function.arity,
                arg_count,
            });
        }

        if (self.frame_count == FRAMES_MAX) {
            return self.runtimeError("Stack overflow.", .{});
        }

        var frame = &self.frames[self.frame_count];
        frame.closure = closure;
        frame.ip = closure.data.Closure.function.chunk.ptr();
        frame.slots = @intCast(u32, self.stack.items.len) - arg_count - 1;

        self.frame_count += 1;
    }

    pub fn defineNative(self: *VM, name: []const u8, function: ObjNative.NativeFn) void {
        self.push(ObjString.copy(name).value());
        self.push(ObjNative.allocate(function).value());
        _ = self.globals.put(self.peek(1).Obj.data.String.bytes, self.peek(0)) catch unreachable;
        _ = self.pop();
        _ = self.pop();
    }

    fn defineMethod(self: *VM, name: *ObjString) !void {
        const method = self.peek(0);
        var class = self.peek(1).Obj.data.Class;
        _ = try class.methods.put(name.bytes, method);
        _ = self.pop();
    }

    fn createClass(self: *VM, name: *ObjString, superclass: ?*ObjClass) !void {
        const class = ObjClass.allocate(name, superclass);
        self.push(class.value());

        // Inherit methods.
        if (superclass) |s| {
            var it = s.methods.iterator();
            while (it.next()) |e| {
                var slot = try class.data.Class.methods.getOrPut(e.key);
                slot.entry.value = e.value;
            }
        }
    }

    fn run(self: *VM) !void {
        var frame = &self.frames[self.frame_count - 1];
        while (true) {
            if (verbose) self.printDebug();
            const instruction = @intToEnum(OpCode, frame.readByte());
            switch (instruction) {
                .Constant => {
                    const constant = frame.readConstant();
                    self.push(constant);
                },
                .Nil => self.push(Value.Nil),
                .True => self.push(Value{ .Bool = true }),
                .False => self.push(Value{ .Bool = false }),
                .Pop => _ = self.pop(),
                .GetLocal => {
                    const slot = frame.readByte();
                    const local = self.stack.items[frame.slots + slot];
                    self.push(local);
                },
                .SetLocal => {
                    const slot = frame.readByte();
                    const value = self.peek(0);
                    self.stack.items[frame.slots + slot] = value;
                },
                .DefineGlobal => {
                    const name = frame.readString();
                    const value = self.peek(0);
                    _ = try self.globals.put(name.bytes, value);
                    const popped = self.pop();
                },
                .GetGlobal => {
                    const name = frame.readString();
                    var value = self.globals.get(name.bytes) orelse {
                        return self.runtimeError("Undefined variable '{}'.", .{name.bytes});
                    };
                    self.push(value);
                },
                .SetGlobal => {
                    const name = frame.readString();
                    const value = self.peek(0);
                    _ = self.globals.put(name.bytes, value) catch {
                        return self.runtimeError("Undefined variable '{}'.", .{name.bytes});
                    };
                },
                .GetUpvalue => {
                    const slot = frame.readByte();
                    self.push(frame.closure.data.Closure.upvalues[slot].data.Upvalue.location.*);
                },
                .SetUpvalue => {
                    const slot = frame.readByte();
                    frame.closure.data.Closure.upvalues[slot].data.Upvalue.location.* = self.peek(0);
                },
                .GetSuper => {
                    const name = frame.readString();
                    const superclass = self.pop();
                    try self.bindMethod(superclass.Obj, name);
                },
                .Equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value{ .Bool = a.equals(b) });
                },
                .Add => {
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
                .Subtract, .Multiply, .Divide, .Greater, .Less => {
                    try self.binary(Value.Number, instruction);
                },
                .Not => {
                    const value = Value{ .Bool = !self.pop().isTruthy() };
                    self.push(value);
                },
                .Negate => {
                    switch (self.peek(0)) {
                        .Number => |x| {
                            const number = self.pop().Number;
                            self.push(Value{ .Number = -number });
                        },
                        else => {
                            return self.runtimeError("Operand must be a number.", .{});
                        },
                    }
                },
                .Print => {
                    const val = self.pop();
                    self.print(val);
                    self.puts("\n");
                },
                .JumpIfFalse => {
                    const offset = frame.readShort();
                    if (!self.peek(0).isTruthy()) frame.ip += offset;
                },
                .Jump => {
                    const offset = frame.readShort();
                    frame.ip += offset;
                },
                .Loop => {
                    const offset = frame.readShort();
                    frame.ip -= offset;
                },
                .Call => {
                    const arg_count = frame.readByte();
                    try self.callValue(self.peek(arg_count), arg_count);
                    frame = &self.frames[self.frame_count - 1];
                },
                .Invoke => {
                    const method = frame.readString();
                    const arg_count = frame.readByte();
                    self.invoke(method, arg_count) catch |_| {
                        return self.runtimeError("Could not invoke method.", .{});
                    };
                    frame = &self.frames[self.frame_count - 1];
                },
                .SuperInvoke => {
                    const method = frame.readString();
                    const arg_count = frame.readByte();
                    const superclass = &self.pop().Obj.data.Class;
                    try self.invokeFromClass(superclass, method, arg_count);
                    frame = &self.frames[self.frame_count - 1];
                },
                .Closure => {
                    const function = &frame.readConstant().Obj.data.Function;
                    var closure = ObjClosure.allocate(function);
                    self.push(closure.value());
                    for (closure.data.Closure.upvalues) |*u| {
                        const isLocal = frame.readByte() != 0;
                        const index = frame.readByte();
                        if (isLocal) {
                            u.* = self.captureUpvalue(&self.stack.items[frame.slots + index]);
                        } else {
                            u.* = frame.closure.data.Closure.upvalues[index];
                        }
                    }
                },
                .CloseUpvalue => {
                    self.closeUpvalues(&self.stack.items[self.stack.items.len - 2]);
                    _ = self.pop();
                },
                .Return => {
                    const result = self.pop();

                    self.closeUpvalues(&self.stack.items[frame.slots]);
                    self.frame_count -= 1;

                    if (self.frame_count == 0) {
                        _ = self.pop();
                        return;
                    }

                    self.stack.items.len -= self.stack.items.len - frame.slots;
                    self.push(result);
                    frame = &self.frames[self.frame_count - 1];
                },
                .Class => {
                    try self.createClass(frame.readString(), null);
                },
                // .Subclass => {
                //     const superclass = self.peek(0);
                //     if (superclass.Obj.data != .Class) {
                //         return self.runtimeError("Superclass must be a class", .{});
                //     }
                //     try self.createClass(frame.readString(), &superclass.Obj.data.Class);
                // },
                .Inherit => {
                    const superclassObject = self.peek(1).Obj.data;
                    if (superclassObject != .Class)
                        return self.runtimeError("Only instances have properties.", .{});

                    const superclass = &superclassObject.Class;
                    const subclass = &self.peek(0).Obj.data.Class;

                    for (superclass.methods.items()) |entry| {
                        try subclass.methods.put(entry.key, entry.value);
                    }
                    _ = self.pop();
                },
                .Method => {
                    try self.defineMethod(frame.readString());
                },
                .GetProperty => {
                    if (self.peek(0).Obj.data != .Instance) {
                        return self.runtimeError("Only instances have properties.", .{});
                    }

                    const instance = self.peek(0).Obj.data.Instance;
                    const name = frame.readString();

                    if (instance.fields.get(name.bytes)) |value| {
                        _ = self.pop(); // Instance.
                        self.push(value);
                    } else {
                        try self.bindMethod(self.peek(0).Obj, name);
                    }
                },
                .SetProperty => {
                    if (self.peek(1).Obj.data != .Instance) {
                        return self.runtimeError("Only instances have properties.", .{});
                    }

                    var instance = self.peek(1).Obj.data.Instance;
                    const name = frame.readString();

                    _ = try instance.fields.put(name.bytes, self.peek(0));

                    const value = self.pop();
                    _ = self.pop();
                    self.push(value);
                },
                .Super => {
                    const name = frame.readString();
                    const superclass = self.pop().Obj;
                    try self.bindMethod(superclass, name);
                },
                .NotEqual, .GreaterEqual, .LessEqual, .And, .Or => {
                    return self.runtimeError("Unused Opcode", .{});
                },
            }
        }
    }

    fn binary(self: *VM, value_type: ValueType, operator: OpCode) !void {
        var val: Value = undefined;
        switch (value_type) {
            .Number => {
                if (self.peek(0) != .Number or self.peek(1) != .Number) {
                    return self.runtimeError("Operands must be numbers", .{});
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
            .Bool => {
                if (self.peek(0) != .Bool or self.peek(1) != .Bool) {
                    return self.runtimeError("Operands must be boolean", .{});
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
        self.initString = null;
        self.freeObjects();
    }
};
