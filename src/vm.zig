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
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    JumpIfFalse,
    Jump,
    Call,
    Closure,
    Loop,
    Return,
};

pub const CallFrame = struct {
    function: *const ObjFunction,
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

    fn readConstant(frame: *CallFrame, chunk: *const Chunk) Value {
        return chunk.constants.at(frame.readByte());
    }

    fn readString(frame: *CallFrame, chunk: *const Chunk) ObjString {
        return chunk.constants.at(frame.readByte()).Obj.data.String;
    }
};

const FRAMES_MAX: u32 = 64;

pub const VM = struct {
    instance: Instance,

    frames: [FRAMES_MAX]CallFrame,
    frame_count: u32,

    stack: std.ArrayList(Value),
    strings: std.HashMap([]const u8, *Obj, ObjString.hashFn, ObjString.eqlFn),
    globals: std.HashMap([]const u8, Value, ObjString.hashFn, ObjString.eqlFn),
    objects: ?*Obj,
    output: *std.Buffer,
    output_stream: *std.io.BufferOutStream,

    pub var output_buffer: std.Buffer = undefined;
    pub var stdout: std.io.BufferOutStream = undefined;

    pub fn create() VM {
        output_buffer = std.Buffer.initSize(allocator, 0) catch unreachable;
        stdout = std.io.BufferOutStream.init(&output_buffer);

        return VM{
            .instance = Instance.create(),
            .frames = undefined,
            .frame_count = 0,
            .stack = std.ArrayList(Value).init(allocator),
            .strings = std.HashMap([]const u8, *Obj, ObjString.hashFn, ObjString.eqlFn).init(allocator),
            .globals = std.HashMap([]const u8, Value, ObjString.hashFn, ObjString.eqlFn).init(allocator),
            .objects = null,
            .output = &output_buffer,
            .output_stream = &stdout,
        };
    }

    fn interpret(self: *VM, source: []const u8) !void {
        const function = try self.instance.compile(source);
        self.push(function.value());
        try self.callValue(function.value(), 0);
        try self.run();
    }

    fn push(self: *VM, value: Value) void {
        self.stack.append(value) catch unreachable;
    }

    fn pop(self: *VM) Value {
        return self.stack.pop();
    }

    fn popFrame(self: *VM, frame: *CallFrame) void {}

    fn peek(self: *VM, distance: u32) Value {
        return self.stack.at(self.stack.len - 1 - distance);
    }

    fn slice(self: *VM, distance: u32) []Value {
        return self.stack.items[self.stack.len - 1 - distance ..];
    }

    fn resetStack(self: *VM) void {
        self.stack.resize(0) catch unreachable;
        self.frame_count = 0;
    }

    fn callValue(self: *VM, callee: Value, arg_count: u8) !void {
        switch (callee) {
            .Obj => |o| switch (o.data) {
                .Closure => return self.call(o, arg_count),
                .Function => return self.call(o, arg_count),
                .Native => |n| {
                    const result = n.function(self.slice(arg_count));
                    self.stack.len -= arg_count;
                    self.push(result);
                    return;
                },
                .String => {},
                else => unreachable,
            },
            .Bool, .Number, .Nil => {},
        }

        self.runtimeError("Can only call functions and classes.");
        return error.NotCallable;
    }

    fn call(self: *VM, function: *const Obj, arg_count: u8) !void {
        if (arg_count != function.data.Function.arity) {
            self.runtimeError("Expected {} arguments but got {}.", function.data.Function.arity, arg_count);
            return error.UnexpectedArgs;
        }

        if (self.frame_count == FRAMES_MAX) {
            self.runtimeError("Stack overflow.");
            return error.StackOverflow;
        }

        var frame = &self.frames[self.frame_count];
        frame.function = &function.data.Function;
        frame.ip = function.data.Function.chunk.ptr();
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

    fn runtimeError(self: *VM, comptime format: []const u8, args: ...) void {
        const frame = &self.frames[self.frame_count - 1];
        const instruction = @ptrToInt(frame.ip) - @ptrToInt(frame.function.chunk.ptr());
        const line = frame.function.chunk.getLine(instruction);

        self.output_stream.stream.print("Runtime error on line {}: ", line) catch unreachable;
        self.output_stream.stream.print(format, args) catch unreachable;
        self.output_stream.stream.print("\n") catch unreachable;

        var i = self.frame_count - 1;
        while (i > 0) : (i -= 1) {
            const prev_frame = &self.frames[i];
            const function = prev_frame.function;
            const offset = @ptrToInt(prev_frame.ip) - @ptrToInt(function.chunk.ptr());
            const prev_instr = @intCast(i32, offset) - (@intCast(i32, function.chunk.code.len) - 1);

            std.debug.warn("[line {}] in ", function.chunk.getLine(@intCast(usize, prev_instr)));

            if (function.name) |n| {
                std.debug.warn("{}()\n", n.bytes);
            } else {
                std.debug.warn("script\n");
            }
        }

        self.resetStack();
    }

    fn printDebug(self: *VM) !void {
        const frame = &self.frames[self.frame_count - 1];
        const instruction = @ptrToInt(frame.ip) - @ptrToInt(frame.function.chunk.ptr());
        for (self.stack.toSlice()) |s, i| {
            try self.output_stream.stream.print("[{}]\n", s.toString());
        }
        _ = frame.function.chunk.disassembleInstruction(instruction);
        try self.output_stream.stream.print("\n");
    }

    pub fn print(self: *VM, value: Value) void {
        const string = value.toString();
        self.output.append(string) catch unreachable;
    }

    pub fn puts(self: *VM, string: []const u8) void {
        self.output.append(string) catch unreachable;
    }

    pub fn flush(self: *VM) void {
        std.debug.warn("{}", self.output.toSliceConst());
        self.output.resize(0) catch unreachable;
    }

    fn run(self: *VM) !void {
        var frame = &self.frames[self.frame_count - 1];
        while (true) {
            if (verbose) try self.printDebug();
            const instruction = @intToEnum(OpCode, frame.readByte());
            switch (instruction) {
                OpCode.Constant => {
                    const constant = frame.readConstant(&frame.function.chunk);
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
                    const name = frame.readString(&frame.function.chunk);
                    const value = self.peek(0);
                    _ = try self.globals.put(name.bytes, value);
                    const popped = self.pop();
                },
                OpCode.GetGlobal => {
                    const name = frame.readString(&frame.function.chunk);
                    var value = self.globals.get(name.bytes) orelse {
                        self.runtimeError("Undefined variable '{}'.", name.bytes);
                        return error.RuntimeError;
                    };
                    self.push(value.value);
                },
                OpCode.SetGlobal => {
                    const name = frame.readString(&frame.function.chunk);
                    const value = self.peek(0);
                    _ = self.globals.put(name.bytes, value) catch {
                        self.runtimeError("Undefined variable '{}'.", name.bytes);
                        return error.RuntimeError;
                    };
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
                            self.runtimeError("Operand must be a number.");
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
                    const function = frame.readConstant(&frame.function.chunk);
                    const closure = ObjClosure.allocate(function.Obj.data.Closure.function);
                    self.push(closure.value());
                },
                OpCode.Return => {
                    const result = self.pop();
                    self.frame_count -= 1;

                    if (self.frame_count == 0)
                        return;

                    self.stack.len -= self.stack.len - frame.slots;
                    self.push(result);
                    frame = &self.frames[self.frame_count - 1];
                },
                else => {
                    self.runtimeError("Unknown instruction");
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
                    self.runtimeError("Operands must be numbers");
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
                    self.runtimeError("Operands must be boolean");
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
        const b = self.pop().Obj.data.String;
        const a = self.pop().Obj.data.String;

        const length = a.bytes.len + b.bytes.len;
        var bytes = try allocator.alloc(u8, length);
        std.mem.copy(u8, bytes[0..a.bytes.len], a.bytes);
        std.mem.copy(u8, bytes[a.bytes.len..], b.bytes);

        const result = ObjString.take(bytes);
        self.push(result.value());
    }

    pub fn freeObjects(self: *VM) void {
        var object = self.objects;
        while (object) |o| {
            const next = o.next;
            Obj.free(o);
            object = next;
        }
    }

    pub fn destroy(self: *VM) void {
        self.stack.deinit();
        self.globals.deinit();
        self.strings.deinit();
        self.output.deinit();
        self.freeObjects();
    }
};
