const std = @import("std");
const allocator = @import("root").allocator;

const Value = @import("./value.zig").Value;
const VM = @import("./vm.zig").VM;

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
    GetSuper,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    JumpIfFalse,
    Jump,
    Call,
    Closure,
    Invoke,
    SuperInvoke,
    Method,
    Class,
    Inherit,
    // Subclass,
    Loop,
    Return,
};

const Line = struct {
    line: u32,
    offset: u32,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    lines: std.ArrayList(Line),
    constants: std.ArrayList(Value),

    pub fn init() Chunk {
        return Chunk{
            .code = std.ArrayList(u8).init(allocator),
            .lines = std.ArrayList(Line).init(allocator),
            .constants = std.ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn destroy(self: *Chunk) void {
        self.code.len = 0;
        self.constants.len = 0;
        self.lines.len = 0;
    }

    pub fn ptr(self: *const Chunk) [*]u8 {
        return self.code.items.ptr;
    }

    pub fn addConstant(self: *Chunk, value: Value) u8 {
        vm.push(value);
        self.constants.append(value) catch unreachable;
        _ = vm.pop();
        return @intCast(u8, self.constants.items.len - 1);
    }

    pub fn write(self: *Chunk, byte: u8, line: usize) !void {
        try self.code.append(byte);

        if (self.lines.items.len == 0 or self.lines.items[self.lines.items.len - 1].line != line)
            try self.lines.append(Line{ .line = @intCast(u32, line), .offset = byte });
    }

    pub fn disassemble(chunk: *Chunk, name: []const u8) void {
        std.debug.warn("== {} ==\n", .{name});
        var i: usize = 0;
        while (i < chunk.code.items.len) {
            i = disassembleInstruction(chunk, i);
        }
    }

    fn instructionAt(chunk: *const Chunk, offset: usize) OpCode {
        return @intToEnum(OpCode, chunk.code.items[offset]);
    }

    pub fn getLine(chunk: *const Chunk, offset: usize) u32 {
        var start: usize = 0;
        var end: usize = chunk.lines.items.len;
        while (start < end) {
            const mid = (start + end) / 2;
            if (chunk.lines.items[mid].offset <= offset) {
                start = mid + 1;
            } else {
                end = mid;
            }
        }

        if (end == 0) return 0;
        return chunk.lines.items[end - 1].line;
    }

    pub fn disassembleInstruction(chunk: *const Chunk, offset: usize) usize {
        const line = chunk.getLine(offset);
        std.debug.warn("{}:{} | ", .{ offset, line });

        const instruction = chunk.instructionAt(offset);
        switch (instruction) {
            .Constant => return constantInstruction("Constant", chunk, offset),
            .Nil => return simpleInstruction("Nil", offset),
            .True => return simpleInstruction("True", offset),
            .False => return simpleInstruction("False", offset),
            .Pop => return simpleInstruction("Pop", offset),
            .GetLocal => return byteInstruction("GetLocal", chunk, offset),
            .SetLocal => return byteInstruction("SetLocal", chunk, offset),
            .GetUpvalue => return byteInstruction("GetUpvalue", chunk, offset),
            .SetUpvalue => return byteInstruction("SetUpvalue", chunk, offset),
            .CloseUpvalue => return simpleInstruction("CloseUpvalue", offset),
            .DefineGlobal => return constantInstruction("DefineGlobal", chunk, offset),
            .GetGlobal => return constantInstruction("GetGlobal", chunk, offset),
            .SetGlobal => return constantInstruction("SetGlobal", chunk, offset),
            .GetSuper => return constantInstruction("GetSuper", chunk, offset),
            .Equal => return simpleInstruction("Equal", offset),
            .Greater => return simpleInstruction("Greater", offset),
            .Less => return simpleInstruction("Less", offset),
            .Add => return simpleInstruction("Add", offset),
            .Subtract => return simpleInstruction("Subtract", offset),
            .Multiply => return simpleInstruction("Multiply", offset),
            .Divide => return simpleInstruction("Divide", offset),
            .Not => return simpleInstruction("Not", offset),
            .Negate => return simpleInstruction("Negate", offset),
            .Print => return simpleInstruction("Print", offset),
            .JumpIfFalse => return jumpInstruction("JumpIfFalse", 1, chunk, offset),
            .Jump => return jumpInstruction("Jump", 1, chunk, offset),
            .Loop => return jumpInstruction("Loop", -1, chunk, offset),
            .GetProperty => return constantInstruction("GetProperty", chunk, offset),
            .SetProperty => return constantInstruction("SetProperty", chunk, offset),
            .Return => return simpleInstruction("Return", offset),
            .Call => return byteInstruction("Call", chunk, offset),
            .Class => return simpleInstruction("Class", offset),
            .Inherit => return simpleInstruction("Inherit", offset),
            // .Subclass => return constantInstruction("Subclass", chunk, offset),
            .Method => return constantInstruction("Method", chunk, offset),
            .Invoke => return invokeInstruction("Invoke", chunk, offset),
            .SuperInvoke => return invokeInstruction("SuperInvoke", chunk, offset),
            .Closure => {
                const constant = chunk.code.items[offset + 1];
                std.debug.warn("Closure {}: {}\n", .{ constant, chunk.constants.items[constant].toString() });
                const function = chunk.constants.items[constant].Obj.asFunction();
                var i: usize = 0;
                while (i < function.upvalueCount) : (i += 1) {
                    const isLocal = chunk.code.items[offset + 2 + i] != 0;
                    const index = chunk.code.items[offset + 3 + i];
                    const text = if (isLocal) "Local" else "Upvalue";
                    std.debug.warn("{}:{} | {} {}\n", .{ offset + 2 + 2 * i, line, text, index });
                }

                return offset + 2 + 2 * i;
            },
            else => {
                std.debug.warn("Unknown opcode: {}\n", .{instruction});
                return offset + 1;
            },
        }
    }

    pub fn invokeInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
        const argCount = chunk.code.items[offset + 1];
        const constant = chunk.code.items[offset + 2];
        std.debug.warn("{} ({} args) {} '{}'\n", .{
            name,
            argCount,
            constant,
            chunk.constants.items[constant].toString(),
        });
        return offset + 3;
    }

    fn jumpInstruction(name: []const u8, sign: i32, chunk: *const Chunk, offset: usize) usize {
        var jump = @intCast(i16, chunk.code.items[offset + 1]) << 8;
        jump |= @intCast(i16, chunk.code.items[offset + 2]);
        std.debug.warn("{}: {} . {}\n", .{
            name,
            offset,
            @intCast(i16, offset + 3) + sign * jump,
        });
        return offset + 3;
    }

    fn byteInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
        const slot = chunk.code.items[offset + 1];
        std.debug.warn("{}: {}\n", .{
            name,
            slot,
        });
        return offset + 2;
    }

    fn constantInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
        const constant = chunk.code.items[offset + 1];
        std.debug.warn("{} {}: {}\n", .{
            name,
            constant,
            chunk.constants.items[constant].toString(),
        });
        return offset + 2;
    }

    fn simpleInstruction(name: []const u8, offset: usize) usize {
        std.debug.warn("{}\n", .{name});
        return offset + 1;
    }
};
