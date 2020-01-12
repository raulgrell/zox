const std = @import("std");
const allocator = @import("root").allocator;

const Value = @import("./value.zig").Value;
const VM = @import("./vm.zig").VM;
const OpCode = @import("./vm.zig").OpCode;

extern var vm: VM;

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
        return @intCast(u8, self.constants.len - 1);
    }

    pub fn write(self: *Chunk, byte: u8, line: usize) !void {
        try self.code.append(byte);

        if (self.lines.len == 0 or self.lines.at(self.lines.len - 1).line != line)
            try self.lines.append(Line{ .line = @intCast(u32, line), .offset = byte });
    }

    pub fn disassemble(chunk: *Chunk, name: []const u8) void {
        std.debug.warn("== {} ==\n", .{name});
        var i: usize = 0;
        while (i < chunk.code.len) {
            i = disassembleInstruction(chunk, i);
        }
    }

    fn instructionAt(chunk: *const Chunk, offset: usize) OpCode {
        return @intToEnum(OpCode, chunk.code.at(offset));
    }

    fn getLine(chunk: *const Chunk, offset: usize) u32 {
        var start: usize = 0;
        var end: usize = chunk.lines.len;
        while (start < end) {
            const mid = (start + end) / 2;
            if (chunk.lines.at(mid).offset <= offset) {
                start = mid + 1;
            } else {
                end = mid;
            }
        }

        if (end == 0) return 0;
        return chunk.lines.at(end - 1).line;
    }

    fn disassembleInstruction(chunk: *const Chunk, offset: usize) usize {
        const line = chunk.getLine(offset);
        std.debug.warn("{}:{} | ", .{ offset, line });

        const instruction = chunk.instructionAt(offset);
        switch (instruction) {
            OpCode.Constant => return constantInstruction("Constant", chunk, offset),
            OpCode.Nil => return simpleInstruction("Nil", offset),
            OpCode.True => return simpleInstruction("True", offset),
            OpCode.False => return simpleInstruction("False", offset),
            OpCode.Pop => return simpleInstruction("Pop", offset),
            OpCode.GetLocal => return byteInstruction("GetLocal", chunk, offset),
            OpCode.SetLocal => return byteInstruction("SetLocal", chunk, offset),
            OpCode.GetUpvalue => return byteInstruction("GetUpvalue", chunk, offset),
            OpCode.SetUpvalue => return byteInstruction("SetUpvalue", chunk, offset),
            OpCode.CloseUpvalue => return simpleInstruction("CloseUpvalue", offset),
            OpCode.DefineGlobal => return constantInstruction("DefineGlobal", chunk, offset),
            OpCode.GetGlobal => return constantInstruction("GetGlobal", chunk, offset),
            OpCode.SetGlobal => return constantInstruction("SetGlobal", chunk, offset),
            OpCode.Equal => return simpleInstruction("Equal", offset),
            OpCode.Greater => return simpleInstruction("Greater", offset),
            OpCode.Less => return simpleInstruction("Less", offset),
            OpCode.Add => return simpleInstruction("Add", offset),
            OpCode.Subtract => return simpleInstruction("Subtract", offset),
            OpCode.Multiply => return simpleInstruction("Multiply", offset),
            OpCode.Divide => return simpleInstruction("Divide", offset),
            OpCode.Not => return simpleInstruction("Not", offset),
            OpCode.Negate => return simpleInstruction("Negate", offset),
            OpCode.Print => return simpleInstruction("Print", offset),
            OpCode.JumpIfFalse => return jumpInstruction("JumpIfFalse", 1, chunk, offset),
            OpCode.Jump => return jumpInstruction("Jump", 1, chunk, offset),
            OpCode.Loop => return jumpInstruction("Loop", -1, chunk, offset),
            OpCode.GetProperty => return constantInstruction("GetProperty", chunk, offset),
            OpCode.SetProperty => return constantInstruction("SetProperty", chunk, offset),
            OpCode.Return => return simpleInstruction("Return", offset),
            OpCode.Call => return byteInstruction("Call", chunk, offset),
            OpCode.Class => return constantInstruction("Class", chunk, offset),
            OpCode.Subclass => return constantInstruction("Subclass", chunk, offset),
            OpCode.Method => return constantInstruction("Method", chunk, offset),
            OpCode.Invoke => return invokeInstruction("Invoke", chunk, offset),
            OpCode.Super => return invokeInstruction("Super", chunk, offset),
            OpCode.Closure => {
                const constant = chunk.code.at(offset + 1);
                std.debug.warn("Closure {}: {}\n", .{ constant, chunk.constants.at(constant).toString() });
                const function = chunk.constants.at(constant).Obj.data.Function;
                var i: usize = 0;
                while (i < function.upvalueCount) : (i += 1) {
                    const isLocal = chunk.code.at(offset + 2 + i) != 0;
                    const index = chunk.code.at(offset + 3 + i);
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
        const argCount = chunk.code.at(offset + 1);
        const constant = chunk.code.at(offset + 2);
        std.debug.warn("{} ({} args) {} '{}'\n", .{
            name,
            argCount,
            constant,
            chunk.constants.at(constant).toString(),
        });
        return offset + 2;
    }

    fn jumpInstruction(name: []const u8, sign: i32, chunk: *const Chunk, offset: usize) usize {
        var jump = @intCast(i16, chunk.code.at(offset + 1)) << 8;
        jump |= @intCast(i16, chunk.code.at(offset + 2));
        std.debug.warn("{}: {} . {}\n", .{ name, offset, @intCast(i16, offset + 3) + sign * jump });
        return offset + 3;
    }

    fn byteInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
        const slot = chunk.code.at(offset + 1);
        std.debug.warn("{}: {}\n", .{ slot, name });
        return offset + 2;
    }

    fn constantInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
        const constant = chunk.code.at(offset + 1);
        std.debug.warn("{} {}: {}\n", .{ name, constant, chunk.constants.at(constant).toString() });
        return offset + 2;
    }

    fn simpleInstruction(name: []const u8, offset: usize) usize {
        std.debug.warn("{}\n", .{name});
        return offset + 1;
    }
};
