const std = @import("std");
const builtin = @import("builtin");

pub const allocator = std.heap.wasm_allocator;

const VM = @import("./vm.zig").VM;
const REPL = @import("./repl.zig").REPL;
const Compiler = @import("./compiler.zig").Compiler;
const Value = @import("./value.zig").Value;

const example_file = @embedFile("../example/script.zox") ++ [_]u8{0};

export var vm: VM = undefined;

pub export fn _wasm_main(input_ptr: [*]const u8, input_len: usize, output_ptr: *[*]u8, output_len: *usize) bool {
    const input = input_ptr[0..input_len];

    vm = VM.create();
    defer vm.destroy();

    vm.interpret(input) catch {};

    const slice = vm.output.toSliceConst();
    var output = allocator.alloc(u8, slice.len) catch return false;
    std.mem.copy(u8, output, slice);

    output_ptr.* = output.ptr;
    output_len.* = output.len;

    return true;
}

pub export fn _wasm_alloc(len: usize) u32 {
    var buf = allocator.alloc(u8, len) catch return 0;
    return @intCast(@intFromPtr(buf.ptr));
}

pub export fn _wasm_dealloc(ptr: [*]const u8, len: usize) void {
    allocator.free(ptr[0..len]);
}
