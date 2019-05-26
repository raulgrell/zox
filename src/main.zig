const std = @import("std");
const builtin = @import("builtin");

const allocator = if (builtin.arch == builtin.Arch.wasm32) std.heap.wasm_allocator else std.debug.global_allocator;

const VM = @import("./vm.zig").VM;
const REPL = @import("./repl.zig").REPL;
const Compiler = @import("./compiler.zig").Compiler;

const example_file = @embedFile("../example/script.zag") ++ []u8 {0};

export var vm: *VM = undefined;

pub fn main() !void {
    var args_it = std.os.args();
    var args_list = std.ArrayList([]const u8).init(allocator);
    defer args_list.deinit();

    while (args_it.next(allocator)) | arg | {
        try args_list.append(try unwrapArg(arg));
    }

    var compiler = Compiler.create();
    var instance = VM.create(&compiler);
    defer instance.destroy();

    vm = &instance;

    switch(args_list.len) {
        1 => try vm.runRepl(),
        2 => try vm.runFile(args_list.at(1)),
        3 => try vm.interpret(example_file),
        else => showUsage()
    }
}

fn unwrapArg(arg: anyerror![]u8) ![]u8 {
    return arg catch |err| {
        std.debug.warn("Unable to parse command line: {}\n", err);
        return err;
    };
}

fn showUsage() void {
    std.debug.warn("Usage: lang [path]\n");
}

export fn _wasm_main(input_ptr: [*]const u8, input_len: usize, output_ptr: *[*]u8, output_len: *usize) bool {

    const input = input_ptr[0..input_len];

    var compiler = Compiler.create();
    var instance = VM.create(&compiler);
    defer instance.destroy();

    vm = &instance;

    vm.interpret(input) catch return false;

    const slice = vm.output.toSliceConst();
    var output = allocator.alloc(u8, slice.len) catch return false;
    std.mem.copy(u8, output, slice);

    output_ptr.* = output.ptr;
    output_len.* = output.len;
    
    return true;
}

export fn _wasm_alloc(len: usize) usize {
    var buf = allocator.alloc(u8, len) catch |err| return 0;
    return @ptrToInt(buf.ptr);
}

export fn _wasm_dealloc(ptr: [*]const u8, len: usize) void {
    allocator.free(ptr[0..len]);
}