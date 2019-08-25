const std = @import("std");
const builtin = @import("builtin");

pub const allocator = if (builtin.arch == builtin.Arch.wasm32) 
    std.heap.wasm_allocator else std.debug.global_allocator;

const VM = @import("./vm.zig").VM;
const REPL = @import("./repl.zig").REPL;
const Compiler = @import("./compiler.zig").Compiler;
const Value = @import("./value.zig").Value;

const example_file = @embedFile("../example/script.zag") ++ [_]u8 {0};

export var vm: VM = undefined;

fn clockNative(args: []Value) Value {
    return Value { .Number = @intToFloat(f64, std.time.milliTimestamp()) / 1000 };
}

pub fn main() !void {
    var args_list = std.ArrayList([]const u8).init(allocator);
    defer args_list.deinit();

    var args_it = std.process.args();
    while (args_it.next(allocator)) | arg | {
        try args_list.append(try unwrapArg(arg));
    }

    vm = VM.create();
    vm.instance.init();

    vm.defineNative("clock", clockNative);

    defer vm.destroy();

    switch(args_list.len) {
        1 => try runRepl(),
        2 => try runFile(args_list.at(1)),
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

fn runRepl() !void {
    var line = [_] u8 {0} ** 256;
    while(true) {
        std.debug.warn( "> ");
        const source = try std.io.readLineSlice(line[0..]);
        const result = vm.interpret(source);
        vm.flush();
    }
}

fn runFile(path: []const u8) !void {
    const source = try std.io.readFileAlloc(allocator, path);
    const result = vm.interpret(source);
    vm.flush();
}

fn showUsage() void {
    std.debug.warn("Usage: lang [path]\n");
}

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
    var buf = allocator.alloc(u8, len) catch |err| return 0;
    return @intCast(u32, @ptrToInt(buf.ptr));
}

pub export fn _wasm_dealloc(ptr: [*]const u8, len: usize) void {
    allocator.free(ptr[0..len]);
}