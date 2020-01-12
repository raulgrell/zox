const std = @import("std");
const builtin = @import("builtin");

const lib = @import("./lib.zig");

const VM = @import("./vm.zig").VM;
const REPL = @import("./repl.zig").REPL;
const Compiler = @import("./compiler.zig").Compiler;
const Value = @import("./value.zig").Value;

pub const allocator = std.debug.global_allocator;
const example_file = @embedFile("../example/script.zox") ++ [_]u8 {0};

export var vm: VM = undefined;

pub fn main() !void {
    var args_list = std.ArrayList([]const u8).init(allocator);
    defer args_list.deinit();

    var args_it = std.process.args();
    while (args_it.next(allocator)) | arg | {
        try args_list.append(try unwrapArg(arg));
    }

    vm = VM.create();
    defer vm.destroy();
    
    try vm.stack.ensureCapacity(1024);
    vm.defineNative("clock", lib.clockNative);

    switch(args_list.len) {
        1 => try runRepl(),
        2 => try runFile(args_list.at(1)),
        3 => try vm.interpret(example_file),
        else => showUsage()
    }
}

fn unwrapArg(arg: anyerror![]u8) ![]u8 {
    return arg catch |err| {
        std.debug.warn("Unable to parse command line: {}\n", .{err});
        return err;
    };
}

fn runRepl() !void {
    var line = [_] u8 {0} ** 256;
    while(true) {
        std.debug.warn("> ", .{});
        const source = try std.io.readLineSlice(line[0..]);
        const result = vm.interpret(source);
    }
}

fn runFile(path: []const u8) !void {
    const source = try std.io.readFileAlloc(allocator, path);
    const result = vm.interpret(source);
}

fn showUsage() void {
    std.debug.warn("Usage: lang [path]\n", .{});
}
