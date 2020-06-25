const std = @import("std");
const builtin = @import("builtin");

const lib = @import("./lib.zig");

const VM = @import("./vm.zig").VM;
const REPL = @import("./repl.zig").REPL;
const Compiler = @import("./compiler.zig").Compiler;
const Value = @import("./value.zig").Value;

pub const allocator = std.testing.allocator;
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

    try vm.initialize();
    defer vm.destroy();
    
    vm.defineNative("clock", lib.clockNative);

    switch(args_list.items.len) {
        1 => try runRepl(),
        2 => try runFile(args_list.items[1]),
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
    const stdIn = std.io.getStdIn();
    const stream = stdIn.inStream();
    while(true) {
        std.debug.warn("> ", .{}); 
        const source = try stream.readUntilDelimiterAlloc(allocator, '\n', 256);
        const result = vm.interpret(source);
        allocator.free(source);
    }
}

fn runFile(path: []const u8) !void {
    var dir: std.fs.Dir = undefined;
    const source = try dir.readFileAlloc(allocator, path, 4096);
    const result = vm.interpret(source);
}

fn showUsage() void {
    std.debug.warn("Usage: lang [path]\n", .{});
}
