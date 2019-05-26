const std = @import("std");
const allocator = std.debug.global_allocator;

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

