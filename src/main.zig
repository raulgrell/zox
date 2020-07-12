const std = @import("std");
const builtin = @import("builtin");

const Allocator = std.mem.Allocator;

const Chunk = @import("./chunk.zig").Chunk;
const Compiler = @import("./compiler.zig").Compiler;
const REPL = @import("./repl.zig").REPL;
const Value = @import("./value.zig").Value;
const VM = @import("./vm.zig").VM;

const lib = @import("./lib.zig");

export var vm: VM = undefined;

pub const allocator = init: {
    if (comptime std.Target.current.isWasm()) {
        break :init std.heap.page_allocator;
    } else {
        break :init std.testing.allocator;
    }
};

pub fn main() !void {
    if (comptime std.Target.current.isWasm()) {
        try repl(allocator);
    } else {
        const args = try std.process.argsAlloc(allocator);
        defer std.process.argsFree(allocator, args);

        switch (args.len) {
            1 => try repl(),
            2 => try runFile(args[1]),
            3 => try runSource(args[2]),
            else => showUsage(),
        }
    }
}

fn repl() !void {
    const stdout = std.io.getStdOut().outStream();
    const stdin = std.io.getStdIn().inStream();

    vm = VM.create();
    try vm.initialize();
    defer vm.destroy();

    vm.defineNative("clock", lib.clockNative);

    while (true) {
        try stdout.print("> ", .{});
        const source = try stdin.readUntilDelimiterAlloc(allocator, '\n', 255);
        vm.interpret(source) catch |err| switch (err) {
            error.CompileError, error.RuntimeError => {},
            else => return err,
        };
        allocator.free(source);
    }
}

fn runFile(path: []const u8) !void {
    vm = VM.create();
    try vm.initialize();
    defer vm.destroy();

    vm.defineNative("clock", lib.clockNative);

    std.debug.warn("Opening: {}\n", .{path});

    const source = try std.fs.cwd().readFileAlloc(allocator, path, 1 * 1024 * 1024);
    defer allocator.free(source);

    vm.interpret(source) catch |err| switch (err) {
        error.CompileError => std.process.exit(65),
        error.RuntimeError => std.process.exit(70),
        else => return err,
    };
}


fn runSource(source: []const u8) !void {
    vm = VM.create();
    try vm.initialize();
    defer vm.destroy();

    vm.defineNative("clock", lib.clockNative);

    std.debug.warn("> {}\n", .{source});

    vm.interpret(source) catch |err| switch (err) {
        error.CompileError => std.process.exit(65),
        error.RuntimeError => std.process.exit(70),
        else => return err,
    };
}

fn showUsage() void {
    std.debug.warn("Usage: zox [path]\n", .{});
    std.process.exit(64);
}
