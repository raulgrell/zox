const std = @import("std");
const Allocator = std.mem.Allocator;

const VM = @import("./vm.zig").VM;
const lib = @import("./lib.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);

    try repl(allocator, args);
}

fn repl(allocator: Allocator, args: [][:0]u8) !void {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();
    const stdin = std.io.getStdIn().reader();

    var vm = try VM.create(allocator);

    try vm.init();
    defer vm.deinit();

    try lib.defineAllNatives(&vm);

    if (args.len > 1) {
        try vm.interpret(args[1]);
        return;
    }

    while (true) {
        try stdout.print("> ", .{});

        const source = stdin.readUntilDelimiterAlloc(allocator, '\n', 255) catch |err| {
            switch (err) {
                error.EndOfStream => return,
                else => return err,
            }
        };
        defer allocator.free(source);

        vm.interpret(source) catch |err| switch (err) {
            error.CompileError, error.RuntimeError => {
                try stderr.print("{}\n", .{err});
            },
            else => return err,
        };
    }
}
