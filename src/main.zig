const std = @import("std");
const builtin = @import("builtin");

const Allocator = std.mem.Allocator;

const Value = @import("./value.zig").Value;
const VM = @import("./vm.zig").VM;

const lib = @import("./lib.zig");
const debug = @import("./debug.zig");
const cli = @import("./deps/args.zig");

const tracy_enabled = false;

const Spec = struct {
    debug: bool = false,
    pub const shorthands = .{
        .d = "debug",
    };
};

const Verb = union(enum) {
    // Run a file
    run: struct {},
    // Run a command/evaluate an expression
    cmd: struct {},
    // Run a REPL
    repl: struct {
        then: ?[]const u8 = null,
    },
    @"test": struct {
        // Will run the test on all files in a directory
        dir: ?[]const u8 = null,
        include: ?[]const u8 = null,
        exclude: ?[]const u8 = null,
    },
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    if (builtin.target.isWasm()) {
        try repl(allocator);
    } else {
        const args = try cli.parseWithVerbForCurrentProcess(Spec, Verb, allocator, .print);
        defer args.deinit();

        debugArgs(args);

        if (args.verb) |v| {
            switch (v) {
                .run => switch (args.positionals.len) {
                    0 => try runFile(allocator, "main.zox"),
                    1 => try runFile(allocator, args.positionals[0]),
                    else => try showUsage(),
                },
                .cmd => switch (args.positionals.len) {
                    0 => try runSource(allocator, args.positionals[0]),
                    else => try showUsage(),
                },
                .repl => switch (args.positionals.len) {
                    0 => try repl(allocator),
                    else => try showUsage(),
                },
                .@"test" => {
                    switch (args.positionals.len) {
                        //0 => try testFile(allocator, "main.zox"),
                        //1 => try testFile(allocator, args.positionals[0]),
                        else => try showUsage(),
                    }
                },
            }
        } else {
            try showUsage();
        }
    }
}

fn repl(allocator: Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    var vm = VM.create(allocator);

    try vm.init();
    defer vm.deinit();

    try vm.defineNative("clock", lib.stdModule.sys.clock);

    while (true) {
        try stdout.print("> ", .{});
        const source = try stdin.readUntilDelimiterAlloc(allocator, '\n', 255);
        defer allocator.free(source);
        vm.interpret(source) catch |err| switch (err) {
            error.CompileError, error.RuntimeError => {},
            else => return err,
        };
    }
}

fn runFile(allocator: Allocator, path: []const u8) !void {
    const stderr = std.io.getStdErr().writer();

    var vm = VM.create(allocator);

    try vm.init();
    defer vm.deinit();

    try vm.defineNative("clock", lib.stdModule.sys.clock);

    std.debug.print("Opening: {s}\n", .{path});

    const source = std.fs.cwd().readFileAlloc(allocator, path, 4 * 1024) catch |err| switch (err) {
        error.FileNotFound => {
            try stderr.print("File not found: {s}\n", .{path});
            return;
        },
        else => return err,
    };
    defer allocator.free(source);

    vm.interpret(source) catch |err| switch (err) {
        error.CompileError => std.process.exit(65),
        error.RuntimeError => std.process.exit(70),
        else => return err,
    };
}

fn runSource(allocator: Allocator, source: []const u8) !void {
    var vm = VM.create(allocator);

    try vm.init();
    defer vm.deinit();

    try vm.defineNative("clock", lib.stdModule.sys.clock);

    std.debug.print("> {s}\n", .{source});

    vm.interpret(source) catch |err| switch (err) {
        error.CompileError => std.process.exit(65),
        error.RuntimeError => std.process.exit(70),
        else => return err,
    };
}

fn debugArgs(args: cli.ParseArgsResult(Spec, Verb)) void {
    std.debug.print("{s}", .{args.executable_name});
    inline for (std.meta.fields(@TypeOf(args.options))) |fld| {
        std.debug.print(" --{s} {any}", .{ fld.name, @field(args.options, fld.name) });
    }

    if (args.verb) |v| {
        std.debug.print(" {s}", .{@tagName(v)});
        switch (v) {
            .run => |opts| {
                inline for (std.meta.fields(@TypeOf(opts))) |fld| {
                    std.debug.print(" --{s} {any}", .{ fld.name, @field(opts, fld.name) });
                }
            },
            .cmd => |opts| {
                inline for (std.meta.fields(@TypeOf(opts))) |fld| {
                    std.debug.print(" --{s} {any}", .{ fld.name, @field(opts, fld.name) });
                }
            },
            .repl => |opts| {
                inline for (std.meta.fields(@TypeOf(opts))) |fld| {
                    std.debug.print(" --{s} {any}", .{ fld.name, @field(opts, fld.name) });
                }
            },
            .@"test" => |opts| {
                inline for (std.meta.fields(@TypeOf(opts))) |fld| {
                    std.debug.print(" --{s} {any}", .{ fld.name, @field(opts, fld.name) });
                }
            },
        }
    }

    if (args.positionals.len > 0) std.debug.print(" --", .{});
    for (args.positionals) |arg| std.debug.print(" '{s}'", .{arg});
    std.debug.print("\n", .{});
}

fn showUsage() !void {
    const stderr = std.io.getStdErr().writer();
    try stderr.print(
        \\  Usage:
        \\      zox <command> [options...] [-- paths...]"
        \\  Options:
        \\      --debug                 # Display debug information
        \\  Commands:
        \\      cmd
        \\      repl [path]             # Open a repl
        \\      run  [path]             # Run a file
        \\      test [paths..]          # Test a file
    , .{});
    std.process.exit(64);
}
