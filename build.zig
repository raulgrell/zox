const std = @import("std");
const tracy = @import("./build_tracy.zig");

const Builder = std.build.Builder;

pub fn build(b: *Builder) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const exe = b.addExecutable(.{
        .name = "zox",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    tracy.link(b, exe, "./tracy");
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    b.installDirectory(std.build.InstallDirectoryOptions{
        .source_dir = .{ .path = "src/lib" },
        .install_dir = .bin,
        .install_subdir = "lib",
        .exclude_extensions = &[_][]const u8{"zig"},
    });
}
