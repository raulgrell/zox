const std = @import("std");
const Builder = std.build.Builder;
const InstallDirectoryOptions = std.build.InstallDirectoryOptions;

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("zox", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    b.default_step.dependOn(&exe.step);

    b.installDirectory(InstallDirectoryOptions{
        .source_dir = "lib",
        .install_dir = .Bin,
        .install_subdir = "lib",
        .exclude_extensions = &[_][]const u8{ "test.zig", "README.md" },
    });

    b.installArtifact(exe);
}
