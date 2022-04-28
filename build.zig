const std = @import("std");
const tracy = @import("./build_tracy.zig");

const Builder = std.build.Builder;

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("zox", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.setOutputDir(".");
    tracy.link(b, exe, "./tracy");
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    b.installDirectory(std.build.InstallDirectoryOptions{
        .source_dir = "src/lib",
        .install_dir = .bin,
        .install_subdir = "lib",
        .exclude_extensions = &[_][]const u8{"zig"},
    });
}
