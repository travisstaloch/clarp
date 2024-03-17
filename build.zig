const std = @import("std");

pub fn build(b: *std.Build) void {
    const mod = b.addModule("clarp", .{
        .root_source_file = .{ .path = "src/clarp.zig" },
    });

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const tests = b.addTest(.{
        .name = "tests",
        .root_source_file = .{ .path = "src/clarp.zig" },
        .target = target,
        .optimize = optimize,
    });
    tests.root_module.addImport("clarp", mod);
    const test_cmd = b.addRunArtifact(tests);
    test_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| test_cmd.addArgs(args);
    const run_step = b.step("test", "Run the tests");
    run_step.dependOn(&test_cmd.step);
}
