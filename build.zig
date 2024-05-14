const std = @import("std");

pub fn build(b: *std.Build) void {
    const build_options = b.addOptions();
    build_options.addOption(
        bool,
        "show_debug",
        b.option(bool, "show-debug", "show debug output") orelse false,
    );

    const mod = b.addModule("clarp", .{
        .root_source_file = .{ .path = "src/clarp.zig" },
        .imports = &.{.{
            .name = "build-options",
            .module = build_options.createModule(),
        }},
    });

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const tests = b.addTest(.{
        .name = "tests",
        .root_source_file = .{ .path = "src/tests.zig" },
        .target = target,
        .optimize = optimize,
        .filter = b.option([]const u8, "test-filter", "test filter"),
    });
    tests.root_module.addImport("clarp", mod);
    const test_cmd = b.addRunArtifact(tests);
    test_cmd.has_side_effects = true;
    test_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| test_cmd.addArgs(args);
    const run_step = b.step("test", "Run the tests");
    run_step.dependOn(&test_cmd.step);
    b.installArtifact(tests);

    const testexe = b.addExecutable(.{
        .name = "testexe",
        .root_source_file = .{ .path = "src/test-demo.zig" },
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(testexe);
    testexe.root_module.addImport("clarp", mod);
    const test_run = b.addRunArtifact(testexe);
    if (b.args) |args| test_run.addArgs(args);
    const test_run_step = b.step("test-demo", "Run the test demo exe");
    test_run_step.dependOn(&test_run.step);
}
