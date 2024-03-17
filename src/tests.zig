const std = @import("std");
const clarp = @import("clarp");
const Options = clarp.Options;

const TestCmd = clarp.Command(union(enum) {
    decode: struct { []const u8 },
    info: Filepath,
    handshake: struct { filepath: []const u8, peer_address: []const u8 },
    download: struct {
        outpath: []const u8,
        piece_index: u32,
        pub const options = Options(@This()){
            .outpath = .{
                .alias = "-o",
                .desc = "file path to output file",
            },
        };
    },
    defaults: struct {
        bar: []const u8 = "bar",
        baz: bool = false,
        int: i32 = -99,
        blip: bool,
        flip: bool = true,
        enyum: enum { delish, tasty, yuck } = .tasty,
    },
    run: union(enum) {
        a: u8,
        b: i8,
        c: Filepath,
        d,
        e: f32,
        f: enum { a, b },
        arr: [20]u8,
    },
    has_underscores: []const u8,
    opt: ?[]const u8,

    const Filepath = struct { filepath: []const u8 };
});

const Root = std.meta.FieldType(TestCmd, .root);
fn expect(args: []const []const u8, expected: Root) !void {
    const x = try TestCmd.parse(args);
    // exercise dump() and printHelp() to catch compile errors
    try TestCmd.printHelp(Root, "", .{}, std.io.null_writer, 0);
    try TestCmd.dump(x, "", .{}, std.io.null_writer, 0);
    try x.dump("", .{}, std.io.null_writer, 0);
    return testing.expectEqualDeep(expected, x.root);
}

const testing = std.testing;
test "Command" {
    try testing.expectError(error.UnknownCommand, TestCmd.parse(
        &.{ "exe", "asfd" },
    ));
    try testing.expectError(error.ExtraArgs, TestCmd.parse(
        &.{ "exe", "decode", "1", "2", "3" },
    ));
    try testing.expectError(error.NotEnoughArgs, TestCmd.parse(&.{"exe"}));

    try expect(&.{ "exe", "decode", "foo" }, .{ .decode = .{"foo"} });
    try expect(&.{ "exe", "info", "foo" }, .{ .info = .{ .filepath = "foo" } });
    try expect(
        &.{ "exe", "info", "--filepath", "foo" },
        .{ .info = .{ .filepath = "foo" } },
    );
    try expect(
        &.{ "exe", "handshake", "foo", "bar" },
        .{ .handshake = .{ .filepath = "foo", .peer_address = "bar" } },
    );
    try expect(
        &.{ "exe", "handshake", "--peer-address", "foo", "bar" },
        .{ .handshake = .{ .filepath = "bar", .peer_address = "foo" } },
    );
    try expect(
        &.{ "exe", "handshake", "--peer-address", "foo", "--filepath", "bar" },
        .{ .handshake = .{ .filepath = "bar", .peer_address = "foo" } },
    );
    try expect(
        &.{ "exe", "download", "-o", "foo", "22" },
        .{ .download = .{ .piece_index = 22, .outpath = "foo" } },
    );
    try expect(
        &.{ "exe", "download", "--outpath", "foo", "22" },
        .{ .download = .{ .piece_index = 22, .outpath = "foo" } },
    );
    try expect(
        &.{ "exe", "defaults" },
        .{ .defaults = .{ .blip = false } },
    );
    try expect(
        &.{ "exe", "defaults", "--int", "42" },
        .{ .defaults = .{ .int = 42, .blip = false } },
    );
    try expect(
        &.{ "exe", "defaults", "--baz" },
        .{ .defaults = .{ .baz = true, .blip = false } },
    );
    try expect(
        &.{ "exe", "defaults", "a", "true" },
        .{ .defaults = .{ .bar = "a", .baz = true, .blip = false } },
    );
    try expect(
        &.{ "exe", "defaults", "a", "true", "42" },
        .{ .defaults = .{ .bar = "a", .baz = true, .int = 42, .blip = false } },
    );
    try expect(
        &.{ "exe", "defaults", "a", "false", "42" },
        .{ .defaults = .{ .bar = "a", .baz = false, .int = 42, .blip = false } },
    );
    try expect(
        &.{ "exe", "defaults", "--enyum", "delish" },
        .{ .defaults = .{ .enyum = .delish, .blip = false } },
    );
    try expect(
        &.{ "exe", "run", "a", "1" },
        .{ .run = .{ .a = 1 } },
    );
    try expect(
        &.{ "exe", "run", "b", "2" },
        .{ .run = .{ .b = 2 } },
    );
    try expect(
        &.{ "exe", "run", "c", "foo" },
        .{ .run = .{ .c = .{ .filepath = "foo" } } },
    );
    try expect(
        &.{ "exe", "run", "c", "--filepath", "foo" },
        .{ .run = .{ .c = .{ .filepath = "foo" } } },
    );
    try expect(
        &.{ "exe", "run", "d" },
        .{ .run = .d },
    );
    try expect(
        &.{ "exe", "run", "e", "0.999" },
        .{ .run = .{ .e = 0.999 } },
    );
    try expect(
        &.{ "exe", "run", "f", "b" },
        .{ .run = .{ .f = .b } },
    );
    {
        const x = try TestCmd.parse(&.{ "exe", "run", "arr", "foo" });
        try testing.expectEqualStrings("foo", x.root.run.arr[0..3]);
    }
    try expect(
        &.{ "exe", "has-underscores", "foo" },
        .{ .has_underscores = "foo" },
    );
    try expect(&.{ "exe", "opt", "null" }, .{ .opt = null });
    try expect(&.{ "exe", "opt", "foo" }, .{ .opt = "foo" });
}
