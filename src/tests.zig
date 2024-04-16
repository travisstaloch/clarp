const std = @import("std");
const clarp = @import("clarp");
const Options = clarp.Options;

const TestParser = clarp.Parser(union(enum) {
    decode: struct { []const u8 },
    info: Filepath,
    handshake: struct { filepath: []const u8, peer_address: []const u8 },
    download: struct {
        outpath: []const u8,
        piece_index: u32,
        pub const clarp_options = Options(@This()){
            .fields = .{
                .outpath = .{
                    .short = "-o",
                    .desc = "file path to output file",
                },
                .piece_index = .{
                    .help =
                    \\  help for this field
                    \\      blah blah
                    ,
                },
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
    opt: ?[]const u8,
    tuple: struct { u8, []const u8 },

    const Filepath = struct { filepath: []const u8 };
}, .{});

fn ExpectFn(comptime P: type) type {
    return fn (args: []const []const u8, expected: P.Root) anyerror!void;
}

fn expectFn(comptime P: type) ExpectFn(P) {
    return struct {
        fn func(args: []const []const u8, expected: P.Root) anyerror!void {
            const x = try P.parse(args, .{});
            // exercise dump() and help() to catch compile errors
            try P.dump(x, "", .{}, std.io.null_writer, 0);
            try x.dump("", .{}, std.io.null_writer, 0);
            try std.io.null_writer.print("{help}", .{x});
            try std.io.null_writer.print("{}", .{x});
            return testing.expectEqualDeep(expected, x.root);
        }
    }.func;
}

const testing = std.testing;
const talloc = testing.allocator;
const exe_path = "/path/to/exe";

test "Command" {
    try testing.expectError(error.UnknownCommand, TestParser.parse(&.{ "exe", "asdf" }, .{}));
    try testing.expectError(error.ExtraArgs, TestParser.parse(&.{ "exe", "decode", "1", "2", "3" }, .{}));
    try testing.expectError(error.NotEnoughArgs, TestParser.parse(&.{"exe"}, .{}));
    try testing.expectError(error.UnknownOption, TestParser.parse(&.{ "exe", "handshake", "foo", "bar" }, .{}));
    try testing.expectError(error.UnknownOption, TestParser.parse(&.{ "exe", "info", "foo" }, .{}));
    try testing.expectError(error.UnknownOption, TestParser.parse(&.{ "exe", "handshake", "--peer_address", "foo", "bar" }, .{}));
    try testing.expectError(error.UnknownOption, TestParser.parse(&.{ "exe", "defaults", "a", "true" }, .{}));
    try testing.expectError(error.UnknownOption, TestParser.parse(&.{ "exe", "defaults", "a", "true", "42" }, .{}));
    try testing.expectError(error.UnknownOption, TestParser.parse(&.{ "exe", "defaults", "a", "false", "42" }, .{}));
    try testing.expectError(error.UnknownOption, TestParser.parse(&.{ "exe", "run", "c", "foo" }, .{}));
    try testing.expectError(error.NotEnoughArgs, TestParser.parse(&.{ "exe", "tuple" }, .{}));
    try testing.expectError(error.ExtraArgs, TestParser.parse(&.{ "exe", "tuple", "1", "2", "3" }, .{}));

    const expect = expectFn(TestParser);
    try expect(&.{ "exe", "decode", "foo" }, .{ .decode = .{"foo"} });
    try expect(
        &.{ "exe", "info", "--filepath", "foo" },
        .{ .info = .{ .filepath = "foo" } },
    );

    try expect(
        &.{ "exe", "handshake", "--peer_address", "foo", "--filepath", "bar" },
        .{ .handshake = .{ .filepath = "bar", .peer_address = "foo" } },
    );
    try expect(
        &.{ "exe", "download", "-o", "foo", "--piece_index", "22" },
        .{ .download = .{ .piece_index = 22, .outpath = "foo" } },
    );
    try expect(
        &.{ "exe", "download", "--outpath", "foo", "--piece_index", "22" },
        .{ .download = .{ .piece_index = 22, .outpath = "foo" } },
    );
    try expect(
        &.{ "exe", "download", "--piece_index", "22", "-o", "foo" },
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
        const x = try TestParser.parse(&.{ "exe", "run", "arr", "foo" }, .{});
        try testing.expectEqualStrings("foo", x.root.run.arr[0..3]);
    }
    try expect(&.{ "exe", "opt", "null" }, .{ .opt = null });
    try expect(&.{ "exe", "opt", "foo" }, .{ .opt = "foo" });
    try expect(&.{ "exe", "tuple", "1", "2" }, .{ .tuple = .{ 1, "2" } });
}

test "flags" {
    const P = clarp.Parser(struct {
        a: bool,
        b: u8,
    }, .{});
    const expect = expectFn(P);
    try expect(&.{ exe_path, "--a", "--b", "1" }, .{ .a = true, .b = 1 });
    try expect(&.{ exe_path, "--b", "1" }, .{ .a = false, .b = 1 });
}

test "overrides" {
    const Ctx = struct { foo: u8 = 0, bar: u8 = 0 };
    var ctx = Ctx{};
    const P = clarp.Parser(struct {
        pub const clarp_options = Options(@This()){
            .overrides = struct {
                pub fn @"--foo"(args: *[]const []const u8, user_ctx: ?*anyopaque) void {
                    @as(*Ctx, @ptrCast(user_ctx)).foo =
                        std.fmt.parseInt(u8, args.*[0], 10) catch unreachable;
                    args.* = args.*[1..];
                }
                pub fn @"--"(args: *[]const []const u8, user_ctx: ?*anyopaque) void {
                    testing.expectEqualStrings("bar", args.*[0]) catch unreachable;
                    args.* = args.*[1..];
                    @as(*Ctx, @ptrCast(user_ctx)).bar =
                        std.fmt.parseInt(u8, args.*[0], 10) catch unreachable;
                    args.* = args.*[1..];
                }
            },
        };
    }, .{});

    _ = try P.parse(&.{ "exe", "--foo", "99", "--", "bar", "100" }, .{ .user_ctx = &ctx });

    try testing.expectEqual(99, ctx.foo);
    try testing.expectEqual(100, ctx.bar);
}

const SimpleOptions = clarp.Parser(struct {
    opt1: []const u8,
    opt2: enum { a, b } = .a,

    pub const clarp_options = clarp.Options(@This()){
        .fields = .{
            .opt1 = .{
                .short = "-o1",
                .desc = "First option description.",
            },
            .opt2 = .{
                .desc = "Second option description.",
            },
        },
        .derive_short_names = true,
    };
}, .{ .help_description_start_column = 25 });

test SimpleOptions {
    const expect = expectFn(SimpleOptions);
    try expect(&.{ exe_path, "--opt1", "foo" }, .{ .opt1 = "foo", .opt2 = .a });
}

test "derive_short_names - struct" {
    const P = clarp.Parser(struct {
        xxx: u8,
        xyy: u8,
        xxy: u8,
        yyy: u8,

        pub const clarp_options = Options(@This()){ .derive_short_names = true };
    }, .{});
    const expect = expectFn(P);
    try expect(
        &.{ exe_path, "-y", "4", "-x", "1", "-xy", "2", "-xx", "3" },
        .{ .xxx = 1, .xyy = 2, .xxy = 3, .yyy = 4 },
    );
}

test "derive_short_names - union" {
    const P = clarp.Parser(union(enum) {
        xxx: u8,
        xyy: u8,
        xxy: u8,
        yyy: u8,

        pub const clarp_options = Options(@This()){ .derive_short_names = true };
    }, .{});
    const expect = expectFn(P);
    try expect(&.{ exe_path, "x", "1" }, .{ .xxx = 1 });
    try expect(&.{ exe_path, "xy", "2" }, .{ .xyy = 2 });
    try expect(&.{ exe_path, "xx", "3" }, .{ .xxy = 3 });
    try expect(&.{ exe_path, "y", "4" }, .{ .yyy = 4 });
}

test "struct end mark" {
    { // default end mark
        const P = clarp.Parser(struct {
            a: struct {
                b: u8 = 1,
            } = .{},
            b: u8 = 2,
        }, .{});
        const expect = expectFn(P);
        try expect(
            &.{ exe_path, "--a", "--b", "20", "--b", "30" },
            .{ .a = .{ .b = 20 }, .b = 30 },
        );
        try expect(&.{ exe_path, "--a", "--end-a", "--b", "20" }, .{ .b = 20 });
    }
    { // custom end mark
        const P = clarp.Parser(struct {
            a: struct {
                b: u8 = 1,
                pub const clarp_options = Options(@This()){ .end_mark = "--my-end-a-mark" };
            } = .{},
            b: u8 = 2,
        }, .{});
        const expect = expectFn(P);
        try expect(
            &.{ exe_path, "--a", "--b", "20", "--b", "30" },
            .{ .a = .{ .b = 20 }, .b = 30 },
        );
        try expect(&.{ exe_path, "--a", "--my-end-a-mark", "--b", "20" }, .{ .b = 20 });
    }
}

test "caseFn - union" {
    const P = clarp.Parser(
        union(enum) { foo_bar: u8 },
        .{ .caseFn = clarp.caseKebab },
    );
    const expect = expectFn(P);
    try expect(&.{ exe_path, "foo-bar", "42" }, .{ .foo_bar = 42 });
}

test "caseFn - struct" {
    const P = clarp.Parser(
        struct { foo_bar: u8 },
        .{ .caseFn = clarp.caseKebab },
    );
    const expect = expectFn(P);
    try expect(&.{ exe_path, "--foo-bar", "42" }, .{ .foo_bar = 42 });
}

test "help override field - struct" {
    const P = clarp.Parser(struct {
        foo: []const u8,
        pub const clarp_options = Options(@This()){
            .fields = .{
                .foo = .{ .help = "--foo: string       custom foo help" },
            },
        };
    }, .{});
    var l = std.ArrayList(u8).init(talloc);
    defer l.deinit();
    _ = P.parse(&.{"exe"}, .{ .err_writer = l.writer().any() }) catch {};
    try testing.expectEqualStrings(
        \\Usage: exe [options]
        \\
        \\Options:
        \\
        \\  --foo: string       custom foo help
        \\
        \\General Options:
        \\
        \\  help, --help, -h    Print command specific usage.
        \\
        \\
    , l.items);
}

test "help override field - union" {
    const P = clarp.Parser(union(enum) {
        foo: []const u8,
        pub const clarp_options = Options(@This()){
            .fields = .{
                .foo = .{ .help = "foo: string         custom foo help" },
            },
        };
    }, .{});
    var l = std.ArrayList(u8).init(talloc);
    defer l.deinit();
    _ = P.parse(&.{"exe"}, .{ .err_writer = l.writer().any() }) catch {};
    try testing.expectEqualStrings(
        \\Usage: exe [command]
        \\
        \\Commands:
        \\
        \\  foo: string         custom foo help
        \\
        \\General Options:
        \\
        \\  help, --help, -h    Print command specific usage.
        \\
        \\
    , l.items);
}

test "help override all - struct" {
    const P = clarp.Parser(struct {
        foo: []const u8,
        pub const clarp_options = Options(@This()){
            .help =
            \\
            \\USAGE: exe <options>
            \\options:
            \\  help --help -h // show this message. must be first argument.
            \\  --foo: string  // foo desc
            \\  --bar: string  // bar desc
            \\
            ,
        };
    }, .{});

    var l = std.ArrayList(u8).init(talloc);
    defer l.deinit();
    _ = P.parse(&.{"exe"}, .{ .err_writer = l.writer().any() }) catch {};
    try testing.expectEqualStrings(P.Root.clarp_options.help.?, l.items);
}

test "help override all - union" {
    const P = clarp.Parser(union(enum) {
        foo: []const u8,
        pub const clarp_options = Options(@This()){
            .help =
            \\
            \\USAGE: exe <commands>
            \\commands:
            \\  help --help -h // show this message. must be first argument.
            \\  foo: string  // foo desc
            \\  bar: string  // bar desc
            \\
            ,
        };
    }, .{});
    var l = std.ArrayList(u8).init(talloc);
    defer l.deinit();
    _ = P.parse(&.{"exe"}, .{ .err_writer = l.writer().any() }) catch {};
    try testing.expectEqualStrings(P.Root.clarp_options.help.?, l.items);
}

test "collapse derived short flags" {
    const P = clarp.Parser(struct {
        foo: bool,
        bar: bool,
        pub const clarp_options = Options(@This()){ .derive_short_names = true };
    }, .{});

    const expect = expectFn(P);
    try expect(&.{exe_path}, .{ .foo = false, .bar = false });
    try expect(&.{ exe_path, "-fb" }, .{ .foo = true, .bar = true });
}

test "array" {
    const P = clarp.Parser(struct {
        arr: [3]u32,
    }, .{});

    const expect = expectFn(P);
    try testing.expectError(error.NotEnoughArgs, P.parse(&.{ exe_path, "--arr", "0", "1" }, .{}));
    try testing.expectError(error.ExtraArgs, P.parse(&.{ exe_path, "--arr", "0", "1", "2", "3" }, .{}));
    try expect(&.{ exe_path, "--arr", "0", "1", "2" }, .{ .arr = .{ 0, 1, 2 } });
}

test "derived shorts + short" {
    const P = clarp.Parser(struct {
        aaa: u8,
        pub const clarp_options = Options(@This()){
            .fields = .{ .aaa = .{ .short = "-aa" } },
            .derive_short_names = true,
        };
    }, .{});

    try testing.expectError(error.UnknownOption, P.parse(&.{ exe_path, "-a", "1" }, .{}));
    const expect = expectFn(P);
    try expect(&.{ exe_path, "-aa", "1" }, .{ .aaa = 1 });
    try expect(&.{ exe_path, "--aaa", "1" }, .{ .aaa = 1 });
}

test "parseWithOptions - struct" {
    const S = struct { aaa: u8 };
    const P = clarp.Parser(S, .{});
    const s = try P.parseWithOptions(&.{ exe_path, "-aa", "1" }, .{}, .{
        .fields = .{ .aaa = .{ .short = "-aa" } },
        .derive_short_names = true,
    });
    try testing.expectEqualDeep(S{ .aaa = 1 }, s.root);
}

test "parseWithOptions - union" {
    const U = union(enum) { aaa: u8 };
    const P = clarp.Parser(U, .{});
    const s = try P.parseWithOptions(&.{ exe_path, "aa", "1" }, .{}, .{
        .fields = .{ .aaa = .{ .short = "aa" } },
        .derive_short_names = true,
    });
    try testing.expectEqualDeep(U{ .aaa = 1 }, s.root);
}

test "rename long - struct" {
    const P = clarp.Parser(struct {
        foo: u8,
        pub const clarp_options = Options(@This()){
            .fields = .{ .foo = .{ .long = "bar" } },
        };
    }, .{});

    try testing.expectError(error.UnknownOption, P.parse(&.{ exe_path, "--foo", "1" }, .{}));
    const expect = expectFn(P);
    try expect(&.{ exe_path, "--bar", "1" }, .{ .foo = 1 });
}

test "rename long - union" {
    const P = clarp.Parser(union(enum) {
        foo: u8,
        pub const clarp_options = Options(@This()){
            .fields = .{ .foo = .{ .long = "bar" } },
        };
    }, .{});

    try testing.expectError(error.UnknownCommand, P.parse(&.{ exe_path, "foo", "1" }, .{}));
    const expect = expectFn(P);
    try expect(&.{ exe_path, "bar", "1" }, .{ .foo = 1 });
}

test "printHelp - struct" {
    var l = std.ArrayList(u8).init(talloc);
    defer l.deinit();
    _ = SimpleOptions.parse(&.{exe_path}, .{ .err_writer = l.writer().any() }) catch {};
    try testing.expectEqualStrings(
        \\Usage: exe [options]
        \\
        \\Options:
        \\
        \\  --opt1, -o1: string    First option description.
        \\  --opt2, -o: enum { a, b } = a
        \\                         Second option description.
        \\
        \\General Options:
        \\
        \\  help, --help, -h       Print command specific usage.
        \\
        \\
    , l.items);
}

test "printHelp - union" {
    var l = std.ArrayList(u8).init(talloc);
    defer l.deinit();
    const P = clarp.Parser(union(enum) { foo: []const u8 }, .{});
    _ = P.parse(&.{exe_path}, .{ .err_writer = l.writer().any() }) catch {};
    try testing.expectEqualStrings(
        \\Usage: exe [command]
        \\
        \\Commands:
        \\
        \\  foo: string
        \\
        \\General Options:
        \\
        \\  help, --help, -h    Print command specific usage.
        \\
        \\
    , l.items);
}

test "printHelp - nested struct" {
    const P = clarp.Parser(struct {
        foo: union(enum) { bar: []const u8 },
        a: u8,
        pub const clarp_options = Options(@This()){
            .fields = .{ .foo = .{ .desc = "Foo desc." } },
        };
    }, .{});
    var l = std.ArrayList(u8).init(talloc);
    defer l.deinit();
    _ = P.parse(&.{ exe_path, "--foo", "-h" }, .{ .err_writer = l.writer().any() }) catch |e| switch (e) {
        error.HelpShown => {},
        else => return e,
    };
    try testing.expectEqualStrings(
        \\Usage: exe --foo [command]
        \\
        \\  Foo desc.
        \\
        \\Commands:
        \\
        \\  bar: string
        \\
        \\General Options:
        \\
        \\  help, --help, -h    Print command specific usage.
        \\
        \\
    , l.items);
}

test "printHelp - nested union" {
    const P = clarp.Parser(union(enum) {
        foo: struct { bar: []const u8 },
        pub const clarp_options = Options(@This()){
            .fields = .{ .foo = .{ .desc = "Foo desc." } },
        };
    }, .{});
    var l = std.ArrayList(u8).init(talloc);
    defer l.deinit();
    _ = P.parse(&.{ exe_path, "foo", "-h" }, .{ .err_writer = l.writer().any() }) catch |e| switch (e) {
        error.HelpShown => {},
        else => return e,
    };
    try testing.expectEqualStrings(
        \\Usage: exe foo [options]
        \\
        \\  Foo desc.
        \\
        \\Options:
        \\
        \\  --bar: string
        \\
        \\General Options:
        \\
        \\  help, --help, -h    Print command specific usage.
        \\
        \\
    , l.items);
}
