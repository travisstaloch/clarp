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
    return fn (args: []const []const u8, expected: P.Result) anyerror!void;
}

fn expectFn(comptime P: type) ExpectFn(P) {
    return struct {
        fn func(args: []const []const u8, expected: P.Result) anyerror!void {
            const x = try P.parse(args, .{ .allocator = talloc });
            defer x.deinit(talloc);
            // exercise dump() and help() to catch compile errors
            try P.dump(x, "", .{}, std.io.null_writer, 0);
            try x.dump("", .{}, std.io.null_writer, 0);
            try std.io.null_writer.print("{help}", .{x});
            try std.io.null_writer.print("{}", .{x});
            return testing.expectEqualDeep(expected, x.result);
        }
    }.func;
}

inline fn argv(comptime s: []const u8) []const []const u8 {
    comptime {
        var res: []const []const u8 = &.{exe_path};
        var it = std.mem.splitScalar(u8, s, ' ');
        while (it.next()) |arg| res = res ++ .{arg};
        return res;
    }
}

const testing = std.testing;
const talloc = testing.allocator;
const exe_path = "/path/to/exe";

test "Command" {
    try testing.expectError(
        error.UnknownCommand,
        TestParser.parse(argv("asdf"), .{}),
    );
    try testing.expectError(
        error.ExtraArgs,
        TestParser.parse(argv("decode 1 2 3"), .{}),
    );
    try testing.expectError(
        error.NotEnoughArgs,
        TestParser.parse(&.{exe_path}, .{}),
    );
    try testing.expectError(
        error.UnknownOption,
        TestParser.parse(argv("handshake foo bar"), .{}),
    );
    try testing.expectError(
        error.UnknownOption,
        TestParser.parse(argv("info foo"), .{}),
    );
    try testing.expectError(
        error.UnknownOption,
        TestParser.parse(argv("handshake --peer_address foo bar"), .{}),
    );
    try testing.expectError(
        error.UnknownOption,
        TestParser.parse(argv("defaults a true"), .{}),
    );
    try testing.expectError(
        error.UnknownOption,
        TestParser.parse(argv("defaults a true 42"), .{}),
    );
    try testing.expectError(
        error.UnknownOption,
        TestParser.parse(argv("defaults a false 42"), .{}),
    );
    try testing.expectError(
        error.UnknownOption,
        TestParser.parse(argv("run c foo"), .{}),
    );
    try testing.expectError(
        error.NotEnoughArgs,
        TestParser.parse(argv("tuple"), .{}),
    );
    try testing.expectError(
        error.ExtraArgs,
        TestParser.parse(argv("tuple 1 2 3"), .{}),
    );

    const expect = expectFn(TestParser);
    try expect(argv("decode foo"), .{ .decode = .{"foo"} });
    try expect(
        argv("info --filepath foo"),
        .{ .info = .{ .filepath = "foo" } },
    );

    try expect(
        argv("handshake --peer_address foo --filepath bar"),
        .{ .handshake = .{ .filepath = "bar", .peer_address = "foo" } },
    );
    try expect(
        argv("download -o foo --piece_index 22"),
        .{ .download = .{ .piece_index = 22, .outpath = "foo" } },
    );
    try expect(
        argv("download --outpath foo --piece_index 22"),
        .{ .download = .{ .piece_index = 22, .outpath = "foo" } },
    );
    try expect(
        argv("download --piece_index 22 -o foo"),
        .{ .download = .{ .piece_index = 22, .outpath = "foo" } },
    );
    try expect(
        argv("defaults"),
        .{ .defaults = .{ .blip = false } },
    );
    try expect(
        argv("defaults --int 42"),
        .{ .defaults = .{ .int = 42, .blip = false } },
    );
    try expect(
        argv("defaults --baz"),
        .{ .defaults = .{ .baz = true, .blip = false } },
    );
    try expect(
        argv("defaults --enyum delish"),
        .{ .defaults = .{ .enyum = .delish, .blip = false } },
    );
    try expect(argv("run a 1"), .{ .run = .{ .a = 1 } });
    try expect(argv("run b 2"), .{ .run = .{ .b = 2 } });
    try expect(
        argv("run c --filepath foo"),
        .{ .run = .{ .c = .{ .filepath = "foo" } } },
    );
    try expect(argv("run d"), .{ .run = .d });
    try expect(argv("run e 0.999"), .{ .run = .{ .e = 0.999 } });
    try expect(argv("run f b"), .{ .run = .{ .f = .b } });
    {
        const x = try TestParser.parse(argv("run arr foo"), .{});
        try testing.expectEqualStrings("foo", x.result.run.arr[0..3]);
    }
    try expect(argv("opt null"), .{ .opt = null });
    try expect(argv("opt foo"), .{ .opt = "foo" });
    try expect(argv("tuple 1 2"), .{ .tuple = .{ 1, "2" } });
}

test "flags" {
    const P = clarp.Parser(struct {
        a: bool,
        b: u8,
        pub const clarp_options = Options(@This()){
            .derive_short_names = true,
        };
    }, .{});
    const expect = expectFn(P);
    try expect(argv("--a --b 1"), .{ .a = true, .b = 1 });
    try expect(argv("--b 1"), .{ .a = false, .b = 1 });
    try expect(argv("--b 1 -a"), .{ .b = 1, .a = true });
    try expect(argv("-a --b 1"), .{ .b = 1, .a = true });
}

test "overrides" {
    const P = clarp.Parser(struct {
        foo: u8,
        bar: u8,
        const Root = @This();
        pub const clarp_options = Options(@This()){
            .overrides = struct {
                pub fn @"--foo"(
                    ctx: clarp.Ctx,
                    self: *Root,
                    fields_seen: ?*std.StaticBitSet(2),
                ) anyerror!void {
                    self.foo = try std.fmt.parseInt(u8, ctx.args.*[0], 10);
                    ctx.args.* = ctx.args.*[1..];
                    fields_seen.?.set(0);
                }

                pub fn @"--"(
                    ctx: clarp.Ctx,
                    self: *Root,
                    fields_seen: ?*std.StaticBitSet(2),
                ) anyerror!void {
                    try testing.expectEqualStrings("bar", ctx.args.*[0]);
                    ctx.args.* = ctx.args.*[1..];
                    self.bar = try std.fmt.parseInt(u8, ctx.args.*[0], 10);
                    ctx.args.* = ctx.args.*[1..];
                    fields_seen.?.set(1);
                }
            },
        };
    }, .{});

    try expectFn(P)(argv("--foo 99 -- bar 100"), .{ .foo = 99, .bar = 100 });

    const P2 = clarp.Parser(union(enum) {
        foo: u8,
        bar: u8,
        const Self = @This();
        pub const clarp_options = Options(@This()){
            .overrides = struct {
                pub fn foo(
                    ctx: clarp.Ctx,
                    self: *Self,
                    fields_seen: ?*std.StaticBitSet(2),
                ) anyerror!void {
                    try testing.expect(fields_seen == null);
                    self.* = .{ .foo = try std.fmt.parseInt(u8, ctx.args.*[0], 10) };
                    ctx.args.* = ctx.args.*[1..];
                }
            },
        };
    }, .{});

    try expectFn(P2)(argv("foo 99"), .{ .foo = 99 });
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
    try expect(argv("--opt1 foo"), .{ .opt1 = "foo", .opt2 = .a });
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
        argv("-y 4 -x 1 -xy 2 -xx 3"),
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
    try expect(argv("x 1"), .{ .xxx = 1 });
    try expect(argv("xy 2"), .{ .xyy = 2 });
    try expect(argv("xx 3"), .{ .xxy = 3 });
    try expect(argv("y 4"), .{ .yyy = 4 });
}

test "struct end mark" {
    { // default end mark
        const P = clarp.Parser(struct {
            a: struct { b: u8 = 1 } = .{},
            b: u8 = 2,
        }, .{});
        const expect = expectFn(P);
        try expect(
            argv("--a --b 20 --b 30"),
            .{ .a = .{ .b = 20 }, .b = 30 },
        );
        try expect(argv("--a --end --b 20"), .{ .b = 20 });
    }
    { // custom end mark
        const P = clarp.Parser(struct {
            a: struct {
                b: u8 = 1,
                pub const clarp_options = Options(@This()){
                    .end_mark = "--my-end-mark",
                };
            } = .{},
            b: u8 = 2,
        }, .{});
        const expect = expectFn(P);
        try expect(argv("--a --b 20 --b 30"), .{ .a = .{ .b = 20 }, .b = 30 });
        try expect(argv("--a --my-end-mark --b 20"), .{ .b = 20 });
    }
}

test "caseFn - union" {
    const P = clarp.Parser(
        union(enum) { foo_bar: u8 },
        .{ .caseFn = clarp.caseKebab },
    );
    const expect = expectFn(P);
    try expect(argv("foo-bar 42"), .{ .foo_bar = 42 });
}

test "caseFn - struct" {
    const P = clarp.Parser(
        struct { foo_bar: u8 },
        .{ .caseFn = clarp.caseKebab },
    );
    const expect = expectFn(P);
    try expect(argv("--foo-bar 42"), .{ .foo_bar = 42 });
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
    _ = P.parse(&.{exe_path}, .{ .err_writer = l.writer().any() }) catch {};
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
    _ = P.parse(&.{exe_path}, .{ .err_writer = l.writer().any() }) catch {};
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
    _ = P.parse(&.{exe_path}, .{ .err_writer = l.writer().any() }) catch {};
    try testing.expectEqualStrings(P.Result.clarp_options.help.?, l.items);
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
    _ = P.parse(&.{exe_path}, .{ .err_writer = l.writer().any() }) catch {};
    try testing.expectEqualStrings(P.Result.clarp_options.help.?, l.items);
}

test "collapse derived short flags" {
    const P = clarp.Parser(struct {
        foo: bool,
        bar: bool,
        pub const clarp_options = Options(@This()){ .derive_short_names = true };
    }, .{});

    const expect = expectFn(P);
    try expect(&.{exe_path}, .{ .foo = false, .bar = false });
    try expect(argv("-fb"), .{ .foo = true, .bar = true });
}

test "array" {
    const P = clarp.Parser(struct {
        arr: [3]u32,
    }, .{});

    const expect = expectFn(P);
    try testing.expectError(
        error.NotEnoughArgs,
        P.parse(argv("--arr 0 1"), .{}),
    );
    try testing.expectError(
        error.ExtraArgs,
        P.parse(argv("--arr 0 1 2 3"), .{}),
    );
    try expect(argv("--arr 0 1 2"), .{ .arr = .{ 0, 1, 2 } });
}

test "derived shorts + short" {
    const P = clarp.Parser(struct {
        aaa: u8,
        pub const clarp_options = Options(@This()){
            .fields = .{ .aaa = .{ .short = "-aa" } },
            .derive_short_names = true,
        };
    }, .{});

    try testing.expectError(error.UnknownOption, P.parse(argv("-a 1"), .{}));
    const expect = expectFn(P);
    try expect(argv("-aa 1"), .{ .aaa = 1 });
    try expect(argv("--aaa 1"), .{ .aaa = 1 });
}

test "parseWithOptions - struct" {
    const S = struct { aaa: u8 };
    const P = clarp.Parser(S, .{});
    const s = try P.parseWithOptions(argv("-aa 1"), .{}, .{
        .fields = .{ .aaa = .{ .short = "-aa" } },
        .derive_short_names = true,
    });
    try testing.expectEqualDeep(S{ .aaa = 1 }, s.result);
}

test "parseWithOptions - union" {
    const U = union(enum) { aaa: u8 };
    const P = clarp.Parser(U, .{});
    const s = try P.parseWithOptions(argv("aa 1"), .{}, .{
        .fields = .{ .aaa = .{ .short = "aa" } },
        .derive_short_names = true,
    });
    try testing.expectEqualDeep(U{ .aaa = 1 }, s.result);
}

test "rename long - struct" {
    const P = clarp.Parser(struct {
        foo: u8,
        pub const clarp_options = Options(@This()){
            .fields = .{ .foo = .{ .long = "bar" } },
        };
    }, .{});

    try testing.expectError(error.UnknownOption, P.parse(argv("--foo 1"), .{}));
    const expect = expectFn(P);
    try expect(argv("--bar 1"), .{ .foo = 1 });
}

test "rename long - union" {
    const P = clarp.Parser(union(enum) {
        foo: u8,
        pub const clarp_options = Options(@This()){
            .fields = .{ .foo = .{ .long = "bar" } },
        };
    }, .{});

    try testing.expectError(error.UnknownCommand, P.parse(argv("foo 1"), .{}));
    const expect = expectFn(P);
    try expect(argv("bar 1"), .{ .foo = 1 });
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
    try testing.expectError(error.HelpShown, P.parse(argv("--foo -h"), .{ .err_writer = l.writer().any() }));
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
    try testing.expectError(error.HelpShown, P.parse(argv("foo -h"), .{ .err_writer = l.writer().any() }));
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

test "allocate slice field" {
    const P = clarp.Parser(struct {
        files: []const []const u8,
        b: u8,
    }, .{});
    try testing.expectError(error.AllocatorRequired, P.parse(argv("--files a"), .{}));
    // successfully parse 'files' field but 'b' field missing and must free slice
    try testing.expectError(
        error.MissingFields,
        P.parse(argv("--files a"), .{ .allocator = talloc }),
    );

    try expectFn(P)(
        argv("--files a b c --b 1"),
        .{ .files = &.{ "a", "b", "c" }, .b = 1 },
    );
}

test "positional field" {
    const P = clarp.Parser(struct {
        files: []const []const u8,
        b: u8,
        pub const clarp_options = Options(@This()){
            .fields = .{ .files = .{ .positional = true } },
        };
    }, .{});
    try testing.expectError(error.AllocatorRequired, P.parse(argv("a"), .{}));
    // successfully parse 'positional' field but 'b' field missing and must free slice
    try testing.expectError(
        error.MissingFields,
        P.parse(argv("a"), .{ .allocator = talloc }),
    );
    const expect = expectFn(P);
    try expect(argv("a b c --b 1"), .{ .files = &.{ "a", "b", "c" }, .b = 1 });
    try expect(argv("--b 1 a b c"), .{ .files = &.{ "a", "b", "c" }, .b = 1 });

    const P2 = clarp.Parser(struct {
        afiles: []const []const u8,
        bfiles: []const []const u8,
        pub const clarp_options = Options(@This()){
            .fields = .{
                .afiles = .{ .positional = true },
                .bfiles = .{ .positional = true },
            },
        };
    }, .{});
    try expectFn(P2)(
        argv("--afiles a b c --bfiles 1 2 3"),
        .{ .afiles = &.{ "a", "b", "c" }, .bfiles = &.{ "1", "2", "3" } },
    );
    try expectFn(P2)(
        argv("a b c --bfiles 1 2 3"),
        .{ .afiles = &.{ "a", "b", "c" }, .bfiles = &.{ "1", "2", "3" } },
    );
    // --end mark for slice field
    try expectFn(P2)(
        argv("--bfiles 1 2 3 --end a b c"),
        .{ .afiles = &.{ "a", "b", "c" }, .bfiles = &.{ "1", "2", "3" } },
    );
}

test "command with positional" {
    const P = clarp.Parser(union(enum) {
        mycmd: struct {
            files: []const []const u8,
            foo: bool,
            bar: bool,
            pub const clarp_options = Options(@This()){
                .fields = .{ .files = .{ .positional = true } },
            };
        },
    }, .{});
    // slice field args must be consecutive
    try testing.expectError(error.UnknownOption, P.parse(
        argv("mycmd file.txt --foo image.png"),
        .{ .allocator = talloc },
    ));
    const expect = expectFn(P);
    try expect(
        argv("mycmd file.txt image.png --foo --bar"),
        .{ .mycmd = .{ .files = &.{ "file.txt", "image.png" }, .foo = true, .bar = true } },
    );
    try expect(
        argv("mycmd --foo file.txt image.png --bar"),
        .{ .mycmd = .{ .files = &.{ "file.txt", "image.png" }, .foo = true, .bar = true } },
    );
    try expect(
        argv("mycmd --bar --foo file.txt image.png"),
        .{ .mycmd = .{ .files = &.{ "file.txt", "image.png" }, .foo = true, .bar = true } },
    );
    try expect(
        argv("mycmd file.txt image.png"),
        .{ .mycmd = .{ .files = &.{ "file.txt", "image.png" }, .foo = false, .bar = false } },
    );
}

test "command with positional - different field order" {
    const P = clarp.Parser(union(enum) {
        mycmd: struct {
            foo: bool,
            files: []const []const u8,
            bar: bool,
            pub const clarp_options = Options(@This()){
                .fields = .{ .files = .{ .positional = true } },
            };
        },
    }, .{});
    try testing.expectError(error.UnknownOption, P.parse(
        argv("mycmd file.txt --foo image.png"),
        .{ .allocator = talloc },
    ));
    const expect = expectFn(P);
    try expect(
        argv("mycmd file.txt image.png --foo --bar"),
        .{ .mycmd = .{ .files = &.{ "file.txt", "image.png" }, .foo = true, .bar = true } },
    );
    try expect(
        argv("mycmd --foo file.txt image.png --bar"),
        .{ .mycmd = .{ .files = &.{ "file.txt", "image.png" }, .foo = true, .bar = true } },
    );
    try expect(
        argv("mycmd --bar --foo file.txt image.png"),
        .{ .mycmd = .{ .files = &.{ "file.txt", "image.png" }, .foo = true, .bar = true } },
    );
    try expect(
        argv("mycmd file.txt image.png"),
        .{ .mycmd = .{ .files = &.{ "file.txt", "image.png" }, .foo = false, .bar = false } },
    );
}

test "partial match" {
    const P = clarp.Parser(struct {
        foo: u8 = 0,
        bar: []const u8 = "",
        baz: bool = false,
        pub const clarp_options = Options(@This()){
            .derive_short_names = true,
        };
    }, .{});
    const expect = expectFn(P);
    try expect(argv("--foo=1"), .{ .foo = 1 });
    try expect(argv("--foo1"), .{ .foo = 1 });
    try expect(argv("-f=1"), .{ .foo = 1 });
    try expect(argv("-f1"), .{ .foo = 1 });
    try expect(argv("--bar=str"), .{ .bar = "str" });
    try expect(argv("--barstr"), .{ .bar = "str" });
    try expect(argv("-b=str"), .{ .bar = "str" });
    try expect(argv("-bstr"), .{ .bar = "str" });
    try expect(argv("--baz=true"), .{ .baz = true });
    try expect(argv("--baztrue"), .{ .baz = true });
    try expect(argv("-ba=true"), .{ .baz = true });
    try expect(argv("-batrue"), .{ .baz = true });
    try expect(argv("-ba"), .{ .baz = true });
}

test "union field name overrides" {
    const P = clarp.Parser(union(enum) {
        foo: bool,
        pub const clarp_options = Options(@This()){
            .fields = .{ .foo = .{ .long = "foo-long", .short = "fo" } },
        };
    }, .{});

    const expect = expectFn(P);
    try expect(argv("foo-long"), .{ .foo = true });
    try expect(argv("fo"), .{ .foo = true });
}

test "utf8 codepoint" {
    const P1 = clarp.Parser(struct {
        codepoint: u21,
        const Self = @This();
        pub const clarp_options = clarp.Options(Self){
            .fields = .{
                .codepoint = .{ .positional = true, .utf8 = true },
            },
        };
    }, .{});
    try expectFn(P1)(argv("a"), .{ .codepoint = 'a' });
    try expectFn(P1)(argv("👏"), .{ .codepoint = '👏' });
}

test "integer parsing" {
    const P1 = clarp.Parser(struct {
        int: isize,
        const Self = @This();
        pub const clarp_options = clarp.Options(Self){
            .fields = .{
                .int = .{ .positional = true },
            },
        };
    }, .{});
    try expectFn(P1)(argv("123"), .{ .int = 123 });
    try expectFn(P1)(argv("-123"), .{ .int = -123 });
    try expectFn(P1)(argv("0x123"), .{ .int = 0x123 });
    try expectFn(P1)(argv("-0x123"), .{ .int = -0x123 });
    try expectFn(P1)(argv("0b101"), .{ .int = 0b101 });
    try expectFn(P1)(argv("-0b101"), .{ .int = -0b101 });
    try expectFn(P1)(argv("0o101"), .{ .int = 0o101 });
    try expectFn(P1)(argv("-0o101"), .{ .int = -0o101 });
}
