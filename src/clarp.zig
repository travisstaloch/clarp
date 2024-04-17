//!
//! clarp - a command line arg parser
//!

const std = @import("std");
const mem = std.mem;
const log = std.log.scoped(.@"cli-parsing"); // TODO remove

pub const FieldOption = struct {
    /// an alternate option or command name for this field.  for a field named
    /// 'foo' and short '-f', either '--foo' and '-f' will match.
    ///
    /// when 'derive_short_names' is true, this overrides the derived short
    /// name.
    short: ?[]const u8 = null,
    /// description for this field to be shown in help
    desc: ?[]const u8 = null,
    /// help text override for this field
    help: ?[]const u8 = null,
    /// changes the option or command name. effectively renames the field.
    long: ?[]const u8 = null,
    /// marks the field as positional. positional options may be unnamed.
    /// only affects non-tuple structs.
    positional: bool = false,
};

/// any struct or enum passed to `clarp.Parser()` may contain
/// `pub const clarp_options` decls of this type where T = @This();
pub fn Options(comptime T: type) type {
    const is_container = isContainer(T);
    return struct {
        /// field options.  a optional struct of `FieldOption` with field names
        /// from T.
        fields: Fields = if (is_container) .{} else {},
        /// help text override for the entrire struct
        help: ?[]const u8 = null,
        /// when found, denotes end of input for a field.  any further
        /// input may apply to other fields.  default '--end'.
        end_mark: []const u8 = "--end",
        /// when true, short names will be derived for each field in the parent
        /// struct. i.e. struct{foo: u8, bar: u8} results in '-f' and '-b'
        /// derived short names.  fields may override their short name by
        /// specifying a field.short value.
        derive_short_names: bool = false,
        /// a struct with pub functions that can be used to override parsing of
        /// individual options.  i.e. `struct {pub fn @"--foo"(...)}` to parse
        /// '--foo' option.
        overrides: ?type = null,

        const Fields = if (is_container)
            std.enums.EnumFieldStruct(std.meta.FieldEnum(T), FieldOption, .{})
        else
            void;
    };
}

/// parse() specific options. runtime.
pub const ParseOptions = struct {
    err_writer: std.io.AnyWriter = std.io.null_writer.any(),
    allocator: ?mem.Allocator = null,
};

/// global options, comptime
pub const ParserOptions = struct {
    help_flags: type = HelpFlags,
    printUsage: PrintUsageFn = defaultPrintUsage,
    caseFn: CaseFn = caseSame,
    help_description_start_column: usize = 22,
};

/// default flags for showing help/usage
pub const HelpFlags = enum { help, @"--help", @"-h" };
pub fn defaultPrintUsage(
    comptime T: type,
    writer: std.io.AnyWriter,
    init_args: []const []const u8,
    rest_args: []const []const u8,
) anyerror!void {
    try writer.print("Usage: {s} ", .{std.fs.path.basename(init_args[0])});
    const args = init_args[1..];
    debug("args {}/{} {s}/{s}", .{ args.len, rest_args.len, args, rest_args });
    for (args[0 .. args.len - rest_args.len]) |arg| {
        try writer.writeAll(arg);
        try writer.writeByte(' ');
    }
    const info = @typeInfo(T);
    switch (info) {
        .Union => try writer.writeAll("[command]\n\n"),
        .Struct => try writer.writeAll("[options]\n\n"),
        else => @compileError("unexpected type '" ++ @typeName(T) ++ "'"),
    }
}
const PrintUsageFn = @TypeOf(defaultPrintUsage);

pub fn Override(comptime T: type) type {
    return *const fn (
        ctx: Ctx,
        payload: *T,
        fields_seen: ?*std.StaticBitSet(std.meta.fields(T).len),
    ) anyerror!void;
}

pub fn caseSame(_: []u8, name: []const u8) []const u8 {
    return name;
}
const CaseFn = @TypeOf(caseSame);

pub fn caseKebab(buf: []u8, name: []const u8) []const u8 {
    for (0..name.len) |i|
        buf[i] = if (name[i] == '_') '-' else name[i];

    return buf[0..name.len];
}

///
/// returns a command line parser. `T` must be an enum or struct type.
///
/// union types describe alternative commands.
///
/// struct types describe named sequences of options.  their field names require
/// leading dashes (ie --field-name).
///
/// tuple types describe strictly unnamed sequences.
///
/// bool field types are flags may be specified as --flag (or true/false when
/// unnamed) and they implicitly default to false.
pub fn Parser(comptime T: type, comptime options: ParserOptions) type {
    return struct {
        root: Root,
        args: []const []const u8,
        rest: []const []const u8,
        user_context: ?*anyopaque = null,

        const Self = @This();
        pub const Root = T;

        /// parse command line args with `clarp_options`
        pub fn parseWithOptions(
            args: []const []const u8,
            parse_options: ParseOptions,
            comptime clarp_options: Options(T),
        ) !Self {
            // debug("args[1] {s}", .{args[1]});
            var rest = args[1..];
            const ctx = Ctx.init(args, &rest, parse_options);
            const root = parsePayload(
                T,
                ctx,
                CtCtx(T).init(null, clarp_options, null),
            ) catch |e| {
                if (e == error.HelpShown) return e;
                try printError(parse_options.err_writer, args, rest);
                return e;
            };
            errdefer deinitPayload(Root, root, parse_options.allocator);
            return if (rest.len != 0)
                err(T, ctx, error.ExtraArgs)
            else
                .{ .root = root, .args = args, .rest = rest };
        }

        /// parse command line args
        pub fn parse(args: []const []const u8, parse_options: ParseOptions) !Self {
            return parseWithOptions(args, parse_options, clarpOptions(T));
        }

        fn err(comptime V: type, ctx: Ctx, e: anyerror) anyerror {
            if ((comptime isContainer(V))) {
                help(
                    V,
                    ctx.init_args,
                    ctx.args.*,
                    ctx.parse_options.err_writer,
                ) catch {};
            }

            return e;
        }

        fn parsePayload(
            comptime V: type,
            ctx: Ctx,
            comptime ct_ctx: CtCtx(V),
        ) !V {
            const info = @typeInfo(V);
            const args = ctx.args;
            const parse_options = ctx.parse_options;
            const field_name = ct_ctx.field_name;
            debug(
                "parsing {s} args len {} field name {?s}",
                .{ @tagName(info), args.len, field_name },
            );
            defer debug(
                "parsing {s} done args len {} field name {?s}",
                .{ @tagName(info), args.len, field_name },
            );
            if (args.len == 0) {
                return if (mustConsume(V))
                    err(V, ctx, error.NotEnoughArgs)
                else
                    initEmpty(V) catch
                        err(V, ctx, error.NotEnoughArgs);
            }

            switch (info) {
                else => |x| @compileError("TODO support " ++ @tagName(x)),
                .Void => return {},
                .Int => {
                    defer args.* = args.*[1..];
                    return std.fmt.parseInt(V, args.*[0], 10);
                },
                .Float => {
                    defer args.* = args.*[1..];
                    return std.fmt.parseFloat(V, args.*[0]);
                },
                .Bool => {
                    debug("bool {s}", .{args.*[0]});
                    if (field_name) |n| {
                        if (mem.startsWith(u8, args.*[0], "--") and
                            mem.eql(u8, args.*[0][2..], n))
                        {
                            args.* = args.*[1..];
                            return true;
                        }
                    }

                    if (std.meta.stringToEnum(enum { true, false }, args.*[0])) |b| {
                        args.* = args.*[1..];
                        return switch (b) {
                            .true => true,
                            .false => false,
                        };
                    }
                    try logErr("invalid bool '{s}'\n", .{args.*[0]}, parse_options.err_writer);
                    return err(V, ctx, error.InvalidBoolean);
                },
                .Optional => |x| if (mem.eql(u8, args.*[0], "null")) {
                    args.* = args.*[1..];
                    return null;
                } else return try parsePayload(
                    x.child,
                    ctx,
                    CtCtx(x.child).init(
                        field_name,
                        clarpOptions(x.child),
                        ct_ctx.outer_desc,
                    ),
                ),
                .Enum => if (std.meta.stringToEnum(V, args.*[0])) |e| {
                    args.* = args.*[1..];
                    return e;
                } else {
                    try logErr("invalid enum tag '{s}'\n", .{args.*[0]}, parse_options.err_writer);
                    return err(V, ctx, error.InvalidEnum);
                },
                .Array => |x| {
                    if (x.child == u8) {
                        if (args.*[0].len > x.len)
                            return err(V, ctx, error.ArrayTooShort);
                        defer args.* = args.*[1..];
                        var a: V = undefined;
                        @memcpy(a[0..args.*[0].len], args.*[0]);
                        return a;
                    } else {
                        var a: V = undefined;
                        for (&a) |*ele| {
                            ele.* = try parsePayload(
                                x.child,
                                ctx,
                                CtCtx(x.child).init(
                                    field_name,
                                    clarpOptions(x.child),
                                    ct_ctx.outer_desc,
                                ),
                            );
                        }
                        return a;
                    }
                },
                .Pointer => |x| if (comptime isZigString(V)) {
                    defer args.* = args.*[1..];
                    return args.*[0];
                } else switch (x.size) {
                    .Slice => if (ctx.parse_options.allocator == null)
                        return error.AllocatorRequired
                    else {
                        var l = std.ArrayList(x.child).init(ctx.parse_options.allocator.?);
                        errdefer l.deinit();
                        while (args.len > 0 and !std.mem.startsWith(u8, args.*[0], "-")) {
                            try l.append(try parsePayload(x.child, ctx, CtCtx(x.child).init(
                                field_name,
                                clarpOptions(x.child),
                                ct_ctx.outer_desc,
                            )));
                        }
                        if (args.len > 0 and
                            std.mem.eql(u8, args.*[0], ct_ctx.clarp_options.end_mark))
                        {
                            args.* = args.*[1..];
                        }
                        return l.toOwnedSlice();
                    },
                    else => @compileError("TODO Pointer support " ++ @tagName(x.size)),
                },
                .Union => return try parseUnion(
                    V,
                    ctx,
                    CtCtx(V).init(
                        field_name,
                        ct_ctx.clarp_options,
                        ct_ctx.outer_desc,
                    ),
                ),
                .Struct => |x| return if (x.is_tuple)
                    try parseTuple(V, ctx)
                else
                    try parseStruct(
                        V,
                        ctx,
                        CtCtx(V).init(
                            field_name,
                            ct_ctx.clarp_options,
                            ct_ctx.outer_desc,
                        ),
                    ),
            }
        }

        fn printHelp0(
            comptime V: type,
            ctx: Ctx,
            comptime outer_desc: ?[]const u8,
        ) !void {
            const parse_options = ctx.parse_options;
            const args = ctx.args;
            const init_args = ctx.init_args;
            const writer = parse_options.err_writer;
            const has_options = @hasDecl(V, "clarp_options");
            const has_help = has_options and V.clarp_options.help != null;
            if (!has_help) {
                try options.printUsage(V, writer, init_args, args.*);
            }
            if (outer_desc != null) {
                try writer.print("  {s}\n\n", .{outer_desc.?});
            }

            try printHelp(V, writer, 1, longestFieldLen(V));

            if (!has_help) {
                try writer.writeAll("\n\nGeneral Options:\n\n");
                var cwriter = std.io.countingWriter(writer);
                const w = cwriter.writer();
                try w.writeAll("  ");
                inline for (@typeInfo(options.help_flags).Enum.fields, 0..) |f, i| {
                    if (i != 0) try w.writeAll(", ");
                    try w.writeAll(f.name);
                }
                try writer.writeByteNTimes(' ', options.help_description_start_column - cwriter.bytes_written);
                try writer.writeAll("Print command specific usage.");
                try writer.writeAll("\n\n");
            }
        }

        fn parseUnion(
            comptime V: type,
            ctx: Ctx,
            comptime ct_ctx: CtCtx(V),
        ) !V {
            const info = @typeInfo(V);
            const clarp_options = ct_ctx.clarp_options;
            const fields = info.Union.fields;
            const parse_options = ctx.parse_options;
            const args = ctx.args;
            const FieldEnum = std.meta.FieldEnum(V);
            const kvs = comptime GenKvs(V, ShortNames(fields, V), FieldEnum, info, clarp_options);
            const map = std.ComptimeStringMap(NamedOption(V, FieldEnum), kvs);

            if (map.get(ctx.args.*[0])) |named_option| {
                // debug("named_option {s}", .{@tagName(named_option)});
                switch (named_option) {
                    .help => {
                        try printHelp0(V, ctx, ct_ctx.outer_desc);
                        return error.HelpShown;
                    },
                    .override => |override| {
                        debug("found override", .{});
                        args.* = args.*[1..];
                        var payload: V = undefined;
                        try override(ctx, &payload, null);
                        return payload;
                    },
                    .end_mark => return err(V, ctx, error.UnionEndMark),
                    .short, .long => |fe| if (@typeInfo(FieldEnum).Enum.fields.len > 0) {
                        switch (fe) {
                            inline else => |tag| {
                                debug("found {s} {s} {s}", .{ @tagName(named_option), args.*[0], @tagName(tag) });
                                args.* = args.*[1..];
                                const Ft = std.meta.TagPayload(V, tag);
                                return @unionInit(V, @tagName(tag), try parsePayload(
                                    Ft,
                                    ctx,
                                    CtCtx(Ft).init(
                                        @tagName(tag),
                                        clarpOptions(Ft),
                                        if (@hasDecl(V, "clarp_options"))
                                            @field(V.clarp_options.fields, @tagName(tag)).desc
                                        else
                                            null,
                                    ),
                                ));
                            },
                        }
                    },
                }
            }

            try logErr("unknown command '{s}'\n", .{args.*[0]}, parse_options.err_writer);
            return err(V, ctx, error.UnknownCommand);
        }

        fn parseStruct(
            comptime V: type,
            ctx: Ctx,
            comptime ct_ctx: CtCtx(V),
        ) !V {
            const info = @typeInfo(V);
            const clarp_options = ct_ctx.clarp_options;
            const parse_options = ctx.parse_options;
            const args = ctx.args;
            const init_args = ctx.init_args;
            const fields = info.Struct.fields;
            const Short = ShortNames(fields, V);
            const FieldEnum = std.meta.FieldEnum(V);
            const kvs = comptime GenKvs(V, Short, FieldEnum, info, clarp_options);
            const map = std.ComptimeStringMap(NamedOption(V, FieldEnum), kvs);

            var payload: V = initEmpty(V) catch undefined;
            errdefer deinitPayload(V, payload, parse_options.allocator);
            var fields_seen = std.StaticBitSet(fields.len).initEmpty();

            debug(
                "parseStruct() kvs.len {} V {s} fields {} {s}",
                .{ kvs.len, @typeName(V), fields.len, std.meta.fieldNames(V) },
            );

            args: while (args.len > 0 and
                (fields_seen.count() < fields.len or
                clarp_options.overrides != null))
            {
                inline for (fields, 0..) |f, i| {
                    if (fields_seen.isSet(i))
                        debug("{s}: {any}", .{ f.name, @field(payload, f.name) });
                }

                debug("args {s}", .{args.*});
                if (map.get(args.*[0])) |named_option| {
                    switch (named_option) {
                        .help => {
                            try printHelp0(V, ctx, ct_ctx.outer_desc);
                            return error.HelpShown;
                        },
                        .end_mark => {
                            debug("found end_mark", .{});
                            args.* = args.*[1..];
                            break :args;
                        },
                        .override => |override| {
                            debug("found override", .{});
                            args.* = args.*[1..];
                            try override(ctx, &payload, &fields_seen);
                            continue :args;
                        },
                        .short, .long => |fe| if (@typeInfo(FieldEnum).Enum.fields.len > 0) {
                            switch (fe) {
                                inline else => |tag| {
                                    debug("found {s} {s} {s}", .{ @tagName(named_option), args.*[0], @tagName(tag) });
                                    const fi = @intFromEnum(tag);
                                    const Ft = @TypeOf(@field(payload, fields[fi].name));
                                    args.* = args.*[@intFromBool(!isFlagType(Ft))..];
                                    @field(payload, @tagName(tag)) = try parsePayload(
                                        Ft,
                                        ctx,
                                        CtCtx(Ft).init(
                                            fields[fi].name,
                                            clarpOptions(Ft),
                                            if (@hasDecl(V, "clarp_options"))
                                                @field(V.clarp_options.fields, @tagName(tag)).desc
                                            else
                                                null,
                                        ),
                                    );
                                    fields_seen.set(fi);
                                    continue :args;
                                },
                            }
                            unreachable;
                        },
                    }
                } else if (clarp_options.derive_short_names and
                    mem.startsWith(u8, args.*[0], "-"))
                {
                    // parse shorts with -abc syntax where
                    // a, b, c are derived short field names
                    var arg = args.*[0][1..];
                    debug("arg {s} shorts {s}", .{ arg, std.meta.fieldNames(Short) });
                    shorts: while (arg.len > 0) {
                        inline for (@typeInfo(Short).Enum.fields, 0..) |f, fi| {
                            if (isFlagType(fields[fi].type) and mem.startsWith(u8, arg, f.name)) {
                                const Ft = @TypeOf(@field(payload, fields[fi].name));
                                // construct a fake, single flag arg.  this allows bool fields to work
                                var tmp_args: []const []const u8 = &[_][]const u8{"--" ++ fields[fi].name};
                                @field(payload, fields[fi].name) = try parsePayload(
                                    Ft,
                                    Ctx.init(
                                        init_args,
                                        &tmp_args,
                                        parse_options,
                                    ),
                                    CtCtx(Ft).init(
                                        fields[fi].name,
                                        clarpOptions(fields[fi].type),
                                        if (@hasDecl(V, "clarp_options"))
                                            @field(V.clarp_options.fields, fields[fi].name).desc
                                        else
                                            null,
                                    ),
                                );
                                fields_seen.set(fi);
                                arg = arg[f.name.len..];
                                continue :shorts;
                            }
                        } else break;
                    }

                    if (arg.len == 0) {
                        args.* = args.*[1..];
                        continue :args;
                    } else {
                        try logErr("unknown short option(s) '{s}'\n", .{arg}, parse_options.err_writer);
                        return err(V, ctx, error.UnknownOption);
                    }
                } else {
                    // handle positional fields
                    var it = fields_seen.iterator(.{ .kind = .unset });
                    while (it.next()) |field_idx| {
                        const fe: FieldEnum = @enumFromInt(field_idx);
                        if (@typeInfo(FieldEnum).Enum.fields.len == 0) continue;
                        switch (fe) {
                            inline else => |tag| {
                                const name = @tagName(tag);
                                const Ft = std.meta.FieldType(V, tag);
                                if (@field(clarp_options.fields, name).positional) {
                                    @field(payload, name) = try parsePayload(
                                        Ft,
                                        ctx,
                                        CtCtx(Ft).init(
                                            name,
                                            clarpOptions(Ft),
                                            @field(V.clarp_options.fields, name).desc,
                                        ),
                                    );
                                    fields_seen.set(field_idx);
                                    continue :args;
                                }
                            },
                        }
                    }
                    return error.UnknownOption;
                }
            }

            // set field default values if provided
            inline for (fields, 0..) |f, i| {
                if (!fields_seen.isSet(i)) {
                    if (f.default_value) |d| {
                        fields_seen.set(i);
                        @field(payload, f.name) = @as(*const f.type, @ptrCast(@alignCast(d))).*;
                    } else if (comptime isFlagType(f.type)) {
                        fields_seen.set(i);
                        @field(payload, f.name) = false;
                    }
                }
            }

            debug("fields seen {}/{}", .{ fields_seen.count(), fields.len });
            const field_names = std.meta.fieldNames(V);
            if (field_names.len != 0 and fields_seen.count() != fields.len) {
                try logErr("missing fields: ", .{}, parse_options.err_writer);
                var iter = fields_seen.iterator(.{ .kind = .unset });
                var i: u32 = 0;
                while (iter.next()) |fi| : (i += 1) {
                    if (i == 0)
                        try logErr("'{s}'", .{field_names[fi]}, parse_options.err_writer)
                    else
                        try logErr(", '{s}'", .{field_names[fi]}, parse_options.err_writer);
                }
                try logErr("\n", .{}, parse_options.err_writer);
                return err(V, ctx, error.MissingFields);
            }

            return payload;
        }

        /// returns kv pair for each override, derived short/short, long and one end_mark
        /// each pair has type struct{[]const u8, NamedOption(V, FieldEnum)}
        fn GenKvs(
            comptime V: type,
            comptime Short: type,
            comptime FieldEnum: type,
            comptime info: std.builtin.Type,
            comptime clarp_options: Options(V),
        ) []const Kv(V, FieldEnum) {
            comptime {
                // TODO don't precalculate buffer size. or figure a way to DRY
                // calculate the buffer size needed
                const end_mark_len = 1;
                const overrides_len = if (clarp_options.overrides != null)
                    std.meta.declarations(clarp_options.overrides.?).len
                else
                    0;
                var dshorts_len: usize = 0;
                if (clarp_options.derive_short_names) {
                    for (std.meta.tags(FieldEnum)) |tag| {
                        if (@field(clarp_options.fields, @tagName(tag)).short != null)
                            continue;
                        dshorts_len += 1;
                    }
                }
                var shorts_len: usize = 0;
                for (std.meta.tags(FieldEnum)) |tag| {
                    if (@field(clarp_options.fields, @tagName(tag)).short != null) {
                        shorts_len += 1;
                    }
                }
                const helps_len = @typeInfo(options.help_flags).Enum.fields.len;

                var longs_len: usize = 0;
                const fields = switch (info) {
                    .Struct => info.Struct.fields,
                    .Union => info.Union.fields,
                    else => unreachable,
                };
                const short_prefix: []const u8 = if (info == .Struct) "-" else "";
                const long_prefix: []const u8 = if (info == .Struct) "--" else "";
                fields: for (fields) |f| {
                    // skip long if there is an override with same name
                    if (clarp_options.overrides != null) {
                        for (std.meta.declarations(clarp_options.overrides.?)) |decl| {
                            if (mem.eql(u8, long_prefix ++ f.name, decl.name))
                                continue :fields;
                        }
                    }
                    longs_len += 1;
                }

                const kv_len: usize = end_mark_len + overrides_len +
                    dshorts_len + shorts_len + helps_len + longs_len;

                // assign kvs
                var kvs: [kv_len]Kv(V, FieldEnum) = undefined;
                var kvidx: usize = 0;

                kvs[kvidx] = .{ clarp_options.end_mark, .end_mark };
                kvidx += 1;

                if (clarp_options.overrides) |overrides| {
                    for (std.meta.declarations(overrides)) |decl| {
                        kvs[kvidx] = .{ decl.name, .{
                            .override = @field(clarp_options.overrides.?, decl.name),
                        } };
                        kvidx += 1;
                    }
                }

                if (clarp_options.derive_short_names) {
                    for (std.meta.tags(Short), 0..) |tag, j| {
                        const fe = std.meta.tags(FieldEnum)[j];
                        if (@field(clarp_options.fields, @tagName(fe)).short != null)
                            continue;
                        kvs[kvidx] = .{ short_prefix ++ @tagName(tag), .{ .short = fe } };
                        kvidx += 1;
                    }
                }

                for (@typeInfo(options.help_flags).Enum.fields) |f| {
                    kvs[kvidx] = .{ f.name, .help };
                    kvidx += 1;
                }

                for (std.meta.tags(FieldEnum)) |tag| {
                    // add long unless there is an override with same name
                    const tagname = @tagName(tag);
                    var buf: [tagname.len]u8 = undefined;
                    const fname = options.caseFn(&buf, tagname);
                    const found_override = if (clarp_options.overrides != null)
                        for (std.meta.declarations(clarp_options.overrides.?)) |decl| {
                            if (mem.eql(u8, long_prefix ++ fname, decl.name)) break true;
                        } else false
                    else
                        false;

                    if (!found_override) {
                        kvs[kvidx] = if (@field(clarp_options.fields, @tagName(tag)).long) |long|
                            .{ long_prefix ++ long, .{ .long = tag } }
                        else
                            .{ long_prefix ++ fname, .{ .long = tag } };
                        kvidx += 1;
                    }

                    // add optional short
                    if (@field(clarp_options.fields, @tagName(tag)).short != null) {
                        kvs[kvidx] = .{
                            @field(clarp_options.fields, @tagName(tag)).short.?,
                            .{ .short = tag },
                        };
                        kvidx += 1;
                    }
                }
                std.debug.assert(kv_len == kvidx);
                // for (kvs) |kv| @compileLog(kv[0], @tagName(kv[1]));
                for (kvs) |kv| {
                    var count: u8 = 0;
                    for (kvs) |kv2| {
                        count += @intFromBool(mem.eql(u8, kv[0], kv2[0]));
                        if (count > 1) @compileError("duplicate key '" ++ kv[0] ++ "'");
                    }
                }
                const ret = &kvs;
                return ret;
            }
        }

        fn parseTuple(comptime V: type, ctx: Ctx) !V {
            const info = @typeInfo(V);
            const args = ctx.args;
            const fields = info.Struct.fields;

            var payload: V = initEmpty(V) catch undefined;
            errdefer deinitPayload(V, payload, ctx.parse_options.allocator);
            debug("parsing tuple. arg {s}", .{args.*[0]});

            inline for (fields) |f| {
                if (args.len == 0) return error.MissingFields;
                @field(payload, f.name) = try parsePayload(
                    f.type,
                    ctx,
                    CtCtx(f.type).init(
                        f.name,
                        clarpOptions(f.type),
                        if (@hasDecl(V, "clarp_options"))
                            @field(V.clarp_options.fields, f.name).desc
                        else
                            null,
                    ),
                );
            }

            return payload;
        }

        fn NamedOption(comptime V: type, comptime FieldEnum: type) type {
            return union(enum) {
                override: Override(V),
                end_mark,
                short: FieldEnum,
                long: FieldEnum,
                help,
            };
        }

        fn Kv(comptime V: type, comptime FieldEnum: type) type {
            comptime return struct { []const u8, NamedOption(V, FieldEnum) };
        }

        // TODO - colored errors somehow
        // fn printError(f: std.fs.File, args: []const []const u8, rest: []const []const u8) !void {
        //     if (@import("builtin").is_test) return;
        //     const writer = f.writer();
        //     // count bytes written for error formatting
        //     var cw = std.io.countingWriter(writer);
        //     const cwriter = cw.writer();
        //     try std.io.tty.detectConfig(f).setColor(f, .bright_red);
        //     try cwriter.writeAll("error");
        //     try std.io.tty.detectConfig(f).setColor(f, .reset);
        //     const err_pos = args.len - rest.len;
        //     try cwriter.print(" at argument {}: ", .{err_pos});
        //     // stop counting bytes at err_pos
        //     var w = cwriter.any();
        //     for (args[1..], 1..) |arg, i| {
        //         if (i != 1) try w.writeAll(" ");
        //         if (i >= err_pos) w = writer.any();
        //         const has_space = mem.indexOfScalar(u8, arg, ' ') != null;
        //         if (has_space) try w.writeByte('\'');
        //         try w.writeAll(arg);
        //         if (has_space) try w.writeByte('\'');
        //     }
        //     try writer.writeByte('\n');
        //     for (0..cw.bytes_written) |_| try writer.writeAll(" ");
        //     // colored pointer and squiggles
        //     try std.io.tty.detectConfig(f).setColor(f, .bright_yellow);
        //     try writer.writeAll("^");
        //     try std.io.tty.detectConfig(f).setColor(f, .yellow);
        //     if (err_pos < args.len)
        //         for (0..args[err_pos].len -| 1) |_| try writer.writeAll("~");
        //     try std.io.tty.detectConfig(f).setColor(f, .reset);
        //     try writer.writeAll("\n");
        // }

        fn printError(writer: std.io.AnyWriter, args: []const []const u8, rest: []const []const u8) !void {
            if (@import("builtin").is_test) return;
            // count bytes written for error formatting
            var cw = std.io.countingWriter(writer);
            const cwriter = cw.writer();
            try cwriter.writeAll("error");
            const err_pos = args.len - rest.len;
            try cwriter.print(" at argument {}: ", .{err_pos});
            // stop counting bytes at err_pos
            var w = cwriter.any();
            for (args[1..], 1..) |arg, i| {
                if (i != 1) try w.writeAll(" ");
                if (i >= err_pos) w = writer;
                const has_space = mem.indexOfScalar(u8, arg, ' ') != null;
                if (has_space) try w.writeByte('\'');
                try w.writeAll(arg);
                if (has_space) try w.writeByte('\'');
            }
            try writer.writeByte('\n');
            for (0..cw.bytes_written) |_| try writer.writeAll(" ");
            try writer.writeAll("^");
            if (err_pos < args.len)
                for (0..args[err_pos].len -| 1) |_| try writer.writeAll("~");
            try writer.writeAll("\n");
        }

        /// when `fmt` is "help", calls help()
        /// when `fmt` is empty, calls dump()
        pub fn format(
            self: Self,
            comptime fmt: []const u8,
            fmt_opts: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            if (fmt.len == 0)
                try dump(self.root, fmt, fmt_opts, writer, 0)
            else if (comptime mem.eql(u8, fmt, "help")) {
                try help(Root, self.args, self.rest, writer);
            } else @compileError("unknown fmt '" ++ fmt ++ "'");
        }

        pub fn dump(
            v: anytype,
            comptime fmt: []const u8,
            fmt_opts: std.fmt.FormatOptions,
            writer: anytype,
            depth: u8,
        ) !void {
            const V = @TypeOf(v);
            switch (@typeInfo(V)) {
                else => |x| @compileError("TODO " ++ @tagName(x) ++ " " ++ @typeName(V)),
                .Void => {},
                .Int, .Bool, .Float => try std.fmt.formatType(v, fmt, fmt_opts, writer, 0),
                .Array => try std.fmt.formatType(v, "any", fmt_opts, writer, 0),
                .Optional => if (v) |u|
                    try dump(u, fmt, fmt_opts, writer, depth)
                else
                    try writer.writeAll("null"),
                .Pointer => |x| if (x.child == anyopaque)
                    try dump(v, "*", fmt_opts, writer, depth)
                else switch (x.size) {
                    .Slice => if (comptime isZigString(V))
                        try writer.print("\"{s}\"", .{v})
                    else if (comptime isZigString(x.child))
                        try writer.print("{s}", .{v})
                    else
                        try std.fmt.formatType(v, "any", fmt_opts, writer, 0),
                    .One => if (V == *anyopaque)
                        try writer.print("{*}", .{v})
                    else
                        try dump(v.*, fmt, fmt_opts, writer, depth),
                    else => @compileError("TODO " ++ @tagName(x) ++ " " ++ @typeName(V)),
                },
                .Enum => try writer.writeAll(@tagName(v)),
                .Struct => |x| inline for (x.fields) |f| {
                    try writer.writeByte('\n');
                    try writer.writeByteNTimes(' ', depth * 2);
                    try writer.print("{s}: ", .{f.name});
                    try dump(@field(v, f.name), fmt, fmt_opts, writer, depth + 1);
                },
                .Union => switch (v) {
                    inline else => |payload, tag| {
                        try writer.writeByte('\n');
                        try writer.writeByteNTimes(' ', depth * 2);
                        try writer.print("{s}: ", .{@tagName(tag)});
                        try dump(payload, fmt, fmt_opts, writer, depth + 1);
                    },
                },
            }
        }

        fn help(
            comptime V: type,
            init_args: []const []const u8,
            rest_args: []const []const u8,
            writer: std.io.AnyWriter,
        ) !void {
            const has_help = comptime isContainer(V) and
                @hasDecl(V, "clarp_options") and V.clarp_options.help != null;
            if (!has_help) {
                try options.printUsage(V, writer, init_args, rest_args);
            }

            try printHelp(V, writer, 1, longestFieldLen(V));

            if (!has_help) {
                try writer.writeAll("\n\nGeneral Options:\n\n");
                var cwriter = std.io.countingWriter(writer);
                const w = cwriter.writer();
                try w.writeAll("  ");
                inline for (@typeInfo(options.help_flags).Enum.fields, 0..) |f, i| {
                    if (i != 0) try w.writeAll(", ");
                    try w.writeAll(f.name);
                }
                try writer.writeByteNTimes(' ', options.help_description_start_column - cwriter.bytes_written);
                try writer.writeAll("Print command specific usage.");
                try writer.writeAll("\n\n");
            }
        }

        fn printHelp(
            comptime V: type,
            writer: std.io.AnyWriter,
            depth: u8, // TODO remove
            comptime buflen: u16,
        ) !void {
            switch (@typeInfo(V)) {
                else => |x| @compileError("TODO " ++ @tagName(x)),
                .Void, .Bool => {},
                .Int, .Float => try writer.writeAll(": " ++ @typeName(V)),
                .Enum => |e| {
                    try writer.writeAll(": enum { ");
                    inline for (e.fields, 0..) |f, i| {
                        if (i != 0) try writer.writeAll(", ");
                        try writer.writeAll(f.name);
                    }
                    try writer.writeAll(" }");
                },
                .Array => |x| try writer.print(
                    ": [{}]{s}",
                    .{ x.len, @typeName(x.child) },
                ),
                .Optional => |x| try writer.print(": ?{s}", .{@typeName(x.child)}),
                .Pointer => if (comptime isZigString(V))
                    try writer.writeAll(": string")
                else
                    try writer.writeAll(": " ++ @typeName(V)),
                .Struct => |x| {
                    const has_options = @hasDecl(V, "clarp_options");
                    if (has_options and V.clarp_options.help != null) {
                        try writer.writeAll(V.clarp_options.help.?);
                        return;
                    }
                    try writer.writeAll("Options:\n");
                    var buf: [buflen]u8 = undefined;
                    inline for (x.fields, 0..) |f, fi| {
                        try writer.writeByte('\n');
                        var cwriter = std.io.countingWriter(writer);
                        const w = cwriter.writer();
                        try w.writeByteNTimes(' ', depth * 2);
                        if (has_options) {
                            if (@field(V.clarp_options.fields, f.name).help) |h| {
                                try w.writeAll(h);
                                continue;
                            }
                        }
                        if (!x.is_tuple) {
                            try w.writeAll("--");
                            const name = options.caseFn(&buf, f.name);
                            try w.writeAll(name);
                        }
                        try printShort(V, x.fields, w, fi);
                        if (!isContainer(f.type))
                            try printHelp(f.type, w.any(), depth, buflen);
                        if (f.default_value) |d| {
                            const dv = @as(*const f.type, @ptrCast(@alignCast(d))).*;
                            switch (@typeInfo(f.type)) {
                                else => if (comptime isZigString(f.type))
                                    try w.print(" = \"{s}\"", .{dv})
                                else
                                    try w.print(" = {}", .{dv}),
                                .Enum => try w.print(" = {s}", .{@tagName(dv)}),
                                .Bool => if (dv) try w.writeAll(" = true"),
                            }
                        }
                        try printDesc(V, writer, f, cwriter.bytes_written);
                    }
                },
                .Union => |x| {
                    const has_options = @hasDecl(V, "clarp_options");
                    if (has_options and V.clarp_options.help != null) {
                        try writer.writeAll(V.clarp_options.help.?);
                        return;
                    }
                    try writer.writeAll("Commands:\n");
                    var buf: [buflen]u8 = undefined;
                    inline for (x.fields, 0..) |f, fi| {
                        try writer.writeByte('\n');
                        var cwriter = std.io.countingWriter(writer);
                        const w = cwriter.writer();
                        try w.writeByteNTimes(' ', depth * 2);
                        if (has_options) {
                            if (@field(V.clarp_options.fields, f.name).help) |h| {
                                try w.writeAll(h);
                                continue;
                            }
                        }
                        const fname = options.caseFn(&buf, f.name);
                        try w.writeAll(fname);
                        try printShort(V, x.fields, w, fi);
                        if (!isContainer(f.type))
                            try printHelp(f.type, w.any(), depth, buflen);
                        try printDesc(V, writer, f, cwriter.bytes_written);
                    }
                },
            }
        }

        fn printShort(comptime V: type, comptime vfields: anytype, writer: anytype, comptime fieldi: usize) !void {
            const field = vfields[fieldi];
            if (@hasDecl(V, "clarp_options")) {
                const opt: FieldOption = @field(V.clarp_options.fields, field.name);
                if (opt.short) |short| {
                    try writer.print(", {s}", .{short});
                    return;
                }
                if (V.clarp_options.derive_short_names) {
                    const short = @typeInfo(ShortNames(vfields, V)).Enum.fields[fieldi];
                    const info = @typeInfo(V);
                    switch (info) {
                        .Struct => try writer.print(", -{s}", .{short.name}),
                        .Union => try writer.print(", {s}", .{short.name}),
                        else => unreachable,
                    }
                }
            }
        }

        fn printDesc(
            comptime V: type,
            writer: anytype,
            field: anytype,
            bytes_written: usize,
        ) !void {
            if (@hasDecl(V, "clarp_options")) {
                const opt: FieldOption = @field(V.clarp_options.fields, field.name);
                if (opt.desc) |d| {
                    if (options.help_description_start_column > bytes_written)
                        try writer.writeByteNTimes(' ', options.help_description_start_column - bytes_written)
                    else {
                        try writer.writeByte('\n');
                        try writer.writeByteNTimes(' ', options.help_description_start_column);
                    }
                    try writer.print("{s}", .{d});
                }
            }
        }

        pub fn deinit(self: Self, allocator: ?mem.Allocator) void {
            deinitPayload(Self.Root, self.root, allocator);
        }

        pub fn deinitPayload(comptime V: type, payload: V, allocator: ?mem.Allocator) void {
            switch (@typeInfo(V)) {
                else => |x| @compileError("TODO " ++ @tagName(x)),
                .Void, .Int, .Float, .Bool, .Enum, .Array => {},
                .Optional => |x| if (payload) |u|
                    deinitPayload(x.child, u, allocator),
                .Pointer => |x| switch (x.size) {
                    .Slice => if (!comptime isZigString(V)) {
                        const a = allocator orelse return;
                        a.free(payload);
                    },
                    else => @compileError("TODO Pointer " ++ @tagName(x.size)),
                },
                .Struct => |x| inline for (x.fields) |f| {
                    deinitPayload(f.type, @field(payload, f.name), allocator);
                },
                .Union => switch (payload) {
                    inline else => |upayload| deinitPayload(@TypeOf(upayload), upayload, allocator),
                },
            }
        }
    };
}

pub const Ctx = struct {
    init_args: []const []const u8,
    args: *[]const []const u8,
    parse_options: ParseOptions,

    pub fn init(
        init_args: []const []const u8,
        args: *[]const []const u8,
        parse_options: ParseOptions,
    ) Ctx {
        return .{
            .init_args = init_args,
            .args = args,
            .parse_options = parse_options,
        };
    }
};

fn CtCtx(comptime V: type) type {
    return struct {
        field_name: ?[]const u8,
        clarp_options: Options(V),
        outer_desc: ?[]const u8,

        pub fn init(
            field_name: ?[]const u8,
            clarp_options: Options(V),
            outer_desc: ?[]const u8,
        ) @This() {
            return .{
                .field_name = field_name,
                .clarp_options = clarp_options,
                .outer_desc = outer_desc,
            };
        }
    };
}

// const show_debug = {};
fn debug(comptime fmt: []const u8, args: anytype) void {
    if (@hasDecl(@This(), "show_debug"))
        log.debug(fmt, args);
}

fn logErr(comptime fmt: anytype, args: anytype, writer: ?std.io.AnyWriter) !void {
    if (@import("builtin").is_test) return;
    try (writer orelse return).print("error(cli-args): " ++ fmt, args);
}

fn isContainer(comptime T: type) bool {
    const info = @typeInfo(T);
    return info == .Struct or info == .Union or info == .Opaque;
}

fn clarpOptions(comptime T: type) Options(T) {
    return if (isContainer(T) and @hasDecl(T, "clarp_options"))
        T.clarp_options
    else
        .{};
}

fn longestFieldLen(comptime V: type) usize {
    comptime {
        var len: u16 = 0;
        if (isContainer(V)) {
            for (std.meta.fields(V)) |f|
                len = @max(len, f.name.len);
        }
        return len;
    }
}

/// returns an enum of shortest possible distinct field names
fn ShortNames(vfields: anytype, comptime V: type) type {
    var fields: [vfields.len]std.builtin.Type.EnumField = undefined;
    const clarp_options = clarpOptions(V);
    for (vfields, 0..) |sf, i| {
        const field_opt: FieldOption = @field(clarp_options.fields, sf.name);
        if (field_opt.short) |short| {
            fields[i] = .{
                .name = (short ++ [1]u8{0})[0..short.len :0],
                .value = i,
            };
            continue;
        }
        var preflen: usize = 1;
        // search previous fields. if duplicate field name found, increase len

        const name = while (preflen <= sf.name.len) : (preflen += 1) {
            const name: []const u8 = sf.name[0..preflen];
            const found = for (0..i) |j| {
                if (mem.eql(u8, fields[j].name, name)) {
                    break true;
                }
            } else false;
            if (!found) break name;
        };
        fields[i] = .{
            .name = (name ++ [1]u8{0})[0..name.len :0],
            .value = i,
        };
    }

    return @Type(.{ .Enum = .{
        .fields = &fields,
        .tag_type = std.math.IntFittingRange(0, vfields.len),
        .decls = &.{},
        .is_exhaustive = true,
    } });
}

fn isFlagType(comptime U: type) bool {
    return U == bool;
}

// return true if `U` must consume 1 or more args.
// return false if `U` may consume 0 args.
inline fn mustConsume(comptime U: type) bool {
    comptime {
        const info = @typeInfo(U);
        const result = switch (info) {
            .Void, .Bool => false,
            // structs are mustConsume if any fields are mustConsume
            .Struct => |x| for (x.fields) |f| {
                if (mustConsume(f.type) and
                    f.default_value == null)
                    break true;
            } else false,
            .Union => true,
            .Enum, .Int, .Float => true,
            .Optional => |x| mustConsume(x.child),
            .Pointer => |x| mustConsume(x.child),
            .Array => |x| x.len != 0,
            else => |x| @compileError("TODO " ++ @tagName(x)),
        };
        return result;
    }
}

fn initEmpty(comptime V: type) !V {
    const info = @typeInfo(V);
    return switch (info) {
        .Pointer => |x| switch (x.size) {
            .Slice => return &.{},
            else => std.debug.panic("TODO {s} {s}", .{ @tagName(x), @typeName(V) }),
        },
        .Union => error.CantInitEmptyUnion,
        .Struct => |x| {
            var v: V = undefined;
            inline for (x.fields) |f| {
                if (f.default_value) |dv|
                    @field(v, f.name) = @as(*const f.type, @ptrCast(@alignCast(dv))).*
                else
                    @field(v, f.name) = try initEmpty(f.type);
            }
            return v;
        },
        else => std.mem.zeroes(V), // std.debug.panic("TODO {s} {s}", .{ @tagName(x), @typeName(V) }),
    };
}

test mustConsume {
    try std.testing.expect(mustConsume(struct {
        a: void,
        b: u8,
    }));
    try std.testing.expect(!mustConsume(struct {
        a: void,
        b: u8 = 0,
    }));
    try std.testing.expect(mustConsume(union(enum) {
        a: u8,
    }));
    try std.testing.expect(!mustConsume(union(enum) {
        a: u8,
        b,
    }));
}

fn isZigString(comptime T: type) bool {
    return comptime blk: {
        // Only pointer types can be strings, no optionals
        const info = @typeInfo(T);
        if (info != .Pointer) break :blk false;
        const ptr = info.Pointer;
        // Check for CV qualifiers that would prevent coerction to []const u8
        if (ptr.is_volatile or ptr.is_allowzero) break :blk false;
        // If it's already a slice, simple check.
        if (ptr.size == .Slice) break :blk ptr.child == u8;

        // Otherwise check if it's an array type that coerces to slice.
        // if (ptr.size == .One) {
        //     const child = @typeInfo(ptr.child);
        //     if (child == .Array) {
        //         const arr = &child.Array;
        //         break :blk arr.child == u8;
        //     }
        // }
        break :blk false;
    };
}
