//!
//! clarp - a command line arg parser
//!

const std = @import("std");
const mem = std.mem;
const log = std.log.scoped(.@"cli-parsing");

pub const FieldOption = struct {
    /// an alternate option or command name for this field.  for a field named
    /// 'foo' and short '-f', either '--foo' and '-f' will match.
    ///
    /// if provided and 'derive_short_names' is true, this becomes the short
    /// name.
    short: ?[]const u8 = null,
    /// description for this field to be shown in help
    desc: ?[]const u8 = null,
    /// help text override for this field
    help: ?[]const u8 = null,
    /// changes the option or command name. effectively renames the field.
    long: ?[]const u8 = null,
};

pub fn Options(comptime T: type) type {
    const is_container = isContainer(T);
    return struct {
        /// field options.  a struct of optional `FieldOption` with field names
        /// matching the parent struct.
        fields: Fields = if (is_container) .{} else {},
        /// help text override for the entrire struct
        help: ?[]const u8 = null,
        /// when found, denotes end of input for this struct.  any further
        /// input may apply to other fields.  default '--end-foo' for a field
        /// named 'foo' .
        end_mark: ?[]const u8 = null,
        /// when true, short names will be derived for each field in the parent
        /// struct. i.e. struct{foo: u8, bar: u8} results in '-f' and '-b'
        /// derived short names
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

/// default flags for showing help/usage
pub const HelpFlags = enum { help, @"--help", @"-h" };
pub const default_usage_fmt = "\nUSAGE: {s} <command> <options>...\ncommands:";
pub fn defaultPrintUsage(writer: anytype, comptime fmt: []const u8, exe_path: []const u8) void {
    writer.print(fmt, .{exe_path}) catch unreachable;
}

pub const UserParseFn = fn (args: *[]const []const u8, ctx: ?*anyopaque) void;

pub const ParseOptions = struct {
    user_ctx: ?*anyopaque = null,
    err_file: ?std.fs.File = null,
};

fn logErr(comptime fmt: anytype, args: anytype, file: ?std.fs.File) !void {
    if (@import("builtin").is_test) return;
    const writer = (file orelse return).writer();
    try writer.print("error(cli-args): " ++ fmt, args);
}

const CaseFn = @TypeOf(caseSame);
fn caseSame(_: []u8, name: []const u8) []const u8 {
    return name;
}

pub fn caseKebab(buf: []u8, name: []const u8) []const u8 {
    for (0..name.len) |i|
        buf[i] = if (name[i] == '_') '-' else name[i];

    return buf[0..name.len];
}

/// global options
pub const ParserOptions = struct {
    help_flags: type = HelpFlags,
    usage_fmt: []const u8 = default_usage_fmt,
    printUsage: fn (
        writer: std.io.AnyWriter,
        comptime fmt: []const u8,
        exe_path: []const u8,
    ) void = defaultPrintUsage,
    caseFn: CaseFn = caseSame,
};

///
/// union types describe alternatives.  their field names don't require any
/// leading dashes and correspond to commands.
///
/// struct types describe optionally named sequences.  their field names require
/// leading dashes (ie --field-name).  when fields are named they may be given
/// out of order.  unnamed values will be assigned to the next unset field.
///
/// tuple types describe strictly unnamed sequences.
///
/// bool types are flags may be specified as --flag (or true/false when unnamed)
/// and they implicitly default to false.
pub fn Parser(comptime T: type, comptime options: ParserOptions) type {
    return struct {
        root: Root,
        exe_path: []const u8,
        user_context: ?*anyopaque = null,

        const Self = @This();
        pub const Root = T;

        /// parse command line args
        pub fn parseWithOptions(
            args: []const []const u8,
            parse_options: ParseOptions,
            clarp_options: Options(T),
        ) !Self {
            // log.debug("args[1] {s}", .{args[1]});
            var rest = args[1..];
            if (rest.len != 0) {
                if (std.meta.stringToEnum(options.help_flags, rest[0])) |_| {
                    const err_file = parse_options.err_file orelse std.io.getStdErr();
                    try help(args[0], err_file.writer().any());
                    return error.HelpShown;
                }
            }
            const root = parsePayload(&rest, T, null, parse_options, clarp_options) catch |e| {
                const err_file = parse_options.err_file orelse std.io.getStdErr();
                try printError(err_file, args, rest);
                return e;
            };
            return if (rest.len != 0)
                error.ExtraArgs
            else
                .{ .root = root, .exe_path = args[0] };
        }

        pub fn parse(args: []const []const u8, parse_options: ParseOptions) !Self {
            return parseWithOptions(args, parse_options, clarpOptions(T));
        }

        pub fn parsePayload(
            args: *[]const []const u8,
            comptime V: type,
            comptime field_name: ?[]const u8,
            parse_options: ParseOptions,
            comptime clarp_options: Options(V),
        ) !V {
            const info = @typeInfo(V);
            log.debug(
                "parsing {s} args len {} field name {?s}",
                .{ @tagName(info), args.len, field_name },
            );
            defer log.debug(
                "parsing {s} done args len {} field name {?s}",
                .{ @tagName(info), args.len, field_name },
            );
            if (args.len == 0) {
                return if (mustConsume(V))
                    error.NotEnoughArgs
                else
                    initEmpty(V) catch error.NotEnoughArgs;
            }

            switch (info) {
                else => |x| if (comptime isZigString(V)) {
                    defer args.* = args.*[1..];
                    return args.*[0];
                } else @compileError("TODO support " ++ @tagName(x)),
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
                    log.debug("bool {s}", .{args.*[0]});
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
                    try logErr("invalid bool '{s}'\n", .{args.*[0]}, parse_options.err_file);
                    return error.InvalidBoolean;
                },
                .Optional => |x| if (mem.eql(u8, args.*[0], "null")) {
                    args.* = args.*[1..];
                    return null;
                } else return try parsePayload(
                    args,
                    x.child,
                    field_name,
                    parse_options,
                    clarpOptions(x.child),
                ),
                .Enum => if (std.meta.stringToEnum(V, args.*[0])) |e| {
                    args.* = args.*[1..];
                    return e;
                } else {
                    try logErr("invalid enum tag '{s}'\n", .{args.*[0]}, parse_options.err_file);
                    return error.InvalidEnum;
                },
                .Array => |x| {
                    if (x.child == u8) {
                        if (args.*[0].len > x.len) return error.ArrayTooShort;
                        defer args.* = args.*[1..];
                        var a: V = undefined;
                        @memcpy(a[0..args.*[0].len], args.*[0]);
                        return a;
                    } else {
                        var a: V = undefined;
                        for (&a) |*ele| {
                            ele.* = try parsePayload(
                                args,
                                x.child,
                                field_name,
                                parse_options,
                                clarpOptions(x.child),
                            );
                        }
                        return a;
                    }
                },
                .Union => return try parseUnion(
                    args,
                    V,
                    field_name,
                    parse_options,
                    info,
                    if (@hasDecl(V, "clarp_options")) V.clarp_options else clarp_options,
                ),
                .Struct => return try parseStruct(
                    args,
                    V,
                    field_name,
                    parse_options,
                    info,
                    if (@hasDecl(V, "clarp_options")) V.clarp_options else clarp_options,
                ),
            }
        }

        fn parseUnion(
            args: *[]const []const u8,
            comptime V: type,
            comptime field_name: ?[]const u8,
            parse_options: ParseOptions,
            comptime info: std.builtin.Type,
            comptime clarp_options: Options(V),
        ) !V {
            const fields = info.Union.fields;
            const FieldEnum = std.meta.FieldEnum(V);
            const kvs = comptime GenKvs(V, ShortNames(fields, V), FieldEnum, info, clarp_options, field_name);
            const map = std.ComptimeStringMap(NamedOption(FieldEnum), kvs);

            if (map.get(args.*[0])) |named_option| {
                switch (named_option) {
                    .override => |override| {
                        log.debug("found override", .{});
                        args.* = args.*[1..];
                        override(args, parse_options.user_ctx);
                    },
                    .end_mark => return error.UnionEndMark,
                    .short, .long => |fe| if (@typeInfo(FieldEnum).Enum.fields.len > 0) {
                        switch (fe) {
                            inline else => |tag| {
                                log.debug("found {s} {s} {s}", .{ @tagName(named_option), args.*[0], @tagName(tag) });
                                args.* = args.*[1..];
                                const Ft = std.meta.TagPayload(V, tag);
                                return @unionInit(V, @tagName(tag), try parsePayload(
                                    args,
                                    Ft,
                                    @tagName(tag),
                                    parse_options,
                                    clarpOptions(Ft),
                                ));
                            },
                        }
                        unreachable;
                    },
                }
            }

            try logErr("unknown command '{s}'\n", .{args.*[0]}, parse_options.err_file);
            return error.UnknownCommand;
        }

        fn parseStruct(
            args: *[]const []const u8,
            comptime V: type,
            comptime field_name: ?[]const u8,
            parse_options: ParseOptions,
            comptime info: std.builtin.Type,
            comptime clarp_options: Options(V),
        ) !V {
            var payload: V = mem.zeroInit(V, .{});
            const fields = info.Struct.fields;
            var fields_seen = std.StaticBitSet(fields.len).initEmpty();
            const vfields = fields;

            const Short = ShortNames(fields, V);
            const FieldEnum = std.meta.FieldEnum(V);
            const kvs = comptime GenKvs(V, Short, FieldEnum, info, clarp_options, field_name);
            const map = std.ComptimeStringMap(NamedOption(FieldEnum), kvs);
            log.debug(
                "parseStruct() kvs.len {} V {s} fields {} {s}",
                .{ kvs.len, @typeName(V), fields.len, std.meta.fieldNames(V) },
            );

            args: while (args.len > 0 and
                (fields_seen.count() < fields.len or
                clarp_options.overrides != null))
            {
                inline for (fields, 0..) |f, i| {
                    if (fields_seen.isSet(i))
                        log.debug("{s}: {any}", .{ f.name, @field(payload, f.name) });
                }

                log.debug("args {s}", .{args.*});
                if (map.get(args.*[0])) |named_option| {
                    switch (named_option) {
                        .end_mark => {
                            log.debug("found end_mark", .{});
                            args.* = args.*[1..];
                            return payload;
                        },
                        .override => |override| {
                            log.debug("found override", .{});
                            args.* = args.*[1..];
                            override(args, parse_options.user_ctx);
                            continue :args;
                        },
                        .short, .long => |fe| if (@typeInfo(FieldEnum).Enum.fields.len > 0) {
                            switch (fe) {
                                inline else => |tag| {
                                    log.debug("found {s} {s} {s}", .{ @tagName(named_option), args.*[0], @tagName(tag) });
                                    const fi = @intFromEnum(tag);
                                    const Ft = @TypeOf(@field(payload, vfields[fi].name));
                                    args.* = args.*[@intFromBool(!isFlagType(Ft))..];
                                    @field(payload, @tagName(tag)) =
                                        try parsePayload(
                                        args,
                                        Ft,
                                        vfields[fi].name,
                                        parse_options,
                                        clarpOptions(Ft),
                                    );
                                    fields_seen.set(fi);
                                    continue :args;
                                },
                            }
                            unreachable;
                        },
                    }
                } else {
                    if (clarp_options.derive_short_names and
                        mem.startsWith(u8, args.*[0], "-"))
                    {
                        // parse shorts with -abc syntax where
                        // a, b, c are derived short field names
                        var arg = args.*[0][1..];
                        log.debug("arg {s} shorts {s}", .{ arg, std.meta.fieldNames(Short) });
                        shorts: while (arg.len > 0) {
                            inline for (@typeInfo(Short).Enum.fields, 0..) |f, fi| {
                                if (isFlagType(vfields[fi].type) and mem.startsWith(u8, arg, f.name)) {
                                    const Ft = @TypeOf(@field(payload, vfields[fi].name));
                                    // construct a fake, single flag arg.  this allows bool fields to work
                                    var tmp_args: []const []const u8 = &[_][]const u8{"--" ++ vfields[fi].name};
                                    @field(payload, vfields[fi].name) =
                                        try parsePayload(
                                        &tmp_args,
                                        Ft,
                                        vfields[fi].name,
                                        parse_options,
                                        clarpOptions(vfields[fi].type),
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
                            try logErr("unknown short option(s) '{s}'\n", .{arg}, parse_options.err_file);
                            return error.UnknownOption;
                        }
                    }

                    // positionals
                    log.debug("parsing positional. fields seen {} arg {s}", .{ fields_seen.count(), args.*[0] });
                    var iter = fields_seen.iterator(.{ .kind = .unset });
                    const next_fieldi = iter.next() orelse {
                        try logErr("extra args {s}\n", .{args.*}, parse_options.err_file);
                        return error.ExtraArgs;
                    };
                    inline for (fields, 0..) |f, fi| {
                        if (fi == next_fieldi) {
                            @field(payload, f.name) = try parsePayload(
                                args,
                                f.type,
                                f.name,
                                parse_options,
                                clarpOptions(f.type),
                            );
                            fields_seen.set(fi);
                            continue :args;
                        }
                    }

                    @panic("unreachable");
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

            log.debug("fields seen {}/{}", .{ fields_seen.count(), fields.len });
            const field_names = std.meta.fieldNames(V);
            if (field_names.len != 0 and fields_seen.count() != fields.len) {
                try logErr("missing fields: ", .{}, parse_options.err_file);
                var iter = fields_seen.iterator(.{ .kind = .unset });
                var i: u32 = 0;
                while (iter.next()) |fi| : (i += 1) {
                    if (i == 0)
                        try logErr("'{s}'", .{field_names[fi]}, parse_options.err_file)
                    else
                        try logErr(", '{s}'", .{field_names[fi]}, parse_options.err_file);
                }
                try logErr("\n", .{}, parse_options.err_file);
                return error.MissingFields;
            }

            return payload;
        }

        /// returns kv pair for each end_mark, override, derived short name, short, long name.
        /// each pair has type struct{[]const u8, NamedOption(FieldEnum)}
        fn GenKvs(
            comptime V: type,
            comptime Short: type,
            comptime FieldEnum: type,
            comptime info: std.builtin.Type,
            comptime clarp_options: Options(V),
            comptime field_name: ?[]const u8,
        ) []const Kv(FieldEnum) {
            comptime {
                // calculate the buffer size needed
                const end_mark_len: usize = @intFromBool(field_name != null or
                    clarp_options.end_mark != null);
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
                const kv_len: usize = end_mark_len + overrides_len +
                    dshorts_len + shorts_len + switch (info) {
                    .Struct => info.Struct.fields.len,
                    .Union => info.Union.fields.len,
                    else => unreachable,
                };

                // assign kvs
                var kvs: [kv_len]Kv(FieldEnum) = undefined;
                var kvidx: usize = 0;
                if (clarp_options.end_mark != null) {
                    kvs[kvidx] = .{ clarp_options.end_mark.?, .end_mark };
                    kvidx += 1;
                } else if (field_name != null) {
                    kvs[kvidx] = .{ "--end-".* ++ field_name.?, .end_mark };
                    kvidx += 1;
                }

                if (clarp_options.overrides) |overrides| {
                    for (std.meta.declarations(overrides)) |decl| {
                        kvs[kvidx] = .{ decl.name, .{
                            .override = @field(clarp_options.overrides.?, decl.name),
                        } };
                        kvidx += 1;
                    }
                }

                const short_prefix: []const u8 = if (info == .Struct) "-" else "";
                const long_prefix: []const u8 = if (info == .Struct) "--" else "";

                if (clarp_options.derive_short_names) {
                    for (std.meta.tags(Short), 0..) |tag, j| {
                        const fe = std.meta.tags(FieldEnum)[j];
                        if (@field(clarp_options.fields, @tagName(fe)).short != null)
                            continue;
                        kvs[kvidx] = .{ short_prefix ++ @tagName(tag), .{ .short = fe } };
                        kvidx += 1;
                    }
                }

                for (std.meta.tags(FieldEnum)) |tag| {
                    // add long
                    const tagname = @tagName(tag);
                    kvs[kvidx] = if (@field(clarp_options.fields, @tagName(tag)).long) |long|
                        .{ long_prefix ++ long, .{ .long = tag } }
                    else blk: {
                        var buf: [tagname.len]u8 = undefined;
                        const fname = options.caseFn(&buf, tagname);
                        break :blk .{ long_prefix ++ fname, .{ .long = tag } };
                    };
                    kvidx += 1;

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

        const Override = *const fn (*[]const []const u8, ?*anyopaque) void;

        fn NamedOption(comptime FieldEnum: type) type {
            return union(enum) {
                override: Override,
                end_mark,
                short: FieldEnum,
                long: FieldEnum,
            };
        }

        fn Kv(comptime FieldEnum: type) type {
            comptime return struct { []const u8, NamedOption(FieldEnum) };
        }

        fn printError(f: std.fs.File, args: []const []const u8, rest: []const []const u8) !void {
            if (@import("builtin").is_test) return;
            const writer = f.writer();
            // count bytes written for error formatting
            var cw = std.io.countingWriter(writer);
            const cwriter = cw.writer();
            try std.io.tty.detectConfig(f).setColor(f, .bright_red);
            try cwriter.writeAll("error");
            try std.io.tty.detectConfig(f).setColor(f, .reset);
            const err_pos = args.len - rest.len;
            try cwriter.print(" at argument {}: ", .{err_pos});
            // stop counting bytes at err_pos
            var w = cwriter.any();
            for (args[1..], 1..) |arg, i| {
                if (i != 1) try w.writeAll(" ");
                if (i >= err_pos) w = writer.any();
                const has_space = mem.indexOfScalar(u8, arg, ' ') != null;
                if (has_space) try w.writeByte('\'');
                try w.writeAll(arg);
                if (has_space) try w.writeByte('\'');
            }
            try writer.writeByte('\n');
            for (0..cw.bytes_written) |_| try writer.writeAll(" ");
            // colored pointer and squiggles
            try std.io.tty.detectConfig(f).setColor(f, .bright_yellow);
            try writer.writeAll("^");
            try std.io.tty.detectConfig(f).setColor(f, .yellow);
            if (err_pos < args.len)
                for (0..args[err_pos].len -| 1) |_| try writer.writeAll("~");
            try std.io.tty.detectConfig(f).setColor(f, .reset);
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
                try help(self.exe_path, writer);
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
                else => |x| if (comptime isZigString(V))
                    try writer.writeAll(v)
                else if (x == .Pointer and x.Pointer.size == .One) {
                    if (V == *anyopaque)
                        try writer.print("{*}", .{v})
                    else
                        try dump(v.*, fmt, fmt_opts, writer, depth);
                } else @compileError("TODO " ++ @tagName(x) ++ " " ++ @typeName(V)),
                .Void => {},
                .Int, .Bool, .Float => try std.fmt.formatType(v, fmt, fmt_opts, writer, 0),
                .Array => try std.fmt.formatType(v, "any", fmt_opts, writer, 0),
                .Optional => if (v) |u|
                    try dump(u, fmt, fmt_opts, writer, depth)
                else
                    try writer.writeAll("null"),
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

        pub fn help(exe_path: []const u8, writer: std.io.AnyWriter) !void {
            const has_help = @hasDecl(T, "clarp_options") and T.clarp_options.help != null;
            if (!has_help) {
                options.printUsage(writer, options.usage_fmt, std.fs.path.basename(exe_path));
                try writer.writeAll("\n  ");
                inline for (@typeInfo(options.help_flags).Enum.fields, 0..) |f, i| {
                    if (i != 0) try writer.writeAll(" ");
                    try writer.writeAll(f.name);
                }
                try writer.writeAll(" // show this message. must be first argument.");
            }
            try printHelp(T, "", .{}, writer, 1);
            if (!has_help)
                try writer.writeAll("\n\n");
        }

        pub fn printHelp(
            comptime V: type,
            comptime fmt: []const u8,
            fmt_opts: std.fmt.FormatOptions,
            writer: std.io.AnyWriter,
            depth: u8,
        ) !void {
            switch (@typeInfo(V)) {
                else => |x| if (comptime isZigString(V))
                    try writer.writeAll(": string")
                else
                    @compileError("TODO " ++ @tagName(x)),
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
                .Struct => |x| {
                    const has_options = @hasDecl(V, "clarp_options");
                    if (has_options and V.clarp_options.help != null) {
                        try writer.writeAll(V.clarp_options.help.?);
                        return;
                    }
                    comptime var buflen: u16 = 0;
                    inline for (x.fields) |f| buflen = @max(buflen, f.name.len);
                    var buf: [buflen]u8 = undefined;
                    inline for (x.fields, 0..) |f, fi| {
                        try writer.writeByte('\n');
                        try writer.writeByteNTimes(' ', depth * 2);
                        if (has_options) {
                            if (@field(V.clarp_options.fields, f.name).help) |h| {
                                try writer.writeAll(h);
                                continue;
                            }
                        }
                        if (!x.is_tuple) {
                            try writer.writeAll("--");
                            const name = options.caseFn(&buf, f.name);
                            try writer.writeAll(name);
                        }
                        try printShort(V, x.fields, writer, fi);
                        try printHelp(f.type, fmt, fmt_opts, writer, depth + 1);
                        if (f.default_value) |d| {
                            const dv = @as(*const f.type, @ptrCast(@alignCast(d))).*;
                            switch (@typeInfo(f.type)) {
                                else => if (comptime isZigString(f.type))
                                    try writer.print(" = \"{s}\"", .{dv})
                                else
                                    try writer.print(" = {}", .{dv}),
                                .Enum => try writer.print(" = {s}", .{@tagName(dv)}),
                                .Bool => if (dv) try writer.writeAll(" = true"),
                            }
                        }
                        try printDesc(V, writer, f);
                    }
                },
                .Union => |x| {
                    const has_options = @hasDecl(V, "clarp_options");
                    if (has_options and V.clarp_options.help != null) {
                        try writer.writeAll(V.clarp_options.help.?);
                        return;
                    }
                    comptime var buflen: u16 = 0;
                    inline for (x.fields) |f| buflen = @max(buflen, f.name.len);
                    var buf: [buflen]u8 = undefined;
                    inline for (x.fields, 0..) |f, fi| {
                        try writer.writeByte('\n');
                        try writer.writeByteNTimes(' ', depth * 2);
                        if (has_options) {
                            if (@field(V.clarp_options.fields, f.name).help) |h| {
                                try writer.writeAll(h);
                                continue;
                            }
                        }
                        const fname = options.caseFn(&buf, f.name);
                        try writer.writeAll(fname);
                        try printShort(V, x.fields, writer, fi);
                        try printHelp(f.type, fmt, fmt_opts, writer, depth + 1);
                        try printDesc(V, writer, f);
                    }
                },
            }
        }

        fn printShort(comptime V: type, comptime vfields: anytype, writer: anytype, comptime fieldi: usize) !void {
            const field = vfields[fieldi];
            if (@hasDecl(V, "clarp_options")) {
                const opt: FieldOption = @field(V.clarp_options.fields, field.name);
                if (opt.short) |short| {
                    try writer.print(" {s}", .{short});
                    return;
                }
                if (V.clarp_options.derive_short_names) {
                    const short = @typeInfo(ShortNames(vfields, V)).Enum.fields[fieldi];
                    const info = @typeInfo(V);
                    switch (info) {
                        .Struct => try writer.print(" -{s}", .{short.name}),
                        .Union => try writer.print(" {s}", .{short.name}),
                        else => unreachable,
                    }
                }
            }
        }

        fn printDesc(comptime V: type, writer: anytype, field: anytype) !void {
            if (@hasDecl(V, "clarp_options")) {
                const opt: FieldOption = @field(V.clarp_options.fields, field.name);
                if (opt.desc) |d| try writer.print(" // {s}", .{d});
            }
        }
    };
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
        .Bool => false,
        .Void => {},
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
        else => |x| std.debug.panic("TODO {s} {s}", .{ @tagName(x), @typeName(V) }),
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
