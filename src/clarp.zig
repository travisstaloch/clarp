//! clarp - a cli arg parser
//!
//!

const std = @import("std");
const mem = std.mem;
const log = std.log.scoped(.clarp);

pub const Option = struct {
    alias: ?[]const u8 = null,
    desc: ?[]const u8 = null,
};

pub fn Options(comptime T: type) type {
    const Fe = std.meta.FieldEnum(T);
    return std.enums.EnumFieldStruct(Fe, Option, .{});
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
    err_writer: ?std.io.AnyWriter = null,
};

pub const HelpOptions = struct {
    err_writer: ?std.io.AnyWriter = null,
};

fn logErr(comptime fmt: anytype, args: anytype) void {
    if (@import("builtin").is_test) return;
    log.err(fmt, args);
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
pub fn Parser(
    comptime T: type,
    comptime options: struct {
        help_flags: type = HelpFlags,
        usage_fmt: []const u8 = default_usage_fmt,
        printUsage: fn (
            writer: std.io.AnyWriter,
            comptime fmt: []const u8,
            exe_path: []const u8,
        ) void = defaultPrintUsage,
        caseFn: CaseFn = caseSame,
    },
) type {
    return struct {
        root: Root,
        exe_path: []const u8,
        user_context: ?*anyopaque = null,

        const Self = @This();
        pub const Root = T;

        /// parse command line args
        pub fn parse(args: []const []const u8, parse_options: ParseOptions) !Self {
            // log.debug("args[1] {s}", .{args[1]});
            var rest = args[1..];
            if (rest.len != 0) {
                if (std.meta.stringToEnum(options.help_flags, rest[0])) |_| {
                    help(args[0], .{ .err_writer = parse_options.err_writer });
                    return error.HelpShown;
                }
            }
            const root = parsePayload(&rest, T, null, parse_options) catch |e| {
                try printError(std.io.getStdErr(), args, rest);
                return e;
            };
            return if (rest.len != 0)
                error.ExtraArgs
            else
                .{ .root = root, .exe_path = args[0] };
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

        pub fn parsePayload(
            args: *[]const []const u8,
            comptime V: type,
            field_name: ?[]const u8,
            parse_options: ParseOptions,
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
                    logErr("invalid bool '{s}'", .{args.*[0]});
                    return error.InvalidBoolean;
                },
                .Optional => |x| if (mem.eql(u8, args.*[0], "null")) {
                    args.* = args.*[1..];
                    return null;
                } else return try parsePayload(args, x.child, field_name, parse_options),
                .Enum => if (std.meta.stringToEnum(V, args.*[0])) |e| {
                    args.* = args.*[1..];
                    return e;
                } else {
                    logErr("invalid enum tag '{s}'", .{args.*[0]});
                    return error.InvalidEnum;
                },
                .Array => |x| {
                    if (args.*[0].len > x.len) return error.ArrayTooShort;
                    defer args.* = args.*[1..];
                    var a: V = undefined;
                    @memcpy(a[0..args.*[0].len], args.*[0]);
                    return a;
                },
                .Union => |x| {
                    const Shorts = ShortNames(x.fields);
                    comptime var buflen: u16 = 0;
                    inline for (x.fields) |f| buflen = @max(buflen, f.name.len);
                    comptime var buf: [buflen]u8 = undefined;
                    inline for (x.fields, @typeInfo(Shorts).Enum.fields) |uf, sf| {
                        const tagname = comptime options.caseFn(&buf, uf.name);
                        if (mem.eql(u8, sf.name, args.*[0]) or
                            mem.eql(u8, tagname, args.*[0]))
                        {
                            args.* = args.*[1..];
                            return @unionInit(V, uf.name, try parsePayload(
                                args,
                                uf.type,
                                uf.name,
                                parse_options,
                            ));
                        }
                    }

                    logErr("unknown command '{s}'", .{args.*[0]});
                    return error.UnknownCommand;
                },
                .Struct => |x| {
                    var payload: V = mem.zeroInit(V, .{});
                    var fields_seen = std.StaticBitSet(x.fields.len).initEmpty();

                    args: while (args.len > 0 and
                        (fields_seen.count() < x.fields.len or
                        @hasDecl(V, "overrides")))
                    {
                        inline for (x.fields, 0..) |f, i| {
                            if (fields_seen.isSet(i))
                                log.debug("{s}: {any}", .{ f.name, @field(payload, f.name) });
                        }

                        if (field_name != null and
                            (@hasDecl(V, "end_mark") and
                            mem.eql(u8, args.*[0], V.end_mark)) or
                            (mem.startsWith(u8, args.*[0], "--end-") and
                            mem.eql(u8, args.*[0][6..], field_name.?)))
                        {
                            args.* = args.*[1..];
                            return payload;
                        }

                        if (@hasDecl(V, "overrides")) {
                            inline for (comptime std.meta.declarations(V.overrides)) |decl| {
                                if (mem.eql(u8, decl.name, args.*[0])) {
                                    args.* = args.*[1..];
                                    const userParseFn: UserParseFn = @field(V.overrides, decl.name);
                                    userParseFn(args, parse_options.user_ctx);
                                    continue :args;
                                }
                            }
                        }

                        // look for derived short names
                        if (@hasDecl(V, "derive_short_names") and
                            V.derive_short_names and
                            mem.startsWith(u8, args.*[0], "-"))
                        {
                            const vfields = @typeInfo(V).Struct.fields;
                            const Shorts = ShortNames(vfields);
                            inline for (@typeInfo(Shorts).Enum.fields, 0..) |f, fi| {
                                if (mem.eql(u8, args.*[0][1..], f.name)) {
                                    log.debug("found derived short name {s} {s}", .{ args.*[0], f.name });
                                    const Ft = @TypeOf(@field(payload, vfields[fi].name));
                                    args.* = args.*[@intFromBool(!isFlagType(Ft))..];
                                    @field(payload, vfields[fi].name) =
                                        try parsePayload(args, Ft, vfields[fi].name, parse_options);
                                    fields_seen.set(fi);
                                    continue :args;
                                }
                            }
                        }

                        // look for alias names
                        if (@hasDecl(V, "options")) {
                            inline for (@typeInfo(@TypeOf(V.options)).Struct.fields, 0..) |f, fi| {
                                const opt: Option = @field(V.options, f.name);
                                const alias = opt.alias orelse continue;
                                if (mem.eql(u8, args.*[0], alias)) {
                                    log.debug("found alias {s} {s}", .{ args.*[0], f.name });
                                    const Ft = @TypeOf(@field(payload, f.name));
                                    args.* = args.*[@intFromBool(!isFlagType(Ft))..];
                                    @field(payload, f.name) =
                                        try parsePayload(args, Ft, alias, parse_options);
                                    fields_seen.set(fi);
                                    continue :args;
                                }
                            }
                        }

                        // look for long names
                        log.debug("arg {s}", .{args.*[0]});
                        const is_long = mem.startsWith(u8, args.*[0], "--");
                        if (is_long) {
                            comptime var buflen: u16 = 0;
                            inline for (x.fields) |f| buflen = @max(buflen, f.name.len);
                            comptime var buf: [buflen]u8 = undefined;
                            inline for (x.fields, 0..) |f, fi| {
                                const fname = comptime options.caseFn(&buf, f.name);
                                if (mem.eql(u8, args.*[0][2..], fname)) {
                                    log.debug("found long {s} {s}", .{ args.*[0], fname });
                                    args.* = args.*[@intFromBool(!isFlagType(f.type))..];
                                    @field(payload, f.name) = try parsePayload(args, f.type, fname, parse_options);
                                    fields_seen.set(fi);
                                    continue :args;
                                }
                            }
                            // error if positional starts with '--'
                            logErr("unknown option '{s}'", .{args.*[0]});
                            return error.UnknownOption;
                        }

                        // positionals
                        log.debug("parsing positional. fields seen {} arg {s}", .{ fields_seen.count(), args.*[0] });
                        var iter = fields_seen.iterator(.{ .kind = .unset });
                        const next_fieldi = iter.next() orelse {
                            logErr("extra args {s}", .{args.*});
                            return error.ExtraArgs;
                        };
                        inline for (x.fields, 0..) |f, fi| {
                            if (fi == next_fieldi) {
                                @field(payload, f.name) = try parsePayload(args, f.type, f.name, parse_options);
                                fields_seen.set(fi);
                                continue :args;
                            }
                        }

                        @panic("unreachable");
                    }

                    // set field default values if provided
                    inline for (x.fields, 0..) |f, i| {
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

                    log.debug("fields seen {}/{}", .{ fields_seen.count(), x.fields.len });
                    const field_names = std.meta.fieldNames(V);
                    if (field_names.len != 0 and fields_seen.count() != x.fields.len) {
                        logErr("missing fields: ", .{});
                        var iter = fields_seen.iterator(.{ .kind = .unset });
                        while (iter.next()) |fi| {
                            logErr("'{s}', ", .{field_names[fi]});
                        }
                        return error.MissingFields;
                    }

                    return payload;
                },
            }
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
                help(self.exe_path, .{ .err_writer = writer });
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

        pub fn help(exe_path: []const u8, help_options: HelpOptions) void {
            const writer = help_options.err_writer orelse std.io.getStdErr().writer().any();
            options.printUsage(writer, options.usage_fmt, std.fs.path.basename(exe_path));
            writer.writeAll("\n  ") catch unreachable;
            inline for (@typeInfo(options.help_flags).Enum.fields, 0..) |f, i| {
                if (i != 0) writer.writeAll(" ") catch unreachable;
                writer.writeAll(f.name) catch unreachable;
            }
            writer.writeAll(" // show this message. must be first argument.") catch unreachable;
            printHelp(T, "", .{}, writer, 1) catch unreachable;
            writer.writeAll("\n\n") catch unreachable;
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
                    comptime var buflen: u16 = 0;
                    inline for (x.fields) |f| buflen = @max(buflen, f.name.len);
                    comptime var buf: [buflen]u8 = undefined;
                    inline for (x.fields, 0..) |f, fi| {
                        try writer.writeByte('\n');
                        try writer.writeByteNTimes(' ', depth * 2);
                        if (!x.is_tuple) {
                            try writer.writeAll("--");
                            try writer.writeAll(comptime options.caseFn(&buf, f.name));
                        }
                        try printShort(V, x.fields, writer, fi);
                        try printAlias(V, writer, f);
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
                    comptime var buflen: u16 = 0;
                    inline for (x.fields) |f| buflen = @max(buflen, f.name.len);
                    comptime var buf: [buflen]u8 = undefined;
                    inline for (x.fields, 0..) |f, fi| {
                        try writer.writeByte('\n');
                        try writer.writeByteNTimes(' ', depth * 2);
                        try writer.writeAll(comptime options.caseFn(&buf, f.name));
                        try printShort(V, x.fields, writer, fi);
                        try printAlias(V, writer, f);
                        try printHelp(f.type, fmt, fmt_opts, writer, depth + 1);
                        try printDesc(V, writer, f);
                    }
                },
            }
        }

        fn printAlias(comptime V: type, writer: anytype, field: anytype) !void {
            if (@hasDecl(V, "options")) {
                const opt: Option = @field(V.options, field.name);
                if (opt.alias) |alias| try writer.print(" {s}", .{alias});
            }
        }

        fn printShort(comptime V: type, vfields: anytype, writer: anytype, comptime fieldi: usize) !void {
            if (@hasDecl(V, "derive_short_names") and V.derive_short_names) {
                const alias = @typeInfo(ShortNames(vfields)).Enum.fields[fieldi];
                const info = @typeInfo(V);
                switch (info) {
                    .Struct => try writer.print(" -{s}", .{alias.name}),
                    .Union => try writer.print(" {s}", .{alias.name}),
                    else => unreachable,
                }
            }
        }

        fn printDesc(comptime V: type, writer: anytype, field: anytype) !void {
            if (@hasDecl(V, "options")) {
                const opt: Option = @field(V.options, field.name);
                if (opt.desc) |d| try writer.print(" // {s}", .{d});
            }
        }
    };
}

/// returns an enum of shortest possible distinct field names
fn ShortNames(vfields: anytype) type {
    var fields: [vfields.len]std.builtin.Type.EnumField = undefined;
    for (vfields, 0..) |sf, i| {
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
            .name = (name[0..name.len].* ++ [1]u8{0})[0..name.len :0],
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

pub fn isZigString(comptime T: type) bool {
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
