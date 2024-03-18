//! clarp - a cli arg parser
//!
//!

const std = @import("std");
const mem = std.mem;
const log = std.log.scoped(.cli_parse);

pub const Option = struct {
    alias: ?[]const u8 = null,
    desc: ?[]const u8 = null,
};

pub fn Options(comptime T: type) type {
    return std.enums.EnumFieldStruct(T, Option, .{});
}

/// default flags for showing help/usage
pub const HelpFlags = enum { help, @"--help", @"-h" };
pub const default_usage_fmt = "\nUSAGE: {s} <command> <options>...\ncommands:";
pub fn defaultPrintUsage(writer: anytype, comptime fmt: []const u8, exe_path: []const u8) void {
    writer.print(fmt, .{exe_path}) catch unreachable;
}

pub const UserParseFn = fn (args: *[]const []const u8, ctx: ?*anyopaque) void;
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
            writer: anytype,
            comptime fmt: []const u8,
            exe_path: []const u8,
        ) void = defaultPrintUsage,
    },
) type {
    return struct {
        root: Root,
        exe_path: []const u8,
        user_context: ?*anyopaque = null,

        const Self = @This();
        pub const Root = T;

        /// parse command line args without user_ctx
        pub fn parse(args: []const []const u8) !Self {
            return parseWithUserCtx(args, null);
        }

        /// parse command line args with user_ctx
        pub fn parseWithUserCtx(args: []const []const u8, user_ctx: ?*anyopaque) !Self {
            // log.debug("args[1] {s}", .{args[1]});
            var rest = args[1..];
            if (rest.len != 0) {
                if (std.meta.stringToEnum(options.help_flags, rest[0])) |_| {
                    help(args[0]);
                    return error.HelpShown;
                }
            }
            const root = parsePayload(&rest, T, null, user_ctx) catch |e| {
                try printError(std.io.getStdErr(), args, rest);
                return e;
            };
            const self = Self{
                .root = root,
                .exe_path = args[0],
            };
            if (rest.len != 0) return error.ExtraArgs;
            return self;
        }

        fn printError(f: std.fs.File, args: []const []const u8, rest: []const []const u8) !void {
            const writer = f.writer();
            // count bytes written for error formatting
            var cw = std.io.countingWriter(writer);
            const cwriter = cw.writer();
            try std.io.tty.detectConfig(f).setColor(f, .bright_red);
            try cwriter.writeAll("error:");
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

        fn isFlagType(comptime U: type) bool {
            return U == bool;
        }

        pub fn parsePayload(
            args: *[]const []const u8,
            comptime V: type,
            field_name: ?[]const u8,
            ctx: ?*anyopaque,
        ) !V {
            const info = @typeInfo(V);
            // structs and void
            if (info != .Void and info != .Struct and args.len == 0)
                return error.NotEnoughArgs;
            // log.debug("{s} {s}", .{ args.*, @typeName(V) });
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
                        if (mem.eql(u8, args.*[0], n)) {
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
                    log.err("invalid bool '{s}'", .{args.*[0]});
                    return error.InvalidBoolean;
                },
                .Optional => |x| if (mem.eql(u8, args.*[0], "null")) {
                    args.* = args.*[1..];
                    return null;
                } else return try parsePayload(args, x.child, field_name, ctx),
                .Enum => if (std.meta.stringToEnum(V, args.*[0])) |e| {
                    args.* = args.*[1..];
                    return e;
                } else {
                    log.err("invalid enum tag '{s}'", .{args.*[0]});
                    return error.InvalidEnum;
                },
                .Array => |x| {
                    if (args.*[0].len > x.len) return error.ArrayTooShort;
                    defer args.* = args.*[1..];
                    var a: V = undefined;
                    @memcpy(a[0..args.*[0].len], args.*[0]);
                    return a;
                },
                .Union => {
                    const tag = std.meta.stringToEnum(std.meta.Tag(V), args.*[0]) orelse {
                        log.err("unknown command '{s}'", .{args.*[0]});
                        return error.UnknownCommand;
                    };
                    args.* = args.*[1..];
                    switch (tag) {
                        inline else => |t| {
                            const tagname = comptime std.fmt.comptimePrint("{s}", .{@tagName(t)});
                            return @unionInit(V, @tagName(t), try parsePayload(
                                args,
                                std.meta.TagPayload(V, t),
                                tagname,
                                ctx,
                            ));
                        },
                    }
                },
                .Struct => |x| {
                    var payload: V = mem.zeroInit(V, .{});
                    var fields_seen = std.StaticBitSet(x.fields.len).initEmpty();

                    args: while (args.len > 0) {
                        inline for (x.fields, 0..) |f, i| {
                            if (fields_seen.isSet(i))
                                log.debug("{s}: {any}", .{ f.name, @field(payload, f.name) });
                        }

                        if (@hasDecl(V, "overrides")) {
                            inline for (comptime std.meta.declarations(V.overrides)) |decl| {
                                if (mem.eql(u8, decl.name, args.*[0])) {
                                    const userParseFn: UserParseFn = @field(V.overrides, decl.name);
                                    userParseFn(args, ctx);
                                    continue :args;
                                }
                            }
                        }

                        // look for alias names
                        if (@hasDecl(V, "options")) {
                            inline for (@typeInfo(@TypeOf(V.options)).Struct.fields) |sf| {
                                const opt: Option = @field(V.options, sf.name);
                                const alias = opt.alias orelse continue;
                                if (mem.eql(u8, args.*[0], alias)) {
                                    log.debug("found alias {s} {s}", .{ args.*[0], sf.name });
                                    const Ft = @TypeOf(@field(payload, sf.name));
                                    args.* = args.*[@intFromBool(!isFlagType(Ft))..];
                                    @field(payload, sf.name) = try parsePayload(args, Ft, alias, ctx);
                                    fields_seen.set(std.meta.fieldIndex(V, sf.name).?);
                                    continue :args;
                                }
                            }
                        }

                        log.debug("arg {s}", .{args.*[0]});
                        const is_long = mem.startsWith(u8, args.*[0], "--");
                        if (is_long) {
                            inline for (x.fields) |f| {
                                const fname = comptime std.fmt.comptimePrint("{s}", .{f.name});
                                if (mem.eql(u8, args.*[0][2..], fname)) {
                                    log.debug("found long {s} {s}", .{ args.*[0], fname });
                                    args.* = args.*[@intFromBool(!isFlagType(f.type))..];
                                    @field(payload, f.name) = try parsePayload(args, f.type, "--" ++ fname, ctx);
                                    fields_seen.set(std.meta.fieldIndex(V, f.name).?);
                                    continue :args;
                                }
                            }
                            // error if positional start with '--'
                            log.err("unknown option '{s}'", .{args.*[0]});
                            return error.UnknownOption;
                        }

                        log.debug("parsing positional. fields seen {} arg {s}", .{ fields_seen.count(), args.*[0] });
                        // positionals
                        var iter = fields_seen.iterator(.{ .kind = .unset });
                        const next_fieldi = iter.next() orelse {
                            log.err("extra args {s}", .{args.*});
                            return error.ExtraArgs;
                        };
                        inline for (x.fields, 0..) |f, fi| {
                            if (fi == next_fieldi) {
                                @field(payload, f.name) = try parsePayload(args, f.type, f.name, ctx);
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
                        log.err("missing fields: ", .{});
                        var iter = fields_seen.iterator(.{ .kind = .unset });
                        while (iter.next()) |fi| {
                            log.err("'{s}', ", .{field_names[fi]});
                        }
                        return error.MissingFields;
                    }

                    return payload;
                },
            }
        }

        pub fn format(
            self: Self,
            comptime fmt: []const u8,
            fmt_opts: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            if (fmt.len == 0)
                try dump(self.root, fmt, fmt_opts, writer, 0)
            else if (comptime mem.eql(u8, fmt, "help")) {
                help(self.exe_path);
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

        pub fn help(exe_path: []const u8) void {
            const writer = std.io.getStdErr().writer();
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
            writer: anytype,
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
                .Struct => |x| inline for (x.fields) |f| {
                    try writer.writeByte('\n');
                    try writer.writeByteNTimes(' ', depth * 2);
                    if (!x.is_tuple) try writer.print("--{s}", .{f.name});
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
                },
                .Union => |x| inline for (x.fields) |f| {
                    try writer.writeByte('\n');
                    try writer.writeByteNTimes(' ', depth * 2);
                    try writer.print("{s}", .{f.name});
                    try printAlias(V, writer, f);
                    try printHelp(f.type, fmt, fmt_opts, writer, depth + 1);
                    try printDesc(V, writer, f);
                },
            }
        }

        fn printAlias(comptime V: type, writer: anytype, field: anytype) !void {
            if (@hasDecl(V, "options")) {
                const opt: Option = @field(V.options, field.name);
                if (opt.alias) |alias| try writer.print(" {s}", .{alias});
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
