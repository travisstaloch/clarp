//! clarp - a cli arg parser
//!
//!

const std = @import("std");
const log = std.log.scoped(.cli_parse);
const clarp = @This();

pub const Option = struct {
    alias: ?[]const u8 = null,
    desc: ?[]const u8 = null,
};

pub fn Options(comptime T: type) type {
    return std.enums.EnumFieldStruct(T, Option, .{});
}

///
/// union types describe alternatives.  their field names don't require any
/// leading dashes and correspond to commands.
///
/// struct types describe optionally named sequences.  their field names require
/// leading dashes (ie --field-name).  when fields are named they may be given
/// out of order.  unnamed values will be assigned to the next unset field.
///
/// tuple types describe unnamed sequences.
///
/// bool types are flags may be specified as --flag (or true/false when unnamed)
/// and they implicitly default to false.
pub fn Command(comptime T: type) type {
    return struct {
        root: T,
        exe_path: []const u8,

        const Self = @This();

        pub fn parse(args: []const []const u8) !Self {
            // log.debug("args[1] {s}", .{args[1]});
            var rest = args[1..];
            const self = Self{
                .root = try parsePayload(&rest, T, null),
                .exe_path = args[0],
            };
            if (rest.len != 0) return error.ExtraArgs;
            return self;
        }

        fn isFlagType(comptime U: type) bool {
            return U == bool;
        }

        pub fn parsePayload(
            args: *[]const []const u8,
            comptime V: type,
            field_name: ?[]const u8,
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
                        if (std.mem.eql(u8, args.*[0], n)) {
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
                    return error.InvalidBoolean;
                },
                .Optional => |x| if (std.mem.eql(u8, args.*[0], "null")) {
                    args.* = args.*[1..];
                    return null;
                } else return try parsePayload(args, x.child, field_name),
                .Enum => if (std.meta.stringToEnum(V, args.*[0])) |e| {
                    args.* = args.*[1..];
                    return e;
                } else return error.InvalidEnum,
                .Array => |x| {
                    if (args.*[0].len > x.len) return error.ArrayTooShort;
                    defer args.* = args.*[1..];
                    var a: V = undefined;
                    @memcpy(a[0..args.*[0].len], args.*[0]);
                    return a;
                },
                .Union => {
                    var buf: [0x100]u8 = undefined;
                    const arg = try toCase(.snake, &buf, args.*[0]);
                    const tag = std.meta.stringToEnum(std.meta.Tag(V), arg) orelse {
                        std.debug.print("unknown command '{s}'\n", .{arg});
                        return error.UnknownCommand;
                    };
                    args.* = args.*[1..];
                    switch (tag) {
                        inline else => |t| {
                            const tagname = comptime std.fmt.comptimePrint("{}", .{kebab(@tagName(t))});
                            return @unionInit(V, @tagName(t), try parsePayload(
                                args,
                                std.meta.TagPayload(V, t),
                                tagname,
                            ));
                        },
                    }
                },
                .Struct => |x| {
                    var payload: V = std.mem.zeroInit(V, .{});
                    var fields_seen = std.StaticBitSet(x.fields.len).initEmpty();

                    args: while (args.len > 0) {
                        inline for (x.fields, 0..) |f, i| {
                            if (fields_seen.isSet(i))
                                log.debug("{s}: {any}", .{ f.name, @field(payload, f.name) });
                        }

                        // look for alias names
                        if (@hasDecl(V, "options")) {
                            inline for (@typeInfo(@TypeOf(V.options)).Struct.fields) |sf| {
                                const opt: Option = @field(V.options, sf.name);
                                const alias = opt.alias orelse continue;
                                if (std.mem.eql(u8, args.*[0], alias)) {
                                    log.debug("found alias {s} {s}", .{ args.*[0], sf.name });
                                    const Ft = @TypeOf(@field(payload, sf.name));
                                    args.* = args.*[@intFromBool(!isFlagType(Ft))..];
                                    @field(payload, sf.name) = try parsePayload(args, Ft, alias);
                                    fields_seen.set(std.meta.fieldIndex(V, sf.name).?);
                                    continue :args;
                                }
                            }
                        }

                        log.debug("arg {s}", .{args.*[0]});
                        const is_long = std.mem.startsWith(u8, args.*[0], "--");
                        if (is_long) {
                            inline for (x.fields) |f| {
                                const fname = comptime std.fmt.comptimePrint("{}", .{kebab(f.name)});
                                if (std.mem.eql(u8, args.*[0][2..], fname)) {
                                    log.debug("found long {s} {s}", .{ args.*[0], fname });
                                    args.* = args.*[@intFromBool(!isFlagType(f.type))..];
                                    @field(payload, f.name) = try parsePayload(args, f.type, "--" ++ fname);
                                    fields_seen.set(std.meta.fieldIndex(V, f.name).?);
                                    continue :args;
                                }
                            }
                            // choose not to return error here, allowing positionals to start with '--'
                        }

                        log.debug("positional fields seen {} arg {s}", .{ fields_seen.count(), args.*[0] });
                        // positionals
                        var iter = fields_seen.iterator(.{ .kind = .unset });
                        const next_fieldi = iter.next() orelse {
                            std.debug.print("extra args {s}\n", .{args.*});
                            return error.ExtraArgs;
                        };
                        inline for (x.fields, 0..) |f, fi| {
                            if (fi == next_fieldi) {
                                @field(payload, f.name) = try parsePayload(args, f.type, f.name);
                                fields_seen.set(fi);
                                continue :args;
                            }
                        }
                        unreachable; // TODO report error
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
                    if (fields_seen.count() != x.fields.len) {
                        std.debug.print("missing fields: ", .{});
                        var iter = fields_seen.iterator(.{ .kind = .unset });
                        while (iter.next()) |fi| {
                            std.debug.print("'{s}', ", .{std.meta.fieldNames(V)[fi]});
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
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            if (fmt.len == 0)
                try dump(self.root, fmt, options, writer, 0)
            else if (comptime std.mem.eql(u8, fmt, "help")) {
                help(self.exe_path);
            } else @compileError("unknown fmt '" ++ fmt ++ "'");
        }

        fn dump(
            v: anytype,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
            depth: u8,
        ) !void {
            const V = @TypeOf(v);
            switch (@typeInfo(V)) {
                else => |x| if (comptime isZigString(V))
                    try writer.writeAll(v)
                else if (x == .Pointer and x.Pointer.size == .One)
                    try dump(v.*, fmt, options, writer, depth)
                else
                    @compileError("TODO " ++ @tagName(x) ++ " " ++ @typeName(V)),
                .Void => {},
                .Int, .Bool, .Float => try std.fmt.formatType(v, fmt, options, writer, 0),
                .Array => try std.fmt.formatType(v, "any", options, writer, 0),
                .Optional => if (v) |u|
                    try dump(u, fmt, options, writer, depth)
                else
                    try writer.writeAll("null"),
                .Enum => try writer.writeAll(@tagName(v)),
                .Struct => |x| inline for (x.fields) |f| {
                    try writer.writeByte('\n');
                    try writer.writeByteNTimes(' ', depth * 2);
                    try writer.print("{s}: ", .{f.name});
                    try dump(@field(v, f.name), fmt, options, writer, depth + 1);
                },
                .Union => switch (v) {
                    inline else => |payload, tag| {
                        try writer.writeByte('\n');
                        try writer.writeByteNTimes(' ', depth * 2);
                        try writer.print("{s}: ", .{@tagName(tag)});
                        try dump(payload, fmt, options, writer, depth + 1);
                    },
                },
            }
        }

        pub fn help(exe_path: []const u8, options: struct { usage_text: ?[]const u8 = null }) void {
            const writer = std.io.getStdErr().writer();
            if (options.usage_text) |text|
                writer.writeAll(text) catch unreachable
            else
                writer.print("\nUSAGE: {s} <command> <options>...\n----  commands  ----", .{std.fs.path.basename(exe_path)}) catch unreachable;
            printHelp(T, "", .{}, writer, 0) catch unreachable;
        }

        pub fn printHelp(
            comptime V: type,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
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
                    if (!x.is_tuple) try writer.print("--{s}", .{kebab(f.name)});
                    try printAlias(V, writer, f);
                    try printHelp(f.type, fmt, options, writer, depth + 1);
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
                    try writer.print("{s}", .{kebab(f.name)});
                    try printAlias(V, writer, f);
                    try printHelp(f.type, fmt, options, writer, depth + 1);
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

const Case = enum { kebab, snake };
const FmtName = struct {
    name: []const u8,
    case: Case,

    pub fn format(
        self: FmtName,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self.case) {
            .kebab => for (self.name) |c| {
                try writer.writeByte(if (c == '_') '-' else c);
            },
            .snake => for (self.name) |c| {
                try writer.writeByte(if (c == '-') '_' else c);
            },
        }
    }
};

fn kebab(name: []const u8) FmtName {
    return .{ .name = name, .case = .kebab };
}

fn snake(name: []const u8) FmtName {
    return .{ .name = name, .case = .snake };
}

fn toCase(case: Case, buf: []u8, name: []const u8) ![]const u8 {
    var fbs = std.io.fixedBufferStream(buf);
    try fbs.writer().print("{}", .{FmtName{ .name = name, .case = case }});
    return fbs.getWritten();
}

pub fn isZigString(comptime T: type) bool {
    return comptime blk: {
        // Only pointer types can be strings, no optionals
        const info = @typeInfo(T);
        if (info != .Pointer) break :blk false;
        const ptr = &info.Pointer;
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
