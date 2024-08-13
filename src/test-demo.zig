const std = @import("std");
const clarp = @import("clarp");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const ArgParser = clarp.Parser(union(enum) {
        cmd1: struct {
            foo: []const u8,
            pub const clarp_options = clarp.Options(@This()){
                .fields = .{
                    .foo = .{ .desc = "Foo description." },
                },
            };
        },
        cmd2: struct { enum { a, b } = .a },
        pub const clarp_options = clarp.Options(@This()){
            .fields = .{
                .cmd1 = .{ .desc = "Cmd1 description.", .short = "c1" },
                .cmd2 = .{ .desc = "Cmd2 description.", .short = "c2" },
            },
        };
    }, .{});

    const args = try std.process.argsAlloc(allocator);
    const parsed = ArgParser.parse(args, .{
        .err_writer = std.io.getStdErr().writer().any(),
    }) catch |e| switch (e) {
        error.HelpShown => return,
        else => return e,
    };
    std.debug.print("{}\n", .{parsed});

    // access parse result
    switch (parsed.result) {
        .cmd1 => {},
        .cmd2 => {},
    }
}
