# About

Derive customizable command line parsers from union and struct types.  Provides nested, context aware usage text similar to the zig compiler.  Works with existing data structures you may not control.

# Features

* field types
  * int, bool, enum, float, optional, array, slice
  * nested unions and structs
* help / usage
  * automatically printed on parsing errors
  * very customizable
  * nested and context aware, showing only one level of help info at once
  * written to `parse_options.err_writer` (default `std.io.null_writer`)
  * accessible from any print() method: `std.debug.print("{help}", .{parse_result});`
* diagnostics which clearly point to parsing errors
* easily dump parse results
  * from any print() method: `std.debug.print("{}", .{parse_result});`
* derive short names and override with `FieldOption.short`
* apply `clarp_options` to types you don't control with `parseWithOptions()`
* rename long names with `FieldOption.long`

# Overview
Union types create alternative commands.  Commands match field names exactly.

Struct types create sequences of options.  Options match field names with leading dashes such as `--text_color` for field `text_color`.  Named options can be parsed out of order.  Unnamed, positional parsing may be enabled by setting `clarp_options.fields.field_name.positional`.

Tuple types create unnamed sequences and are parsed strictly by position.

Bool fields create 'flags' and may be specified as `--flag` or `true`/`false` when unnamed.  They are always optional and default to false.

Slice fields require an allocator and consume input until an argument is found which starts with '-' or end of arguments.  Allocator may be provided via `ParseOptions.allocator`.

## Zig version
This package was developed against zig version 0.12.0-dev.3594+355cceebc

# Usage
You can find many examples in the [tests](src/tests.zig).

## Add clarp dependency

#### Fetch
```console
$ zig fetch --save=clarp https://github.com/travisstaloch/clarp/archive/<commit-hash>.tar.gz
```
This will add the following
```zig
// build.zig.zon
.dependencies = .{
    .clarp = .{
        .url = "https://github.com/travisstaloch/clarp/archive/<commit-hash>.tar.gz",
        .hash = ...,
    },
},
```

#### Modify build.zig
see [here](build.zig#L26)

```zig
// build.zig
pub fn build(b: *std.Build) void {
    const clarp = b.dependency("clarp", .{}).module("clarp");
    const exe = b.addExecutable(.{...});
    exe.root_module.addImport("clarp", clarp);
}
```

## Full Example
This program creates a parser and dumps the result to stderr.  It is available [here](src/test-demo.zig) and can be run with `$ zig build test-demo -- args`.
```zig
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
                    .foo = .{ .desc = "Foo desc." },
                },
            };
        },
        cmd2: struct { enum { a, b } = .a },
        pub const clarp_options = clarp.Options(@This()){
            .fields = .{
                .cmd1 = .{ .desc = "Cmd1 desc.", .short = "c1" },
                .cmd2 = .{ .desc = "Cmd2 desc.", .short = "c2" },
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
}
```

## [clarp_options](src/clarp.zig#L24)
When a struct or union contains a `pub const clarp_options` declaration, it changes parsing behavior.  Nested structs and unions may declare their own `clarp_options`.

## [ParserOptions](src/clarp.zig#L59)
These are comptime global parsing options. The second argument to clarp.Parser().

### help flags
By default, if an arg is `help`, `--help` or `-h` context aware usage is displayed.  You may change the help flags by passing an enum type as `ParserOptions.help_type`.

```console
$ zig-out/bin/testexe help
Usage: testexe [command]

Commands:

  cmd1, c1            Cmd1 description.
  cmd2, c2            Cmd2 description.

General Options:

  help, --help, -h    Print command specific usage.
```

Notice how this message is derived from the enum above and that its `clarp_options` declaration affects the output, adding the `c1` and `c2` shorts and descriptions.

To see help for cmd1:

```console
$ zig-out/bin/testexe cmd1 help
Usage: testexe cmd1 [options]

  Cmd1 description.

Options:

  --foo: string       Foo description.

General Options:

  help, --help, -h    Print command specific usage.
```

## Command line examples
These simple examples show the `ArgParser` we defined above in [Full Example](#full-example) in action.

#### Long names
```console
$ zig-out/bin/testexe cmd1 --foo 'opt1 value'

cmd1: 
  foo: "opt1 value"
```

#### Short names
```console
$ zig-out/bin/testexe c1 --foo 'opt1 value'

cmd1: 
  foo: "opt1 value"

$ zig-out/bin/testexe c2 b

cmd2: 
  0: b
```
#### Diagnostics
```console
$ zig-out/bin/testexe foo
Usage: testexe [command]

Commands:

  cmd1, c1            Cmd1 description.
  cmd2, c2            Cmd2 description.

General Options:

  help, --help, -h    Print command specific usage.

error at argument 1: foo
                     ^~~
error: UnknownCommand
#... stack trace omitted
```
