# About

Create command line parsers from zig unions and structs.

# Features

* field types
  * int, bool, enum, float, optional, array
  * nested unions and structs
* help / usage
  * automatically generated
  * automatically printed on parsing errors
  * customizable via options
  * nested and context aware, showing only one level of help info at once
  * writes to `parse_options.err_writer` (default `std.io.null_writer`)
  * from any print() method: `std.debug.print("{help}", .{parse_result});`
* diagnostics which point to errors
* dump parsed results
  * from any print() method: `std.debug.print("{}", .{parse_result});`
* derive short names and override with `FieldOption.short`
* apply `clarp_options` to types you don't control with `parseWithOptions()`
* rename long names with `FieldOption.long`

# Overview
Union types create alternative commands.  Commands match field names exactly.

Struct types create sequences of options.  Options match field names with leading dashes such as `--text_color` for field `text_color`.  Named options can be parsed out of order.  Unnamed, positional options will be assigned to the next unset field in field order.

Tuple types create unnamed sequences and are parsed strictly by position.

Bool fields create 'flags' and may be specified as `--flag` or `true`/`false` when unnamed.  They are always optional and default to false.

## Zig version
This package was developed against zig version 0.12.0-dev.3594+355cceebc

# Usage
You can find many examples in the [tests](src/tests.zig).

## Add clarp dependency

#### Fetch
```console
$ zig fetch --save=clarp https://github.com/travisstaloch/clarp/archive/53b7ebad33e35359a5dedf111ba5387c604a927a.tar.gz
```
This will add the following
```zig
// build.zig.zon
.dependencies = .{
    .clarp = .{
        .url = "https://github.com/travisstaloch/clarp/archive/53b7ebad33e35359a5dedf111ba5387c604a927a.tar.gz",
        .hash = "1220adb587cde6b1e62a03779e7724a51aea94822e4a7db6bd5f326f870adfb9c9a7",
    },
},
```

#### Modify build.zig
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
#### No name
Unnamed options are assigned to the next unset field in field declaration order.
```console
$ zig-out/bin/testexe c1 'opt1 value'

cmd1: 
  foo: "opt1 value"
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
