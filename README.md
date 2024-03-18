# About

Create command line parsers from zig struct, union, and enum declarations.

# Features

* customizable generated help/usage messages
* dump parsed results

# Overview
Union types create alternative commands.  Commands are field names.

Struct types create sequences of options.  Options are field names with leading dashes such as `--text_color` for field `text_color`.  Named options can be parsed out of order.  An unnamed, positional argument will be assigned to the next unset field in field order.

Tuple types create unnamed sequences and are parsed strictly by position.

Bool field types create 'flags' and may be specified as `--flag` or `true`/`false` when unnamed.  They are always optional and default to false.

## Zig version
This package was developed against zig version 0.12.0-dev.3074+ae7f3fc36

# Usage
You can find these examples and more in the [tests](src/tests.zig).

## Add clarp dependency
```zig
// build.zig
pub fn build(b: *std.Build) void {
    const clarp = b.dependency("clarp", .{}).module("clarp");
    const exe = b.addExecutable(.{...});
    exe.root_module.addImport("clarp", clarp);
}
```

## Structs
This program creates a parser from a struct and dumps the result to stderr:
```zig
const std = @import("std");
const clarp = @import("clarp");

const ArgParser = clarp.Parser(struct {
    opt1: []const u8,
    opt2: enum { a, b } = .a,

    pub const options = clarp.Options(@This()){
        .opt1 = .{
            .alias = "-o1",
            .desc = "first option description",
        },
    };
}, .{ .usage_fmt = "\nUSAGE: $ {s} <options>\n\noptions:" });

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    const opts = ArgParser.parse(args) catch |e| switch (e) {
        error.HelpShown => return,
        else => return e,
    };
    std.debug.print("{}\n", .{opts});
}
```

### Default help flags
If the first arg is `help`, `--help` or `-h` usage is displayed.  Here our `usage_fmt` option appears before the generated text.

```console
$ zig-out/bin/testexe help

USAGE: $ testexe <options>...

options:
  help --help -h // show this message. must be first argument.
  --opt1 -o1: string // first option description
  --opt2: enum { a, b } = a

```

Notice how this message is derived from the struct above and that its `options` declaration affects the output, adding the `-o1` alias and description.

### Options
second argument to `clarp.Parser()`
#### help_flags: type (enum)
```zig
clarp.Parser(..., .{ .help_flags = enum { usage, @"--usage" } });
```
Now `help` won't show the usage but `usage` or `--usage` will.
#### usage_fmt: []const u8
```zig
clarp.Parser(..., .{ .usage_fmt = "\nUSAGE: $ {s} <options>\n\noptions:" });
```
Now usage begins with this message.
#### printUsage: fn(writer, comptime fmt, args)
```zig
clarp.Parser(..., .{ .printUsage = myPrintUsage });
```
Now `myPrintUsage()` will be called to print the usage text.


### Parsing args
Named options may occur in any order.
#### Long names
```console
$ zig-out/bin/testexe --opt1 'opt1 value'

opt1: opt1 value
opt2: a
```

Here opt2 is named and occurs before an unnamed positional opt1.
```console
$ zig-out/bin/testexe --opt2 b 'opt1 value'

opt1: opt1 value
opt2: b
```
#### Alias names
```console
$ zig-out/bin/testexe -o1 'opt1 value'

opt1: opt1 value
opt2: a
```
#### No name
Unnamed args are assigned to the next unset field in field declaration order.
```console
$ zig-out/bin/testexe 'opt1 value' b

opt1: opt1 value
opt2: b
```
#### Diagnostics
```console
$ zig-out/bin/testexe --foo 'opt1 value'
unknown option '--foo'
error at argument 1: --foo 'opt1 value'
                     ^~~~~
# usage omitted
```

# Other features
* allow users to manually parse arbitrary options by providing an `overrides` struct.  if any of its pub method names match an argument, that method will be called with an args pointer and optional user ctx pointer.  see test "parseWithUserCtx" in [tests](src/tests.zig).

# Todo
- [ ] document commands
- [ ] parse long names before aliases
- [ ] add Option to auto create shorts
  - [ ] validate shorts and aliases don't collide
- [ ] add colors to help output
- [ ] support some 'end of sequence' marker, allow user to override