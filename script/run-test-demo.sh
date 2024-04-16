set -xe

zig build

zig-out/bin/testexe help
zig-out/bin/testexe cmd1 help
zig-out/bin/testexe cmd1 --foo 'opt1 value'
zig-out/bin/testexe c1 --foo 'opt1 value'
zig-out/bin/testexe c2 b
zig-out/bin/testexe foo
