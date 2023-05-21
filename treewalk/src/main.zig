pub fn main() !void {
    std.debug.print("jlox", .{});
}

test "run all tests" {
    std.testing.refAllDecls(@This());
}

const std = @import("std");
