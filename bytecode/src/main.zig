pub const std_options = struct {
    pub const log_level = if (@import("builtin").mode == .Debug) .debug else .info;
};

pub fn main() !void {}

test "run all tests" {
    std.testing.refAllDeclsRecursive(@This());
}

const std = @import("std");
var gpa = std.heap.GeneralPurposeAllocator(.{
    .stack_trace_frames = 16,
}){};
var heap = gpa.allocator();

const log = std.log.scoped(.main);
