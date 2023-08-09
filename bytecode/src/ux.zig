pub const in = std.io.getStdIn().reader();

pub const stdout_unbuffered = std.io.getStdOut().writer();
pub var stdout_buffer = std.io.bufferedWriter(stdout_unbuffered);
pub const out = stdout_buffer.writer();

pub const stderr_unbuffered = std.io.getStdErr();
pub const err = stderr_unbuffered.writer();

const std = @import("std");
const lex = @import("lex.zig");

const log = std.log.scoped(.ux);
