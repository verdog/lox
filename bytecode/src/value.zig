//! ...

pub const Value = union(enum) {
    nil: void,
    booln: bool,
    number: f64,

    pub fn is_truthy(v: Value) bool {
        switch (v) {
            .nil => return false,
            .booln => |b| return b,
            else => return true,
        }
    }

    pub fn check_eql(a: Value, b: Value) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        switch (a) {
            .nil => return true,
            .booln => return a.booln == b.booln,
            .number => return a.number == b.number,
        }
    }
};

pub fn print_value(val: Value, out: anytype) void {
    switch (val) {
        .number => |n| out.print("{d}", .{n}) catch @panic("OOM"),
        .booln => |b| out.print("{}", .{b}) catch @panic("OOM"),
        .nil => out.print("(nil)", .{}) catch @panic("OOM"),
    }
}

const std = @import("std");
