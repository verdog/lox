//! ...

pub const OpCode = enum(u8) {
    @"return",
    constant,
    negate,
    add,
    subtract,
    multiply,
    divide,
    nil,
    true,
    false,
    not,
    equal,
    greater,
    less,
    _,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    lines: std.ArrayList(u32),
    constants: std.ArrayList(value.Value),

    pub fn init(alctr: std.mem.Allocator) Chunk {
        return .{
            .code = std.ArrayList(u8).init(alctr),
            .lines = std.ArrayList(u32).init(alctr),
            .constants = std.ArrayList(value.Value).init(alctr),
        };
    }

    pub fn deinit(ch: Chunk, alctr: std.mem.Allocator) void {
        ch.code.deinit();
        ch.lines.deinit();
        for (ch.constants.items) |c| {
            c.deinit(alctr);
        }
        ch.constants.deinit();
    }

    pub fn write(c: *Chunk, data: u8, line: u32) void {
        c.code.append(data) catch @panic("OOM");
        c.lines.append(line) catch @panic("OOM");
    }

    pub fn writeOpCode(c: *Chunk, opcode: OpCode, line: u32) void {
        return c.write(@intFromEnum(opcode), line);
    }

    pub fn addConstant(c: *Chunk, val: value.Value) u8 {
        c.constants.append(val) catch @panic("OOM");
        std.debug.assert(c.constants.items.len < std.math.maxInt(u8));
        return @intCast(c.constants.items.len - 1);
    }
};

const std = @import("std");

const value = @import("value.zig");
