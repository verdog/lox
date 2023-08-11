//! ...

pub const OpCode = enum(u8) {
    @"return",
    constant,
    negate,
    add,
    subtract,
    multiply,
    divide,
    _,
};

pub const Chunk = struct {
    alctr: std.mem.Allocator,
    code: std.ArrayList(u8),
    lines: std.ArrayList(u32),
    constants: std.ArrayList(value.Value),

    pub fn init(alctr: std.mem.Allocator) Chunk {
        return .{
            .alctr = alctr,
            .code = std.ArrayList(u8).init(alctr),
            .lines = std.ArrayList(u32).init(alctr),
            .constants = std.ArrayList(value.Value).init(alctr),
        };
    }

    pub fn deinit(c: Chunk) void {
        c.code.deinit();
        c.lines.deinit();
        c.constants.deinit();
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

test "compile test: construst/destruct a chunk" {
    const chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();
}

test "compile test: write a byte to a chunk" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();
    chunk.write(@intFromEnum(OpCode.@"return"), 123);
    chunk.writeOpCode(OpCode.@"return", 123);
}

const std = @import("std");

const value = @import("value.zig");
