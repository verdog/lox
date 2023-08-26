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
    print,
    pop,
    define_global,
    get_global,
    set_global,
    get_local,
    set_local,
    jump,
    jump_if_false,
    loop,
    _,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    lines: std.ArrayList(i32),
    constants: std.ArrayList(value.Value),
    source: []const u8,

    pub fn init(alctr: std.mem.Allocator, source: []const u8) Chunk {
        return .{
            .code = std.ArrayList(u8).init(alctr),
            .lines = std.ArrayList(i32).init(alctr),
            .constants = std.ArrayList(value.Value).init(alctr),
            .source = source,
        };
    }

    pub fn deinit(ch: Chunk) void {
        ch.code.deinit();
        ch.lines.deinit();
        // constants with items on heap are tracked in the vm's obj list
        ch.constants.deinit();
    }

    pub fn write(c: *Chunk, data: u8, line: i32) void {
        c.code.append(data) catch @panic("OOM");
        c.lines.append(line) catch @panic("OOM");
    }

    pub fn write_opcode(c: *Chunk, opcode: OpCode, line: i32) void {
        return c.write(@intFromEnum(opcode), line);
    }

    pub fn add_constant(c: *Chunk, val: value.Value) u8 {
        c.constants.append(val) catch @panic("OOM");
        std.debug.assert(c.constants.items.len < std.math.maxInt(u8));
        return @intCast(c.constants.items.len - 1);
    }

    pub fn get_source_line(c: Chunk, line: usize) []const u8 {
        // slow and naive, but fast enough for our purposes now.
        var it = std.mem.splitScalar(u8, c.source, '\n');
        var i: usize = 0;
        while (i < line - 1) : ({
            _ = it.next();
            i += 1;
        }) {}
        return it.next().?;
    }
};

const std = @import("std");

const value = @import("value.zig");
