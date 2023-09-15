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
    get_upvalue,
    set_upvalue,
    jump,
    jump_if_false,
    loop,
    call,
    closure,
    close_upvalue,
    _,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    lines: std.ArrayList(i32),
    constants: std.ArrayList(value.Value),

    pub fn init(alctr: std.mem.Allocator) Chunk {
        return .{
            .code = std.ArrayList(u8).init(alctr),
            .lines = std.ArrayList(i32).init(alctr),
            .constants = std.ArrayList(value.Value).init(alctr),
        };
    }

    pub fn deinit(ch: Chunk) void {
        ch.code.deinit();
        ch.lines.deinit();
        // constants with items on heap are tracked in the vm's obj list, so we don't go
        // through `constants` and deinit the contained objects.
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
};

const std = @import("std");

const value = @import("value.zig");
