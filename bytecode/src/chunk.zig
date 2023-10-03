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
    class,
    get_property,
    set_property,
    method,
    invoke,
    inherit,
    get_super,
    super_invoke,
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

    pub fn add_constant(c: *Chunk, val: value.Value) !u8 {
        if (c.constants.items.len == std.math.maxInt(u8) + 1) {
            return error.too_many_constants;
        }
        {
            var buf: [256]u8 = undefined;
            var str = val.buf_print(&buf);
            log.debug("add constant {s}", .{str});
        }

        for (c.constants.items, 0..) |cstnt, i| {
            if (cstnt.check_eql(val)) return @truncate(i);
        }

        c.constants.append(val) catch @panic("OOM");
        return @intCast(c.constants.items.len - 1);
    }
};

const std = @import("std");
const log = std.log.scoped(.chunk);

const value = @import("value.zig");
