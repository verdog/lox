//! debug tools

pub const DebugOptions = struct {
    trace_execution: bool = true,
    print_code: bool = false, // it's adequately represented in the exec trace
    dump_stack_on_runtime_error: bool = false, // it's adequately represented in the exec trace
};

pub var options = DebugOptions{};

pub const Disassembler = struct {
    const border_len = @as(usize, 80);

    pub fn chunk(ch: Chunk, name: []const u8, out: anytype) void {
        border(name, out);

        var offset = @as(usize, 0);
        while (offset < ch.code.items.len) {
            offset = instruction(ch, offset, out);
            out.print("\n", .{}) catch unreachable;
        }

        border(name, out);
    }

    fn border(name: []const u8, out: anytype) void {
        var decor_len = border_len / 2 - name.len / 2 - 1;
        var decor = @as(usize, 0);

        while (decor < decor_len) : (decor += 1) {
            out.print("-", .{}) catch unreachable;
        }
        out.print(" {s} ", .{name}) catch unreachable;

        decor = 0;
        if (name.len & 1 == 1) decor_len -= 1;
        while (decor < decor_len) : (decor += 1) {
            out.print("-", .{}) catch unreachable;
        }
        out.print("\n", .{}) catch unreachable;
    }

    pub fn instruction(ch: Chunk, offset: usize, out: anytype) usize {
        out.print("0x{x:0>4} ", .{offset}) catch unreachable;

        if (offset > 0 and ch.lines.items[offset] == ch.lines.items[offset - 1]) {
            out.print(" |    ", .{}) catch unreachable;
        } else {
            out.print(" {d: <4} ", .{ch.lines.items[offset]}) catch unreachable;
        }

        const opcode = @as(OpCode, @enumFromInt(ch.code.items[offset]));
        switch (opcode) {
            .@"return",
            .print,
            .negate,
            .add,
            .subtract,
            .multiply,
            .divide,
            .nil,
            .true,
            .false,
            .not,
            .equal,
            .less,
            .greater,
            .pop,
            => return simple_inst(opcode, offset, out),

            .define_global,
            .get_global,
            .set_global,
            .constant,
            => return constant_inst(opcode, ch, offset, out),

            .get_local,
            .set_local,
            => return byte_inst(opcode, ch, offset, out),

            _ => {
                out.print("Unknown opcode: {x}", .{ch.code.items[offset]}) catch unreachable;
                return offset + 1;
            },
        }
    }

    fn simple_inst(opcode: OpCode, offset: usize, out: anytype) usize {
        out.print("{s: <31}", .{@tagName(opcode)}) catch unreachable;
        return offset + 1;
    }

    fn constant_inst(opcode: OpCode, ch: Chunk, offset: usize, out: anytype) usize {
        const constant_byte = ch.code.items[offset + 1];
        out.print("{s: <14} {d: <3} ", .{ @tagName(opcode), constant_byte }) catch unreachable;
        const val = ch.constants.items[constant_byte];
        switch (val) {
            .number => |n| {
                const digits = @floor(std.math.log10(n)) + 1;
                if (digits <= 12) {
                    out.print("{d: <12}", .{n}) catch unreachable;
                } else {
                    out.print(" ...    ", .{}) catch unreachable;
                }
            },
            .booln => |b| out.print("{: <12}", .{b}) catch unreachable,
            .nil => out.print("(nil)", .{}) catch unreachable,
            .obj => |o| {
                switch (o.typ) {
                    .string => {
                        const buf = val.as_string().buf;
                        if (buf.len <= 12) {
                            out.print("{s: <12}", .{buf}) catch unreachable;
                        } else {
                            out.print("{s}.", .{buf[0..11]}) catch unreachable;
                        }
                    },
                }
            },
        }
        return offset + 2;
    }

    fn byte_inst(opcode: OpCode, ch: Chunk, offset: usize, out: anytype) usize {
        const slot = ch.code.items[offset + 1];
        out.print("{s: <14} {d: <16}", .{ @tagName(opcode), slot }) catch unreachable;
        return offset + 2;
    }
};

test "disassembler header length: even length name" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();
    chunk.write_opcode(OpCode.@"return", 123);

    var out_buf = std.ArrayList(u8).init(std.testing.allocator);
    defer out_buf.deinit();
    var out = out_buf.writer();

    Disassembler.chunk(chunk, "test chunk", out);

    const first_line = std.mem.sliceTo(out_buf.items, '\n');
    errdefer std.debug.print("Failing line was: \"{s}\"\n", .{first_line});

    try std.testing.expectEqual(Disassembler.border_len, first_line.len);
}

test "disassembler header length: odd length name" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();
    chunk.write_opcode(OpCode.@"return", 123);

    var out_buf = std.ArrayList(u8).init(std.testing.allocator);
    defer out_buf.deinit();
    var out = out_buf.writer();

    Disassembler.chunk(chunk, "chunk", out);

    const first_line = std.mem.sliceTo(out_buf.items, '\n');
    errdefer std.debug.print("Failing line was: \"{s}\"\n", .{first_line});

    try std.testing.expectEqual(Disassembler.border_len, first_line.len);
}

const std = @import("std");

const vl = @import("value.zig");

const OpCode = @import("chunk.zig").OpCode;
const Chunk = @import("chunk.zig").Chunk;
const Value = vl.Value;
