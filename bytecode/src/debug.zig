//! debug tools

pub const DebugOptions = struct {
    trace_execution: bool = true,
    print_code: bool = true,
    print_color: bool = true,
    dump_stack_on_runtime_error: bool = false, // it's adequately represented in the exec trace
    stress_garbage_collection: bool = true,
    log_garbage_collection: bool = false,
};

pub var options = switch (@import("builtin").is_test) {
    false => DebugOptions{},
    true => DebugOptions{
        .trace_execution = false,
        .print_code = false,
        .print_color = false,
        .dump_stack_on_runtime_error = false,
        .stress_garbage_collection = true,
        .log_garbage_collection = false,
    },
};

pub const Disassembler = struct {
    const border_len = @as(usize, 80);

    pub fn chunk(ch: Chunk, name: []const u8, source: []const u8, out: anytype) void {
        border(name, out);
        out.print("{s: <7}{s: <5}{s: <5}{s: <16} {s: <16}\n", .{ "offset", "byte", "line", "meaning", "encoded data" }) catch unreachable;
        var offset = @as(usize, 0);
        while (offset < ch.code.items.len) {
            line(ch, offset, source, out);
            offset = instruction(ch, offset, null, out);
        }
    }

    pub fn border(name: []const u8, out: anytype) void {
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

    fn emit_line(offset: usize, byte: u8, opcode_string: []const u8, extra: []const u8, vm: ?*VM, out: anytype) void {
        var stack_buf: [1024]u8 = undefined;
        var stack_str: []u8 = undefined;
        var fbs = std.io.fixedBufferStream(&stack_buf);
        var writer = fbs.writer();
        if (vm) |v| {
            var stack_idx = @as(usize, 0);
            while (stack_idx < v.stack_top) : (stack_idx += 1) {
                writer.print("[ ", .{}) catch unreachable;
                v.stack[stack_idx].print(writer);
                writer.print(" ]", .{}) catch unreachable;
            }
            stack_str = stack_buf[0..writer.context.pos];
        } else {
            stack_str = stack_buf[0..0];
        }

        out.print("0x{x:0>4} 0x{x:0>2}      {s: <16} {s: <16} | {s} \n", .{ offset, byte, opcode_string, extra, stack_str }) catch unreachable;
    }

    pub fn line(ch: Chunk, offset: usize, source: []const u8, out: anytype) void {
        if (offset > 0 and ch.lines.items[offset] == ch.lines.items[offset - 1]) {} else {
            const l = ch.lines.items[offset];
            out.print("\n{d: >16} {s}\n", .{ @as(usize, @intCast(l)), get_source_line(source, @intCast(l)) }) catch unreachable;
        }
    }

    fn get_source_line(source: []const u8, l: usize) []const u8 {
        // slow and naive, but fast enough for our purposes now.
        var it = std.mem.splitScalar(u8, source, '\n');
        var i: usize = 0;
        while (i < l - 1) : ({
            _ = it.next();
            i += 1;
        }) {}
        return it.next().?;
    }

    pub fn instruction(ch: Chunk, offset: usize, vm: ?*VM, out: anytype) usize {
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
            .close_upvalue,
            => return simple_inst(opcode, offset, vm, out),

            .define_global,
            .get_global,
            .set_global,
            .constant,
            .class,
            .get_property,
            .set_property,
            => return constant_inst(opcode, ch, offset, vm, out),

            .get_local,
            .set_local,
            .get_upvalue,
            .set_upvalue,
            .call,
            => return byte_inst(opcode, ch, offset, vm, out),

            .jump,
            .jump_if_false,
            => return jump_inst(opcode, 1, ch, offset, vm, out),

            .loop => return jump_inst(opcode, -1, ch, offset, vm, out),

            .closure => {
                emit_line(offset, @intFromEnum(opcode), @tagName(opcode), "", vm, out);
                const func_idx = constant(ch, offset + 1, vm, out);

                var w_offset = offset + 2;

                const func = ch.constants.items[func_idx].as(ObjFunction);
                for (0..@intCast(func.upvalue_count)) |_| {
                    const is_local = ch.code.items[w_offset];
                    const index = ch.code.items[w_offset + 1];
                    const index_val = Value{ .number = @floatFromInt(index) };

                    const str = if (is_local != 0) "local" else "upvalue";
                    var val_buf: [16]u8 = undefined;

                    emit_line(w_offset, is_local, "  (loc. or upv.)", str, vm, out);
                    w_offset += 1;
                    emit_line(w_offset, index, "  (index)", stringify_value(index_val, &val_buf), vm, out);
                    w_offset += 1;
                }

                return w_offset;
            },

            _ => {
                out.print("Unknown opcode: {x}", .{ch.code.items[offset]}) catch unreachable;
                return offset + 1;
            },
        }
    }

    fn simple_inst(opcode: OpCode, offset: usize, vm: ?*VM, out: anytype) usize {
        emit_line(offset, @intFromEnum(opcode), @tagName(opcode), "", vm, out);
        return offset + 1;
    }

    fn constant_inst(opcode: OpCode, ch: Chunk, offset: usize, vm: ?*VM, out: anytype) usize {
        emit_line(offset, @intFromEnum(opcode), @tagName(opcode), "", vm, out);
        _ = constant(ch, offset + 1, vm, out);
        return offset + 2;
    }

    fn stringify_value(val: Value, buf: *[16]u8) []u8 {
        switch (val) {
            .number => |n| {
                const digits = @floor(std.math.log10(n)) + 1;
                if (digits <= 16) {
                    return std.fmt.bufPrint(buf, "{d: <16}", .{n}) catch unreachable;
                } else {
                    return std.fmt.bufPrint(buf, " ...    ", .{}) catch unreachable;
                }
            },
            .booln => |b| return std.fmt.bufPrint(buf, "{: <16}", .{b}) catch unreachable,
            .nil => return std.fmt.bufPrint(buf, "nil", .{}) catch unreachable,
            .obj => |o| {
                switch (o.otype) {
                    .string => {
                        const strbuf = val.as(ObjString).buf;
                        const spaces = " " ** 64;
                        if (strbuf.len <= 14) {
                            return std.fmt.bufPrint(buf, "\"{s}\"{s}", .{ strbuf, spaces[0..(14 - strbuf.len)] }) catch unreachable;
                        } else {
                            return std.fmt.bufPrint(buf, "\"{s}\".", .{strbuf[0..13]}) catch unreachable;
                        }
                    },
                    .function => {
                        const name = val.as(ObjFunction).name.?.buf;
                        const spaces = " " ** 64;
                        if (name.len <= 14) {
                            return std.fmt.bufPrint(buf, "<{s}>{s}", .{ name, spaces[0..(14 - name.len)] }) catch unreachable;
                        } else {
                            return std.fmt.bufPrint(buf, "<{s}>.", .{name[0..13]}) catch unreachable;
                        }
                    },
                    .native => {
                        return std.fmt.bufPrint(buf, "<native fn>", .{}) catch unreachable;
                    },
                    .closure => {
                        const name = val.as(ObjClosure).func.name.?.buf;
                        const spaces = " " ** 64;
                        if (name.len <= 14) {
                            return std.fmt.bufPrint(buf, "<{s}>{s}", .{ name, spaces[0..(14 - name.len)] }) catch unreachable;
                        } else {
                            return std.fmt.bufPrint(buf, "<{s}>.", .{name[0..13]}) catch unreachable;
                        }
                    },
                    .upvalue => {
                        return std.fmt.bufPrint(buf, "{{upvalue}}", .{}) catch unreachable;
                    },
                    .class => {
                        const strbuf = val.as(ObjClass).name.buf;
                        const spaces = " " ** 64;
                        if (strbuf.len <= 14) {
                            return std.fmt.bufPrint(buf, "\"{s}\"{s}", .{ strbuf, spaces[0..(14 - strbuf.len)] }) catch unreachable;
                        } else {
                            return std.fmt.bufPrint(buf, "\"{s}\".", .{strbuf[0..13]}) catch unreachable;
                        }
                    },
                    .instance => {
                        const strbuf = val.as(ObjInstance).class.name.buf;
                        const spaces = " " ** 64;
                        if (strbuf.len <= 12) {
                            return std.fmt.bufPrint(buf, "i:\"{s}\"{s}", .{ strbuf, spaces[0..(12 - strbuf.len)] }) catch unreachable;
                        } else {
                            return std.fmt.bufPrint(buf, "i:\"{s}\".", .{strbuf[0..11]}) catch unreachable;
                        }
                    },
                }
            },
        }
    }

    fn constant(ch: Chunk, offset: usize, vm: ?*VM, out: anytype) usize {
        const constant_byte = ch.code.items[offset];
        const val = ch.constants.items[constant_byte];
        var buf: [16]u8 = undefined;
        const val_str = stringify_value(val, &buf);
        emit_line(offset, constant_byte, "  (constant)", val_str, vm, out);
        return constant_byte;
    }

    fn byte_inst(opcode: OpCode, ch: Chunk, offset: usize, vm: ?*VM, out: anytype) usize {
        emit_line(offset, @intFromEnum(opcode), @tagName(opcode), "", vm, out);

        const slot = ch.code.items[offset + 1];
        var slot_buf: [16]u8 = undefined;
        const slot_str = std.fmt.bufPrint(&slot_buf, "{d}", .{slot}) catch unreachable;
        emit_line(offset + 1, slot, "  (...)", slot_str, vm, out);
        return offset + 2;
    }

    fn jump_inst(opcode: OpCode, sign: i2, ch: Chunk, offset: usize, vm: ?*VM, out: anytype) usize {
        const hi_byte = ch.code.items[offset + 1];
        const hi: u16 = @as(u16, hi_byte) << 8;
        const lo: u16 = ch.code.items[offset + 2];
        const jump = @as(i32, hi | lo);
        const dest: usize = @intCast(@as(i64, @intCast(offset + 3)) + sign * jump);

        var buf: [16]u8 = undefined;
        const str = std.fmt.bufPrint(&buf, "0x{x:0>4} -> 0x{x:0>4}", .{ offset, dest }) catch unreachable;

        emit_line(offset, @intFromEnum(opcode), @tagName(opcode), str, vm, out);
        emit_line(offset + 1, hi_byte, "  (jump hi byte)", "", vm, out);
        emit_line(offset + 2, @truncate(lo), "  (jump lo byte)", "", vm, out);
        return offset + 3;
    }
};

const std = @import("std");

const vl = @import("value.zig");
const VM = @import("VM.zig").VM;

const OpCode = @import("chunk.zig").OpCode;
const Chunk = @import("chunk.zig").Chunk;
const Value = vl.Value;
const ObjString = vl.ObjString;
const ObjFunction = vl.ObjFunction;
const ObjClosure = vl.ObjClosure;
const ObjClass = vl.ObjClass;
const ObjInstance = vl.ObjInstance;
