//! ...

pub const VM = @This();

pub const Error = error{
    compile_error,
    runtime_error,
};

const CallFrame = struct {
    function: *vl.ObjFunction,
    ip: usize,
    slots: []vl.Value,
};

const frames_max = 256;
// each frame can only be 256 items long
const stack_max = frames_max * std.math.maxInt(u8) + 1;

source: []const u8 = undefined,
stack: [stack_max]Value = undefined,
stack_top: usize = undefined,
frames: [frames_max]CallFrame = undefined,
frames_count: usize = undefined,
pool: vl.ObjPool = undefined,

pub fn init(alctr: std.mem.Allocator) VM {
    return .{
        .pool = vl.ObjPool.init(alctr),
    };
}

pub fn deinit(vm: VM) void {
    vm.pool.deinit();
}

pub fn interpret(vm: *VM, source_text: []const u8, alctr: std.mem.Allocator, out: anytype) !void {
    const top_func = try cpl.compile(source_text, &vm.pool, out);
    defer {
        top_func.deinit(alctr);
        alctr.destroy(top_func);
    }

    vm.stack_reset();

    // initial call frame
    vm.stack_push(Value.from(vl.ObjFunction, top_func));
    var bottom_frame = &vm.frames[0];
    vm.frames_count = 1;
    bottom_frame.function = top_func;
    bottom_frame.ip = 0;
    bottom_frame.slots = &vm.stack;

    vm.source = source_text;

    if (dbg.options.trace_execution) {
        dbg.Disassembler.border("execution trace", out);
        out.print("{s: <7}{s: <5}{s: <5}{s: <16} {s: <16} {s: <5}\n", .{ "offset", "byte", "line", "meaning", "encoded data", "stack before inst. exec." }) catch unreachable;
    }
    return vm.run(alctr, out);
}

fn run(vm: *VM, alctr: std.mem.Allocator, out: anytype) !void {
    var frame = &vm.frames[vm.frames_count - 1];

    while (true) {
        if (dbg.options.trace_execution) {
            dbg.Disassembler.line(frame.function.chunk, frame.ip, vm.source, out);
            // trace instruction
            _ = dbg.Disassembler.instruction(frame.function.chunk, frame.ip, vm, out);
        }
        const inst = @as(OpCode, @enumFromInt(vm.read_byte()));

        switch (inst) {
            .@"return" => {
                return;
            },
            .print => {
                vm.stack_pop().print(out);
                out.print("\n", .{}) catch unreachable;
            },
            .constant => {
                const constant = vm.read_constant();
                vm.stack_push(constant);
            },
            .nil => vm.stack_push(Value{ .nil = {} }),
            .true => vm.stack_push(Value{ .booln = true }),
            .false => vm.stack_push(Value{ .booln = false }),
            .negate => {
                var operand = vm.stack_peek(0);
                switch (operand) {
                    .number => {
                        vm.stack_push(Value{ .number = -vm.stack_pop().number });
                    },
                    else => {
                        vm.print_runtime_error(out, "Operand must be a number.", .{});
                        return Error.runtime_error;
                    },
                }
            },
            .not => {
                vm.stack_push(Value{ .booln = !vm.stack_pop().is_truthy() });
            },
            .add,
            .subtract,
            .multiply,
            .divide,
            .less,
            .greater,
            => |op| {
                if (op == .add and vm.stack_peek(0).is(ObjString) and vm.stack_peek(1).is(ObjString)) {
                    // concatenate strings
                    const b = vm.stack_pop().as(ObjString);
                    const a = vm.stack_pop().as(ObjString);

                    const new_len = a.buf.len + b.buf.len;
                    const new_chars = alctr.alloc(u8, new_len) catch @panic("OOM");
                    @memcpy(new_chars[0..a.buf.len], a.buf);
                    @memcpy(new_chars[a.buf.len..], b.buf);

                    const result = vm.pool.take_string_value(new_chars);
                    vm.stack_push(result);
                } else if (std.meta.activeTag(vm.stack_peek(0)) == .number and
                    std.meta.activeTag(vm.stack_peek(1)) == .number)
                {
                    const b = vm.stack_pop();
                    const a = vm.stack_pop();
                    vm.stack_push(switch (op) {
                        .add => Value{ .number = a.number + b.number },
                        .subtract => Value{ .number = a.number - b.number },
                        .multiply => Value{ .number = a.number * b.number },
                        .divide => Value{ .number = a.number / b.number },
                        .less => Value{ .booln = a.number < b.number },
                        .greater => Value{ .booln = a.number > b.number },
                        else => unreachable,
                    });
                } else {
                    if (op == .add) {
                        vm.print_runtime_error(out, "Operands must be numbers or strings.", .{});
                    } else {
                        vm.print_runtime_error(out, "Operands must be numbers.", .{});
                    }
                    return Error.runtime_error;
                }
            },
            .equal => {
                const b = vm.stack_pop();
                const a = vm.stack_pop();
                vm.stack_push(Value{ .booln = a.check_eql(b) });
            },
            .pop => _ = vm.stack_pop(),
            .define_global => {
                const name = vm.read_constant().as(ObjString);
                _ = vm.pool.globals.set(name, vm.stack_peek(0));
                // book says: stack_pop outside of the call to set so that the vm can
                // still find the value if garbage collection kicks in while setting the table item
                _ = vm.stack_pop();
            },
            .get_global => {
                const name = vm.read_constant().as(ObjString);
                if (vm.pool.globals.get(name)) |val| {
                    vm.stack_push(val);
                } else {
                    vm.print_runtime_error(out, "Undefined variable '{s}'.", .{name.buf});
                    return Error.runtime_error;
                }
            },
            .set_global => {
                const name = vm.read_constant().as(ObjString);
                if (vm.pool.globals.set(name, vm.stack_peek(0))) {
                    const delete_result = vm.pool.globals.delete(name);
                    std.debug.assert(delete_result);
                    vm.print_runtime_error(out, "Undefined variable '{s}'.", .{name.buf});
                    return Error.runtime_error;
                }
            },
            .get_local => {
                const slot = vm.read_byte();
                vm.stack_push(frame.slots[slot]);
            },
            .set_local => {
                const slot = vm.read_byte();
                frame.slots[slot] = vm.stack_peek(0);
            },
            .jump => {
                const offset = vm.read_short();
                frame.ip += offset;
            },
            .jump_if_false => {
                const offset = vm.read_short();
                if (!vm.stack_peek(0).is_truthy()) frame.ip += offset;
            },
            .loop => {
                const offset = vm.read_short();
                frame.ip -= offset;
            },
            _ => return Error.runtime_error, // unknown opcode
        }
    }
}

// TODO consider combining these read_ functions into single ones that take a type param
fn read_byte(vm: *VM) u8 {
    var frame = &vm.frames[vm.frames_count - 1];
    const b = frame.function.chunk.code.items[frame.ip];
    frame.ip += 1;
    return b;
}

fn read_short(vm: *VM) u16 {
    var frame = &vm.frames[vm.frames_count - 1];
    const hi: u16 = frame.function.chunk.code.items[frame.ip];
    const lo = frame.function.chunk.code.items[frame.ip + 1];
    frame.ip += 2;
    return (hi << 8) | lo;
}

fn read_constant(vm: *VM) Value {
    var frame = &vm.frames[vm.frames_count - 1];
    return frame.function.chunk.constants.items[vm.read_byte()];
}

fn stack_reset(vm: *VM) void {
    vm.stack_top = 0;
}

fn stack_push(vm: *VM, value: Value) void {
    vm.stack[vm.stack_top] = value;
    vm.stack_top +%= 1;
}

fn stack_pop(vm: *VM) Value {
    const value = vm.stack[vm.stack_top - 1];
    vm.stack_top -%= 1;
    return value;
}

fn stack_peek(vm: *VM, distance: usize) Value {
    return vm.stack[vm.stack_top - 1 - distance];
}

fn track_obj(vm: *VM, obj: *vl.Obj) void {
    obj.next = vm.objs;
    vm.objs = obj;
}

fn print_runtime_error(vm: *VM, out: anytype, comptime fmt: []const u8, vars: anytype) void {
    out.print(fmt, vars) catch unreachable;

    var frame = &vm.frames[vm.frames_count - 1];
    const instruction = frame.ip - 1;
    const line = frame.function.chunk.lines.items[instruction];
    out.print("\n[line {d}] in script\n", .{line}) catch unreachable;

    if (dbg.options.dump_stack_on_runtime_error) {
        out.print("Stack, starting at bottom:\n", .{}) catch unreachable;
        var i = @as(usize, 0);
        while (i < vm.stack_top) : (i += 1) {
            vm.stack[i].print(out);
            out.print("\n", .{}) catch unreachable;
        }
    }

    vm.stack_reset();
}

const std = @import("std");
const log = std.log.scoped(.vm);

const vl = @import("value.zig");
const dbg = @import("debug.zig");
const cpl = @import("compiler.zig");
const tbl = @import("table.zig");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = vl.Value;
const ObjString = vl.ObjString;
