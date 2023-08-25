//! ...

const VM = @This();

pub const Error = error{
    compile_error,
    runtime_error,
};

const stack_max = 256;

chunk: Chunk = undefined,
ip: usize = undefined,
stack: [stack_max]Value = undefined,
stack_top: usize = undefined,
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
    var ch = Chunk.init(alctr, source_text);
    defer ch.deinit();

    const compile_result = cpl.compile(source_text, &ch, &vm.pool, out);

    if (!compile_result) return Error.compile_error;

    vm.chunk = ch;
    vm.ip = 0;
    vm.stack_reset();

    if (dbg.options.trace_execution) {
        out.print("Execution trace:\n", .{}) catch unreachable;
    }
    return vm.run(alctr, out);
}

fn run(vm: *VM, alctr: std.mem.Allocator, out: anytype) !void {
    while (true) {
        if (dbg.options.trace_execution) {
            dbg.Disassembler.line(vm.chunk, vm.ip, out);
            // trace instruction
            _ = dbg.Disassembler.instruction(vm.chunk, vm.ip, out);
            { // trace stack
                out.print(" | ", .{}) catch unreachable;
                var stack_idx = @as(usize, 0);
                while (stack_idx < vm.stack_top) : (stack_idx += 1) {
                    out.print("[ ", .{}) catch unreachable;
                    vm.stack[stack_idx].print(out);
                    out.print(" ]", .{}) catch unreachable;
                }
                out.print("\n", .{}) catch unreachable;
            }
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
                if (op == .add and vm.stack_peek(0).is_string() and vm.stack_peek(1).is_string()) {
                    // concatenate strings
                    const b = vm.stack_pop().as_string();
                    const a = vm.stack_pop().as_string();

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
                const name = vm.read_constant().as_string();
                _ = vm.pool.globals.set(name, vm.stack_peek(0));
                // book says: stack_pop outside of the call to set so that the vm can
                // still find the value if garbage collection kicks in while setting the table item
                _ = vm.stack_pop();
            },
            .get_global => {
                const name = vm.read_constant().as_string();
                if (vm.pool.globals.get(name)) |val| {
                    vm.stack_push(val);
                } else {
                    vm.print_runtime_error(out, "Undefined variable '{s}'.", .{name.buf});
                    return Error.runtime_error;
                }
            },
            .set_global => {
                const name = vm.read_constant().as_string();
                if (vm.pool.globals.set(name, vm.stack_peek(0))) {
                    const delete_result = vm.pool.globals.delete(name);
                    std.debug.assert(delete_result);
                    vm.print_runtime_error(out, "Undefined variable '{s}'.", .{name.buf});
                    return Error.runtime_error;
                }
            },
            .get_local => {
                const slot = vm.read_byte();
                vm.stack_push(vm.stack[slot]);
            },
            .set_local => {
                const slot = vm.read_byte();
                vm.stack[slot] = vm.stack_peek(0);
            },
            .jump => {
                const offset = vm.read_short();
                vm.ip += offset;
            },
            .jump_if_false => {
                const offset = vm.read_short();
                if (!vm.stack_peek(0).is_truthy()) vm.ip += offset;
            },
            _ => return Error.runtime_error, // unknown opcode
        }
    }
}

// TODO consider combining these read_ functions into single ones that take a type param
fn read_byte(vm: *VM) u8 {
    const b = vm.chunk.code.items[vm.ip];
    vm.ip += 1;
    return b;
}

fn read_short(vm: *VM) u16 {
    const hi: u16 = vm.chunk.code.items[vm.ip];
    const lo = vm.chunk.code.items[vm.ip + 1];
    vm.ip += 2;
    return (hi << 8) | lo;
}

fn read_constant(vm: *VM) Value {
    return vm.chunk.constants.items[vm.read_byte()];
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

    const instruction = vm.ip - 1;
    const line = vm.chunk.lines.items[instruction];
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
