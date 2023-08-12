//! ...

const VM = @This();

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

const stack_max = 256;

chunk: Chunk = undefined,
ip: usize = undefined,
stack: [stack_max]Value = undefined,
stack_top: usize = undefined,

pub fn init() VM {
    return .{};
}

pub fn deinit(vm: VM) void {
    _ = vm;
    // vm.chunk.deinit(); // ?
}

pub fn interpret(vm: *VM, source_text: []const u8, alctr: std.mem.Allocator, out: anytype) InterpretResult {
    var ch = Chunk.init(alctr);
    defer ch.deinit();

    if (!cpl.compile(source_text, &ch, out)) {
        return .compile_error;
    }

    vm.chunk = ch;
    vm.ip = 0;
    vm.stack_reset();

    if (dbg.options.trace_execution) {
        out.print("Execution trace:\n", .{}) catch unreachable;
    }
    return vm.run(out);
}

fn run(vm: *VM, out: anytype) InterpretResult {
    while (true) {
        if (dbg.options.trace_execution) {
            // trace instruction
            _ = dbg.Disassembler.instruction(vm.chunk, vm.ip, out);
            { // trace stack
                out.print(" | ", .{}) catch unreachable;
                var stack_idx = @as(usize, 0);
                while (stack_idx < vm.stack_top) : (stack_idx += 1) {
                    out.print("[ ", .{}) catch unreachable;
                    vl.print_value(vm.stack[stack_idx], out);
                    out.print(" ]", .{}) catch unreachable;
                }
                out.print("\n", .{}) catch unreachable;
            }
        }
        const inst = @as(OpCode, @enumFromInt(vm.read_byte()));

        switch (inst) {
            .@"return" => {
                vl.print_value(vm.stack_pop(), out);
                out.print("\n", .{}) catch unreachable;
                return .ok;
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
                        return .runtime_error;
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
                if (std.meta.activeTag(vm.stack_peek(0)) != .number or
                    std.meta.activeTag(vm.stack_peek(1)) != .number)
                {
                    vm.print_runtime_error(out, "Operands must be numbers.", .{});
                    return .runtime_error;
                }
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
            },
            .equal => {
                const b = vm.stack_pop();
                const a = vm.stack_pop();
                vm.stack_push(Value{ .booln = a.check_eql(b) });
            },
            _ => return .runtime_error, // unknown opcode
        }
    }
}

fn read_byte(vm: *VM) u8 {
    const b = vm.chunk.code.items[vm.ip];
    vm.ip += 1;
    return b;
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

fn print_runtime_error(vm: *VM, out: anytype, comptime fmt: []const u8, vars: anytype) void {
    out.print(fmt, vars) catch unreachable;

    const instruction = vm.ip - 1;
    const line = vm.chunk.lines.items[instruction];
    out.print("\n[line {d}] in script\n", .{line}) catch unreachable;

    // TODO: dump stack if enabled

    vm.stack_reset();
}

const std = @import("std");

const vl = @import("value.zig");
const dbg = @import("debug.zig");
const cpl = @import("compiler.zig");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = vl.Value;
