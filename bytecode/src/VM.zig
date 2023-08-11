//! ...

const VM = @This();

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

const stack_max = 256;

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

pub fn interpret(vm: *VM, source_text: []const u8, out: anytype) InterpretResult {
    _ = vm;
    _ = out;

    cpl.compile(source_text);
    return .ok;

    // TODO hook up interpreter again
    // vm.ip = 0;
    // vm.stack_reset();
    // return vm.run(out);
}

fn run(vm: *VM, out: anytype) InterpretResult {
    while (true) {
        if (dbg.options.trace_execution) {
            { // trace stack
                out.print("        |", .{}) catch @panic("OOM");
                var stack_idx = @as(usize, 0);
                while (stack_idx < vm.stack_top) : (stack_idx += 1) {
                    out.print("[ ", .{}) catch @panic("OOM");
                    vl.print_value(vm.stack[stack_idx], out);
                    out.print(" ]", .{}) catch @panic("OOM");
                }
                out.print("|\n", .{}) catch @panic("OOM");
            }

            // trace instruction
            _ = dbg.Disassembler.instruction(vm.chunk, vm.ip, out);
        }
        const inst = @as(OpCode, @enumFromInt(vm.read_byte()));

        switch (inst) {
            .@"return" => {
                vl.print_value(vm.stack_pop(), out);
                out.print("\n", .{}) catch @panic("OOM");
                return .ok;
            },
            .constant => {
                const constant = vm.read_constant();
                vm.stack_push(constant);
            },
            .negate => {
                vm.stack_push(-vm.stack_pop());
            },
            .add,
            .subtract,
            .multiply,
            .divide,
            => |op| {
                const b = vm.stack_pop();
                const a = vm.stack_pop();
                vm.stack_push(switch (op) {
                    .add => a + b,
                    .subtract => a - b,
                    .multiply => a * b,
                    .divide => a / b,
                    else => unreachable,
                });
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

const std = @import("std");

const vl = @import("value.zig");
const dbg = @import("debug.zig");
const cpl = @import("compiler.zig");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = vl.Value;
