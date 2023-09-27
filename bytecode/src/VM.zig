//! ...

pub const VM = @This();

pub const Error = error{
    compile_error,
    runtime_error,
};

const CallFrame = struct {
    closure: *vl.ObjClosure,
    ip: usize,
    // window into vm.stack
    slots: []vl.Value,
};

const frames_max = 256;
// each frame can only be 256 items long
const stack_max = frames_max * std.math.maxInt(u8) + 1;

source: []const u8 = undefined,
stack: [stack_max]Value = undefined,
// stack_top points to the next *empty* slot
stack_top: usize = undefined,
frames: [frames_max]CallFrame = undefined,
frames_count: usize = undefined,
pool: vl.ObjPool,
open_upvalues: ?*ObjUpvalue,
gray_stack: std.ArrayList(*Obj),

start_time: i64,

pub fn init_in_place(vm: *VM, alctr: std.mem.Allocator) void {
    vm.* = .{
        .pool = vl.ObjPool.init(alctr, vm),
        .start_time = std.time.milliTimestamp(),
        .open_upvalues = null,
        .gray_stack = @TypeOf(vm.gray_stack).init(alctr),
    };
}

fn define_natives(vm: *VM) void {
    log.debug("define natives", .{});
    vm.define_native("clock", nat.clock, 0);
}

pub fn deinit(vm: VM) void {
    vm.pool.deinit();
    vm.gray_stack.deinit();
}

pub fn interpret(vm: *VM, source_text: []const u8, alctr: std.mem.Allocator, outs: anytype) !void {
    vm.stack_reset();
    vm.frames_count = 0;

    const top_func = try cpl.compile(source_text, &vm.pool, outs);
    vm.stack_push(Value.from(vl.ObjFunction, top_func)); // keep safe from gc

    vm.define_natives();

    log.debug("wrap initial func in closure", .{});
    const closure = vm.pool.add(ObjClosure, .{top_func}).as(ObjClosure);
    _ = vm.stack_pop(); // top_func
    vm.stack_push(Value.from(ObjClosure, closure));

    log.debug("set up initial call frame", .{});
    _ = vm.call(closure, 0, outs);

    vm.source = source_text;

    if (dbg.options.trace_execution) {
        dbg.Disassembler.border("execution trace", usx.err);
        outs.err.print("{s: <7}{s: <5}{s: <5}{s: <16} {s: <16} {s: <5}\n", .{ "offset", "byte", "line", "meaning", "encoded data", "stack before inst. exec." }) catch unreachable;
    }
    log.debug("start running", .{});
    return vm.run(alctr, outs);
}

fn run(vm: *VM, alctr: std.mem.Allocator, outs: anytype) !void {
    var frame = &vm.frames[vm.frames_count - 1];

    while (true) {
        if (dbg.options.trace_execution) {
            dbg.Disassembler.line(frame.closure.func.chunk, frame.ip, vm.source, usx.err);
            // trace instruction
            _ = dbg.Disassembler.instruction(frame.closure.func.chunk, frame.ip, vm, usx.err);
        }
        const inst = @as(OpCode, @enumFromInt(vm.read_byte()));

        switch (inst) {
            .@"return" => {
                const result = vm.stack_pop();
                vm.close_upvalues(frame.slots.ptr);
                vm.frames_count -= 1;
                if (vm.frames_count == 0) {
                    // return from script
                    _ = vm.stack_pop();
                    return;
                }

                vm.stack_top = (@intFromPtr(frame.slots.ptr) - @intFromPtr(&vm.stack)) / @sizeOf(Value);
                vm.stack_push(result);
                frame = &vm.frames[vm.frames_count - 1];
            },
            .print => {
                if (dbg.options.print_color) {
                    const make_blue = "\x1b[94m";
                    const make_bold = "\x1b[1m";
                    outs.out.print("{s}{s}", .{ make_bold, make_blue }) catch unreachable;
                }
                vm.stack_pop().print(outs.out);
                if (dbg.options.print_color) {
                    const reset_color = "\x1b[0m";
                    outs.out.print("{s}\n", .{reset_color}) catch unreachable;
                } else {
                    outs.out.print("\n", .{}) catch unreachable;
                }
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
                        vm.print_runtime_error(outs.err, "Operand must be a number.", .{});
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
                        vm.print_runtime_error(outs.err, "Operands must be two numbers or two strings.", .{});
                    } else {
                        vm.print_runtime_error(outs.err, "Operands must be numbers.", .{});
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
                    vm.print_runtime_error(outs.err, "Undefined variable '{s}'.", .{name.buf});
                    return Error.runtime_error;
                }
            },
            .set_global => {
                const name = vm.read_constant().as(ObjString);
                if (vm.pool.globals.set(name, vm.stack_peek(0))) {
                    const delete_result = vm.pool.globals.delete(name);
                    std.debug.assert(delete_result);
                    vm.print_runtime_error(outs.err, "Undefined variable '{s}'.", .{name.buf});
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
            .get_upvalue => {
                const slot = vm.read_byte();
                vm.stack_push(frame.closure.upvalues[slot].location.*);
            },
            .set_upvalue => {
                const slot = vm.read_byte();
                frame.closure.upvalues[slot].location.* = vm.stack_peek(0);
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
            .call => {
                const arg_count = vm.read_byte();
                if (!vm.call_value(vm.stack_peek(arg_count), arg_count, outs)) {
                    return Error.runtime_error;
                }
                frame = &vm.frames[vm.frames_count - 1];
            },
            .closure => {
                const func = vm.read_constant().as(ObjFunction);
                const closure = vm.pool.add(ObjClosure, .{func}).as(ObjClosure);
                const closure_val = Value.from(ObjClosure, closure);
                vm.stack_push(closure_val);

                // this list is full of undefined memory right now, be careful! also, *uv
                // is a pointer to a pointer.
                for (closure.upvalues) |*uv| {
                    const is_local = vm.read_byte() != 0;
                    const index = vm.read_byte();

                    if (is_local) {
                        uv.* = vm.capture_upvalue(&frame.slots[index]);
                    } else {
                        uv.* = frame.closure.upvalues[index];
                    }

                    closure.upvalues_initd += 1;
                }
            },
            .close_upvalue => {
                vm.close_upvalues(@ptrCast(&vm.stack[vm.stack_top - 1]));
                _ = vm.stack_pop();
            },
            .class => {
                const class = vm.pool.add(ObjClass, .{vm.read_constant().as(ObjString)});
                vm.stack_push(class);
            },
            .get_property => {
                if (!vm.stack_peek(0).is(ObjInstance)) {
                    vm.print_runtime_error(outs.err, "Only instances have properties.", .{});
                    return Error.runtime_error;
                }

                const instance = vm.stack_peek(0).as(ObjInstance);
                const name = vm.read_constant().as(ObjString);

                if (instance.fields.get(name)) |value| {
                    _ = vm.stack_pop(); // the instance
                    vm.stack_push(value);
                } else if (!vm.bind_method(instance.class, name)) {
                    vm.print_runtime_error(outs.err, "Undefined property '{s}'.", .{name.buf});
                    return Error.runtime_error;
                }
            },
            .set_property => {
                if (!vm.stack_peek(1).is(ObjInstance)) {
                    vm.print_runtime_error(outs.err, "Only instances have fields.", .{});
                    return Error.runtime_error;
                }

                var instance = vm.stack_peek(1).as(ObjInstance);
                _ = instance.fields.set(vm.read_constant().as(ObjString), vm.stack_peek(0));
                const value = vm.stack_pop();
                _ = vm.stack_pop(); // the instance
                vm.stack_push(value);
            },
            .method => {
                vm.define_method(vm.read_constant().as(ObjString));
            },
            _ => return Error.runtime_error, // unknown opcode
        }
    }
}

fn capture_upvalue(vm: *VM, val: *Value) *ObjUpvalue {
    var prev: ?*ObjUpvalue = null;
    var curr: ?*ObjUpvalue = vm.open_upvalues;

    // since we close upvalues when we go out of scope, the open upvalues list will only
    // contain local variables from a single function. the variables are stored in a
    // contiguous array, and we keep the open upvalues list sorted based on stack index,
    // so we can get away with some short circuiting tricks by comparing the addresses of
    // the operands.

    while (curr != null and @intFromPtr(curr.?.location) > @intFromPtr(val)) : (curr = curr.?.next) {
        prev = curr;
    }

    if (curr != null and curr.?.location == val) {
        return curr.?;
    }

    const value = vm.pool.add(ObjUpvalue, .{ val, curr });
    const uv = value.as(ObjUpvalue);
    if (prev) |p| {
        p.next = uv;
    } else {
        vm.open_upvalues = uv;
    }

    return uv;
}

fn close_upvalues(vm: *VM, above_this_one: [*]Value) void {
    while (vm.open_upvalues != null and
        @intFromPtr(vm.open_upvalues.?.location) >= @intFromPtr(above_this_one))
    {
        var uv = vm.open_upvalues.?;
        uv.closed = uv.location.*;
        uv.location = &uv.closed;
        vm.open_upvalues = uv.next;
    }
}

fn call_value(vm: *VM, callee: Value, arg_count: u32, outs: anytype) bool {
    switch (callee) {
        .obj => |o| {
            switch (o.otype) {
                .closure => return vm.call(callee.as(ObjClosure), arg_count, outs),
                // functions should always be wrapped in closures
                .function => unreachable,
                .bound_method => return vm.call(callee.as(ObjBoundMethod).method, arg_count, outs),
                .native => {
                    const native = callee.as(ObjNative);

                    if (native.arity != arg_count) {
                        vm.print_runtime_error(outs.err, "Expect {d} arguments, got {d}.", .{ native.arity, arg_count });
                        return false;
                    }

                    const first_arg_idx = vm.stack_top - arg_count;
                    var had_native_error = false;
                    const result = native.function(vm, vm.stack[first_arg_idx .. first_arg_idx + arg_count]) catch blk: {
                        had_native_error = true;
                        break :blk Value{ .nil = {} };
                    };

                    if (!had_native_error) {
                        // pop arguments/function object and push result
                        vm.stack_top -= arg_count + 1;
                        vm.stack_push(result);
                        return true;
                    } else {
                        vm.print_runtime_error(outs.err, "Runtime error in native function.", .{});
                        // print_runtime_error resets the stack
                        return false;
                    }
                },
                .class => {
                    const class = callee.as(ObjClass);
                    const instance = vm.pool.add(ObjInstance, .{class});
                    vm.stack[vm.stack_top - arg_count - 1] = instance;
                    return true;
                },
                else => {}, // fall through
            }
        },
        else => {}, // fall through
    }

    vm.print_runtime_error(outs.err, "Can only call functions and classes.", .{});
    return false;
}

fn call(vm: *VM, callee: *ObjClosure, arg_count: u32, outs: anytype) bool {
    if (arg_count != callee.func.arity) {
        vm.print_runtime_error(outs.err, "Expected {d} arguments but got {d}.", .{ callee.func.arity, arg_count });
        return false;
    }

    if (vm.frames_count == VM.frames_max) {
        vm.print_runtime_error(outs.err, "Stack overflow.", .{});
        return false;
    }

    var frame = &vm.frames[vm.frames_count];
    vm.frames_count += 1;
    frame.* = undefined;

    frame.closure = callee;
    frame.ip = 0;
    frame.slots = vm.stack[vm.stack_top - arg_count - 1 ..];

    return true;
}

fn define_method(vm: *VM, name: *ObjString) void {
    const method = vm.stack_peek(0);
    var class = vm.stack_peek(1).as(ObjClass);
    _ = class.methods.set(name, method);
    _ = vm.stack_pop();
}

fn bind_method(vm: *VM, class: *ObjClass, name: *ObjString) bool {
    if (class.methods.get(name)) |method| {
        const bound = vm.pool.add(ObjBoundMethod, .{ vm.stack_peek(0), method.as(ObjClosure) });
        _ = vm.stack_pop();
        vm.stack_push(bound);
        return true;
    } else {
        return false;
    }
}

// TODO consider combining these read_ functions into single ones that take a type param
fn read_byte(vm: *VM) u8 {
    var frame = &vm.frames[vm.frames_count - 1];
    const b = frame.closure.func.chunk.code.items[frame.ip];
    frame.ip += 1;
    return b;
}

fn read_short(vm: *VM) u16 {
    var frame = &vm.frames[vm.frames_count - 1];
    const hi: u16 = frame.closure.func.chunk.code.items[frame.ip];
    const lo = frame.closure.func.chunk.code.items[frame.ip + 1];
    frame.ip += 2;
    return (hi << 8) | lo;
}

fn read_constant(vm: *VM) Value {
    var frame = &vm.frames[vm.frames_count - 1];
    return frame.closure.func.chunk.constants.items[vm.read_byte()];
}

fn stack_reset(vm: *VM) void {
    log.debug("stack reset", .{});
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

fn define_native(vm: *VM, name: []const u8, f: ObjNative.Fn, arity: u8) void {
    const name_val = vm.pool.make_string_value(name);
    // push to protect from gc
    vm.stack_push(name_val);

    const native_val = vm.pool.add(ObjNative, .{ f, arity });
    // push to protect from gc
    vm.stack_push(native_val);

    const is_new = vm.pool.globals.set(vm.stack_peek(1).as(ObjString), vm.stack_peek(0));
    std.debug.assert(is_new);

    _ = vm.stack_pop();
    _ = vm.stack_pop();
}

fn print_runtime_error(vm: *VM, out: anytype, comptime fmt: []const u8, vars: anytype) void {
    out.print(fmt, vars) catch unreachable;
    out.print("\n", .{}) catch unreachable;

    {
        var i = vm.frames_count - 1;
        while (i >= 0) : (i -= 1) {
            const frame = &vm.frames[i];
            const func = frame.closure.func;
            const instruction = frame.ip - 1;
            const line = frame.closure.func.chunk.lines.items[instruction];

            out.print("[line {d}] in ", .{line}) catch unreachable;
            if (func.ftype == .script) {
                out.print("script\n", .{}) catch unreachable;
            } else {
                out.print("{s}()\n", .{func.name.?.buf}) catch unreachable;
            }

            if (i == 0) break; // last frame, decrement would underflow
        }
    }

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
const nat = @import("native.zig");
const usx = @import("ux.zig");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = vl.Value;

const Obj = vl.Obj;
const ObjString = vl.ObjString;
const ObjFunction = vl.ObjFunction;
const ObjNative = vl.ObjNative;
const ObjClosure = vl.ObjClosure;
const ObjUpvalue = vl.ObjUpvalue;
const ObjClass = vl.ObjClass;
const ObjInstance = vl.ObjInstance;
const ObjBoundMethod = vl.ObjBoundMethod;
