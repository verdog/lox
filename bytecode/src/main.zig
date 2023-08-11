pub const std_options = struct {
    pub const log_level = if (@import("builtin").mode == .Debug) .debug else .info;
};

pub fn main() !void {
    defer ux.stdout_buffer.flush() catch {};

    var chunk = Chunk.init(heap);
    defer chunk.deinit();

    var constant = chunk.addConstant(1.2);
    chunk.writeOpCode(.constant, 123);
    chunk.write(constant, 123);

    constant = chunk.addConstant(3.4);
    chunk.writeOpCode(.constant, 123);
    chunk.write(constant, 123);

    chunk.writeOpCode(.add, 123);

    constant = chunk.addConstant(5.6);
    chunk.writeOpCode(.constant, 123);
    chunk.write(constant, 123);

    chunk.writeOpCode(.divide, 123);

    chunk.writeOpCode(.negate, 123);

    chunk.writeOpCode(.@"return", 123);

    dbg.Disassembler.chunk(chunk, "test chunk", ux.out);

    var vm = VM.init();
    _ = vm.interpret(chunk, ux.out);
}

test "run all tests" {
    std.testing.refAllDeclsRecursive(@This());
}

const std = @import("std");
var gpa = std.heap.GeneralPurposeAllocator(.{
    .stack_trace_frames = 16,
}){};
var heap = gpa.allocator();

const log = std.log.scoped(.main);

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const VM = @import("VM.zig");

const ux = @import("ux.zig");
const dbg = @import("debug.zig");
