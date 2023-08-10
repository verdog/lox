pub const std_options = struct {
    pub const log_level = if (@import("builtin").mode == .Debug) .debug else .info;
};

pub fn main() !void {
    defer ux.stdout_buffer.flush() catch {};

    var chunk = Chunk.init(heap);
    defer chunk.deinit();

    {
        const c = chunk.addConstant(1.1);
        chunk.writeOpCode(OpCode.constant, 123);
        chunk.write(@intCast(c), 123);
    }
    {
        const c = chunk.addConstant(2.2);
        chunk.writeOpCode(OpCode.constant, 125);
        chunk.write(@intCast(c), 125);
    }
    {
        const c = chunk.addConstant(3.3);
        chunk.writeOpCode(OpCode.constant, 127);
        chunk.write(@intCast(c), 127);
        chunk.writeOpCode(OpCode.@"return", 128);
    }

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
