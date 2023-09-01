//! available native functions

// TODO

pub fn clock(vm: *VM, args: []Value) ObjNative.Error!Value {
    _ = args;
    const milliseconds_ellapsed = std.time.milliTimestamp() - vm.start_time;
    const fme: f64 = @floatFromInt(milliseconds_ellapsed);

    return Value{ .number = fme / 1000 };
}

const std = @import("std");
const val = @import("value.zig");

const VM = @import("VM.zig");
const Value = val.Value;
const ObjNative = val.ObjNative;
