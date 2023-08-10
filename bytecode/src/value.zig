//! ...

pub const Value = f64;

pub fn print_value(val: Value, out: anytype) void {
    out.print("{d}", .{val}) catch @panic("OOM");
}
