//! ...

pub const Value = union(enum) {
    nil: void,
    booln: bool,
    number: f64,
    obj: *Obj,

    pub fn deinit(v: Value, alctr: std.mem.Allocator) void {
        switch (v) {
            .obj => |o| {
                o.deinit(alctr);
                alctr.destroy(@fieldParentPtr(ObjString, "obj", o));
            },
            else => {}, // nothing to do
        }
    }

    pub fn is_truthy(v: Value) bool {
        switch (v) {
            .nil => return false,
            .booln => |b| return b,
            else => return true,
        }
    }

    pub fn check_eql(a: Value, b: Value) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        switch (a) {
            .nil => return true,
            .booln => return a.booln == b.booln,
            .number => return a.number == b.number,
            .obj => |o| {
                switch (o.typ) {
                    .string => {
                        return std.mem.eql(u8, a.as_string_buf(), b.as_string_buf());
                    },
                }
            },
        }
    }

    pub fn is_string(v: Value) bool {
        return std.meta.activeTag(v) == .obj and v.obj.typ == .string;
    }

    pub fn as_string(v: Value) *ObjString {
        std.debug.assert(std.meta.activeTag(v) == .obj);
        std.debug.assert(v.obj.typ == .string);

        return @fieldParentPtr(ObjString, "obj", v.obj);
    }

    pub fn as_string_buf(v: Value) []u8 {
        std.debug.assert(std.meta.activeTag(v) == .obj);
        std.debug.assert(v.obj.typ == .string);

        return @fieldParentPtr(ObjString, "obj", v.obj).buf;
    }
};

pub const Obj = struct {
    pub const Type = enum {
        string,
    };

    pub fn deinit(o: *Obj, alctr: std.mem.Allocator) void {
        switch (o.typ) {
            .string => {
                @fieldParentPtr(ObjString, "obj", o).deinit(alctr);
            },
        }
    }

    typ: Type,
    next: ?*Obj,
};

pub const ObjString = struct {
    obj: Obj,
    buf: []u8,

    pub fn deinit(os: ObjString, alctr: std.mem.Allocator) void {
        alctr.free(os.buf);
    }

    pub fn init_in_place(os: *ObjString, chars: []u8) void {
        os.obj.typ = .string;
        os.obj.next = null;
        os.buf = chars;
    }
};

pub fn make_string_value(text: []const u8, alctr: std.mem.Allocator) Value {
    const chars = alctr.dupe(u8, text) catch @panic("OOM");
    return take_string_value(chars, alctr);
}

pub fn take_string_value(text: []u8, alctr: std.mem.Allocator) Value {
    var obj_s = alctr.create(ObjString) catch @panic("OOM");
    obj_s.init_in_place(text);

    const val = Value{ .obj = &obj_s.obj };

    return val;
}

pub fn print_value(val: Value, out: anytype) void {
    switch (val) {
        .number => |n| out.print("{d}", .{n}) catch unreachable,
        .booln => |b| out.print("{}", .{b}) catch unreachable,
        .nil => out.print("(nil)", .{}) catch unreachable,
        .obj => |o| {
            switch (o.typ) {
                .string => {
                    var str = val.as_string();
                    out.print("{s}", .{str.buf}) catch unreachable;
                },
            }
        },
    }
}

const std = @import("std");
