//! ...

pub const Value = union(enum) {
    nil: void,
    booln: bool,
    number: f64,
    obj: *Obj,

    pub fn deinit(v: Value, alctr: std.mem.Allocator) void {
        switch (v) {
            .obj => |o| {
                switch (o.typ) {
                    .string => {
                        o.deinit(alctr);
                        alctr.destroy(v.as_string());
                    },
                }
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
            .obj => return a.obj == b.obj,
        }
    }

    pub fn is_string(v: Value) bool {
        return std.meta.activeTag(v) == .obj and v.obj.typ == .string;
    }

    pub fn as_string(v: Value) *ObjString {
        std.debug.assert(std.meta.activeTag(v) == .obj);
        return v.obj.as_string();
    }

    pub fn print(val: Value, out: anytype) void {
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
};

pub const Obj = struct {
    pub const Type = enum {
        string,
    };

    pub fn deinit(o: *Obj, alctr: std.mem.Allocator) void {
        switch (o.typ) {
            .string => {
                o.as_string().deinit(alctr);
            },
        }
    }

    pub fn as_string(o: *Obj) *ObjString {
        std.debug.assert(o.typ == .string);
        return @fieldParentPtr(ObjString, "obj", o);
    }

    typ: Type,
    next: ?*Obj,
};

pub const ObjString = struct {
    obj: Obj,
    buf: []u8,
    hash: u32,

    pub fn deinit(os: ObjString, alctr: std.mem.Allocator) void {
        alctr.free(os.buf);
    }

    pub fn init_in_place(os: *ObjString, chars: []u8, hash: u32) void {
        os.obj.typ = .string;
        os.obj.next = null;

        os.buf = chars;
        os.hash = hash;
    }
};

pub const ObjPool = struct {
    alctr: std.mem.Allocator,
    cached_strings: tbl.Table,
    objs_list: ?*Obj,

    pub fn init(alctr: std.mem.Allocator) ObjPool {
        return .{
            .alctr = alctr,
            .cached_strings = tbl.Table.init(alctr),
            .objs_list = @as(?*Obj, null),
        };
    }

    pub fn deinit(pl: ObjPool) void {
        // free objs list
        var m_obj: ?*Obj = pl.objs_list;
        var count = @as(usize, 0);
        while (m_obj) |obj| {
            count += 1;
            const next = obj.next;
            obj.deinit(pl.alctr);
            switch (obj.typ) {
                .string => pl.alctr.destroy(obj.as_string()),
            }
            m_obj = next;
        }
        log.debug("freed {d} objs", .{count});

        // free cached strings
        pl.cached_strings.deinit();
    }

    pub fn make_string_value(pl: *ObjPool, text: []const u8) Value {
        if (pl.cached_strings.find_string(text)) |cached| return cached;

        const chars = pl.alctr.dupe(u8, text) catch @panic("OOM");
        return pl.take_string_value(chars);
    }

    pub fn take_string_value(pl: *ObjPool, text: []u8) Value {
        if (pl.cached_strings.find_string(text)) |cached| {
            pl.alctr.free(text);
            return cached;
        }

        // allocate and init
        var obj_s = pl.alctr.create(ObjString) catch @panic("OOM");
        obj_s.init_in_place(text, tbl.hash(text));

        // track
        obj_s.obj.next = pl.objs_list;
        pl.objs_list = &obj_s.obj;

        // cache
        const unique = pl.cached_strings.set(obj_s, Value{ .nil = {} });
        std.debug.assert(unique);

        const val = Value{ .obj = &obj_s.obj };
        return val;
    }
};

const std = @import("std");
const log = std.log.scoped(.value);

const tbl = @import("table.zig");
