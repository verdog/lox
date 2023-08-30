//! ...

pub const Value = union(enum) {
    nil: void,
    booln: bool,
    number: f64,
    obj: *Obj,

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

    pub fn is(v: Value, comptime T: type) bool {
        return std.meta.activeTag(v) == .obj and v.obj.otype == Obj.Type.tag(T);
    }

    pub fn as(v: Value, comptime T: type) *T {
        return v.obj.as(T);
    }

    pub fn from(comptime T: type, obj: *T) Value {
        return Value{ .obj = &obj.obj };
    }

    pub fn print(val: Value, out: anytype) void {
        switch (val) {
            .number => |n| out.print("{d:.4}", .{n}) catch unreachable,
            .booln => |b| out.print("{}", .{b}) catch unreachable,
            .nil => out.print("(nil)", .{}) catch unreachable,
            .obj => |o| {
                switch (o.otype) {
                    .string => {
                        var str = val.as(ObjString);
                        out.print("{s}", .{str.buf}) catch unreachable;
                    },
                    .function => {
                        var func = val.as(ObjFunction);
                        const name = if (func.ftype == .script) "<script>" else func.name.buf;
                        out.print("<fn {s}>", .{name}) catch unreachable;
                    },
                    .native => {
                        out.print("<native fn>", .{}) catch unreachable;
                    },
                }
            },
        }
    }
};

pub const Obj = struct {
    pub const Type = enum {
        string,
        function,
        native,

        pub fn tag(comptime T: type) Type {
            return switch (T) {
                ObjString => .string,
                ObjFunction => .function,
                ObjNative => .native,
                else => @compileError("Not an obj type"),
            };
        }

        pub fn typ(comptime tg: Type) type {
            return switch (tg) {
                .string => ObjString,
                .function => ObjFunction,
                .native => ObjNative,
            };
        }
    };

    pub fn init_in_place(o: *Obj, typ: Type) void {
        o.otype = typ;
        o.next = null;
    }

    pub fn deinit(o: *Obj, alctr: std.mem.Allocator) void {
        switch (o.otype) {
            inline else => |tg| o.as(Type.typ(tg)).deinit(alctr),
        }
    }

    pub fn as(o: *Obj, comptime T: type) *T {
        std.debug.assert(o.otype == Type.tag(T));
        return @fieldParentPtr(T, "obj", o);
    }

    otype: Type,
    next: ?*Obj,
};

pub const ObjString = struct {
    obj: Obj,
    buf: []u8,
    hash: u32,

    pub fn alloc(chars: []u8, alctr: std.mem.Allocator) *ObjString {
        var obj_s = alctr.create(ObjString) catch @panic("OOM");
        obj_s.init_in_place(chars, tbl.hash(chars));
        return obj_s;
    }

    fn init_in_place(os: *ObjString, chars: []u8, hash: u32) void {
        os.obj.init_in_place(Obj.Type.tag(ObjString));

        os.buf = chars;
        os.hash = hash;
    }

    pub fn deinit(os: ObjString, alctr: std.mem.Allocator) void {
        alctr.free(os.buf);
    }
};

pub const ObjFunction = struct {
    obj: Obj,
    ftype: Type,
    arity: u8,
    chunk: Chunk,
    name: *const ObjString,

    pub const Type = enum {
        script, // the top level lox script, where e.g. global variable semantics are different
        function, // everything not that
    };

    pub fn alloc(fname: *const ObjString, ftype: Type, alctr: std.mem.Allocator) *ObjFunction {
        var obj_f = alctr.create(ObjFunction) catch @panic("OOM");
        obj_f.init_in_place(fname, ftype, alctr);
        return obj_f;
    }

    fn init_in_place(of: *ObjFunction, fname: *const ObjString, ftype: Type, alctr: std.mem.Allocator) void {
        of.* = .{
            .obj = undefined,
            .ftype = ftype,
            .arity = 0,
            .chunk = Chunk.init(alctr),
            .name = fname,
        };

        of.obj.init_in_place(Obj.Type.tag(ObjFunction));
    }

    pub fn deinit(of: *ObjFunction, alctr: std.mem.Allocator) void {
        _ = alctr;
        of.chunk.deinit();
        // name free'd by owning ObjPool
    }
};

pub const ObjNative = struct {
    pub const Fn = *const fn (vm: *VM, args: []Value) Value;

    obj: Obj,
    function: Fn,

    pub fn alloc(f: Fn, alctr: std.mem.Allocator) *ObjNative {
        var obj_n = alctr.create(ObjNative) catch @panic("OOM");
        obj_n.init_in_place(f);
        return obj_n;
    }

    fn init_in_place(on: *ObjNative, f: Fn) void {
        on.* = .{
            .obj = undefined,
            .function = f,
        };

        on.obj.init_in_place(Obj.Type.tag(ObjNative));
    }

    pub fn deinit(on: *ObjNative, alctr: std.mem.Allocator) void {
        _ = alctr;
        _ = on;
        // nothing to do
    }
};

pub const ObjPool = struct {
    alctr: std.mem.Allocator,
    cached_strings: tbl.Table,
    globals: tbl.Table,
    objs_list: ?*Obj,

    pub fn init(alctr: std.mem.Allocator) ObjPool {
        return .{
            .alctr = alctr,
            .cached_strings = tbl.Table.init(alctr),
            .globals = tbl.Table.init(alctr),
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
            switch (obj.otype) {
                inline else => |tg| pl.alctr.destroy(obj.as(Obj.Type.typ(tg))),
            }
            m_obj = next;
        }
        log.debug("freed {d} objs", .{count});

        // free cached strings
        pl.cached_strings.deinit();

        // free globals
        pl.globals.deinit();
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

        // allocate
        var obj_s = ObjString.alloc(text, pl.alctr);

        // track
        pl.track_obj(&obj_s.obj);

        // cache
        const unique = pl.cached_strings.set(obj_s, Value{ .nil = {} });
        std.debug.assert(unique);

        const val = Value{ .obj = &obj_s.obj };
        return val;
    }

    pub fn add(pl: *ObjPool, comptime T: type, args: anytype) Value {
        if (T == ObjString) @compileError("Use make/take string for that.");

        var ptr = @call(.auto, T.alloc, args ++ .{pl.alctr});
        pl.track_obj(&ptr.obj);
        return Value.from(T, ptr);
    }

    fn track_obj(pl: *ObjPool, obj: *Obj) void {
        obj.next = pl.objs_list;
        pl.objs_list = obj;
    }
};

const std = @import("std");
const log = std.log.scoped(.value);

const tbl = @import("table.zig");

const Chunk = @import("chunk.zig").Chunk;
const VM = @import("VM.zig");
