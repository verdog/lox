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

    pub fn buf_print(val: Value, buf: anytype) []u8 {
        var fbs = std.io.fixedBufferStream(buf);
        var writer = fbs.writer();
        val.print(writer);
        return fbs.getWritten();
    }

    pub fn print(val: Value, out: anytype) void {
        switch (val) {
            .number => |n| out.print("{d}", .{n}) catch unreachable,
            .booln => |b| out.print("{}", .{b}) catch unreachable,
            .nil => out.print("nil", .{}) catch unreachable,
            .obj => |o| {
                switch (o.otype) {
                    .string => {
                        var str = val.as(ObjString);
                        out.print("{s}", .{str.buf}) catch unreachable;
                    },
                    .function => {
                        var func = val.as(ObjFunction);
                        const name = blk: {
                            if (func.ftype == .script) break :blk "<script>";
                            if (func.name) |n| break :blk n.buf;
                            break :blk "<no name?>";
                        };
                        out.print("<fn {s}>", .{name}) catch unreachable;
                    },
                    .native => {
                        out.print("<native fn>", .{}) catch unreachable;
                    },
                    .closure => {
                        Value.from(ObjFunction, val.as(ObjClosure).func).print(out);
                    },
                    .upvalue => {
                        // should never be visible to users
                        out.print("{{upvalue {d}}}", .{@intFromPtr(val.obj)}) catch unreachable;
                    },
                    .class => {
                        out.print("{s}", .{val.as(ObjClass).name.buf}) catch unreachable;
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
        closure,
        upvalue,
        class,

        pub fn tag(comptime T: type) Type {
            return switch (T) {
                ObjString => .string,
                ObjFunction => .function,
                ObjNative => .native,
                ObjClosure => .closure,
                ObjUpvalue => .upvalue,
                ObjClass => .class,
                else => @compileError("Not an obj type"),
            };
        }

        pub fn typ(comptime tg: Type) type {
            return switch (tg) {
                .string => ObjString,
                .function => ObjFunction,
                .native => ObjNative,
                .closure => ObjClosure,
                .upvalue => ObjUpvalue,
                .class => ObjClass,
            };
        }
    };

    pub fn init_in_place(o: *Obj, typ: Type) void {
        o.otype = typ;
        o.next = null;
        o.is_marked = false;
    }

    pub fn deinit(o: *Obj, alctr: std.mem.Allocator) void {
        switch (o.otype) {
            inline else => |tg| {
                if (dbg.options.log_garbage_collection) {
                    log.debug(" gc deinit 0x{x}, {s}", .{ @intFromPtr(o), @tagName(tg) });
                }
                o.as(Type.typ(tg)).deinit(alctr);
            },
        }
    }

    pub fn as(o: *Obj, comptime T: type) *T {
        std.debug.assert(o.otype == Type.tag(T));
        return @fieldParentPtr(T, "obj", o);
    }

    otype: Type,
    next: ?*Obj,
    is_marked: bool,
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
    upvalue_count: i16,
    chunk: Chunk,

    // the name has to be optional because during construction, the ObjFunction has to
    // live without one for a moment. see Parser.function in compiler.zig.
    name: ?*ObjString,

    pub const Type = enum {
        script, // the top level lox script, where e.g. global variable semantics are different
        function, // everything not that
    };

    pub fn alloc(fname: ?*ObjString, ftype: Type, alctr: std.mem.Allocator) *ObjFunction {
        var obj_f = alctr.create(ObjFunction) catch @panic("OOM");
        obj_f.init_in_place(fname, ftype, alctr);
        return obj_f;
    }

    fn init_in_place(of: *ObjFunction, fname: ?*ObjString, ftype: Type, alctr: std.mem.Allocator) void {
        of.* = .{
            .obj = undefined,
            .ftype = ftype,
            .arity = 0,
            .upvalue_count = 0,
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
    pub const Error = error{
        native_runtime_error,
    };

    pub const Fn = *const fn (vm: *VM, args: []Value) Error!Value;

    obj: Obj,
    function: Fn,
    arity: u8,

    pub fn alloc(f: Fn, arity: u8, alctr: std.mem.Allocator) *ObjNative {
        var obj_n = alctr.create(ObjNative) catch @panic("OOM");
        obj_n.init_in_place(f, arity);
        return obj_n;
    }

    fn init_in_place(on: *ObjNative, f: Fn, arity: u8) void {
        on.* = .{
            .obj = undefined,
            .function = f,
            .arity = arity,
        };

        on.obj.init_in_place(Obj.Type.tag(ObjNative));
    }

    pub fn deinit(on: *ObjNative, alctr: std.mem.Allocator) void {
        _ = alctr;
        _ = on;
        // nothing to do
    }
};

pub const ObjClosure = struct {
    obj: Obj,
    func: *ObjFunction,
    upvalues: []*ObjUpvalue,
    upvalues_initd: usize,

    pub fn alloc(func: *ObjFunction, alctr: std.mem.Allocator) *ObjClosure {
        var obj_c = alctr.create(ObjClosure) catch @panic("OOM");
        obj_c.init_in_place(func, alctr);
        return obj_c;
    }

    fn init_in_place(oc: *ObjClosure, func: *ObjFunction, alctr: std.mem.Allocator) void {
        oc.* = .{
            .obj = undefined,
            .func = func,
            .upvalues = alctr.alloc(*ObjUpvalue, @intCast(func.upvalue_count)) catch @panic("OOM"),
            .upvalues_initd = 0,
        };

        // book nulls all the upvalue pointers here to protect from gc. instead, i'll keep
        // track of how many upvalues have been initialized. this data is only meaningful
        // during init, so it'd be nice to find a clean way to drop it once init is done.

        oc.obj.init_in_place(Obj.Type.tag(ObjClosure));
    }

    pub fn deinit(oc: *ObjClosure, alctr: std.mem.Allocator) void {
        // ObjClosure doesn't own the *pointed to* ObjUpvalues, but we need to clear the
        // list of pointers
        alctr.free(oc.upvalues);
    }
};

pub const ObjUpvalue = struct {
    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,

    pub fn alloc(val: *Value, next: ?*ObjUpvalue, alctr: std.mem.Allocator) *ObjUpvalue {
        var obj_up = alctr.create(ObjUpvalue) catch @panic("OOM");
        obj_up.init_in_place(val, next);
        return obj_up;
    }

    fn init_in_place(ouv: *ObjUpvalue, val: *Value, next: ?*ObjUpvalue) void {
        ouv.* = .{
            .obj = undefined,
            .location = val,
            .closed = Value{ .nil = {} },
            .next = next,
        };

        ouv.obj.init_in_place(Obj.Type.tag(ObjUpvalue));
    }

    pub fn deinit(ouv: *ObjUpvalue, alctr: std.mem.Allocator) void {
        _ = alctr;
        _ = ouv;
        // nothing to do
    }
};

pub const ObjClass = struct {
    obj: Obj,
    name: *ObjString,

    pub fn alloc(name: *ObjString, alctr: std.mem.Allocator) *ObjClass {
        var obj_cl = alctr.create(ObjClass) catch @panic("OOM");
        obj_cl.init_in_place(name);
        return obj_cl;
    }

    fn init_in_place(oc: *ObjClass, name: *ObjString) void {
        oc.* = .{
            .obj = undefined,
            .name = name,
        };

        oc.obj.init_in_place(Obj.Type.tag(ObjClass));
    }

    pub fn deinit(oc: *ObjClass, alctr: std.mem.Allocator) void {
        _ = alctr;
        _ = oc;
        // nothing to do
    }
};

pub const ObjPool = struct {
    alctr: std.mem.Allocator,
    cached_strings: tbl.Table,
    globals: tbl.Table,
    objs_list: ?*Obj,

    /// bytes_allocated and next_garbage_collection are unused. the book has us track
    /// every allocation and deallocation to decide when to collect garbage. since i'm using
    /// the "unmanaged" style, where I pass the allocator to the deinit function, decreasing
    /// bytes_allocated after deinit is hard.
    ///
    /// eventually i should write an allocator wrapper that will track that cleanly.
    bytes_allocated: usize,
    next_garbage_collection: usize,

    // XXX: sloppy backpointers...
    vm: *VM,
    compiler_ref: ?**cpl.Compiler,

    pub fn init(alctr: std.mem.Allocator, vm: *VM) ObjPool {
        return .{
            .alctr = alctr,
            .cached_strings = tbl.Table.init(alctr),
            .globals = tbl.Table.init(alctr),
            .objs_list = @as(?*Obj, null),
            .vm = vm,
            .compiler_ref = null,
            .bytes_allocated = 0,
            .next_garbage_collection = 1024,
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
        if (dbg.options.log_garbage_collection)
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

        if (dbg.options.stress_garbage_collection) {
            pl.collect_garbage();
        }

        // allocate
        var obj_s = ObjString.alloc(text, pl.alctr);
        if (dbg.options.log_garbage_collection)
            log.debug(" gc add 0x{x}, string, {s}:{d} to 0x{x}", .{ @intFromPtr(&obj_s.obj), text, obj_s.hash, @intFromPtr(pl) });

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

        if (dbg.options.stress_garbage_collection) {
            pl.collect_garbage();
        }

        var ptr = @call(.auto, T.alloc, args ++ .{pl.alctr});
        pl.track_obj(&ptr.obj);

        if (dbg.options.log_garbage_collection) {
            var buf: [256]u8 = undefined;
            var str = (Value{ .obj = &ptr.obj }).buf_print(&buf);
            log.debug(" gc add 0x{x}, {s}, {s} to 0x{x}", .{ @intFromPtr(ptr), @typeName(T), str, @intFromPtr(pl) });
        }

        return Value.from(T, ptr);
    }

    fn track_obj(pl: *ObjPool, obj: *Obj) void {
        obj.next = pl.objs_list;
        pl.objs_list = obj;
    }

    fn collect_garbage(pl: *ObjPool) void {
        if (dbg.options.log_garbage_collection)
            log.debug(">- gc begin", .{});

        pl.mark_roots();
        pl.trace_refs();
        pl.cached_strings.remove_white();
        pl.sweep();

        if (dbg.options.log_garbage_collection)
            log.debug("<- gc end", .{});
    }

    fn mark_roots(pl: *ObjPool) void {
        log.debug(" gc mark stack roots", .{});
        for (0..pl.vm.stack_top) |i| {
            pl.mark_value(pl.vm.stack[i]);
        }

        log.debug(" gc mark closures", .{});
        for (0..pl.vm.frames_count) |i| {
            pl.mark_obj(&pl.vm.frames[i].closure.obj);
        }

        log.debug(" gc mark open upvalues", .{});
        {
            var m_uv = pl.vm.open_upvalues;
            while (m_uv) |uv| : (m_uv = uv.next) {
                pl.mark_obj(&uv.obj);
            }
        }

        log.debug(" gc mark globals", .{});
        pl.mark_table(&pl.globals);

        log.debug(" gc mark internal roots", .{});
        pl.mark_compiler_roots();
    }

    fn mark_compiler_roots(pl: *ObjPool) void {
        if (pl.compiler_ref) |cref| {
            var maybe_comp: ?*cpl.Compiler = cref.*;
            while (maybe_comp) |comp| {
                pl.mark_obj(&comp.function.obj);
                maybe_comp = comp.enclosing;
            }
        }
    }

    fn mark_value(pl: *ObjPool, val: Value) void {
        if (std.meta.activeTag(val) == .obj) pl.mark_obj(val.obj);
    }

    fn mark_obj(pl: *ObjPool, obj: ?*Obj) void {
        if (obj) |o| {
            if (o.is_marked) return; // avoid getting trapped in reference cycles

            if (dbg.options.log_garbage_collection) {
                var buf: [256]u8 = undefined;
                var str = (Value{ .obj = o }).buf_print(&buf);
                log.debug(" gc mark 0x{x} {s} {s}", .{ @intFromPtr(o), @tagName(o.otype), str });
            }
            o.is_marked = true;

            pl.vm.gray_stack.append(o) catch @panic("OOM");
        }
    }

    fn mark_table(pl: *ObjPool, table: *tbl.Table) void {
        for (table.entries) |*entry| {
            if (entry.key) |k| pl.mark_obj(&k.obj);
            pl.mark_value(entry.value);
        }
    }

    fn mark_array(pl: *ObjPool, array: []Value) void {
        for (array) |v| pl.mark_value(v);
    }

    fn trace_refs(pl: *ObjPool) void {
        while (pl.vm.gray_stack.items.len > 0) {
            var top = pl.vm.gray_stack.pop();
            pl.blacken_obj(top);
        }
    }

    fn blacken_obj(pl: *ObjPool, obj: *Obj) void {
        if (dbg.options.log_garbage_collection) {
            var buf: [256]u8 = undefined;
            var str = (Value{ .obj = obj }).buf_print(&buf);
            log.debug(" gc blacken 0x{x} {s} {s} {}", .{ @intFromPtr(obj), @tagName(obj.otype), str, obj.is_marked });
        }

        switch (obj.otype) {
            .native,
            .string,
            => {}, // contain no outgoing references

            .upvalue => {
                pl.mark_value(obj.as(ObjUpvalue).closed);
            },

            .function => {
                var func = obj.as(ObjFunction);
                if (func.ftype == .function) {
                    log.debug(" gc blacken function name...", .{});
                    // we might be collecting garbage in the tiny window between function
                    // creation time and function naming, so it's okay for the name to be
                    // null here
                    if (func.name) |n| {
                        pl.mark_obj(&n.obj);
                    } else {
                        log.debug(" (no name)", .{});
                    }
                }
                log.debug(" gc blacken function constants...", .{});
                pl.mark_array(func.chunk.constants.items);
            },

            .closure => {
                var closure = obj.as(ObjClosure);
                pl.mark_obj(&closure.func.obj);
                for (0..closure.upvalues_initd) |i| {
                    var uv = closure.upvalues[i];
                    pl.mark_obj(&uv.obj);
                }
            },

            .class => {
                pl.mark_obj(&obj.as(ObjClass).name.obj);
            },
        }
    }

    fn sweep(pl: *ObjPool) void {
        var prev: ?*Obj = null;
        var curr: ?*Obj = pl.objs_list;

        while (curr) |obj| {
            if (obj.is_marked) {
                // ignore and advance
                obj.is_marked = false;
                prev = obj;
                curr = obj.next;
            } else {
                // clean up
                if (dbg.options.log_garbage_collection) {
                    var buf: [256]u8 = undefined;
                    var str = (Value{ .obj = obj }).buf_print(&buf);
                    log.debug(" gc sweep 0x{x} {s} {s}", .{ @intFromPtr(obj), @tagName(obj.otype), str });
                }

                var unreached = obj;
                curr = obj.next;
                if (prev) |p| {
                    p.next = curr;
                } else {
                    pl.objs_list = curr;
                }

                unreached.deinit(pl.alctr);
                switch (unreached.otype) {
                    inline else => |tg| pl.alctr.destroy(obj.as(Obj.Type.typ(tg))),
                }
            }
        }
    }
};

const std = @import("std");
const log = std.log.scoped(.pool);

const tbl = @import("table.zig");
const dbg = @import("debug.zig");
const cpl = @import("compiler.zig");

const Chunk = @import("chunk.zig").Chunk;
const VM = @import("VM.zig");
