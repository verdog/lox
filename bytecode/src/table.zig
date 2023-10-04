//! ...

pub const Table = struct {
    count: usize,
    entries: []Entry,
    alctr: std.mem.Allocator,

    const max_load: f64 = 0.75;

    pub const Entry = struct {
        key: ?*vl.ObjString,
        value: vl.Value,
    };

    pub fn init(alctr: std.mem.Allocator) Table {
        const t = Table{
            .count = 0,
            .entries = alctr.alloc(Entry, 8) catch @panic("OOM"),
            .alctr = alctr,
        };

        for (t.entries) |*e| {
            e.key = null;
            e.value = vl.Value{ .nil = {} };
        }

        return t;
    }

    pub fn deinit(t: Table) void {
        t.alctr.free(t.entries);
        // gc will free individual entries

        // book has table reset here? might need a different reset operation later
        // t.count = 0;
        // t.entries = undefined;
    }

    pub fn set(t: *Table, key: *vl.ObjString, value: vl.Value) bool {
        if (t.count + 1 > @as(usize, @intFromFloat(@as(f64, @floatFromInt(t.entries.len)) * Table.max_load))) {
            const cap = t.get_next_capacity();
            t.adjust_capacity(cap);
        }

        var find_result = t.find_entry(t.entries, key);
        var entry = find_result.entry;
        const is_new_slot = find_result.is_new;
        const is_new_to_user = find_result.entry.key == null;

        if (is_new_slot) t.count += 1;

        entry.key = key;
        entry.value = value;
        return is_new_to_user;
    }

    pub fn get(t: *Table, key: *vl.ObjString) ?vl.Value {
        if (t.count == 0) return null;

        var find_result = t.find_entry(t.entries, key);
        if (find_result.entry.key == null) return null;

        return find_result.entry.value;
    }

    pub fn delete(t: *Table, key: *vl.ObjString) bool {
        if (t.count == 0) return false;

        var find_result = t.find_entry(t.entries, key);
        if (find_result.entry.key) |_| {
            find_result.entry.key = null;
            find_result.entry.value = vl.Value{ .booln = true }; // tombstone
            return true;
        } else {
            return false;
        }
    }

    pub fn find_string(t: Table, string: []const u8) ?vl.Value {
        if (t.count == 0) return null;

        // XXX: the allocation functions that call find_string calculate the hash *again*.
        // either take the hash as an argument or return it somehow.
        const hsh = hash(string);

        // assume power of two in order to optimize modulo with bitwise and
        std.debug.assert(@popCount(t.entries.len) == 1);
        const mask = @as(u32, @truncate(t.entries.len - 1));
        var index: u32 = hsh & mask;

        while (true) {
            const potential_entry = &t.entries[index];
            if (potential_entry.key == null) {
                // stop if we find an empty non-tombstone
                if (std.meta.activeTag(potential_entry.value) == .nil) return null;
            } else if (potential_entry.key.?.hash == hsh and std.mem.eql(u8, potential_entry.key.?.buf, string)) {
                // found it
                return vl.Value{ .obj = &potential_entry.key.?.obj };
            }

            index = (index + 1) & mask;
        }
    }

    pub fn add_all(t: *Table, from: Table) void {
        for (from.entries) |e| {
            if (e.key) |k| {
                _ = t.set(k, e.value);
            }
        }
    }

    pub fn remove_white(t: *Table) void {
        for (t.entries) |e| {
            if (e.key) |k| {
                if (!k.obj.is_marked) {
                    log.debug(" gc remove white {s} 0x{x}", .{ k.buf, @intFromPtr(&k.obj) });
                    _ = t.delete(k);
                }
            }
        }
    }

    fn find_entry(t: *Table, entries: []Entry, key: *vl.ObjString) struct { is_new: bool, entry: *Entry } {
        _ = t;

        // assume power of two in order to optimize modulo with bitwise and
        std.debug.assert(@popCount(entries.len) == 1);
        const mask = @as(u32, @truncate(entries.len - 1));

        var index: u32 = key.hash & mask;
        var first_tombstone: ?*Entry = null;
        while (true) {
            const potential_entry = &entries[index];
            if (potential_entry.key) |exist_key| {
                if (exist_key == key) return .{ .is_new = false, .entry = potential_entry };
            } else {
                if (std.meta.activeTag(potential_entry.value) == .nil) {
                    // truely empty
                    return .{ .is_new = first_tombstone == null, .entry = first_tombstone orelse potential_entry };
                } else {
                    std.debug.assert(std.meta.activeTag(potential_entry.value) == .booln);
                    // found a tombstone
                    if (first_tombstone == null) first_tombstone = potential_entry;
                }
            }

            index = (index + 1) & mask;
        }
    }

    fn get_next_capacity(t: Table) usize {
        return t.entries.len * 2;
    }

    fn adjust_capacity(t: *Table, new_cap: usize) void {
        log.debug("adjust capacity", .{});
        var new_entries = t.alctr.alloc(Entry, new_cap) catch @panic("OOM");
        for (new_entries) |*e| {
            e.key = null;
            e.value = vl.Value{ .nil = {} };
        }

        var new_count: usize = 0;
        for (t.entries) |*e| {
            if (e.key) |k| {
                var dest_result = t.find_entry(new_entries, k);
                dest_result.entry.key = k;
                dest_result.entry.value = e.value;
                new_count += 1;
            }
        }

        t.alctr.free(t.entries);
        t.entries = new_entries;
        t.count = new_count;
    }
};

pub fn hash(text: []const u8) u32 {
    var h: u32 = 2166136261;
    for (text) |c| {
        h ^= c;
        h *%= 16777619;
    }
    return h;
}

// TODO add extensive unit tests

const std = @import("std");

const vl = @import("value.zig");
const log = std.log.scoped(.table);
