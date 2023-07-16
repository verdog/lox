const Value = union(enum) {
    nil: void,
    booln: bool,
    number: f64,
    string: std.ArrayList(u8),

    pub fn deinit(v: Value) void {
        switch (v) {
            .string => |s| s.deinit(),
            else => {},
        }
    }
};

const Interpreter = struct {
    pub fn init() Interpreter {
        return .{};
    }

    pub fn deinit(intr: *Interpreter) void {
        _ = intr;
    }

    /// implements visitor interface required by parse.Expr.acceptVisitor
    pub fn visit(intr: Interpreter, expr: prs.Expr, pl: prs.ExprPool, alctr: std.mem.Allocator) Value {
        switch (expr) {
            .binary => |b| {
                var left = pl.fromIndex(b.left).acceptVisitor(pl, alctr, intr);
                defer left.deinit();
                var right = pl.fromIndex(b.right).acceptVisitor(pl, alctr, intr);
                defer right.deinit();

                // for now.
                std.debug.assert(std.meta.activeTag(left) == std.meta.activeTag(right));

                switch (b.operator.typ) {
                    .plus => {
                        // relying on the above assert that the active tags are equal
                        switch (left) {
                            .number => return .{ .number = left.number + right.number },
                            .string => {
                                var result: Value = .{ .string = left.string.clone() catch @panic("OOM") };
                                result.string.appendSlice(right.string.items) catch @panic("OOM");
                                return result;
                            },
                            else => unreachable,
                        }
                    },
                    .minus => {
                        std.debug.assert(std.meta.activeTag(left) == .number);
                        std.debug.assert(std.meta.activeTag(right) == .number);
                        return .{ .number = left.number - right.number };
                    },
                    .star => {
                        std.debug.assert(std.meta.activeTag(left) == .number);
                        std.debug.assert(std.meta.activeTag(right) == .number);
                        return .{ .number = left.number * right.number };
                    },
                    .slash => {
                        std.debug.assert(std.meta.activeTag(left) == .number);
                        std.debug.assert(std.meta.activeTag(right) == .number);
                        return .{ .number = left.number / right.number };
                    },
                    .greater => {
                        std.debug.assert(std.meta.activeTag(left) == .number);
                        std.debug.assert(std.meta.activeTag(right) == .number);
                        return .{ .booln = left.number > right.number };
                    },
                    .greater_eql => {
                        std.debug.assert(std.meta.activeTag(left) == .number);
                        std.debug.assert(std.meta.activeTag(right) == .number);
                        return .{ .booln = left.number >= right.number };
                    },
                    .less => {
                        std.debug.assert(std.meta.activeTag(left) == .number);
                        std.debug.assert(std.meta.activeTag(right) == .number);
                        return .{ .booln = left.number < right.number };
                    },
                    .less_eql => {
                        std.debug.assert(std.meta.activeTag(left) == .number);
                        std.debug.assert(std.meta.activeTag(right) == .number);
                        return .{ .booln = left.number <= right.number };
                    },
                    .eql_eql => {
                        std.debug.assert(std.meta.activeTag(left) == .number);
                        std.debug.assert(std.meta.activeTag(right) == .number);
                        return .{ .booln = std.meta.eql(left, right) };
                    },
                    .bang_eql => {
                        std.debug.assert(std.meta.activeTag(left) == .number);
                        std.debug.assert(std.meta.activeTag(right) == .number);
                        return .{ .booln = !std.meta.eql(left, right) };
                    },

                    else => unreachable,
                }
            },
            .unary => |u| {
                var value = pl.fromIndex(u.right).acceptVisitor(pl, alctr, intr);
                switch (u.operator.typ) {
                    .minus => {
                        std.debug.assert(std.meta.activeTag(value) == .number);
                        return .{ .number = -value.number };
                    },
                    .bang => {
                        return .{ .booln = !isTruthy(value) };
                    },

                    else => unreachable,
                }
            },
            .grouping => |g| return pl.fromIndex(g.expression).acceptVisitor(pl, alctr, intr),
            .literal => |l| return interpretTokenValue(l.value, alctr),
        }

        unreachable;
    }

    fn stringValueFromSlice(string: []const u8, alctr: std.mem.Allocator) Value {
        const string_memory = alctr.dupe(u8, string) catch @panic("OOM");
        return .{ .string = std.ArrayList(u8).fromOwnedSlice(alctr, string_memory) };
    }

    fn interpretTokenValue(token: lex.Token, alctr: std.mem.Allocator) Value {
        return switch (token.typ) {
            .number => .{ .number = std.fmt.parseFloat(f64, token.lexeme) catch @panic("NAN") },
            .true => .{ .booln = true },
            .false => .{ .booln = false },
            .string => {
                // lexemes retain their quotes, so remove them
                return stringValueFromSlice(token.lexeme[1 .. token.lexeme.len - 1], alctr);
            },
            .identifier => return .{ .nil = {} }, // TODO

            else => unreachable,
        };
    }

    fn isTruthy(v: Value) bool {
        // false and nil are falsey, everything else is truthy
        return switch (v) {
            .nil => false,
            .booln => |b| b,
            else => true,
        };
    }
};

fn testInterpreter(
    comptime text: []const u8,
    result: Value,
) !void {
    const alctr = std.testing.allocator;

    var lexer = lex.Lexer.init(text, alctr);
    defer lexer.deinit();

    const tokens = try lexer.scanTokens();
    defer alctr.free(tokens);

    var parser = prs.Parser.init(tokens, alctr);
    defer parser.deinit();

    const expr = try parser.parse();

    var interpreter = Interpreter.init();
    defer interpreter.deinit();

    const interpreted_result = parser.pool.fromIndex(expr.index).acceptVisitor(parser.pool, alctr, interpreter);
    defer interpreted_result.deinit();

    switch (result) {
        .string => |s| try std.testing.expectEqualStrings(s.items, interpreted_result.string.items),
        else => {
            try std.testing.expectEqual(result, interpreted_result);
        },
    }
}

test "interpret: parse literals" {
    try testInterpreter("true", .{ .booln = true });
    try testInterpreter("false", .{ .booln = false });
    try testInterpreter("1", .{ .number = 1.0 });
    try testInterpreter("1.1", .{ .number = 1.1 });
    try testInterpreter("3.1415", .{ .number = 3.1415 });

    {
        const alctr = std.testing.allocator;
        // note the removed quotes
        const result = Interpreter.stringValueFromSlice("fee fie foh fum", alctr);
        defer result.deinit();

        try testInterpreter("\"fee fie foh fum\"", result);
    }
}

test "interpret: simple literal groupings" {
    try testInterpreter("(true)", .{ .booln = true });
    try testInterpreter("(((((true)))))", .{ .booln = true });
    try testInterpreter("(false)", .{ .booln = false });
    try testInterpreter("(((((false)))))", .{ .booln = false });
    try testInterpreter("(1)", .{ .number = 1.0 });
    try testInterpreter("(((((1)))))", .{ .number = 1.0 });
    try testInterpreter("(1.1)", .{ .number = 1.1 });
    try testInterpreter("(((((1.1)))))", .{ .number = 1.1 });
    try testInterpreter("(3.1415)", .{ .number = 3.1415 });
    try testInterpreter("(((((3.1415)))))", .{ .number = 3.1415 });

    {
        const alctr = std.testing.allocator;
        // note the removed quotes
        const result = Interpreter.stringValueFromSlice("fee fie foh fum", alctr);
        defer result.deinit();

        try testInterpreter("(\"fee fie foh fum\")", result);
        try testInterpreter("(((((\"fee fie foh fum\")))))", result);
    }
}

test "interpret: unary operations" {
    try testInterpreter("!true", .{ .booln = false });
    try testInterpreter("!false", .{ .booln = true });
    try testInterpreter("!!true", .{ .booln = true });
    try testInterpreter("!!false", .{ .booln = false });

    try testInterpreter("-1", .{ .number = -1.0 });
    try testInterpreter("-1.1", .{ .number = -1.1 });
    try testInterpreter("-3.1415", .{ .number = -3.1415 });
}

test "interpret: binary operations" {
    try testInterpreter("1 + 1", .{ .number = 2 });
    try testInterpreter("1 - 1", .{ .number = 0 });
    try testInterpreter("2 * 2", .{ .number = 4 });
    try testInterpreter("2 / 2", .{ .number = 1 });

    try testInterpreter("1 > 0", .{ .booln = true });
    try testInterpreter("1 >= 0", .{ .booln = true });
    try testInterpreter("1 >= 1", .{ .booln = true });
    try testInterpreter("1 < 2", .{ .booln = true });
    try testInterpreter("1 <= 2", .{ .booln = true });
    try testInterpreter("1 <= 1", .{ .booln = true });
    try testInterpreter("1 == 1", .{ .booln = true });
    try testInterpreter("1 != 2", .{ .booln = true });

    {
        const alctr = std.testing.allocator;
        const result = Interpreter.stringValueFromSlice("hello_world", alctr);
        defer result.deinit();

        try testInterpreter("\"hello_\" + \"world\"", result);
    }
}

const std = @import("std");
const log = std.log.scoped(.interpret);

const ux = @import("ux.zig");
const prs = @import("parse.zig");
const lex = @import("lex.zig");
