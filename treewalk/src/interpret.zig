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
            .binary => |_| {},
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

const std = @import("std");
const log = std.log.scoped(.interpret);

const ux = @import("ux.zig");
const prs = @import("parse.zig");
const lex = @import("lex.zig");
