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

    pub fn toString(v: Value, alctr: std.mem.Allocator) []u8 {
        switch (v) {
            .nil => return alctr.dupe(u8, "(nil)") catch @panic("OOM"),
            .booln => |b| return if (b)
                alctr.dupe(u8, "true") catch @panic("OOM")
            else
                alctr.dupe(u8, "false") catch @panic("OOM"),
            .number => |n| return std.fmt.allocPrint(alctr, "{d:.4}", .{n}) catch @panic("OOM"),
            .string => |s| return std.fmt.allocPrint(alctr, "\"{s}\"", .{s.items}) catch @panic("OOM"),
        }
    }
};

pub const Interpreter = struct {
    const Error = error{
        InvalidOperand,
    };

    pub fn init() Interpreter {
        return .{};
    }

    pub fn deinit(intr: *Interpreter) void {
        _ = intr;
    }

    /// implements visitor interface required by parse.Expr.acceptVisitor
    pub fn visit(intr: Interpreter, node: anytype, pl: prs.Pool, alctr: std.mem.Allocator) Error!Value {
        switch (@TypeOf(node)) {
            prs.Expr => return try intr.visitExpr(node, pl, alctr),
            prs.Stmt => return try intr.visitStmt(node, pl, alctr),
            else => @compileError("oops"),
        }
    }

    fn visitExpr(intr: Interpreter, expr: prs.Expr, pl: prs.Pool, alctr: std.mem.Allocator) Error!Value {
        switch (expr) {
            .binary => |b| {
                var left = try pl.getExpr(b.left).acceptVisitor(pl, alctr, intr);
                defer left.deinit();
                var right = try pl.getExpr(b.right).acceptVisitor(pl, alctr, intr);
                defer right.deinit();

                switch (b.operator.typ) {
                    .plus => {
                        switch (left) {
                            .number => {
                                if (std.meta.activeTag(right) != .number)
                                    return Error.InvalidOperand;
                                return .{ .number = left.number + right.number };
                            },
                            .string => {
                                if (std.meta.activeTag(right) != .string)
                                    return Error.InvalidOperand;
                                var result: Value = .{ .string = left.string.clone() catch @panic("OOM") };
                                result.string.appendSlice(right.string.items) catch @panic("OOM");
                                return result;
                            },
                            else => {
                                return Error.InvalidOperand;
                            },
                        }
                    },
                    .minus => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .number = left.number - right.number };
                    },
                    .star => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .number = left.number * right.number };
                    },
                    .slash => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .number = left.number / right.number };
                    },
                    .greater => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .booln = left.number > right.number };
                    },
                    .greater_eql => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .booln = left.number >= right.number };
                    },
                    .less => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .booln = left.number < right.number };
                    },
                    .less_eql => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .booln = left.number <= right.number };
                    },
                    .eql_eql => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .booln = std.meta.eql(left, right) };
                    },
                    .bang_eql => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .booln = !std.meta.eql(left, right) };
                    },

                    else => unreachable,
                }
            },
            .unary => |u| {
                var value = try pl.getExpr(u.right).acceptVisitor(pl, alctr, intr);

                switch (u.operator.typ) {
                    .minus => {
                        try assertNumberOperand(u.operator.typ, value);
                        return .{ .number = -value.number };
                    },
                    .bang => {
                        return .{ .booln = !isTruthy(value) };
                    },

                    else => unreachable,
                }
            },
            .grouping => |g| return try pl.getExpr(g.expression).acceptVisitor(pl, alctr, intr),
            .literal => |l| return try interpretTokenValue(l.value, alctr),
        }
    }

    fn visitStmt(intr: Interpreter, stmt: prs.Stmt, pl: prs.Pool, alctr: std.mem.Allocator) Error!Value {
        switch (stmt) {
            .print => |print| {
                const value = try intr.visitExpr(pl.getExpr(print.expr), pl, alctr);
                const string = value.toString(alctr);
                defer alctr.free(string);
                ux.out.print("{s}\n", .{string}) catch @panic("stdout failure");
                return .{ .nil = {} };
            },
            .expr => |exprstmt| {
                return try intr.visitExpr(pl.getExpr(exprstmt.expr), pl, alctr);
            },
        }
    }

    fn stringValueFromSlice(string: []const u8, alctr: std.mem.Allocator) Value {
        const string_memory = alctr.dupe(u8, string) catch @panic("OOM");
        return .{ .string = std.ArrayList(u8).fromOwnedSlice(alctr, string_memory) };
    }

    fn interpretTokenValue(token: lex.Token, alctr: std.mem.Allocator) Error!Value {
        return switch (token.typ) {
            .number => .{ .number = std.fmt.parseFloat(f64, token.lexeme) catch return Error.InvalidOperand },
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

    fn assertNumberOperand(operator: lex.TokenType, value: Value) !void {
        _ = operator;
        if (std.meta.activeTag(value) == .number) return;
        return Error.InvalidOperand;
    }

    fn assertNumberOperands(operator: lex.TokenType, left: Value, right: Value) !void {
        _ = operator;
        if (std.meta.activeTag(left) == .number and std.meta.activeTag(right) == .number) return;
        // TODO be more detailed
        return Error.InvalidOperand;
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

    const stmts = try parser.parse();
    try std.testing.expectEqual(stmts.len, 1);
    defer alctr.free(stmts);

    var interpreter = Interpreter.init();
    defer interpreter.deinit();

    for (stmts) |stmt| {
        const interpreted_result = try parser.pool.getStmt(stmt).acceptVisitor(parser.pool, alctr, interpreter);
        defer interpreted_result.deinit();

        switch (result) {
            .string => |s| try std.testing.expectEqualStrings(s.items, interpreted_result.string.items),
            else => {
                try std.testing.expectEqual(result, interpreted_result);
            },
        }
    }
}

test "interpret: parse literals" {
    try testInterpreter("true;", .{ .booln = true });
    try testInterpreter("false;", .{ .booln = false });
    try testInterpreter("1;", .{ .number = 1.0 });
    try testInterpreter("1.1;", .{ .number = 1.1 });
    try testInterpreter("3.1415;", .{ .number = 3.1415 });

    {
        const alctr = std.testing.allocator;
        // note the removed quotes
        const result = Interpreter.stringValueFromSlice("fee fie foh fum", alctr);
        defer result.deinit();

        try testInterpreter("\"fee fie foh fum\";", result);
    }
}

test "interpret: simple literal groupings" {
    try testInterpreter("(true);", .{ .booln = true });
    try testInterpreter("(((((true)))));", .{ .booln = true });
    try testInterpreter("(false);", .{ .booln = false });
    try testInterpreter("(((((false)))));", .{ .booln = false });
    try testInterpreter("(1);", .{ .number = 1.0 });
    try testInterpreter("(((((1)))));", .{ .number = 1.0 });
    try testInterpreter("(1.1);", .{ .number = 1.1 });
    try testInterpreter("(((((1.1)))));", .{ .number = 1.1 });
    try testInterpreter("(3.1415);", .{ .number = 3.1415 });
    try testInterpreter("(((((3.1415)))));", .{ .number = 3.1415 });

    {
        const alctr = std.testing.allocator;
        // note the removed quotes
        const result = Interpreter.stringValueFromSlice("fee fie foh fum", alctr);
        defer result.deinit();

        try testInterpreter("(\"fee fie foh fum\");", result);
        try testInterpreter("(((((\"fee fie foh fum\")))));", result);
    }
}

test "interpret: unary operations" {
    try testInterpreter("!true;", .{ .booln = false });
    try testInterpreter("!false;", .{ .booln = true });
    try testInterpreter("!!true;", .{ .booln = true });
    try testInterpreter("!!false;", .{ .booln = false });

    try testInterpreter("-1;", .{ .number = -1.0 });
    try testInterpreter("-1.1;", .{ .number = -1.1 });
    try testInterpreter("-3.1415;", .{ .number = -3.1415 });
}

test "interpret: binary operations" {
    try testInterpreter("1 + 1;", .{ .number = 2 });
    try testInterpreter("1 - 1;", .{ .number = 0 });
    try testInterpreter("2 * 2;", .{ .number = 4 });
    try testInterpreter("2 / 2;", .{ .number = 1 });

    try testInterpreter("1 > 0;", .{ .booln = true });
    try testInterpreter("1 >= 0;", .{ .booln = true });
    try testInterpreter("1 >= 1;", .{ .booln = true });
    try testInterpreter("1 < 2;", .{ .booln = true });
    try testInterpreter("1 <= 2;", .{ .booln = true });
    try testInterpreter("1 <= 1;", .{ .booln = true });
    try testInterpreter("1 == 1;", .{ .booln = true });
    try testInterpreter("1 != 2;", .{ .booln = true });

    {
        const alctr = std.testing.allocator;
        const result = Interpreter.stringValueFromSlice("hello_world", alctr);
        defer result.deinit();

        try testInterpreter("\"hello_\" + \"world\";", result);
    }
}

const std = @import("std");
const log = std.log.scoped(.interpret);

const ux = @import("ux.zig");
const prs = @import("parse.zig");
const lex = @import("lex.zig");
