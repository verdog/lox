const Expr = union(enum) {
    binary: Binary,
    unary: Unary,
    grouping: Grouping,
    literal: Literal,

    pub fn acceptVisitor(expr: Expr, pool: ExprPool, alctr: std.mem.Allocator, visitor: anytype) VisitorResult(@TypeOf(visitor)) {
        return visitor.visit(expr, pool, alctr);
    }

    fn VisitorResult(comptime visitor: type) type {
        return @typeInfo(@TypeOf(@field(visitor, "visit"))).Fn.return_type.?;
    }
};

const ExprPool = struct {
    const Index = u16;
    const Handle = struct {
        index: Index,
        ptr: *Expr,
    };

    alctr: std.mem.Allocator,
    buf: std.ArrayList(Expr),

    pub fn init(alctr: std.mem.Allocator) ExprPool {
        return .{
            .alctr = alctr,
            .buf = std.ArrayList(Expr).init(alctr),
        };
    }

    pub fn deinit(self: ExprPool) void {
        self.buf.deinit();
    }

    pub fn addLiteral(pool: *ExprPool, typ: lex.TokenType, lexeme: []const u8, line: i32) Handle {
        pool.buf.append(undefined) catch @panic("OOM");
        pool.buf.items[pool.buf.items.len - 1] = .{ .literal = .{ .value = .{
            .typ = typ,
            .lexeme = lexeme,
            .line = line,
        } } };
        return pool.lastHandle();
    }

    pub fn addGrouping(pool: *ExprPool, expression: Index) Handle {
        pool.buf.append(undefined) catch @panic("OOM");
        pool.buf.items[pool.buf.items.len - 1] = .{ .grouping = .{
            .expression = expression,
        } };
        return pool.lastHandle();
    }

    pub fn addBinary(pool: *ExprPool, operator: lex.Token, left: Index, right: Index) Handle {
        pool.buf.append(undefined) catch @panic("OOM");
        pool.buf.items[pool.buf.items.len - 1] = .{ .binary = .{
            .left = left,
            .operator = operator,
            .right = right,
        } };
        return pool.lastHandle();
    }

    pub fn addUnary(pool: *ExprPool, operator: lex.Token, right: Index) Handle {
        pool.buf.append(undefined) catch @panic("OOM");
        pool.buf.items[pool.buf.items.len - 1] = .{ .unary = .{
            .operator = operator,
            .right = right,
        } };
        return pool.lastHandle();
    }

    fn lastHandle(pool: ExprPool) Handle {
        return .{
            .index = @intCast(Index, pool.buf.items.len - 1),
            .ptr = &pool.buf.items[pool.buf.items.len - 1],
        };
    }
};

const Binary = struct {
    left: ExprPool.Index,
    operator: lex.Token,
    right: ExprPool.Index,
};

const Unary = struct {
    operator: lex.Token,
    right: ExprPool.Index,
};

const Grouping = struct {
    expression: ExprPool.Index,
};

const Literal = struct {
    value: lex.Token,
};

pub fn printAst(expr: Expr, pool: ExprPool, alctr: std.mem.Allocator) []u8 {
    const visitor = struct {
        pub fn visit(self: @This(), exp: Expr, pl: ExprPool, alct: std.mem.Allocator) []u8 {
            switch (exp) {
                .binary => |bin| {
                    const left = pl.buf.items[bin.left].acceptVisitor(pl, alct, self);
                    defer alct.free(left);
                    const right = pl.buf.items[bin.right].acceptVisitor(pl, alct, self);
                    defer alct.free(right);
                    return std.fmt.allocPrint(alct, "({s} {s} {s})", .{ bin.operator.lexeme, left, right }) catch @panic("OOM");
                },
                .unary => |un| {
                    const right = pl.buf.items[un.right].acceptVisitor(pl, alct, self);
                    defer alct.free(right);
                    return std.fmt.allocPrint(alct, "({s} {s})", .{ un.operator.lexeme, right }) catch @panic("OOM");
                },
                .grouping => |grp| {
                    const expression = pl.buf.items[grp.expression].acceptVisitor(pl, alct, self);
                    defer alct.free(expression);
                    return std.fmt.allocPrint(alct, "(group {s})", .{expression}) catch @panic("OOM");
                },
                .literal => |lit| {
                    return std.fmt.allocPrint(alct, "{s}", .{lit.value.lexeme}) catch @panic("OOM");
                },
            }
        }
    }{};

    return expr.acceptVisitor(pool, alctr, visitor);
}

pub fn countNodesInTree(expr: Expr, pool: ExprPool) usize {
    const visitor = struct {
        pub fn visit(self: @This(), exp: Expr, pl: ExprPool, alct: std.mem.Allocator) usize {
            switch (exp) {
                .binary => |bin| {
                    const left = pl.buf.items[bin.left].acceptVisitor(pl, alct, self);
                    const right = pl.buf.items[bin.right].acceptVisitor(pl, alct, self);
                    return 1 + left + right;
                },
                .unary => |un| {
                    return 1 + pl.buf.items[un.right].acceptVisitor(pl, alct, self);
                },
                .grouping => |grp| {
                    return 1 + pl.buf.items[grp.expression].acceptVisitor(pl, alct, self);
                },
                .literal => {
                    return 1;
                },
            }
        }
    }{};

    return expr.acceptVisitor(pool, undefined, visitor);
}

test "printAst" {
    const alctr = std.testing.allocator;
    var pool = ExprPool.init(alctr);
    defer pool.deinit();

    const b = pool.addBinary(
        .{ .typ = .star, .lexeme = "*", .line = 0 },
        pool.addUnary(
            .{ .typ = .minus, .lexeme = "-", .line = 0 },
            pool.addLiteral(.number, "123", 0).index,
        ).index,
        pool.addGrouping(
            pool.addLiteral(.number, "45.67", 0).index,
        ).index,
    );

    const string = printAst(b.ptr.*, pool, alctr);
    defer alctr.free(string);

    try std.testing.expectEqualStrings("(* (- 123) (group 45.67))", string);
}

test "countNodesInTree" {
    const alctr = std.testing.allocator;
    var pool = ExprPool.init(alctr);
    defer pool.deinit();

    const b = pool.addBinary(
        .{ .typ = .star, .lexeme = "*", .line = 0 },
        pool.addUnary(
            .{ .typ = .minus, .lexeme = "-", .line = 0 },
            pool.addLiteral(.number, "123", 0).index,
        ).index,
        pool.addGrouping(
            pool.addLiteral(.number, "45.67", 0).index,
        ).index,
    );

    try std.testing.expectEqual(@as(usize, 5), countNodesInTree(b.ptr.*, pool));
}

const std = @import("std");
const lex = @import("lex.zig");
