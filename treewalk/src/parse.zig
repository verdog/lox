// TODO examine and polish the api for all this stuff

const Expr = union(enum) {
    binary: Binary,
    unary: Unary,
    grouping: Grouping,
    literal: Literal,

    pub fn accept(expr: Expr, visitor: anytype, pool: ExprPool) []u8 {
        return visitor.visit(expr, pool);
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

    pub fn addBinary(pool: *ExprPool, left: Index, operator: lex.Token, right: Index) Handle {
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

pub fn printAst(expr: Expr, pool: ExprPool) []u8 {
    const visitor = struct {
        pub fn visit(self: @This(), exp: Expr, pl: ExprPool) []u8 {
            switch (exp) {
                .binary => |bin| {
                    const left = pl.buf.items[bin.left].accept(self, pl);
                    defer pl.alctr.free(left);
                    const right = pl.buf.items[bin.right].accept(self, pl);
                    defer pl.alctr.free(right);
                    return std.fmt.allocPrint(pl.alctr, "({s} {s} {s})", .{ bin.operator.lexeme, left, right }) catch @panic("OOM");
                },
                .unary => |un| {
                    const right = pl.buf.items[un.right].accept(self, pl);
                    defer pl.alctr.free(right);
                    return std.fmt.allocPrint(pl.alctr, "({s} {s})", .{ un.operator.lexeme, right }) catch @panic("OOM");
                },
                .grouping => |grp| {
                    const expression = pl.buf.items[grp.expression].accept(self, pl);
                    defer pl.alctr.free(expression);
                    return std.fmt.allocPrint(pl.alctr, "(group {s})", .{expression}) catch @panic("OOM");
                },
                .literal => |lit| {
                    return std.fmt.allocPrint(pl.alctr, "{s}", .{lit.value.lexeme}) catch @panic("OOM");
                },
            }
        }
    }{};

    return expr.accept(visitor, pool);
}

test "printAst" {
    const alctr = std.testing.allocator;
    var pool = ExprPool.init(alctr);
    defer pool.deinit();

    const f = pool.addLiteral(.number, "45.67", 0);
    const g = pool.addGrouping(f.index);
    const f2 = pool.addLiteral(.number, "123", 0);
    const u = pool.addUnary(.{ .typ = .minus, .lexeme = "-", .line = 0 }, f2.index);
    const b = pool.addBinary(u.index, .{ .typ = .star, .lexeme = "*", .line = 0 }, g.index);

    const string = printAst(b.ptr.*, pool);
    defer alctr.free(string);

    std.debug.print("{s}\n", .{string});
}

const std = @import("std");
const lex = @import("lex.zig");
