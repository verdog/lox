const Expr = union(enum) {
    binary: Binary,
    unary: Unary,
    grouping: Grouping,
    literal: Literal,

    pub fn acceptVisitor(expr: Expr, pool: ExprPool, alctr: std.mem.Allocator, visitor: anytype) VisitorResult(@TypeOf(visitor)) {
        return visitor.visit(expr, pool, alctr);
    }

    fn VisitorResult(comptime visitor: type) type {
        // uff da!
        // get the return type of the visit function in the visitor type.
        return @typeInfo(@TypeOf(@field(visitor, "visit"))).Fn.return_type.?;
    }
};

const ExprPool = struct {
    const Index = u16;
    const Handle = struct {
        index: Index,
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

const Parser = struct {
    tokens: []lex.Token,
    current_token: usize,
    pool: ExprPool,
    alctr: std.mem.Allocator,

    pub fn init(tokens: []lex.Token, alctr: std.mem.Allocator) Parser {
        return .{
            .tokens = tokens,
            .current_token = 0,
            .pool = ExprPool.init(alctr),
            .alctr = alctr,
        };
    }

    pub fn deinit(p: *Parser) void {
        p.pool.deinit();
    }

    // The grammar is as follows:
    //
    // low precedence
    //
    // expression -> equality ;
    // equality   -> comparison ( ( "!=" | "==" ) comparison )* ;
    // comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    // term       -> factor ( ( "-" | "+" ) factor )* ;
    // factor     -> unary ( ( "/" | "*" ) unary )* ;
    // unary      -> ( "!" | "-" ) unary
    //            | primary ;
    // primary    -> NUMBER | STRING | "true" | "false" | "nil"
    //            | "(" expression ")" ;
    //
    // high precedence
    //
    // Everything is left associative except unary, which is right associative.

    fn expression(p: *Parser) ExprPool.Handle {
        return p.equality();
    }

    fn equality(p: *Parser) ExprPool.Handle {
        var expr = p.comparison();
        while (p.match(.bang_eql) or p.match(.eql_eql)) {
            const operator = p.previous();
            const right = p.comparison();
            expr = p.pool.addBinary(operator, expr.index, right.index);
        }
        return expr;
    }

    fn comparison(p: *Parser) ExprPool.Handle {
        var expr = p.term();
        while (p.match(.greater) or p.match(.greater_eql) or p.match(.less) or p.match(.less_eql)) {
            const operator = p.previous();
            const right = p.term();
            expr = p.pool.addBinary(operator, expr.index, right.index);
        }
        return expr;
    }

    fn term(p: *Parser) ExprPool.Handle {
        var expr = p.factor();
        while (p.match(.minus) or p.match(.plus)) {
            const operator = p.previous();
            const right = p.factor();
            expr = p.pool.addBinary(operator, expr.index, right.index);
        }
        return expr;
    }

    fn factor(p: *Parser) ExprPool.Handle {
        var expr = p.unary();
        while (p.match(.slash) or p.match(.star)) {
            const operator = p.previous();
            const right = p.unary();
            expr = p.pool.addBinary(operator, expr.index, right.index);
        }
        return expr;
    }

    fn unary(p: *Parser) ExprPool.Handle {
        if (p.match(.bang) or p.match(.minus)) {
            const operator = p.previous();
            const right = p.unary();
            return p.pool.addUnary(operator, right.index);
        } else {
            return p.primary();
        }
    }

    fn primary(p: *Parser) ExprPool.Handle {
        if (p.match(.true)) return p.pool.addLiteral(.true, "true", 0);
        if (p.match(.false)) return p.pool.addLiteral(.false, "false", 0);
        if (p.match(.nil)) return p.pool.addLiteral(.nil, "nil", 0);

        if (p.match(.number) or p.match(.string))
            return p.pool.addLiteral(.number, p.previous().lexeme, 0);

        if (p.match(.lparen)) {
            const expr = p.expression();
            _ = p.consume(.rparen, "Expected ')' after expression.");
            return p.pool.addGrouping(expr.index);
        }

        // throw error?
        unreachable;
    }

    // parsing operations

    fn match(p: *Parser, typ: lex.TokenType) bool {
        if (p.check(typ)) {
            _ = p.advance();
            return true;
        }
        return false;
    }

    fn check(p: Parser, typ: lex.TokenType) bool {
        if (p.isAtEnd()) return false;
        return p.peek().typ == typ;
    }

    fn advance(p: *Parser) lex.Token {
        if (!p.isAtEnd()) p.current_token += 1;
        return p.previous();
    }

    fn isAtEnd(p: Parser) bool {
        return p.peek().typ == .eof;
    }

    fn peek(p: Parser) lex.Token {
        return p.tokens[p.current_token];
    }

    fn previous(p: Parser) lex.Token {
        return p.tokens[p.current_token - 1];
    }

    fn consume(p: *Parser, typ: lex.TokenType, message: []const u8) lex.Token {
        _ = message;
        if (p.check(typ)) return p.advance();
        // error
        unreachable;
    }
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

    const string = printAst(pool.buf.items[b.index], pool, alctr);
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

    try std.testing.expectEqual(@as(usize, 5), countNodesInTree(pool.buf.items[b.index], pool));
}

test "parse" {
    const alctr = std.testing.allocator;
    const text =
        \\23 <= 24
    ;

    var lexer = lex.Lexer.init(text, alctr);
    defer lexer.deinit();

    const result = try lexer.scanTokens();
    try std.testing.expectEqual(false, result.had_error);

    var parser = Parser.init(lexer.tokens.items, alctr);
    defer parser.deinit();

    const expr = parser.expression();

    const string = printAst(parser.pool.buf.items[expr.index], parser.pool, alctr);
    defer alctr.free(string);

    try std.testing.expectEqualStrings("(<= 23 24)", string);
}

const std = @import("std");
const lex = @import("lex.zig");
