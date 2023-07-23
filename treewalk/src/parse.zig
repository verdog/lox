pub const Expr = union(enum) {
    binary: BinaryExpr,
    unary: UnaryExpr,
    grouping: GroupingExpr,
    literal: LiteralExpr,
    variable: VariableExpr,
    assign: AssignExpr,

    pub fn acceptVisitor(expr: Expr, pool: Pool, ctx: anytype, visitor: anytype) VisitorResult(@TypeOf(visitor.*)) {
        return visitor.visit(expr, pool, ctx);
    }

    fn VisitorResult(comptime visitor: type) type {
        // uff da!
        // get the return type of the visit function in the visitor type.
        return @typeInfo(@TypeOf(@field(visitor, "visit"))).Fn.return_type.?;
    }
};

const BinaryExpr = struct {
    left: Pool.ExprIndex,
    operator: lex.Token,
    right: Pool.ExprIndex,
};

const UnaryExpr = struct {
    operator: lex.Token,
    right: Pool.ExprIndex,
};

const GroupingExpr = struct {
    expression: Pool.ExprIndex,
};

const LiteralExpr = struct {
    value: lex.Token,
};

const VariableExpr = struct {
    name: lex.Token,
};

const AssignExpr = struct {
    name: lex.Token,
    right: Pool.ExprIndex,
};

pub const Stmt = union(enum) {
    expr: ExprStmt,
    print: PrintStmt,
    vari: VarStmt,

    pub fn acceptVisitor(stmt: Stmt, pool: Pool, ctx: anytype, visitor: anytype) VisitorResult(@TypeOf(visitor.*)) {
        return visitor.visit(stmt, pool, ctx);
    }

    fn VisitorResult(comptime visitor: type) type {
        return @typeInfo(@TypeOf(@field(visitor, "visit"))).Fn.return_type.?;
    }
};

const ExprStmt = struct {
    expr: Pool.ExprIndex,
};

const PrintStmt = struct {
    expr: Pool.ExprIndex,
};

const VarStmt = struct {
    name: lex.Token,
    initializer: ?Pool.ExprIndex,
};

pub const Pool = struct {
    const ExprIndex = u16;
    const StmtIndex = u16;

    const Handle = union(enum) {
        expr_index: ExprIndex,
        stmt_index: StmtIndex,
    };

    alctr: std.mem.Allocator,
    exprs: std.ArrayList(Expr),
    stmts: std.ArrayList(Stmt),

    pub fn init(alctr: std.mem.Allocator) Pool {
        return .{
            .alctr = alctr,
            .exprs = std.ArrayList(Expr).init(alctr),
            .stmts = std.ArrayList(Stmt).init(alctr),
        };
    }

    pub fn deinit(self: Pool) void {
        self.exprs.deinit();
        self.stmts.deinit();
    }

    pub fn addPrintStmt(pool: *Pool, expr: ExprIndex) Handle {
        pool.stmts.append(undefined) catch @panic("OOM");
        pool.stmts.items[pool.stmts.items.len - 1] = .{ .print = .{ .expr = expr } };
        return pool.lastStmt();
    }

    pub fn addExprStmt(pool: *Pool, expr: ExprIndex) Handle {
        pool.stmts.append(undefined) catch @panic("OOM");
        pool.stmts.items[pool.stmts.items.len - 1] = .{ .expr = .{ .expr = expr } };
        return pool.lastStmt();
    }

    pub fn addVarStmt(pool: *Pool, name: lex.Token, initializer: ?ExprIndex) Handle {
        pool.stmts.append(undefined) catch @panic("OOM");
        pool.stmts.items[pool.stmts.items.len - 1] = .{ .vari = .{ .name = name, .initializer = initializer } };
        return pool.lastStmt();
    }

    pub fn addLiteral(pool: *Pool, token: lex.Token) Handle {
        pool.exprs.append(undefined) catch @panic("OOM");
        pool.exprs.items[pool.exprs.items.len - 1] = .{ .literal = .{ .value = token } };
        return pool.lastExpr();
    }

    pub fn addGrouping(pool: *Pool, expression: ExprIndex) Handle {
        pool.exprs.append(undefined) catch @panic("OOM");
        pool.exprs.items[pool.exprs.items.len - 1] = .{ .grouping = .{
            .expression = expression,
        } };
        return pool.lastExpr();
    }

    pub fn addBinary(pool: *Pool, operator: lex.Token, left: ExprIndex, right: ExprIndex) Handle {
        pool.exprs.append(undefined) catch @panic("OOM");
        pool.exprs.items[pool.exprs.items.len - 1] = .{ .binary = .{
            .left = left,
            .operator = operator,
            .right = right,
        } };
        return pool.lastExpr();
    }

    pub fn addUnary(pool: *Pool, operator: lex.Token, right: ExprIndex) Handle {
        pool.exprs.append(undefined) catch @panic("OOM");
        pool.exprs.items[pool.exprs.items.len - 1] = .{ .unary = .{
            .operator = operator,
            .right = right,
        } };
        return pool.lastExpr();
    }

    pub fn addVariable(pool: *Pool, name: lex.Token) Handle {
        pool.exprs.append(undefined) catch @panic("OOM");
        pool.exprs.items[pool.exprs.items.len - 1] = .{ .variable = .{
            .name = name,
        } };
        return pool.lastExpr();
    }

    pub fn addAssign(pool: *Pool, name: lex.Token, right: ExprIndex) Handle {
        pool.exprs.append(undefined) catch @panic("OOM");
        pool.exprs.items[pool.exprs.items.len - 1] = .{ .assign = .{
            .name = name,
            .right = right,
        } };
        return pool.lastExpr();
    }

    fn lastExpr(pool: Pool) Handle {
        return .{
            .expr_index = @intCast(pool.exprs.items.len - 1),
        };
    }

    fn lastStmt(pool: Pool) Handle {
        return .{
            .stmt_index = @intCast(pool.stmts.items.len - 1),
        };
    }

    // TODO name this something better
    pub fn getExpr(pool: Pool, index: ExprIndex) Expr {
        std.debug.assert(index < pool.exprs.items.len);
        return pool.exprs.items[index];
    }

    pub fn getStmt(pool: Pool, index: StmtIndex) Stmt {
        std.debug.assert(index < pool.stmts.items.len);
        return pool.stmts.items[index];
    }
};

pub const Parser = struct {
    tokens: []lex.Token,
    current_token: usize,
    pool: Pool,
    alctr: std.mem.Allocator,

    last_error: ?Error = null,
    const Error = error{
        UnexpectedToken,
        InvalidAssignment,
        Recoverable,
    };

    pub fn init(tokens: []lex.Token, alctr: std.mem.Allocator) Parser {
        return .{
            .tokens = tokens,
            .current_token = 0,
            .pool = Pool.init(alctr),
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
    // program     -> declaration* EOF ;
    // declaration -> var_decl
    //             |  statement ;
    //
    // var_decl    -> "var" IDENTIFIER ( "=" expression )? ";" ;
    //
    // statement   -> expr_stmt
    //             |  print_stmt ;
    // expr_stmt   -> expression ";" ;
    // print_stmt  -> "print" expression ";" ;
    //
    // expression  -> assignment ;
    // assignment  -> IDENTIFIER "=" assignment
    //             |  equality ;
    // equality    -> comparison ( ( "!=" | "==" ) comparison )* ;
    // comparison  -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    // term        -> factor ( ( "-" | "+" ) factor )* ;
    // factor      -> unary ( ( "/" | "*" ) unary )* ;
    // unary       -> ( "!" | "   -" ) unary
    //             | primary ;
    // primary     -> NUMBER | STRING | "true" | "false" | "nil"
    //             | "(" expression ")" | IDENTIFIER ;
    //
    // high precedence
    //
    // Everything is left associative except unary and assignment, which are right associative.

    pub fn parse(p: *Parser) Error![]Pool.StmtIndex {
        var statements = std.ArrayList(Pool.StmtIndex).init(p.alctr);
        errdefer statements.deinit();

        while (!p.isAtEnd()) {
            const handle = p.declaration() catch |e| switch (e) {
                Error.Recoverable => {
                    // TODO
                    log.debug("Caught recoverable error, continuing...", .{});
                    continue;
                },
                else => return e,
            };
            statements.append(handle.stmt_index) catch @panic("OOM");
        }

        return statements.toOwnedSlice() catch @panic("OOM");
    }

    fn declaration(p: *Parser) Error!Pool.Handle {
        const result = blk: {
            if (p.match(.@"var")) break :blk p.var_decl();
            break :blk p.statement();
        } catch {
            p.synchronize();
            return Error.Recoverable;
        };

        return result;
    }

    fn var_decl(p: *Parser) Error!Pool.Handle {
        const name = try p.consume(.identifier, "Expected variable name");

        const initr_handle = if (p.match(.eql))
            try p.expression()
        else
            null;

        _ = try p.consume(.semicolon, "Expected ';' after variable declaration");
        return p.pool.addVarStmt(name, if (initr_handle) |h| h.expr_index else null);
    }

    fn statement(p: *Parser) Error!Pool.Handle {
        if (p.match(.print)) return try p.print_statement();
        return try p.expression_statement();
    }

    fn print_statement(p: *Parser) Error!Pool.Handle {
        var expr = try p.expression();
        _ = try p.consume(.semicolon, "Expected ';' after print statement");
        return p.pool.addPrintStmt(expr.expr_index);
    }

    fn expression_statement(p: *Parser) Error!Pool.Handle {
        var expr = try p.expression();
        _ = try p.consume(.semicolon, "Expected ';' after expression");
        return p.pool.addExprStmt(expr.expr_index);
    }

    fn expression(p: *Parser) Error!Pool.Handle {
        return try p.assignment();
    }

    fn assignment(p: *Parser) Error!Pool.Handle {
        const expr = try p.equality();

        if (p.match(.eql)) {
            const value = try p.assignment();

            if (std.meta.activeTag(expr) != .expr_index or
                std.meta.activeTag(p.pool.getExpr(expr.expr_index)) != .variable)
            {
                return Error.InvalidAssignment;
            }

            return p.pool.addAssign(p.pool.getExpr(expr.expr_index).variable.name, value.expr_index);
        }

        return expr;
    }

    fn equality(p: *Parser) Error!Pool.Handle {
        var expr = try p.comparison();
        while (p.match(.bang_eql) or p.match(.eql_eql)) {
            const operator = p.previous();
            const right = try p.comparison();
            expr = p.pool.addBinary(operator, expr.expr_index, right.expr_index);
        }
        return expr;
    }

    fn comparison(p: *Parser) Error!Pool.Handle {
        var expr = try p.term();
        while (p.match(.greater) or p.match(.greater_eql) or p.match(.less) or p.match(.less_eql)) {
            const operator = p.previous();
            const right = try p.term();
            expr = p.pool.addBinary(operator, expr.expr_index, right.expr_index);
        }
        return expr;
    }

    fn term(p: *Parser) Error!Pool.Handle {
        var expr = try p.factor();
        while (p.match(.minus) or p.match(.plus)) {
            const operator = p.previous();
            const right = try p.factor();
            expr = p.pool.addBinary(operator, expr.expr_index, right.expr_index);
        }
        return expr;
    }

    fn factor(p: *Parser) Error!Pool.Handle {
        var expr = try p.unary();
        while (p.match(.slash) or p.match(.star)) {
            const operator = p.previous();
            const right = try p.unary();
            expr = p.pool.addBinary(operator, expr.expr_index, right.expr_index);
        }
        return expr;
    }

    fn unary(p: *Parser) Error!Pool.Handle {
        if (p.match(.bang) or p.match(.minus)) {
            const operator = p.previous();
            const right = try p.unary();
            return p.pool.addUnary(operator, right.expr_index);
        } else {
            return try p.primary();
        }
    }

    fn primary(p: *Parser) Error!Pool.Handle {
        if (p.match(.true) or
            p.match(.false) or
            p.match(.nil) or
            p.match(.number) or
            p.match(.string))
            return p.pool.addLiteral(p.previous());

        if (p.match(.lparen)) {
            const expr = try p.expression();
            _ = try p.consume(.rparen, "Expected ')' after expression");
            return p.pool.addGrouping(expr.expr_index);
        }

        if (p.match(.identifier)) {
            return p.pool.addVariable(p.previous());
        }

        Parser.printUserErrorAtToken(p.peek(), "Expected expression");
        return Error.UnexpectedToken;
    }

    fn synchronize(p: *Parser) void {
        // advance to the start of the next statement
        _ = p.advance();
        while (!p.isAtEnd()) {
            if (p.previous().typ == .semicolon) return;

            switch (p.peek().typ) {
                .class, .@"for", .fun, .@"if", .print, .@"return", .@"var", .@"while" => return,
                else => {}, // try again
            }

            _ = p.advance();
        }
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

    fn consume(p: *Parser, typ: lex.TokenType, comptime message: []const u8) Error!lex.Token {
        if (p.check(typ)) return p.advance();
        Parser.printUserErrorAtToken(p.peek(), message);
        return Error.UnexpectedToken;
    }

    fn printUserErrorAtToken(token: lex.Token, comptime message: []const u8) void {
        switch (token.typ) {
            .eof => ux.printUserError(token.line, "eof", message),
            else => ux.printUserError(token.line, token.lexeme, message),
        }
    }
};

pub const AstPrinter = struct {
    pub fn visit(self: @This(), node: anytype, pl: Pool, ctx: anytype) []u8 {
        return switch (@TypeOf(node)) {
            Expr => self.visitExpr(node, pl, ctx),
            Stmt => self.visitStmt(node, pl, ctx),
            else => @compileError("oops"),
        };
    }

    fn visitExpr(self: @This(), exp: Expr, pl: Pool, ctx: anytype) []u8 {
        switch (exp) {
            .binary => |bin| {
                const left = pl.getExpr(bin.left).acceptVisitor(pl, ctx, &self);
                defer ctx.alctr.free(left);
                const right = pl.getExpr(bin.right).acceptVisitor(pl, ctx, &self);
                defer ctx.alctr.free(right);
                return std.fmt.allocPrint(ctx.alctr, "({s} {s} {s})", .{ bin.operator.lexeme, left, right }) catch @panic("OOM");
            },
            .unary => |un| {
                const right = pl.getExpr(un.right).acceptVisitor(pl, ctx, &self);
                defer ctx.alctr.free(right);
                return std.fmt.allocPrint(ctx.alctr, "({s} {s})", .{ un.operator.lexeme, right }) catch @panic("OOM");
            },
            .grouping => |grp| {
                const expression = pl.getExpr(grp.expression).acceptVisitor(pl, ctx, &self);
                defer ctx.alctr.free(expression);
                return std.fmt.allocPrint(ctx.alctr, "(group {s})", .{expression}) catch @panic("OOM");
            },
            .literal => |lit| {
                return std.fmt.allocPrint(ctx.alctr, "{s}", .{lit.value.lexeme}) catch @panic("OOM");
            },
            .variable => |v| {
                return std.fmt.allocPrint(ctx.alctr, "var:{s}", .{v.name.lexeme}) catch @panic("OOM");
            },
            .assign => |a| {
                const string = pl.getExpr(a.right).acceptVisitor(pl, ctx, &self);
                defer ctx.alctr.free(string);
                return std.fmt.allocPrint(ctx.alctr, "assign:{s}={s}", .{ a.name.lexeme, string }) catch @panic("OOM");
            },
        }
    }

    fn visitStmt(self: @This(), stm: Stmt, pl: Pool, ctx: anytype) []u8 {
        switch (stm) {
            .print => |print| {
                const expr = pl.getExpr(print.expr).acceptVisitor(pl, ctx, &self);
                defer ctx.alctr.free(expr);
                return std.fmt.allocPrint(ctx.alctr, "print: {s}", .{expr}) catch @panic("OOM");
            },
            .expr => |exprstmt| {
                const expr = pl.getExpr(exprstmt.expr).acceptVisitor(pl, ctx, &self);
                defer ctx.alctr.free(expr);
                return std.fmt.allocPrint(ctx.alctr, "expr: {s}", .{expr}) catch @panic("OOM");
            },
            .vari => |vari| {
                const name = vari.name.lexeme;
                const maybe_initr = if (vari.initializer) |i| pl.getExpr(i).acceptVisitor(pl, ctx, &self) else null;
                defer if (maybe_initr) |initr| ctx.alctr.free(initr);
                return std.fmt.allocPrint(ctx.alctr, "var_decl: {s}: {?s}", .{ name, maybe_initr }) catch @panic("OOM");
            },
        }
    }
};

test "printAst" {
    const alctr = std.testing.allocator;
    var pool = Pool.init(alctr);
    defer pool.deinit();

    const b = pool.addBinary(
        .{ .typ = .star, .lexeme = "*", .line = 0 },
        pool.addUnary(
            .{ .typ = .minus, .lexeme = "-", .line = 0 },
            pool.addLiteral(.{ .typ = .number, .lexeme = "123", .line = 0 }).expr_index,
        ).expr_index,
        pool.addGrouping(
            pool.addLiteral(.{ .typ = .number, .lexeme = "45.67", .line = 0 }).expr_index,
        ).expr_index,
    );

    var printer = AstPrinter{};
    var ctx = .{ .alctr = alctr };
    const string = pool.getExpr(b.expr_index).acceptVisitor(pool, ctx, &printer);
    defer alctr.free(string);

    try std.testing.expectEqualStrings("(* (- 123) (group 45.67))", string);
}

fn testParser(
    comptime text: []const u8,
    comptime expected_prints: []const []const u8,
) !void {
    const alctr = std.testing.allocator;

    var lexer = lex.Lexer.init(text, alctr);
    defer lexer.deinit();

    const tokens = try lexer.scanTokens();
    defer alctr.free(tokens);

    var parser = Parser.init(tokens, alctr);
    defer parser.deinit();

    const stmts = try parser.parse();
    defer alctr.free(stmts);

    for (stmts, expected_prints) |stmt, expected| {
        var printer = AstPrinter{};
        var ctx = .{ .alctr = alctr };
        const string = parser.pool.getStmt(stmt).acceptVisitor(parser.pool, ctx, &printer);
        defer alctr.free(string);
        try std.testing.expectEqualStrings(expected, string);
    }
}

test "parse test 1" {
    const text =
        \\"hello" != "world";
    ;
    const expected_tree =
        \\expr: (!= "hello" "world")
    ;
    try testParser(text, &.{expected_tree});
}

test "parse test 2" {
    const text =
        \\"hello" != "world" != "goodbye";
    ;
    const expected_tree =
        \\expr: (!= (!= "hello" "world") "goodbye")
    ;
    try testParser(text, &.{expected_tree});
}

test "parse test 3" {
    const text =
        \\"hello" != "world" != ( "goodbye" );
    ;
    const expected_tree =
        \\expr: (!= (!= "hello" "world") (group "goodbye"))
    ;
    try testParser(text, &.{expected_tree});
}

test "parse test 4" {
    const text =
        \\"hello" != "world" != ( "goodbye" + 6 );
    ;
    const expected_tree =
        \\expr: (!= (!= "hello" "world") (group (+ "goodbye" 6)))
    ;
    try testParser(text, &.{expected_tree});
}

test "parse test 5" {
    const text =
        \\ (true + false) == "hello world" - 2.2;
    ;
    const expected_tree =
        \\expr: (== (group (+ true false)) (- "hello world" 2.2))
    ;
    try testParser(text, &.{expected_tree});
}

test "parse test 6" {
    const text =
        \\ var foo = "bar";
    ;
    const expected_tree =
        \\var_decl: foo: "bar"
    ;
    try testParser(text, &.{expected_tree});
}

test "parse test 7" {
    const text =
        \\ var foo;
    ;
    const expected_tree =
        \\var_decl: foo: null
    ;
    try testParser(text, &.{expected_tree});
}

test "parse test 8" {
    const text =
        \\ var foo;
        \\ var bar = foo + 2;
    ;
    const expected_trees = &.{
        "var_decl: foo: null",
        "var_decl: bar: (+ var:foo 2)",
    };
    try testParser(text, expected_trees);
}

test "parse test 9" {
    const text =
        \\ var foo;
        \\ var bar = foo + 2;
        \\ bar = foo * foo;
    ;
    const expected_trees = &.{
        "var_decl: foo: null",
        "var_decl: bar: (+ var:foo 2)",
        "expr: assign:bar=(* var:foo var:foo)",
    };
    try testParser(text, expected_trees);
}

const std = @import("std");
const lex = @import("lex.zig");
const ux = @import("ux.zig");

const log = std.log.scoped(.parse);
