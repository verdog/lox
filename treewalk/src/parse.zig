pub const Expr = union(enum) {
    binary: BinaryExpr,
    unary: UnaryExpr,
    grouping: GroupingExpr,
    literal: LiteralExpr,
    variable: VariableExpr,
    assign: AssignExpr,
    logical: LogicalExpr,
    call: CallExpr,

    pub fn acceptVisitor(expr: Expr, pool: Pool, ctx: anytype, visitor: anytype) VisitorResult(@TypeOf(visitor.*)) {
        return visitor.visit(expr, pool, ctx);
    }

    pub fn deinit(expr: *Expr, owning_allocator: std.mem.Allocator) void {
        switch (expr.*) {
            .call => |c| owning_allocator.free(c.args),
            inline else => {},
        }
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

const LogicalExpr = struct {
    left: Pool.ExprIndex,
    operator: lex.Token,
    right: Pool.ExprIndex,
};

const CallExpr = struct {
    callee: Pool.ExprIndex,
    closing_paren: lex.Token,
    args: []Pool.ExprIndex,
};

pub const Stmt = union(enum) {
    expr: ExprStmt,
    print: PrintStmt,
    vari: VarStmt,
    block: BlockStmt,
    ifelse: IfStmt,
    whil: WhileStmt,
    func: FunctionStmt,

    pub fn acceptVisitor(stmt: Stmt, pool: Pool, ctx: anytype, visitor: anytype) VisitorResult(@TypeOf(visitor.*)) {
        return visitor.visit(stmt, pool, ctx);
    }

    pub fn deinit(stmt: *Stmt, owning_alctr: std.mem.Allocator) void {
        switch (stmt.*) {
            .block => |block| owning_alctr.free(block.statements),
            .func => |f| {
                owning_alctr.free(f.params);
                owning_alctr.free(f.body);
            },
            inline else => {},
        }
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

const BlockStmt = struct {
    statements: []Pool.StmtIndex,
};

const IfStmt = struct {
    condition: Pool.ExprIndex,
    then: Pool.StmtIndex,
    els: ?Pool.StmtIndex,
};

const WhileStmt = struct {
    condition: Pool.ExprIndex,
    do: Pool.StmtIndex,
};

const FunctionStmt = struct {
    name: lex.Token,
    params: []lex.Token,
    body: []Pool.StmtIndex,
};

pub const Pool = struct {
    pub const ExprIndex = u16;
    pub const StmtIndex = u16;

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
        for (self.stmts.items) |*stmt| {
            stmt.deinit(self.alctr);
        }
        self.stmts.deinit();
        for (self.exprs.items) |*expr| {
            expr.deinit(self.alctr);
        }
        self.exprs.deinit();
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

    pub fn addBlockStmt(pool: *Pool) Handle {
        pool.stmts.append(undefined) catch @panic("OOM");
        pool.stmts.items[pool.stmts.items.len - 1] = .{ .block = .{ .statements = undefined } };
        return pool.lastStmt();
    }

    pub fn addIfStmt(pool: *Pool, expr: ExprIndex, then: StmtIndex, els: ?StmtIndex) Handle {
        pool.stmts.append(undefined) catch @panic("OOM");
        pool.stmts.items[pool.stmts.items.len - 1] = .{ .ifelse = .{ .condition = expr, .then = then, .els = els } };
        return pool.lastStmt();
    }

    pub fn addWhileStmt(pool: *Pool, expr: ExprIndex, do: StmtIndex) Handle {
        pool.stmts.append(undefined) catch @panic("OOM");
        pool.stmts.items[pool.stmts.items.len - 1] = .{ .whil = .{ .condition = expr, .do = do } };
        return pool.lastStmt();
    }

    pub fn addFunctionStmt(pool: *Pool, name: lex.Token, params: []lex.Token, body: []StmtIndex) Handle {
        pool.stmts.append(undefined) catch @panic("OOM");
        pool.stmts.items[pool.stmts.items.len - 1] = .{ .func = .{
            .name = name,
            .params = params,
            .body = body,
        } };
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

    pub fn addLogical(pool: *Pool, operator: lex.Token, left: ExprIndex, right: ExprIndex) Handle {
        pool.exprs.append(undefined) catch @panic("OOM");
        pool.exprs.items[pool.exprs.items.len - 1] = .{ .logical = .{
            .left = left,
            .operator = operator,
            .right = right,
        } };
        return pool.lastExpr();
    }

    pub fn addCall(pool: *Pool, callee: ExprIndex, end_paren: lex.Token, args: []ExprIndex) Handle {
        pool.exprs.append(undefined) catch @panic("OOM");
        pool.exprs.items[pool.exprs.items.len - 1] = .{ .call = .{
            .callee = callee,
            .closing_paren = end_paren,
            .args = args,
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
        TooManyArguments,
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
    //             |  func_decl
    //             |  statement ;
    //
    // var_decl    -> "var" IDENTIFIER ( "=" expression )? ";" ;
    //
    // func_decl   -> "fun" function ;
    // function    -> IDENTIFIER "(" parameters? ")" block ;
    // parameters  -> IDENTIFIER ( "," IDENTIFIER )* ;
    //
    // statement   -> expr_stmt
    //             |  for_stmt
    //             |  if_stmt
    //             |  print_stmt
    //             |  while_stmt
    //             |  block ;
    // for_stmt    -> "for" "(" ( var_decl | exprStmt | ";" )
    //                expression? ";" expression? ")" statement ;
    // if_stmt     -> "if" "(" expression ")" statement ( "else" statement )? ;
    // expr_stmt   -> expression ";" ;
    // print_stmt  -> "print" expression ";" ;
    // while_stmt  -> "while" "(" expression ")" statement ;
    // block       -> "{" declaration* "}"
    //
    // expression  -> assignment ;
    // assignment  -> IDENTIFIER "=" assignment
    //             -> logic_or ;
    // logic_or    -> logic_and ( "or" logic_and )* ;
    // logic_and   -> equality ( "and" equality )* ;
    // equality    -> comparison ( ( "!=" | "==" ) comparison )* ;
    // comparison  -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    // term        -> factor ( ( "-" | "+" ) factor )* ;
    // factor      -> unary ( ( "/" | "*" ) unary )* ;
    // unary       -> ( "!" | "   -" ) unary | call ;
    // call        -> primary ( "(" arguments? ")" )* ;
    // arguments   -> expression ( "," expression )* ;
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
            if (p.match(.fun)) break :blk p.func_decl("function");
            if (p.match(.@"var")) break :blk p.var_decl();
            break :blk p.statement();
        } catch {
            p.synchronize();
            return Error.Recoverable;
        };

        return result;
    }

    fn func_decl(p: *Parser, comptime kind: []const u8) Error!Pool.Handle {
        const name = try p.consume(.identifier, "Expected " ++ kind ++ " name");
        _ = try p.consume(.lparen, "Expected '(' after " ++ kind ++ " name");

        var params = std.ArrayList(lex.Token).init(p.alctr);
        defer params.deinit();

        if (!p.check(.rparen)) {
            // do
            {
                params.append(try p.consume(.identifier, "Expected parameter name")) catch @panic("OOM");
            }
            while (p.match(.comma)) {
                if (params.items.len >= 255) return Error.TooManyArguments;
                params.append(try p.consume(.identifier, "Expected parameter name")) catch @panic("OOM");
            }
        }

        _ = try p.consume(.rparen, "Expected ')' after " ++ kind ++ " parameters");
        _ = try p.consume(.lbrace, "Expected '{' before " ++ kind ++ " body");

        var body = try p.blockContents();

        return p.pool.addFunctionStmt(name, params.toOwnedSlice() catch @panic("OOM"), body);
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
        if (p.match(.lbrace)) return try p.block();
        if (p.match(.@"if")) return try p.if_statement();
        if (p.match(.@"while")) return try p.while_statement();
        if (p.match(.@"for")) return try p.for_statement();

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

    fn block(p: *Parser) Error!Pool.Handle {
        var blk_h = p.pool.addBlockStmt();
        const contents = try p.blockContents();
        p.pool.stmts.items[blk_h.stmt_index].block.statements = contents;
        return blk_h;
    }

    fn blockContents(p: *Parser) ![]Pool.StmtIndex {
        var list = std.ArrayList(Pool.StmtIndex).init(p.alctr);

        while (!p.check(.rbrace) and !p.isAtEnd()) {
            const stmt = try p.declaration();
            list.append(stmt.stmt_index) catch @panic("OOM");
        }

        _ = try p.consume(.rbrace, "Expected '}' after block");

        return list.toOwnedSlice() catch @panic("OOM");
    }

    fn if_statement(p: *Parser) Error!Pool.Handle {
        _ = try p.consume(.lparen, "Expected '(' after if");
        const expr_h = try p.expression();
        const expr = expr_h.expr_index;
        _ = try p.consume(.rparen, "Expected ')' after condition");

        const then_h = try p.statement();
        const then = then_h.stmt_index;

        const els = blk: {
            if (p.match(.@"else")) {
                const stmt = try p.statement();
                break :blk stmt.stmt_index;
            } else {
                break :blk null;
            }
        };

        return p.pool.addIfStmt(expr, then, els);
    }

    fn while_statement(p: *Parser) Error!Pool.Handle {
        _ = try p.consume(.lparen, "Expected '(' after while");
        const expr_h = try p.expression();
        const expr = expr_h.expr_index;
        _ = try p.consume(.rparen, "Expected ')' after condition");

        const do_h = try p.statement();
        const do = do_h.stmt_index;

        return p.pool.addWhileStmt(expr, do);
    }

    fn for_statement(p: *Parser) Error!Pool.Handle {
        // the for loop is de-sugared into an equivalent while loop.
        //   for (var i = 0; i < 10; i = i + 1) print i;
        //     turns into
        //   {
        //     var i = 0;
        //     while (i < 10) {
        //       print i;
        //       i = i + 1;
        //     }
        //   }

        _ = try p.consume(.lparen, "Expected '(' after for");

        const initr = blk: {
            if (p.match(.semicolon)) break :blk null else if (p.match(.@"var")) break :blk try p.var_decl() else break :blk try p.expression_statement();
        };
        // no consume semicolon here because it is included in var_decl and
        // expression_statement

        var condition = blk: {
            if (!p.check(.semicolon)) break :blk try p.expression() else break :blk null;
        };
        _ = try p.consume(.semicolon, "Expected ';' after for loop condition");

        const incr = blk: {
            if (!p.check(.rparen)) break :blk try p.expression() else break :blk null;
        };
        _ = try p.consume(.rparen, "Expected ')' after for clauses");

        var body = try p.statement();

        if (incr) |inc| {
            var blk_h = p.pool.addBlockStmt();
            var inc_h = p.pool.addExprStmt(inc.expr_index);

            var statements = &p.pool.stmts.items[blk_h.stmt_index].block.statements;
            statements.* = p.pool.alctr.alloc(Pool.ExprIndex, 2) catch @panic("OOM");

            statements.*[0] = body.stmt_index;
            statements.*[1] = inc_h.stmt_index;

            body = blk_h;
        }

        if (condition == null) condition = p.pool.addLiteral(.{ .typ = .true, .lexeme = undefined, .line = -1 });
        body = p.pool.addWhileStmt(condition.?.expr_index, body.stmt_index);

        if (initr) |i| {
            var blk_h = p.pool.addBlockStmt();
            var statements = &p.pool.stmts.items[blk_h.stmt_index].block.statements;
            statements.* = p.pool.alctr.alloc(Pool.ExprIndex, 2) catch @panic("OOM");

            statements.*[0] = i.stmt_index;
            statements.*[1] = body.stmt_index;

            body = blk_h;
        }

        return body;
    }

    fn expression(p: *Parser) Error!Pool.Handle {
        return try p.assignment();
    }

    fn assignment(p: *Parser) Error!Pool.Handle {
        const expr = try p.logicOr();

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

    fn logicOr(p: *Parser) Error!Pool.Handle {
        var expr = try p.logicAnd();
        while (p.match(.@"or")) {
            const op = p.previous();
            const right = try p.logicAnd();
            expr = p.pool.addLogical(op, expr.expr_index, right.expr_index);
        }
        return expr;
    }

    fn logicAnd(p: *Parser) Error!Pool.Handle {
        var expr = try p.equality();
        while (p.match(.@"and")) {
            const op = p.previous();
            const right = try p.equality();
            expr = p.pool.addLogical(op, expr.expr_index, right.expr_index);
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
            return try p.call();
        }
    }

    fn call(p: *Parser) Error!Pool.Handle {
        var expr = try p.primary();

        while (true) {
            if (p.match(.lparen)) {
                expr = try p.finishCall(expr);
            } else {
                break;
            }
        }

        return expr;
    }

    fn finishCall(p: *Parser, callee: Pool.Handle) Error!Pool.Handle {
        var list = std.ArrayList(Pool.ExprIndex).init(p.alctr);

        if (!p.check(.rparen)) {
            { // do
                var expr = try p.expression();
                list.append(expr.expr_index) catch @panic("OOM");
            }
            while (p.match(.comma)) {
                if (list.items.len >= 255) {
                    return Error.TooManyArguments;
                }
                var expr = try p.expression();
                list.append(expr.expr_index) catch @panic("OOM");
            }
        }

        const end_paren = try p.consume(.rparen, "Expected ')' after arguments");

        return p.pool.addCall(callee.expr_index, end_paren, list.toOwnedSlice() catch @panic("OOM"));
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
            .logical => |l| {
                const left = pl.getExpr(l.left).acceptVisitor(pl, ctx, &self);
                defer ctx.alctr.free(left);
                const right = pl.getExpr(l.right).acceptVisitor(pl, ctx, &self);
                defer ctx.alctr.free(right);
                return std.fmt.allocPrint(ctx.alctr, "({s} {s} {s})", .{ l.operator.lexeme, left, right }) catch @panic("OOM");
            },
            .call => |c| {
                var string = std.ArrayList(u8).init(ctx.alctr);
                var writer = string.writer();

                const callee = pl.getExpr(c.callee).acceptVisitor(pl, ctx, &self);
                defer ctx.alctr.free(callee);

                writer.print("{s}(", .{callee}) catch @panic("OOM");

                var comma = false;
                for (c.args) |a| {
                    const a_str = pl.getExpr(a).acceptVisitor(pl, ctx, &self);
                    defer ctx.alctr.free(a_str);
                    if (comma) writer.print(", ", .{}) catch @panic("OOM");
                    writer.print("{s}", .{a_str}) catch @panic("OOM");
                    comma = true;
                }
                writer.print(")", .{}) catch @panic("OOM");

                return string.toOwnedSlice() catch @panic("OOM");
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
            .func => |f| {
                var string = std.ArrayList(u8).init(ctx.alctr);
                var writer = string.writer();

                writer.print("function: {s}(", .{f.name.lexeme}) catch @panic("OOM");

                var comma = false;
                for (f.params) |p| {
                    if (comma) writer.print(", ", .{}) catch @panic("OOM");
                    writer.print("{s}", .{p.lexeme}) catch @panic("OOM");
                    comma = true;
                }

                writer.print(") len:{d} {{", .{f.body.len}) catch @panic("OOM");

                for (f.body) |sidx| {
                    const contents = pl.getStmt(sidx).acceptVisitor(pl, ctx, &self);
                    defer ctx.alctr.free(contents);
                    writer.print("{s},", .{contents}) catch @panic("OOM");
                }

                writer.print("}}", .{}) catch @panic("OOM");

                return string.toOwnedSlice() catch @panic("OOM");
            },
            .block => |block| {
                var string = std.ArrayList(u8).init(ctx.alctr);
                var writer = string.writer();

                writer.print("block len:{d} : ", .{block.statements.len}) catch @panic("OOM");
                for (block.statements) |stmt| {
                    const contents = pl.getStmt(stmt).acceptVisitor(pl, ctx, &self);
                    defer ctx.alctr.free(contents);
                    writer.print("{s},", .{contents}) catch @panic("OOM");
                }

                return string.toOwnedSlice() catch @panic("OOM");
            },
            .ifelse => |ifelse| {
                const condition = pl.getExpr(ifelse.condition).acceptVisitor(pl, ctx, &self);
                defer ctx.alctr.free(condition);
                const then = pl.getStmt(ifelse.then).acceptVisitor(pl, ctx, &self);
                defer ctx.alctr.free(then);
                const els = if (ifelse.els) |e| pl.getStmt(e).acceptVisitor(pl, ctx, &self) else ctx.alctr.dupe(u8, "") catch @panic("OOM");
                defer ctx.alctr.free(els);

                return std.fmt.allocPrint(ctx.alctr, "if({s})then({s})else({s})", .{ condition, then, els }) catch @panic("OOM");
            },
            .whil => |whil| {
                const condition = pl.getExpr(whil.condition).acceptVisitor(pl, ctx, &self);
                defer ctx.alctr.free(condition);
                const do = pl.getStmt(whil.do).acceptVisitor(pl, ctx, &self);
                defer ctx.alctr.free(do);

                return std.fmt.allocPrint(ctx.alctr, "while({s})do({s})", .{ condition, do }) catch @panic("OOM");
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

test "parse test 10" {
    const text =
        \\{
        \\var foo = 2 * 2;
        \\}
    ;
    const expected_trees = &.{
        "block len:1 : var_decl: foo: (* 2 2),",
    };
    try testParser(text, expected_trees);
}

test "parse test 11" {
    const text =
        \\var foo = 0;
        \\{
        \\var foo = 1;
        \\var bar = 2;
        \\}
        \\var bar = 3;
    ;
    const expected_trees = &.{
        "var_decl: foo: 0",
        "block len:2 : var_decl: foo: 1,var_decl: bar: 2,",
        "var_decl: bar: 3",
    };
    try testParser(text, expected_trees);
}

test "parse test 12" {
    const text =
        \\var a = 0;
        \\var temp;
        \\
        \\for (var b = 1; a < 10000; b = temp + b) {
        \\  print a;
        \\  temp = a;
        \\  a = b;
        \\}
    ;

    const expected_trees = &.{
        "var_decl: a: 0",
        "var_decl: temp: null",
        "block len:2 : var_decl: b: 1,while((< var:a 10000))do(block len:2 : block len:3 : print: var:a,expr: assign:temp=var:a,expr: assign:a=var:b,,expr: assign:b=(+ var:temp var:b),),",
    };

    try testParser(text, expected_trees);
}

test "parse test 13" {
    const text =
        \\var b = 0;
        \\while (b < 3) {
        \\  b = b + 1;
        \\}
    ;

    const expected_trees = &.{
        "var_decl: b: 0",
        "while((< var:b 3))do(block len:1 : expr: assign:b=(+ var:b 1),)",
    };

    try testParser(text, expected_trees);
}

test "parse test 14" {
    const text =
        \\foo();
        \\getfoo()();
        \\getfoo(1, 2)("hello world");
        \\getfoo()("hello world");
        \\getfoo(1, 2)();
    ;

    const expected_trees = &.{
        "expr: var:foo()",
        "expr: var:getfoo()()",
        "expr: var:getfoo(1, 2)(\"hello world\")",
        "expr: var:getfoo()(\"hello world\")",
        "expr: var:getfoo(1, 2)()",
    };

    try testParser(text, expected_trees);
}

test "parse test 15" {
    const text =
        \\fun foo(bar) {
        \\    print bar;
        \\}
    ;

    const expected_trees = &.{
        "function: foo(bar) len:1 {print: var:bar,}",
    };

    try testParser(text, expected_trees);
}

test "parse test 16" {
    const text =
        \\fun foo(bar) {
        \\    print bar;
        \\}
        \\fun baz(bar) {
        \\    foo(bar);
        \\}
    ;

    const expected_trees = &.{
        "function: foo(bar) len:1 {print: var:bar,}",
        "function: baz(bar) len:1 {expr: var:foo(var:bar),}",
    };

    try testParser(text, expected_trees);
}

const std = @import("std");
const lex = @import("lex.zig");
const ux = @import("ux.zig");

const log = std.log.scoped(.parse);
