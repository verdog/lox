//! ...
const Scanner = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: i32,

    pub fn init(source_text: []const u8) Scanner {
        return .{
            .source = source_text,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    pub fn scan_token(s: *Scanner) Token {
        s.skip_whitespace();

        s.start = s.current;

        if (s.is_at_end()) return s.make_token(.eof);

        const c = s.advance();

        if (s.is_alpha(c)) return s.identifier();
        if (s.is_digit(c)) return s.number();

        switch (c) {
            // single character tokens
            '(' => return s.make_token(.lparen),
            ')' => return s.make_token(.rparen),
            '{' => return s.make_token(.lbrace),
            '}' => return s.make_token(.rbrace),
            ';' => return s.make_token(.semicolon),
            ',' => return s.make_token(.comma),
            '.' => return s.make_token(.dot),
            '-' => return s.make_token(.minus),
            '+' => return s.make_token(.plus),
            '/' => return s.make_token(.slash),
            '*' => return s.make_token(.star),

            // single or double character tokens
            '!' => return s.make_token(if (s.match('=')) .bang_eql else .bang),
            '=' => return s.make_token(if (s.match('=')) .eql_eql else .eql),
            '<' => return s.make_token(if (s.match('=')) .less else .less_eql),
            '>' => return s.make_token(if (s.match('=')) .greater else .greater_eql),

            // other
            '"' => return s.string(),

            else => {
                std.debug.print("\"{c}\"\n", .{c});
                return s.make_error_token("Unexpected character.");
            },
        }
    }

    fn is_alpha(s: Scanner, c: u8) bool {
        _ = s;
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn identifier(s: *Scanner) Token {
        while (s.is_alpha(s.peek()) or s.is_digit(s.peek())) _ = s.advance();
        return s.make_token(s.identifier_type());
    }

    fn identifier_type(s: Scanner) Token.Type {
        // keyword trie
        switch (s.source[s.start]) {
            'a' => return s.check_keyword(1, "nd", .@"and"),
            'c' => return s.check_keyword(1, "lass", .class),
            'e' => return s.check_keyword(1, "lse", .@"else"),
            'i' => return s.check_keyword(1, "f", .@"if"),
            'n' => return s.check_keyword(1, "il", .nil),
            'o' => return s.check_keyword(1, "r", .@"or"),
            'p' => return s.check_keyword(1, "rint", .print),
            'r' => return s.check_keyword(1, "eturn", .@"return"),
            's' => return s.check_keyword(1, "uper", .super),
            'v' => return s.check_keyword(1, "ar", .@"var"),
            'w' => return s.check_keyword(1, "hile", .@"while"),
            'f' => if (s.current - s.start > 1) {
                switch (s.source[s.start + 1]) {
                    'a' => return s.check_keyword(2, "lse", .false),
                    'o' => return s.check_keyword(2, "r", .@"for"),
                    'u' => return s.check_keyword(2, "n", .fun),
                    else => {}, // fall out to .identifier
                }
            },
            't' => if (s.current - s.start > 1) {
                switch (s.source[s.start + 1]) {
                    'h' => return s.check_keyword(2, "is", .this),
                    'r' => return s.check_keyword(2, "ue", .true),
                    else => {}, // fall out to .identifier
                }
            },
            else => {}, // fall out to .identifier
        }
        return .identifier;
    }

    fn check_keyword(s: Scanner, start: usize, remaining: []const u8, result_type: Token.Type) Token.Type {
        if (s.current - s.start == start + remaining.len and std.mem.eql(u8, s.source[s.start + start ..][0..remaining.len], remaining)) {
            return result_type;
        }
        return .identifier;
    }

    fn is_digit(s: Scanner, c: u8) bool {
        _ = s;
        return c >= '0' and c <= '9';
    }

    fn number(s: *Scanner) Token {
        while (s.is_digit(s.peek())) _ = s.advance();

        // look for a fractional part
        if (s.peek() == '.' and s.is_digit(s.peek_next())) {
            // consume '.'
            _ = s.advance();
            while (s.is_digit(s.peek())) _ = s.advance();
        }

        return s.make_token(.number);
    }

    fn string(s: *Scanner) Token {
        while (s.peek() != '"' and !s.is_at_end()) {
            if (s.peek() == '\n') s.line += 1;
            _ = s.advance();
        }

        if (s.is_at_end()) return s.make_error_token("Unterminated string.");

        // consume the closing quote
        _ = s.advance();
        return s.make_token(.string);
    }

    fn skip_whitespace(s: *Scanner) void {
        // XXX: what happens if the source text ends in whitespace?
        while (true) {
            const c = s.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = s.advance(),
                '\n' => {
                    s.line += 1;
                    _ = s.advance();
                },
                '/' => {
                    if (s.peek_next() == '/') {
                        // a comment goes until the end of the line
                        while (s.peek() != '\n' and !s.is_at_end()) _ = s.advance();
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn advance(s: *Scanner) u8 {
        s.current += 1;
        return s.source[s.current - 1];
    }

    fn is_at_end(s: Scanner) bool {
        return s.current >= s.source.len;
    }

    fn match(s: *Scanner, expected: u8) bool {
        if (s.is_at_end()) return false;
        if (s.source[s.current] != expected) return false;
        // otherwise we have a match
        s.current += 1;
        return true;
    }

    fn peek(s: Scanner) u8 {
        if (s.is_at_end()) return '\x00'; // XXX: not sure if this maps to zig correctly
        return s.source[s.current];
    }

    fn peek_next(s: Scanner) u8 {
        if (s.is_at_end()) return '\x00'; // XXX: not sure if this maps to zig correctly
        return s.source[s.current + 1];
    }

    fn make_token(s: Scanner, typ: Token.Type) Token {
        return .{ .typ = typ, .lexeme = s.source[s.start..s.current], .line = s.line };
    }

    fn make_error_token(s: Scanner, err: []const u8) Token {
        return .{ .typ = .@"error", .lexeme = err, .line = s.line };
    }
};

const Token = struct {
    // zig fmt: off
    pub const Type = enum {
        // single character tokens
        lparen, rparen, lbrace, rbrace,
        comma, dot, minus, plus, semicolon, slash, star,

        // one or two character tokens
        bang, bang_eql, eql, eql_eql,
        less, less_eql, greater, greater_eql,

        // literals
        identifier, string, number,

        // keywords
        @"and", class, @"else", false, fun, @"for",
        @"if", nil, @"or", print, @"return",
        super, this, true, @"var", @"while",

        @"error", eof
    };
    // zig fmt: on

    typ: Type,
    lexeme: []const u8,
    line: i32,
};

const Local = struct {
    name: Token,
    depth: i32,
};

const Compiler = struct {
    locals: [locals_capacity]Local = undefined,
    locals_count: i16 = 0,
    scope_depth: i32 = 0,

    const locals_capacity = std.math.maxInt(u8) + 1;
};

fn Parser(comptime Context: type) type {
    return struct {
        scanner: Scanner,
        compiler: Compiler,
        previous: Token,
        current: Token,
        pool: *vl.ObjPool,
        had_error: bool,
        in_panic_mode: bool,
        ctx: Context,

        const P = @This();

        pub fn init(scanner: Scanner, pool: *vl.ObjPool, ctx: Context) P {
            return .{
                .scanner = scanner,
                .compiler = Compiler{},
                .previous = undefined,
                .current = undefined,
                .pool = pool,
                .had_error = false,
                .in_panic_mode = false,
                .ctx = ctx,
            };
        }

        const Rule = struct {
            prefix: *const fn (p: *P, can_assign: bool) void,
            infix: *const fn (p: *P, can_assign: bool) void,
            precedence: Precedence,
        };

        const Precedence = enum(u8) {
            none,
            assignment, // =
            @"or", // or
            @"and", // and
            equality, // == !=
            comparison, // < > <= >=
            term, // + -
            factor, // * /
            unary, // ! -
            call, // . ()
            primary,
        };

        fn get_rule(p: P, typ: Token.Type) Rule {
            _ = p;
            return switch (typ) {
                .lparen => .{ .prefix = P.grouping, .infix = P.unimpl, .precedence = .none },
                // .rparen => {},
                // .lbrace => {},
                // .rbrace => {},
                // .comma => {},
                // .dot => {},
                .minus => .{ .prefix = P.unary, .infix = P.binary, .precedence = .term },
                .plus => .{ .prefix = P.unimpl, .infix = P.binary, .precedence = .term },
                // .semicolon => {},
                .slash => .{ .prefix = P.unimpl, .infix = P.binary, .precedence = .factor },
                .star => .{ .prefix = P.unimpl, .infix = P.binary, .precedence = .factor },
                .bang => .{ .prefix = P.unary, .infix = P.unimpl, .precedence = .none },
                .bang_eql => .{ .prefix = P.unimpl, .infix = P.binary, .precedence = .equality },
                // .eql => {},
                .eql_eql => .{ .prefix = P.unimpl, .infix = P.binary, .precedence = .equality },
                .less => .{ .prefix = P.unimpl, .infix = P.binary, .precedence = .comparison },
                .less_eql => .{ .prefix = P.unimpl, .infix = P.binary, .precedence = .comparison },
                .greater => .{ .prefix = P.unimpl, .infix = P.binary, .precedence = .comparison },
                .greater_eql => .{ .prefix = P.unimpl, .infix = P.binary, .precedence = .comparison },
                .identifier => .{ .prefix = P.variable, .infix = P.unimpl, .precedence = .none },
                .string => .{ .prefix = P.string, .infix = P.unimpl, .precedence = .none },
                .number => .{ .prefix = P.number, .infix = P.unimpl, .precedence = .none },
                .@"and" => .{ .prefix = P.unimpl, .infix = P.@"and", .precedence = .@"and" },
                // .class => {},
                // .@"else" => {},
                .false => .{ .prefix = P.literal, .infix = P.unimpl, .precedence = .none },
                // .fun => {},
                // .@"for" => {},
                // .@"if" => {},
                .nil => .{ .prefix = P.literal, .infix = P.unimpl, .precedence = .none },
                .@"or" => .{ .prefix = P.unimpl, .infix = P.@"or", .precedence = .@"or" },
                // .print => {},
                // .@"return" => {},
                // .super => {},
                // .this => {},
                .true => .{ .prefix = P.literal, .infix = P.unimpl, .precedence = .none },
                // .@"var" => {},
                // .@"while" => {},
                // .@"error" => {},
                // .eof => {},
                else => .{ .prefix = P.unimpl, .infix = P.unimpl, .precedence = .none },
            };
        }

        pub fn advance(p: *P) void {
            p.previous = p.current;

            while (true) {
                p.current = p.scanner.scan_token();
                if (p.current.typ != .@"error") break;
                p.print_error_at_current(p.current.lexeme);
            }
        }

        pub fn match(p: *P, typ: Token.Type) bool {
            if (!p.check(typ)) return false;
            p.advance();
            return true;
        }

        pub fn check(p: *P, typ: Token.Type) bool {
            return p.current.typ == typ;
        }

        pub fn consume(p: *P, typ: Token.Type, message: []const u8) void {
            if (p.current.typ == typ) {
                p.advance();
                return;
            }
            p.print_error_at_current(message);
        }

        pub fn synchronize(p: *P) void {
            p.in_panic_mode = false;

            while (p.current.typ != .eof) : (p.advance()) {
                if (p.previous.typ == .semicolon) return;
                switch (p.current.typ) {
                    .class,
                    .fun,
                    .@"var",
                    .@"for",
                    .@"if",
                    .@"while",
                    .print,
                    .@"return",
                    => return,

                    else => {}, // do nothing
                }
            }
        }

        pub fn declaration(p: *P) void {
            if (p.match(.@"var")) {
                p.var_declaration();
            } else {
                p.statement();
            }

            if (p.in_panic_mode) p.synchronize();
        }

        pub fn var_declaration(p: *P) void {
            const global = p.parse_variable("Expected variable name.");

            if (p.match(.eql)) {
                p.expression();
            } else {
                emit_op(.nil);
            }
            p.consume(.semicolon, "Expected ';' after variable declaration.");

            p.define_variable(global);
        }

        fn parse_variable(p: *P, err_msg: []const u8) u8 {
            p.consume(.identifier, err_msg);

            p.declare_variable();
            // locals are not looked up by name, so no need to fall into
            // identifier_constant if we're not in global scope. 0 is a dummy value.
            if (p.compiler.scope_depth > 0) return 0;

            return p.identifier_constant(p.previous);
        }

        fn define_variable(p: *P, global: u8) void {
            // if we're not in global scope, the vm has already executed the variables
            // initializer and placed the value on to the top of the stack. new variables
            // are stored on the top of the stack, so we just need to mark it ready for
            // use.
            if (p.compiler.scope_depth > 0) {
                p.compiler.locals[@intCast(p.compiler.locals_count - 1)].depth = p.compiler.scope_depth;
                return;
            }

            emit_bytes(@intFromEnum(OpCode.define_global), global);
        }

        fn declare_variable(p: *P) void {
            if (p.compiler.scope_depth == 0) return;

            const name = p.previous;

            {
                var i = p.compiler.locals_count - 1;
                while (i >= 0) : (i -= 1) {
                    var local = &p.compiler.locals[@intCast(i)];
                    if (local.depth != -1 and local.depth < p.compiler.scope_depth) {
                        break;
                    }

                    if (std.mem.eql(u8, name.lexeme, local.name.lexeme)) {
                        p.print_error("Already a variable with this name in this scope.");
                    }
                }
            }

            p.add_local(name);
        }

        fn add_local(p: *P, name: Token) void {
            if (p.compiler.locals_count == Compiler.locals_capacity) {
                p.print_error("Too many local variables in function.");
                return;
            }
            var new_local_mem = &p.compiler.locals[@intCast(p.compiler.locals_count)];
            p.compiler.locals_count += 1;
            new_local_mem.name = name;
            new_local_mem.depth = -1; // -1 means uninitialized
        }

        fn identifier_constant(p: *P, name: Token) u8 {
            return current_chunk.add_constant(p.pool.make_string_value(name.lexeme));
        }

        pub fn statement(p: *P) void {
            if (p.match(.print)) {
                p.print_statement();
            } else if (p.match(.@"if")) {
                p.if_statement();
            } else if (p.match(.lbrace)) {
                p.begin_scope();
                p.block_statement();
                p.end_scope();
            } else {
                p.expression_statement();
            }
        }

        pub fn print_statement(p: *P) void {
            p.expression();
            p.consume(.semicolon, "Expected ';' after value.");
            emit_op(.print);
        }

        pub fn expression_statement(p: *P) void {
            p.expression();
            p.consume(.semicolon, "Expected ';' after expression.");
            emit_op(.pop);
        }

        pub fn if_statement(p: *P) void {
            p.consume(.lparen, "Expected '(' after 'if'.");
            p.expression();
            p.consume(.rparen, "Expected ')' after condition.");

            const then_jump = emit_jump(.jump_if_false);
            emit_op(.pop); // clean up condition result

            p.statement(); // then block
            const else_jump = emit_jump(.jump);

            patch_jump(then_jump);
            emit_op(.pop); // clean up condition result

            if (p.match(.@"else")) p.statement();

            patch_jump(else_jump);
        }

        pub fn block_statement(p: *P) void {
            while (!p.check(.rbrace) and !p.check(.eof)) {
                p.declaration();
            }

            p.consume(.rbrace, "Expected '}' after block.");
        }

        fn begin_scope(p: *P) void {
            p.compiler.scope_depth += 1;
        }

        fn end_scope(p: *P) void {
            p.compiler.scope_depth -= 1;

            while (p.compiler.locals_count > 0 and
                p.compiler.locals[@intCast(p.compiler.locals_count - 1)].depth > p.compiler.scope_depth)
            {
                emit_op(.pop);
                p.compiler.locals_count -= 1;
            }
        }

        pub fn variable(p: *P, can_assign: bool) void {
            p.named_variable(p.previous, can_assign);
        }

        fn named_variable(p: *P, name: Token, can_assign: bool) void {
            var get_op: u8 = undefined;
            var set_op: u8 = undefined;
            var arg = p.resolve_local(name);

            if (arg != -1) {
                get_op = @intFromEnum(OpCode.get_local);
                set_op = @intFromEnum(OpCode.set_local);
            } else {
                arg = p.identifier_constant(name);
                get_op = @intFromEnum(OpCode.get_global);
                set_op = @intFromEnum(OpCode.set_global);
            }

            if (can_assign and p.match(.eql)) {
                p.expression();
                emit_bytes(set_op, @intCast(arg));
            } else {
                emit_bytes(get_op, @intCast(arg));
            }
        }

        fn resolve_local(p: *P, name: Token) i16 {
            var i = p.compiler.locals_count - 1;
            while (i >= 0) : (i -= 1) {
                const local = &p.compiler.locals[@intCast(i)];
                if (std.mem.eql(u8, name.lexeme, local.name.lexeme)) {
                    if (local.depth == -1) {
                        p.print_error("Can't read local variable in its own initializer.");
                    }
                    return i;
                }
            }

            return -1;
        }

        pub fn expression(p: *P) void {
            p.parse_precedence(.assignment);
        }

        pub fn number(p: *P, can_assign: bool) void {
            _ = can_assign;
            const value: Value = .{ .number = std.fmt.parseFloat(f64, p.previous.lexeme) catch unreachable };
            emit_constant(value);
        }

        pub fn literal(p: *P, can_assign: bool) void {
            _ = can_assign;
            switch (p.previous.typ) {
                .false => emit_byte(@intFromEnum(OpCode.false)),
                .true => emit_byte(@intFromEnum(OpCode.true)),
                .nil => emit_byte(@intFromEnum(OpCode.nil)),
                else => unreachable,
            }
        }

        pub fn string(p: *P, can_assign: bool) void {
            _ = can_assign;
            emit_constant(p.pool.make_string_value(p.previous.lexeme[1 .. p.previous.lexeme.len - 1]));
        }

        pub fn grouping(p: *P, can_assign: bool) void {
            _ = can_assign;
            p.expression();
            p.consume(.rparen, "Expected ')' after expression.");
        }

        pub fn unary(p: *P, can_assign: bool) void {
            _ = can_assign;
            const typ = p.previous.typ;

            // compile the operand
            p.parse_precedence(.unary);

            // negate it
            switch (typ) {
                .minus => emit_byte(@intFromEnum(OpCode.negate)),
                .bang => emit_byte(@intFromEnum(OpCode.not)),
                else => unreachable,
            }
        }

        pub fn binary(p: *P, can_assign: bool) void {
            _ = can_assign;
            const typ = p.previous.typ;
            const rule = p.get_rule(typ);
            p.parse_precedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

            switch (typ) {
                .plus => emit_byte(@intFromEnum(OpCode.add)),
                .minus => emit_byte(@intFromEnum(OpCode.subtract)),
                .star => emit_byte(@intFromEnum(OpCode.multiply)),
                .slash => emit_byte(@intFromEnum(OpCode.divide)),
                .bang_eql => emit_ops(.equal, .not),
                .eql_eql => emit_op(.equal),
                .greater => emit_op(.greater),
                .greater_eql => emit_ops(.less, .not),
                .less => emit_op(.less),
                .less_eql => emit_ops(.greater, .not),
                else => unreachable,
            }
        }

        fn @"and"(p: *P, can_assign: bool) void {
            _ = can_assign;
            const end_jump = emit_jump(.jump_if_false);
            emit_op(.pop);
            p.parse_precedence(.@"and");
            patch_jump(end_jump);
        }

        fn @"or"(p: *P, can_assign: bool) void {
            _ = can_assign;
            const else_jump = emit_jump(.jump_if_false);
            const end_jump = emit_jump(.jump);

            patch_jump(else_jump);
            emit_op(.pop);

            p.parse_precedence(.@"or");
            patch_jump(end_jump);
        }

        // used to fill in unimplemented entries in the rules table
        fn unimpl(p: *P, can_assign: bool) void {
            _ = can_assign;
            _ = p;
            unreachable;
        }

        pub fn parse_precedence(p: *P, precedence: Precedence) void {
            const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.assignment);

            {
                p.advance();
                const rule = p.get_rule(p.previous.typ);
                if (rule.prefix == &P.unimpl) {
                    p.print_error("Expected expression.");
                    return;
                }
                rule.prefix(p, can_assign);
            }

            while (@intFromEnum(precedence) <= @intFromEnum(p.get_rule(p.current.typ).precedence)) {
                p.advance();
                const rule = p.get_rule(p.previous.typ);
                rule.infix(p, can_assign);
            }

            if (can_assign and p.match(.eql)) {
                p.print_error("Invalid assignment target.");
            }
        }

        fn print_error(p: *P, message: []const u8) void {
            p.print_error_at(p.previous, message);
        }

        fn print_error_at_current(p: *P, message: []const u8) void {
            p.print_error_at(p.current, message);
        }

        fn print_error_at(p: *P, token: Token, message: []const u8) void {
            // TODO convert this fn to use zig errors
            // TODO hoist the flag setting out of an otherwise side effect free print fn
            if (p.in_panic_mode) return;
            p.had_error = true;
            p.in_panic_mode = true;

            p.ctx.out.print("[line {d}] Error", .{token.line}) catch unreachable;

            if (token.typ == .eof) {
                p.ctx.out.print(" at end", .{}) catch unreachable;
            } else if (token.typ == .@"error") {
                // nothing
            } else {
                p.ctx.out.print(" at '{s}'", .{token.lexeme}) catch unreachable;
            }

            p.ctx.out.print(": {s}\n", .{message}) catch unreachable;
        }
    };
}

// XXX going by the book for now, but eventually fix up this global variable stuff...
var current_chunk: *Chunk = undefined;

fn emit_byte(b: u8) void {
    // XXX in the book this grabs the line number from the global parser...
    current_chunk.write(b, 1);
}

fn emit_bytes(b1: u8, b2: u8) void {
    // XXX in the book this grabs the line number from the global parser...
    current_chunk.write(b1, 1);
    current_chunk.write(b2, 1);
}

fn emit_op(o: OpCode) void {
    emit_byte(@intFromEnum(o));
}

fn emit_ops(o1: OpCode, o2: OpCode) void {
    emit_bytes(@intFromEnum(o1), @intFromEnum(o2));
}

fn emit_constant(value: Value) void {
    emit_bytes(@intFromEnum(OpCode.constant), current_chunk.add_constant(value));
}

fn emit_jump(o: OpCode) i32 {
    emit_op(o);
    emit_bytes(0xff, 0xff); // placeholder to be patched later
    return @as(i32, @intCast(current_chunk.code.items.len)) - 2;
}

fn patch_jump(offset: i32) void {
    // -2 to include the 2 byte jump amount itself, which will be read before the actual jump
    const jump_length = @as(usize, @intCast(@as(i32, @intCast(current_chunk.code.items.len)) - offset - 2));

    if (jump_length > std.math.maxInt(u16)) {
        // somehow error here...
        // error("Too much code to jump over".);
        unreachable;
    }

    current_chunk.code.items[@intCast(offset)] = @truncate(jump_length >> 8);
    current_chunk.code.items[@intCast(offset + 1)] = @truncate(jump_length);
}

pub fn compile(source_text: []const u8, ch: *Chunk, pool: *vl.ObjPool, err_printer: anytype) bool {
    var s = Scanner.init(source_text);
    const ctx = .{ .out = err_printer };
    var p = Parser(@TypeOf(ctx)).init(s, pool, ctx);

    current_chunk = ch;

    p.advance();
    while (!p.match(.eof)) {
        p.declaration();
    }

    // "endCompiler()"
    //   "emitReturn()"
    emit_byte(@intFromEnum(OpCode.@"return"));

    if (dbg.options.print_code and !p.had_error) {
        dbg.Disassembler.chunk(current_chunk.*, "bytecode", err_printer);
    }

    return !p.had_error;
}

const vl = @import("value.zig");
const ux = @import("ux.zig");
const dbg = @import("debug.zig");
const tbl = @import("table.zig");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;

const std = @import("std");
const log = std.log.scoped(.cpl);
