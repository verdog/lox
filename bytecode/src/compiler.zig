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

fn Parser(comptime Context: type) type {
    return struct {
        scanner: Scanner,
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
                .previous = undefined,
                .current = undefined,
                .pool = pool,
                .had_error = false,
                .in_panic_mode = false,
                .ctx = ctx,
            };
        }

        const Rule = struct {
            prefix: *const fn (p: *P) void,
            infix: *const fn (p: *P) void,
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
                .lparen => .{ .prefix = P.grouping, .infix = P.unimplemented, .precedence = .none },
                // .rparen => {},
                // .lbrace => {},
                // .rbrace => {},
                // .comma => {},
                // .dot => {},
                .minus => .{ .prefix = P.unary, .infix = P.binary, .precedence = .term },
                .plus => .{ .prefix = P.unimplemented, .infix = P.binary, .precedence = .term },
                // .semicolon => {},
                .slash => .{ .prefix = P.unimplemented, .infix = P.binary, .precedence = .factor },
                .star => .{ .prefix = P.unimplemented, .infix = P.binary, .precedence = .factor },
                .bang => .{ .prefix = P.unary, .infix = P.unimplemented, .precedence = .none },
                .bang_eql => .{ .prefix = P.unimplemented, .infix = P.binary, .precedence = .equality },
                // .eql => {},
                .eql_eql => .{ .prefix = P.unimplemented, .infix = P.binary, .precedence = .equality },
                .less => .{ .prefix = P.unimplemented, .infix = P.binary, .precedence = .comparison },
                .less_eql => .{ .prefix = P.unimplemented, .infix = P.binary, .precedence = .comparison },
                .greater => .{ .prefix = P.unimplemented, .infix = P.binary, .precedence = .comparison },
                .greater_eql => .{ .prefix = P.unimplemented, .infix = P.binary, .precedence = .comparison },
                .identifier => .{ .prefix = P.variable, .infix = P.unimplemented, .precedence = .none },
                .string => .{ .prefix = P.string, .infix = P.unimplemented, .precedence = .none },
                .number => .{ .prefix = P.number, .infix = P.unimplemented, .precedence = .none },
                // .@"and" => {},
                // .class => {},
                // .@"else" => {},
                .false => .{ .prefix = P.literal, .infix = P.unimplemented, .precedence = .none },
                // .fun => {},
                // .@"for" => {},
                // .@"if" => {},
                .nil => .{ .prefix = P.literal, .infix = P.unimplemented, .precedence = .none },
                // .@"or" => {},
                // .print => {},
                // .@"return" => {},
                // .super => {},
                // .this => {},
                .true => .{ .prefix = P.literal, .infix = P.unimplemented, .precedence = .none },
                // .@"var" => {},
                // .@"while" => {},
                // .@"error" => {},
                // .eof => {},
                else => .{ .prefix = P.unimplemented, .infix = P.unimplemented, .precedence = .none },
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
            return p.identifier_constant(p.previous);
        }

        fn identifier_constant(p: *P, name: Token) u8 {
            return current_chunk.add_constant(p.pool.make_string_value(name.lexeme));
        }

        fn define_variable(p: *P, global: u8) void {
            _ = p;
            emit_bytes(@intFromEnum(OpCode.define_global), global);
        }

        pub fn statement(p: *P) void {
            if (p.match(.print)) {
                p.print_statement();
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

        pub fn variable(p: *P) void {
            p.named_variable(p.previous);
        }

        fn named_variable(p: *P, name: Token) void {
            const arg = p.identifier_constant(name);
            emit_bytes(@intFromEnum(OpCode.get_global), arg);
        }

        pub fn expression(p: *P) void {
            p.parse_precedence(.assignment);
        }

        pub fn number(p: *P) void {
            const value: Value = .{ .number = std.fmt.parseFloat(f64, p.previous.lexeme) catch unreachable };
            emit_constant(value);
        }

        pub fn literal(p: *P) void {
            switch (p.previous.typ) {
                .false => emit_byte(@intFromEnum(OpCode.false)),
                .true => emit_byte(@intFromEnum(OpCode.true)),
                .nil => emit_byte(@intFromEnum(OpCode.nil)),
                else => unreachable,
            }
        }

        pub fn string(p: *P) void {
            emit_constant(p.pool.make_string_value(p.previous.lexeme[1 .. p.previous.lexeme.len - 1]));
        }

        pub fn grouping(p: *P) void {
            p.expression();
            p.consume(.rparen, "Expected ')' after expression.");
        }

        pub fn unary(p: *P) void {
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

        pub fn binary(p: *P) void {
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

        // used to fill in unimplemented entries in the rules table
        fn unimplemented(p: *P) void {
            _ = p;
            unreachable;
        }

        pub fn parse_precedence(p: *P, precedence: Precedence) void {
            {
                p.advance();
                const rule = p.get_rule(p.previous.typ);
                if (rule.prefix == &P.unimplemented) {
                    p.print_error("Expected expression.");
                    return;
                }
                rule.prefix(p);
            }

            while (@intFromEnum(precedence) <= @intFromEnum(p.get_rule(p.current.typ).precedence)) {
                p.advance();
                const rule = p.get_rule(p.previous.typ);
                rule.infix(p);
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
