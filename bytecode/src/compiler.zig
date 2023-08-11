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

pub fn compile(source_text: []const u8) void {
    var scanner = Scanner.init(source_text);

    var line = @as(i32, -1);
    while (true) {
        const token = scanner.scan_token();

        if (token.line != line) {
            std.debug.print("{d: >4} ", .{token.line});
        } else {
            std.debug.print("   | ", .{});
        }

        std.debug.print("{s: >10} {s: <16} |", .{ @tagName(token.typ), token.lexeme });

        if (token.line != line) {
            const source_line = std.mem.sliceTo(source_text[scanner.start..], '\n');
            std.debug.print(" {s}", .{source_line});
            line = token.line;
        }

        std.debug.print("\n", .{});

        if (token.typ == .eof) break;
    }
}

const std = @import("std");
const log = std.log.scoped(.cpl);
