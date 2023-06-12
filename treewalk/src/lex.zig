// zig fmt: off
const TokenType = enum {
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

    eof
};
// zig fmt: on

const Token = struct {
    typ: TokenType,
    lexeme: []const u8,
    // literal: Literal,
    line: i32,
};

pub const Lexer = struct {
    source: []const u8,
    tokens: std.ArrayList(Token),

    current_lexeme_start: i32,
    current_lexeme_char: i32,
    current_line: i32,

    pub fn init(source: []const u8, alctr: std.mem.Allocator) Lexer {
        return .{
            .source = source,
            .tokens = std.ArrayList(Token).init(alctr),

            .current_lexeme_start = 0,
            .current_lexeme_char = 0,
            .current_line = 1,
        };
    }

    pub fn deinit(self: *Lexer) void {
        self.tokens.deinit();
    }

    pub fn scanTokens(self: *Lexer) !ux.Result {
        var result = ux.Result{};

        while (!self.atEnd()) {
            self.current_lexeme_start = self.current_lexeme_char;
            try self.scanToken(&result);
        }

        try self.tokens.append(Token{
            .typ = .eof,
            .lexeme = "",
            .line = self.current_line,
        });

        return result;
    }

    fn scanToken(self: *Lexer, result: *ux.Result) !void {
        const c = self.advance();

        switch (c) {
            // single character
            '(' => try self.addToken(.lparen),
            ')' => try self.addToken(.rparen),
            '{' => try self.addToken(.lbrace),
            '}' => try self.addToken(.rbrace),
            ',' => try self.addToken(.comma),
            '.' => try self.addToken(.dot),
            '-' => try self.addToken(.minus),
            '+' => try self.addToken(.plus),
            ';' => try self.addToken(.semicolon),
            '*' => try self.addToken(.star),

            // one or two characters
            '!' => try self.addToken(if (self.match('=')) .bang_eql else .bang),
            '=' => try self.addToken(if (self.match('=')) .eql_eql else .eql),
            '<' => try self.addToken(if (self.match('=')) .less_eql else .less),
            '>' => try self.addToken(if (self.match('=')) .greater_eql else .greater),

            '/' => {
                if (self.match('/')) {
                    // found a comment
                    while (self.peek() != '\n' and !self.atEnd()) _ = self.advance();
                } else {
                    try self.addToken(.slash);
                }
            },

            '"' => try self.scanString(result),

            '0'...'9' => try self.scanNumber(result),

            // ignore whitespace
            ' ', '\r', '\t' => {},

            '\n' => self.current_line += 1,

            else => {
                result.had_error = true;
                ux.printUserErrorWhere(self.current_line, self.currentLexeme(), "Unexpected character");
            },
        }
    }

    fn scanString(self: *Lexer, result: *ux.Result) !void {
        while (self.peek() != '"' and !self.atEnd()) {
            if (self.peek() == '\n') self.current_line += 1;
            _ = self.advance();
        }

        if (self.atEnd()) {
            result.had_error = true;
            ux.printUserErrorWhere(self.current_line, self.currentLexeme(), "Unterminated string");
        }

        _ = self.advance(); // the closing "

        try self.addToken(.string);
    }

    fn scanNumber(self: *Lexer, result: *ux.Result) !void {
        // XXX: not handling error cases?
        _ = result;

        while (std.ascii.isDigit(self.peek())) _ = self.advance();

        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            // consume the '.'
            _ = self.advance();

            while (std.ascii.isDigit(self.peek())) _ = self.advance();
        }

        try self.addToken(.number);
    }

    fn advance(self: *Lexer) u8 {
        const c = self.peek();
        self.current_lexeme_char += 1;
        return c;
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.atEnd()) return false;
        if (self.peek() != expected) return false;
        _ = self.advance();
        return true;
    }

    fn peek(self: Lexer) u8 {
        if (self.atEnd()) return '\x00';
        return self.source[@intCast(usize, self.current_lexeme_char)];
    }

    fn peekNext(self: Lexer) u8 {
        if (self.current_lexeme_char + 1 >= self.source.len) return '\x00';
        return self.source[@intCast(usize, self.current_lexeme_char + 1)];
    }

    fn atEnd(self: Lexer) bool {
        return self.current_lexeme_char >= self.source.len;
    }

    fn addToken(self: *Lexer, typ: TokenType) !void {
        try self.tokens.append(Token{
            .typ = typ,
            .lexeme = self.currentLexeme(),
            .line = self.current_line,
        });
        const new_token = &self.tokens.items[self.tokens.items.len - 1];
        log.debug("addToken {s}: {s}", .{ @tagName(new_token.typ), new_token.lexeme });
    }

    fn currentLexeme(self: Lexer) []const u8 {
        return self.source[@intCast(usize, self.current_lexeme_start)..@intCast(usize, self.current_lexeme_char)];
    }
};

fn testLexer(
    comptime text: []const u8,
    comptime expected_tokens: []const TokenType,
    comptime expected_lexemes: []const []const u8,
) !void {
    const alctr = std.testing.allocator;

    var lexer = Lexer.init(text, alctr);
    defer lexer.deinit();

    const result = try lexer.scanTokens();
    try std.testing.expectEqual(false, result.had_error);

    if (expected_tokens.len != expected_lexemes.len) {
        @compileError("Mismatched lexer test lengths");
    }

    for (expected_tokens, 0..) |token, i| {
        errdefer std.debug.print("i was {}\n", .{i});
        try std.testing.expectEqual(@as(TokenType, token), lexer.tokens.items[i].typ);
    }

    for (expected_lexemes, 0..) |lexeme, i| {
        errdefer std.debug.print("i was {}\n", .{i});
        try std.testing.expectEqualStrings(lexeme, lexer.tokens.items[i].lexeme);
    }
}

test "scan test 1" {
    const text = "\"hello\" != \"world\"";

    try testLexer(
        text,
        &[_]TokenType{ .string, .bang_eql, .string },
        &[_][]const u8{ "\"hello\"", "!=", "\"world\"" },
    );
}

test "scan test 2" {
    const text =
        \\"hello" != "world"
        \\
    ;

    try testLexer(
        text,
        &[_]TokenType{ .string, .bang_eql, .string },
        &[_][]const u8{ "\"hello\"", "!=", "\"world\"" },
    );
}

test "scan test 3" {
    const text =
        \\"hello" != "world"
        \\// this is a comment and will be ignored
        \\(( ))
    ;

    try testLexer(
        text,
        &[_]TokenType{ .string, .bang_eql, .string, .lparen, .lparen, .rparen, .rparen },
        &[_][]const u8{ "\"hello\"", "!=", "\"world\"", "(", "(", ")", ")" },
    );
}

test "scan test 4" {
    const text =
        \\((1 1.2 11.22))
    ;

    try testLexer(
        text,
        &[_]TokenType{ .lparen, .lparen, .number, .number, .number, .rparen, .rparen },
        &[_][]const u8{ "(", "(", "1", "1.2", "11.22", ")", ")" },
    );
}

const std = @import("std");
const ux = @import("ux.zig");

const log = std.log.scoped(.lex);
