// zig fmt: off
pub const TokenType = enum {
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

pub const Token = struct {
    typ: TokenType,
    lexeme: []const u8,
    line: i32,
};

pub const Lexer = struct {
    source: []const u8,
    tokens: std.ArrayList(Token),

    current_lexeme_start: i32,
    current_lexeme_char: i32,
    current_line: i32,

    last_error: ?LexError = null,

    const LexError = error{
        UnexpectedCharacter,
        UnterminatedString,
    };

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

    pub fn scanTokens(self: *Lexer) LexError![]Token {
        while (!self.atEnd()) {
            self.current_lexeme_start = self.current_lexeme_char;
            self.scanToken();
        }

        self.tokens.append(Token{
            .typ = .eof,
            .lexeme = "",
            .line = self.current_line,
        }) catch @panic("OOM");

        if (self.last_error) |e| return e;

        return self.tokens.toOwnedSlice() catch @panic("OOM");
    }

    fn scanToken(self: *Lexer) void {
        const c = self.advance();

        switch (c) {
            // single character
            '(' => self.addToken(.lparen),
            ')' => self.addToken(.rparen),
            '{' => self.addToken(.lbrace),
            '}' => self.addToken(.rbrace),
            ',' => self.addToken(.comma),
            '.' => self.addToken(.dot),
            '-' => self.addToken(.minus),
            '+' => self.addToken(.plus),
            ';' => self.addToken(.semicolon),
            '*' => self.addToken(.star),

            // one or two characters
            '!' => self.addToken(if (self.match('=')) .bang_eql else .bang),
            '=' => self.addToken(if (self.match('=')) .eql_eql else .eql),
            '<' => self.addToken(if (self.match('=')) .less_eql else .less),
            '>' => self.addToken(if (self.match('=')) .greater_eql else .greater),

            '/' => {
                if (self.match('/')) {
                    // found a comment
                    while (self.peek() != '\n' and !self.atEnd()) _ = self.advance();
                } else {
                    self.addToken(.slash);
                }
            },

            '"' => self.scanString(),

            '0'...'9' => self.scanNumber(),

            // ignore whitespace
            ' ', '\r', '\t' => {},

            '\n' => self.current_line += 1,

            else => {
                if (std.ascii.isAlphabetic(c) or c == '_') {
                    self.scanWord();
                } else {
                    ux.printUserError(self.current_line, self.currentLexeme(), "Unexpected character");
                    self.last_error = LexError.UnexpectedCharacter;
                }
            },
        }
    }

    fn scanString(self: *Lexer) void {
        while (self.peek() != '"' and !self.atEnd()) {
            if (self.peek() == '\n') self.current_line += 1;
            _ = self.advance();
        }

        if (!self.atEnd()) {
            _ = self.advance(); // the closing "
            self.addToken(.string);
        } else {
            ux.printUserError(self.current_line, self.currentLexeme(), "Unterminated string");
            self.last_error = LexError.UnterminatedString;
        }
    }

    fn scanNumber(self: *Lexer) void {
        while (std.ascii.isDigit(self.peek())) _ = self.advance();

        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            // consume the '.'
            _ = self.advance();

            while (std.ascii.isDigit(self.peek())) _ = self.advance();
        }

        self.addToken(.number);
    }

    /// scan an identifier or a language keyword
    fn scanWord(self: *Lexer) void {
        while (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_') _ = self.advance();

        if (std.meta.stringToEnum(TokenType, self.currentLexeme())) |t| {
            self.addToken(t);
        } else {
            self.addToken(.identifier);
        }
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

    fn addToken(self: *Lexer, typ: TokenType) void {
        self.tokens.append(Token{
            .typ = typ,
            .lexeme = self.currentLexeme(),
            .line = self.current_line,
        }) catch @panic("OOM");
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

    const tokens = try lexer.scanTokens();
    defer alctr.free(tokens);

    if (expected_tokens.len != expected_lexemes.len) {
        @compileError("Mismatched lexer test lengths");
    }

    for (expected_tokens, 0..) |token, i| {
        errdefer std.debug.print("i was {}\n", .{i});
        try std.testing.expectEqual(@as(TokenType, token), tokens[i].typ);
    }

    for (expected_lexemes, 0..) |lexeme, i| {
        errdefer std.debug.print("i was {}\n", .{i});
        try std.testing.expectEqualStrings(lexeme, tokens[i].lexeme);
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

test "scan test 5" {
    const text =
        \\foo == bar
    ;

    try testLexer(
        text,
        &[_]TokenType{ .identifier, .eql_eql, .identifier },
        &[_][]const u8{ "foo", "==", "bar" },
    );
}

test "scan test 6" {
    const text =
        \\foo_bar == zig_zag_100
    ;

    try testLexer(
        text,
        &[_]TokenType{ .identifier, .eql_eql, .identifier },
        &[_][]const u8{ "foo_bar", "==", "zig_zag_100" },
    );
}

test "scan test 7" {
    const text =
        \\if (1 < 2) foo = bar * 12.23 else return
        \\while (true) class super this = false nil
    ;

    try testLexer(
        text,
        &[_]TokenType{ .@"if", .lparen, .number, .less, .number, .rparen, .identifier, .eql, .identifier, .star, .number, .@"else", .@"return", .@"while", .lparen, .true, .rparen, .class, .super, .this, .eql, .false, .nil },
        &[_][]const u8{ "if", "(", "1", "<", "2", ")", "foo", "=", "bar", "*", "12.23", "else", "return", "while", "(", "true", ")", "class", "super", "this", "=", "false", "nil" },
    );
}

test "scan test 8" {
    const text =
        \\foo_bar == 23
    ;

    try testLexer(
        text,
        &[_]TokenType{ .identifier, .eql_eql, .number },
        &[_][]const u8{ "foo_bar", "==", "23" },
    );
}

const std = @import("std");
const ux = @import("ux.zig");

const log = std.log.scoped(.lex);
