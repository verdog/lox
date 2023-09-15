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
            '<' => return s.make_token(if (s.match('=')) .less_eql else .less),
            '>' => return s.make_token(if (s.match('=')) .greater_eql else .greater),

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
    is_captured: bool,
};

const Upvalue = struct {
    index: u8,
    is_local: bool,
};

const Compiler = struct {
    function: *val.ObjFunction,
    locals: [locals_capacity]Local,
    locals_count: i16,
    upvalues: [locals_capacity]Upvalue,
    scope_depth: i32,
    enclosing: ?*Compiler,

    const locals_capacity = std.math.maxInt(u8) + 1;
    const upvalues_capacity = locals_capacity;

    pub fn init(f: *val.ObjFunction, enclosing: ?*Compiler) Compiler {
        var c = Compiler{
            .function = f,
            .locals = undefined,
            .locals_count = 1, // slot 0 reserved below
            .upvalues = undefined,
            .scope_depth = 0,
            .enclosing = enclosing,
        };

        var reserved_local = &c.locals[0];
        reserved_local.depth = 0;
        reserved_local.name.lexeme = ""; // so the user can't refer to it
        reserved_local.is_captured = false;

        return c;
    }

    pub fn end(comp: *Compiler, line: i32) *val.ObjFunction {
        comp.emit_return(line);
        return comp.function;
    }

    pub fn print(comp: Compiler, source: []const u8, out: anytype) void {
        if (dbg.options.print_code) {
            dbg.Disassembler.border("constants table", out);
            {
                for (comp.function.chunk.constants.items, 0..) |c, i| {
                    out.print("{x:0>2}: ", .{i}) catch unreachable;
                    c.print(out);
                    out.print("\n", .{}) catch unreachable;
                }
            }

            dbg.Disassembler.border("compiled bytecode", out);
            {
                var it = std.mem.window(u8, comp.function.chunk.code.items, 16, 16);
                var offset: usize = 0;
                while (it.next()) |win| : (offset += 16) {
                    out.print("0x{x:0>4}: ", .{offset}) catch unreachable;
                    for (win) |b| {
                        out.print("{x:0>2} ", .{b}) catch unreachable;
                    }
                    out.print("\n", .{}) catch unreachable;
                }
            }

            const name = if (comp.function.ftype == .script) "<script>" else comp.function.name.buf;
            dbg.Disassembler.chunk(comp.function.chunk, name, source, out);
        }
    }

    pub fn resolve_local(c: *Compiler, name: Token) !i16 {
        var i = c.locals_count - 1;
        while (i >= 0) : (i -= 1) {
            const local = &c.locals[@intCast(i)];
            if (std.mem.eql(u8, name.lexeme, local.name.lexeme)) {
                if (local.depth == -1) {
                    return error.recursive_init;
                }
                return i;
            }
        }

        return -1;
    }

    pub fn add_local(c: *Compiler, name: Token) !void {
        if (c.locals_count == Compiler.locals_capacity) {
            return error.too_many_locals;
        }
        var new_local_mem = &c.locals[@intCast(c.locals_count)];
        c.locals_count += 1;
        new_local_mem.name = name;
        new_local_mem.depth = -1; // -1 means uninitialized
        new_local_mem.is_captured = false;
    }

    pub fn resolve_upvalue(c: *Compiler, name: Token) !i16 {
        if (c.enclosing) |enclosing| {
            const local = try enclosing.resolve_local(name);
            if (local != -1) {
                enclosing.locals[@intCast(local)].is_captured = true;
                return c.add_upvalue(@truncate(@as(u16, @intCast(local))), true);
            }

            const upvalue = try enclosing.resolve_upvalue(name);
            if (upvalue != -1)
                return c.add_upvalue(@truncate(@as(u16, @intCast(upvalue))), false);

            return -1;
        } else {
            return -1;
        }
    }

    pub fn add_upvalue(c: *Compiler, index: u8, is_local: bool) !i16 {
        var upvalue_count = c.function.upvalue_count;

        if (upvalue_count == Compiler.upvalues_capacity) {
            return error.too_many_upvalues;
        }

        for (0..upvalue_count) |i| {
            const upvalue = c.upvalues[i];
            // XXX: use std.mem.eql here?
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return @as(i16, @intCast(i));
            }
        }

        c.upvalues[upvalue_count].is_local = is_local;
        c.upvalues[upvalue_count].index = index;
        defer c.function.upvalue_count += 1;
        return c.function.upvalue_count;
    }

    pub fn emit_byte(comp: *Compiler, b: u8, line: i32) void {
        comp.function.chunk.write(b, line);
    }

    pub fn emit_constant(comp: *Compiler, value: val.Value, line: i32) void {
        comp.emit_byte(@intFromEnum(chk.OpCode.constant), line);
        comp.emit_byte(comp.function.chunk.add_constant(value), line);
    }

    pub fn emit_jump(comp: *Compiler, o: chk.OpCode, line: i32) i32 {
        comp.emit_byte(@intFromEnum(o), line);
        // emit placeholder to be patched later by patch_jump
        comp.emit_byte(0xff, line);
        comp.emit_byte(0xff, line);
        return @as(i32, @intCast(comp.function.chunk.code.items.len)) - 2;
    }

    pub fn patch_jump(comp: *Compiler, offset: i32) !void {
        // -2 to include the 2 byte jump amount itself, which will be read before the actual jump
        const jump_length = @as(usize, @intCast(@as(i32, @intCast(comp.function.chunk.code.items.len)) - offset - 2));

        if (jump_length > std.math.maxInt(u16)) {
            return error.jump_too_large;
        }

        comp.function.chunk.code.items[@intCast(offset)] = @truncate(jump_length >> 8);
        comp.function.chunk.code.items[@intCast(offset + 1)] = @truncate(jump_length);
    }

    pub fn emit_loop(comp: *Compiler, loop_start: usize, line: i32) !void {
        comp.emit_byte(@intFromEnum(chk.OpCode.loop), line);

        const offset = comp.function.chunk.code.items.len - loop_start + 2;
        if (offset > std.math.maxInt(u16)) return error.loop_body_too_large;

        comp.emit_byte(@truncate(offset >> 8), line);
        comp.emit_byte(@truncate(offset), line);
    }

    pub fn emit_return(comp: *Compiler, line: i32) void {
        comp.emit_byte(@intFromEnum(chk.OpCode.nil), line);
        comp.emit_byte(@intFromEnum(chk.OpCode.@"return"), line);
    }
};

fn Parser(comptime Context: type) type {
    return struct {
        scanner: Scanner,
        compiler: *Compiler,
        previous: Token,
        current: Token,
        pool: *val.ObjPool,
        had_error: bool,
        in_panic_mode: bool,
        ctx: Context,

        const P = @This();

        pub fn init(scanner: Scanner, comp: *Compiler, pool: *val.ObjPool, ctx: Context) P {
            return .{
                .scanner = scanner,
                .compiler = comp,
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
                .lparen => .{ .prefix = P.grouping, .infix = P.call, .precedence = .call },
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
            if (p.match(.fun)) {
                p.fun_declaration();
            } else if (p.match(.@"var")) {
                p.var_declaration();
            } else {
                p.statement();
            }

            if (p.in_panic_mode) p.synchronize();
        }

        fn fun_declaration(p: *P) void {
            const global = p.parse_variable("Expected function name.");
            p.mark_top_local_as_initialized();
            p.function(.function);
            p.define_variable(global);
        }

        fn var_declaration(p: *P) void {
            const global = p.parse_variable("Expected variable name.");

            if (p.match(.eql)) {
                p.expression();
            } else {
                p.emit_op(.nil);
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
                p.mark_top_local_as_initialized();
                return;
            }

            p.emit_bytes(@intFromEnum(chk.OpCode.define_global), global);
        }

        fn mark_top_local_as_initialized(p: *P) void {
            if (p.compiler.scope_depth == 0) return;
            p.compiler.locals[@intCast(p.compiler.locals_count - 1)].depth = p.compiler.scope_depth;
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
                        p.print_error_msg("Already a variable with this name in this scope.");
                    }
                }
            }

            p.add_local(name);
        }

        fn identifier_constant(p: *P, name: Token) u8 {
            return p.compiler.function.chunk.add_constant(p.pool.make_string_value(name.lexeme));
        }

        fn statement(p: *P) void {
            if (p.match(.print)) {
                p.print_statement();
            } else if (p.match(.@"for")) {
                p.for_statement();
            } else if (p.match(.@"if")) {
                p.if_statement();
            } else if (p.match(.@"return")) {
                p.return_statement();
            } else if (p.match(.@"while")) {
                p.while_statement();
            } else if (p.match(.lbrace)) {
                p.begin_scope();
                p.block_statement();
                p.end_scope();
            } else {
                p.expression_statement();
            }
        }

        fn print_statement(p: *P) void {
            p.expression();
            p.consume(.semicolon, "Expected ';' after value.");
            p.emit_op(.print);
        }

        fn expression_statement(p: *P) void {
            p.expression();
            p.consume(.semicolon, "Expected ';' after expression.");
            p.emit_op(.pop);
        }

        fn for_statement(p: *P) void {
            p.begin_scope();
            p.consume(.lparen, "Expected '(' after 'for'.");
            if (p.match(.semicolon)) {
                // no initializer present.
            } else if (p.match(.@"var")) {
                // includes semicolon
                p.var_declaration();
            } else {
                // includes semicolon and pop instruction
                p.expression_statement();
            }

            var loop_start = p.compiler.function.chunk.code.items.len;
            var exit_jump: i32 = -1;
            if (!p.match(.semicolon)) {
                p.expression();
                p.consume(.semicolon, "Expected ';' after loop condition.");

                // jump out of the loop if the condition is false
                exit_jump = p.emit_jump(.jump_if_false);
                p.emit_op(.pop);
            }

            if (!p.match(.rparen)) {
                const body_jump = p.emit_jump(.jump);
                const incr_start = p.compiler.function.chunk.code.items.len;
                p.expression();
                p.emit_op(.pop);
                p.consume(.rparen, "Expected ')' after for clauses.");

                p.emit_loop(loop_start) catch |e| p.print_error(e);
                loop_start = incr_start;
                p.patch_jump(body_jump) catch |e| p.print_error(e);
            }

            p.statement();
            p.emit_loop(loop_start) catch |e| p.print_error(e);

            if (exit_jump != -1) {
                p.patch_jump(exit_jump) catch |e| p.print_error(e);
                p.emit_op(.pop);
            }

            p.end_scope();
        }

        fn if_statement(p: *P) void {
            p.consume(.lparen, "Expected '(' after 'if'.");
            p.expression();
            p.consume(.rparen, "Expected ')' after condition.");

            const then_jump = p.emit_jump(.jump_if_false);
            p.emit_op(.pop); // clean up condition result

            p.statement(); // then block
            const else_jump = p.emit_jump(.jump);

            p.patch_jump(then_jump) catch |e| p.print_error(e);
            p.emit_op(.pop); // clean up condition result

            if (p.match(.@"else")) p.statement();

            p.patch_jump(else_jump) catch |e| p.print_error(e);
        }

        fn return_statement(p: *P) void {
            if (p.compiler.function.ftype == .script) {
                p.print_error_msg("Can't return from top-level code.");
            }

            if (p.match(.semicolon)) {
                p.emit_return();
            } else {
                p.expression();
                p.consume(.semicolon, "Expected ';' after return value.");
                p.emit_op(.@"return");
            }
        }

        fn while_statement(p: *P) void {
            const loop_start = p.compiler.function.chunk.code.items.len;
            p.consume(.lparen, "Expected '(' after 'if'.");
            p.expression();
            p.consume(.rparen, "Expected ')' after condition.");

            const exit_jump = p.emit_jump(.jump_if_false);
            p.emit_op(.pop); // clean up condition result
            p.statement(); // inner block
            p.emit_loop(loop_start) catch |e| p.print_error(e);

            p.patch_jump(exit_jump) catch |e| p.print_error(e);
            p.emit_op(.pop);
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
                if (p.compiler.locals[@intCast(p.compiler.locals_count - 1)].is_captured) {
                    p.emit_op(.close_upvalue);
                } else {
                    p.emit_op(.pop);
                }
                p.compiler.locals_count -= 1;
            }
        }

        pub fn variable(p: *P, can_assign: bool) void {
            p.named_variable(p.previous, can_assign);
        }

        fn named_variable(p: *P, name: Token, can_assign: bool) void {
            var arg: i16 = undefined;
            var get_op: u8 = undefined;
            var set_op: u8 = undefined;

            blk: {
                arg = p.resolve_local(name);
                if (arg != -1) {
                    get_op = @intFromEnum(chk.OpCode.get_local);
                    set_op = @intFromEnum(chk.OpCode.set_local);
                    break :blk;
                }

                arg = p.resolve_upvalue(name);
                if (arg != -1) {
                    get_op = @intFromEnum(chk.OpCode.get_upvalue);
                    set_op = @intFromEnum(chk.OpCode.set_upvalue);
                    break :blk;
                }

                arg = p.identifier_constant(name);
                get_op = @intFromEnum(chk.OpCode.get_global);
                set_op = @intFromEnum(chk.OpCode.set_global);
            }

            if (can_assign and p.match(.eql)) {
                p.expression();
                p.emit_bytes(set_op, @intCast(arg));
            } else {
                p.emit_bytes(get_op, @intCast(arg));
            }
        }

        fn resolve_local(p: *P, name: Token) i16 {
            return p.compiler.resolve_local(name) catch |e| {
                p.print_error(e);
                return -1;
            };
        }

        fn add_local(p: *P, name: Token) void {
            p.compiler.add_local(name) catch |e| p.print_error(e);
        }

        fn resolve_upvalue(p: *P, name: Token) i16 {
            return p.compiler.resolve_upvalue(name) catch |e| {
                p.print_error(e);
                return -1;
            };
        }

        pub fn expression(p: *P) void {
            p.parse_precedence(.assignment);
        }

        pub fn number(p: *P, can_assign: bool) void {
            _ = can_assign;
            const value: val.Value = .{ .number = std.fmt.parseFloat(f64, p.previous.lexeme) catch unreachable };
            p.emit_constant(value);
        }

        pub fn literal(p: *P, can_assign: bool) void {
            _ = can_assign;
            switch (p.previous.typ) {
                .false => p.emit_byte(@intFromEnum(chk.OpCode.false)),
                .true => p.emit_byte(@intFromEnum(chk.OpCode.true)),
                .nil => p.emit_byte(@intFromEnum(chk.OpCode.nil)),
                else => unreachable,
            }
        }

        pub fn string(p: *P, can_assign: bool) void {
            _ = can_assign;
            p.emit_constant(p.pool.make_string_value(p.previous.lexeme[1 .. p.previous.lexeme.len - 1]));
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
                .minus => p.emit_byte(@intFromEnum(chk.OpCode.negate)),
                .bang => p.emit_byte(@intFromEnum(chk.OpCode.not)),
                else => unreachable,
            }
        }

        pub fn binary(p: *P, can_assign: bool) void {
            _ = can_assign;
            const typ = p.previous.typ;
            const rule = p.get_rule(typ);
            p.parse_precedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

            switch (typ) {
                .plus => p.emit_byte(@intFromEnum(chk.OpCode.add)),
                .minus => p.emit_byte(@intFromEnum(chk.OpCode.subtract)),
                .star => p.emit_byte(@intFromEnum(chk.OpCode.multiply)),
                .slash => p.emit_byte(@intFromEnum(chk.OpCode.divide)),
                .bang_eql => p.emit_ops(.equal, .not),
                .eql_eql => p.emit_op(.equal),
                .greater => p.emit_op(.greater),
                .greater_eql => p.emit_ops(.less, .not),
                .less => p.emit_op(.less),
                .less_eql => p.emit_ops(.greater, .not),
                else => unreachable,
            }
        }

        fn @"and"(p: *P, can_assign: bool) void {
            _ = can_assign;
            const end_jump = p.emit_jump(.jump_if_false);
            p.emit_op(.pop);
            p.parse_precedence(.@"and");
            p.patch_jump(end_jump) catch |e| p.print_error(e);
        }

        fn @"or"(p: *P, can_assign: bool) void {
            _ = can_assign;
            const else_jump = p.emit_jump(.jump_if_false);
            const end_jump = p.emit_jump(.jump);

            p.patch_jump(else_jump) catch |e| p.print_error(e);
            p.emit_op(.pop);

            p.parse_precedence(.@"or");
            p.patch_jump(end_jump) catch |e| p.print_error(e);
        }

        fn function(p: *P, ftype: val.ObjFunction.Type) void {
            const fname = p.pool.make_string_value(p.previous.lexeme).as(val.ObjString);
            const f = p.pool.add(val.ObjFunction, .{ fname, ftype }).as(val.ObjFunction);
            var inner = Compiler.init(f, p.compiler);
            p.compiler = &inner;
            // no need to close this scope since we toss the whole compiler at the end
            p.begin_scope();

            p.consume(.lparen, "Expected '(' after function name.");
            if (!p.check(.rparen)) {
                var matched = true;
                while (matched) : (matched = p.match(.comma)) {
                    p.compiler.function.arity += 1;
                    if (p.compiler.function.arity > std.math.maxInt(u8)) {
                        p.print_error_at_current("Can't have more than 255 parameters.");
                    }
                    const constant = p.parse_variable("Expected parameter name.");
                    p.define_variable(constant);
                }
            }
            p.consume(.rparen, "Expected ')' after parameters.");
            p.consume(.lbrace, "Expected '{' before function body.");

            p.block_statement();

            var result = inner.end(p.previous.line);
            if (!p.had_error) {
                inner.print(p.scanner.source, p.ctx.out);
            }
            p.compiler = inner.enclosing.?;

            p.emit_bytes(
                @intFromEnum(chk.OpCode.closure),
                p.compiler.function.chunk.add_constant(val.Value.from(val.ObjFunction, result)),
            );

            for (0..result.upvalue_count) |i| {
                p.emit_bytes(
                    @intFromBool(inner.upvalues[i].is_local),
                    inner.upvalues[i].index,
                );
            }
        }

        fn call(p: *P, can_assign: bool) void {
            _ = can_assign;
            const arg_count = p.arg_list();
            p.emit_bytes(@intFromEnum(chk.OpCode.call), arg_count);
        }

        fn arg_list(p: *P) u8 {
            var arg_count: u8 = 0;
            if (!p.check(.rparen)) {
                var mtch = true;
                while (mtch) : (mtch = p.match(.comma)) {
                    p.expression();
                    if (arg_count == 255) p.print_error_msg("Can't have more than 255 arguments.");
                    // let the value overflow to prevent panic
                    arg_count +%= 1;
                }
            }
            p.consume(.rparen, "Expected ')' after arguments.");
            return arg_count;
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
                    p.print_error_msg("Expected expression.");
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
                p.print_error_msg("Invalid assignment target.");
            }
        }

        fn print_error(p: *P, e: anyerror) void {
            switch (e) {
                error.jump_too_large => p.print_error_msg("Jump too large."),
                error.loop_body_too_large => p.print_error_msg("Loop body too large."),
                error.recursive_init => p.print_error_msg("Can't initialize variable with itself."),
                else => {
                    std.debug.assert(false);
                    p.print_error_msg("Unknown error.");
                },
            }
        }

        fn print_error_msg(p: *P, message: []const u8) void {
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

        fn emit_byte(p: *P, b: u8) void {
            p.compiler.emit_byte(b, p.previous.line);
        }

        fn emit_bytes(p: *P, b1: u8, b2: u8) void {
            p.emit_byte(b1);
            p.emit_byte(b2);
        }

        fn emit_op(p: *P, o: chk.OpCode) void {
            p.emit_byte(@intFromEnum(o));
        }

        fn emit_ops(p: *P, o1: chk.OpCode, o2: chk.OpCode) void {
            p.emit_byte(@intFromEnum(o1));
            p.emit_byte(@intFromEnum(o2));
        }

        fn emit_constant(p: *P, value: val.Value) void {
            p.compiler.emit_constant(value, p.previous.line);
        }

        fn emit_jump(p: *P, o: chk.OpCode) i32 {
            return p.compiler.emit_jump(o, p.previous.line);
        }

        fn patch_jump(p: *P, offset: i32) !void {
            return p.compiler.patch_jump(offset);
        }

        fn emit_loop(p: *P, loop_start: usize) !void {
            return p.compiler.emit_loop(loop_start, p.previous.line);
        }

        fn emit_return(p: *P) void {
            p.compiler.emit_return(p.previous.line);
        }
    };
}

pub fn compile(source_text: []const u8, pool: *val.ObjPool, err_printer: anytype) !*val.ObjFunction {
    var s = Scanner.init(source_text);
    var outer_f = pool.add(val.ObjFunction, .{ undefined, .script }).as(val.ObjFunction);
    var comp = Compiler.init(outer_f, null);

    const ctx = .{ .out = usx.err };
    var p = Parser(@TypeOf(ctx)).init(s, &comp, pool, ctx);

    p.advance();
    while (!p.match(.eof)) {
        p.declaration();
    }

    const func = comp.end(p.previous.line);

    if (!p.had_error) {
        comp.print(source_text, err_printer);
    }

    if (p.had_error) return error.compile_error;
    return func;
}

const val = @import("value.zig");
const usx = @import("ux.zig");
const dbg = @import("debug.zig");
const tbl = @import("table.zig");
const chk = @import("chunk.zig");

const std = @import("std");
const log = std.log.scoped(.cpl);
