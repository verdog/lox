const Value = union(enum) {
    nil: void,
    booln: bool,
    number: f64,
    string: std.ArrayList(u8),

    pub fn deinit(v: Value) void {
        switch (v) {
            .string => |s| s.deinit(),
            else => {},
        }
    }

    pub fn toString(v: Value, alctr: std.mem.Allocator) []u8 {
        switch (v) {
            .nil => return alctr.dupe(u8, "(nil)") catch @panic("OOM"),
            .booln => |b| return if (b)
                alctr.dupe(u8, "true") catch @panic("OOM")
            else
                alctr.dupe(u8, "false") catch @panic("OOM"),
            .number => |n| return std.fmt.allocPrint(alctr, "{d:.4}", .{n}) catch @panic("OOM"),
            .string => |s| return std.fmt.allocPrint(alctr, "{s}", .{s.items}) catch @panic("OOM"),
        }
    }

    pub fn dupe(v: Value) Value {
        switch (v) {
            .string => |s| return Value{ .string = s.clone() catch @panic("OOM") },
            else => return v, // no heap memory to manage in other cases
        }
    }
};

const Environment = struct {
    alctr: std.mem.Allocator,
    values: std.StringArrayHashMap(Value),
    parent: ?*Environment,

    pub fn init(alctr: std.mem.Allocator) Environment {
        return .{
            .alctr = alctr,
            .values = std.StringArrayHashMap(Value).init(alctr),
            .parent = null,
        };
    }

    pub fn deinit(env: *Environment) void {
        {
            var i = env.values.iterator();
            while (i.next()) |pair| {
                env.alctr.free(pair.key_ptr.*);
                pair.value_ptr.deinit();
            }
        }
        env.values.deinit();
    }

    pub fn define(env: *Environment, name: []const u8, value: Value) void {
        var key = env.alctr.dupe(u8, name) catch @panic("OOM");
        var gop = env.values.getOrPut(key) catch @panic("OOM");
        if (gop.found_existing)
            env.alctr.free(key);
        gop.value_ptr.* = value;
    }

    pub fn assign(env: *Environment, name: []const u8, value: Value) InterpreterError!void {
        var m_value = env.values.getPtr(@constCast(name));
        if (m_value) |v_ptr| {
            v_ptr.deinit();
            v_ptr.* = value;
            return;
        }

        // not found
        if (env.parent) |parent| return try parent.assign(name, value);
        return InterpreterError.UndefinedVariable;
    }

    pub fn get(env: *Environment, name: []const u8) InterpreterError!Value {
        const maybe_value = env.values.get(@constCast(name));
        if (maybe_value) |val| return val;
        if (env.parent) |parent| return try parent.get(name);
        return InterpreterError.UndefinedVariable;
    }
};

pub const InterpreterError = error{
    InvalidOperand,
    UndefinedVariable,
    Unimplemented,
};

pub const Interpreter = struct {
    alctr: std.mem.Allocator,
    root_env: *Environment,
    current_env: *Environment,

    pub fn init(alctr: std.mem.Allocator) @This() {
        var root_env = alctr.create(Environment) catch @panic("OOM");
        root_env.* = Environment.init(alctr);
        return .{
            .alctr = alctr,
            .root_env = root_env,
            .current_env = root_env,
        };
    }

    pub fn deinit(intr: *@This()) void {
        intr.root_env.deinit();
        intr.alctr.destroy(intr.root_env);
    }

    /// implements visitor interface required by parse.Expr.acceptVisitor
    pub fn visit(intr: *@This(), node: anytype, pl: prs.Pool, ctx: anytype) InterpreterError!Value {
        switch (@TypeOf(node)) {
            prs.Expr => return try intr.visitExpr(node, pl, ctx),
            prs.Stmt => {
                try intr.visitStmt(node, pl, ctx);
                return Value{ .nil = {} };
            },
            else => @compileError("oops"),
        }
    }

    fn visitExpr(intr: *@This(), expr: prs.Expr, pl: prs.Pool, ctx: anytype) InterpreterError!Value {
        switch (expr) {
            .binary => |b| {
                var left = try pl.getExpr(b.left).acceptVisitor(pl, ctx, intr);
                defer left.deinit();
                var right = try pl.getExpr(b.right).acceptVisitor(pl, ctx, intr);
                defer right.deinit();

                switch (b.operator.typ) {
                    .plus => {
                        switch (left) {
                            .number => {
                                if (std.meta.activeTag(right) != .number)
                                    return InterpreterError.InvalidOperand;
                                return .{ .number = left.number + right.number };
                            },
                            .string => {
                                if (std.meta.activeTag(right) != .string)
                                    return InterpreterError.InvalidOperand;
                                var result: Value = .{ .string = left.string.clone() catch @panic("OOM") };
                                result.string.appendSlice(right.string.items) catch @panic("OOM");
                                return result;
                            },
                            else => {
                                return InterpreterError.InvalidOperand;
                            },
                        }
                    },
                    .minus => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .number = left.number - right.number };
                    },
                    .star => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .number = left.number * right.number };
                    },
                    .slash => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .number = left.number / right.number };
                    },
                    .greater => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .booln = left.number > right.number };
                    },
                    .greater_eql => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .booln = left.number >= right.number };
                    },
                    .less => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .booln = left.number < right.number };
                    },
                    .less_eql => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .booln = left.number <= right.number };
                    },
                    .eql_eql => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .booln = std.meta.eql(left, right) };
                    },
                    .bang_eql => |o| {
                        try assertNumberOperands(o, left, right);
                        return .{ .booln = !std.meta.eql(left, right) };
                    },

                    else => unreachable,
                }
            },
            .unary => |u| {
                var value = try pl.getExpr(u.right).acceptVisitor(pl, ctx, intr);
                defer value.deinit();

                switch (u.operator.typ) {
                    .minus => {
                        try assertNumberOperand(u.operator.typ, value);
                        return .{ .number = -value.number };
                    },
                    .bang => {
                        return .{ .booln = !isTruthy(value) };
                    },

                    else => unreachable,
                }
            },
            .grouping => |g| return try pl.getExpr(g.expression).acceptVisitor(pl, ctx, intr),
            .literal => |l| return try interpretTokenValue(l.value, ctx.alctr),
            .variable => |v| {
                const value = try intr.current_env.get(v.name.lexeme);
                return value.dupe();
            },
            .assign => |a| {
                const value = try pl.getExpr(a.right).acceptVisitor(pl, ctx, intr);
                // env owns value now
                try intr.current_env.assign(a.name.lexeme, value);
                return value.dupe();
            },
            .logical => |l| {
                const left = try pl.getExpr(l.left).acceptVisitor(pl, ctx, intr);

                if (l.operator.typ == .@"or") {
                    if (isTruthy(left)) return left;
                } else {
                    if (!isTruthy(left)) return left;
                }

                left.deinit();

                return try pl.getExpr(l.right).acceptVisitor(pl, ctx, intr);
            },
            .call => |_| unreachable,
        }
    }

    fn visitStmt(intr: *@This(), stmt: prs.Stmt, pl: prs.Pool, ctx: anytype) InterpreterError!void {
        switch (stmt) {
            .print => |print| {
                const value = try intr.visitExpr(pl.getExpr(print.expr), pl, ctx);
                defer value.deinit();
                const string = value.toString(ctx.alctr);
                defer ctx.alctr.free(string);
                ctx.out.print("{s}\n", .{string}) catch @panic("stdout failure");
                return;
            },
            .expr => |exprstmt| {
                var value = try intr.visitExpr(pl.getExpr(exprstmt.expr), pl, ctx);
                value.deinit();
                return;
            },
            .vari => |vari| {
                var value = Value{ .nil = {} };
                if (vari.initializer) |initr| {
                    value = try intr.visitExpr(pl.getExpr(initr), pl, ctx);
                }

                // environment owns the value now
                intr.current_env.define(vari.name.lexeme, value);
                return;
            },
            .block => |block| {
                var env = ctx.alctr.create(Environment) catch @panic("OOM");
                defer ctx.alctr.destroy(env);
                env.* = Environment.init(ctx.alctr);
                env.parent = intr.current_env;
                defer env.deinit();

                try intr.executeBlock(pl, block.statements, ctx, env);
                return;
            },
            .ifelse => |ifelse| {
                const condition_eval = try intr.visitExpr(pl.getExpr(ifelse.condition), pl, ctx);
                defer condition_eval.deinit();
                if (isTruthy(condition_eval)) {
                    try intr.visitStmt(pl.getStmt(ifelse.then), pl, ctx);
                } else if (ifelse.els) |els| {
                    try intr.visitStmt(pl.getStmt(els), pl, ctx);
                }
                return;
            },
            .whil => |whil| {
                var condition_eval = try intr.visitExpr(pl.getExpr(whil.condition), pl, ctx);
                defer condition_eval.deinit();
                while (isTruthy(condition_eval)) {
                    try intr.visitStmt(pl.getStmt(whil.do), pl, ctx);
                    condition_eval.deinit();
                    condition_eval = try intr.visitExpr(pl.getExpr(whil.condition), pl, ctx);
                }
                return;
            },
        }
    }

    fn executeBlock(intr: *@This(), pool: prs.Pool, stmts: []prs.Pool.StmtIndex, ctx: anytype, new_env: *Environment) !void {
        const previous_env = intr.current_env;
        intr.current_env = new_env;
        defer intr.current_env = previous_env;

        for (stmts) |stmt_index| {
            try intr.visitStmt(pool.getStmt(stmt_index), pool, ctx);
        }
    }

    fn interpretTokenValue(token: lex.Token, alctr: std.mem.Allocator) InterpreterError!Value {
        return switch (token.typ) {
            .number => .{ .number = std.fmt.parseFloat(f64, token.lexeme) catch return InterpreterError.InvalidOperand },
            .true => .{ .booln = true },
            .false => .{ .booln = false },
            .string => {
                // lexemes retain their quotes, so remove them
                return stringValueFromSlice(token.lexeme[1 .. token.lexeme.len - 1], alctr);
            },
            .identifier => return .{ .nil = {} }, // TODO
            .nil => return .{ .nil = {} },

            else => unreachable,
        };
    }

    fn isTruthy(v: Value) bool {
        // false and nil are falsey, everything else is truthy
        return switch (v) {
            .nil => false,
            .booln => |b| b,
            else => true,
        };
    }

    fn assertNumberOperand(operator: lex.TokenType, value: Value) !void {
        _ = operator;
        if (std.meta.activeTag(value) == .number) return;
        return InterpreterError.InvalidOperand;
    }

    fn assertNumberOperands(operator: lex.TokenType, left: Value, right: Value) !void {
        _ = operator;
        if (std.meta.activeTag(left) == .number and std.meta.activeTag(right) == .number) return;
        // TODO be more detailed
        return InterpreterError.InvalidOperand;
    }
};

fn stringValueFromSlice(string: []const u8, alctr: std.mem.Allocator) Value {
    const string_memory = alctr.dupe(u8, string) catch @panic("OOM");
    return .{ .string = std.ArrayList(u8).fromOwnedSlice(alctr, string_memory) };
}

// TODO convert these tests to the output checking form
//
// fn testInterpreter(
//     comptime text: []const u8,
//     result: Value,
// ) !void {
//     const alctr = std.testing.allocator;
//
//     var lexer = lex.Lexer.init(text, alctr);
//     defer lexer.deinit();
//
//     const tokens = try lexer.scanTokens();
//     defer alctr.free(tokens);
//
//     var parser = prs.Parser.init(tokens, alctr);
//     defer parser.deinit();
//
//     const stmts = try parser.parse();
//     try std.testing.expectEqual(stmts.len, 1);
//     defer alctr.free(stmts);
//
//     var interpreter = Interpreter.init(alctr);
//     defer interpreter.deinit();
//
//     var ctx = .{ .alctr = alctr, .out = std.io.null_writer };
//
//     for (stmts) |stmt| {
//         const interpreted_result = try parser.pool.getStmt(stmt).acceptVisitor(parser.pool, ctx, &interpreter);
//         defer interpreted_result.deinit();
//
//         switch (result) {
//             .string => |s| try std.testing.expectEqualStrings(s.items, interpreted_result.string.items),
//             else => {
//                 try std.testing.expectEqual(result, interpreted_result);
//             },
//         }
//     }
// }
//
// test "interpret: parse literals" {
//     try testInterpreter("true;", .{ .booln = true });
//     try testInterpreter("false;", .{ .booln = false });
//     try testInterpreter("1;", .{ .number = 1.0 });
//     try testInterpreter("1.1;", .{ .number = 1.1 });
//     try testInterpreter("3.1415;", .{ .number = 3.1415 });
//
//     {
//         const alctr = std.testing.allocator;
//         // note the removed quotes
//         const result = stringValueFromSlice("fee fie foh fum", alctr);
//         defer result.deinit();
//
//         try testInterpreter("\"fee fie foh fum\";", result);
//     }
// }
//
// test "interpret: simple literal groupings" {
//     try testInterpreter("(true);", .{ .booln = true });
//     try testInterpreter("(((((true)))));", .{ .booln = true });
//     try testInterpreter("(false);", .{ .booln = false });
//     try testInterpreter("(((((false)))));", .{ .booln = false });
//     try testInterpreter("(1);", .{ .number = 1.0 });
//     try testInterpreter("(((((1)))));", .{ .number = 1.0 });
//     try testInterpreter("(1.1);", .{ .number = 1.1 });
//     try testInterpreter("(((((1.1)))));", .{ .number = 1.1 });
//     try testInterpreter("(3.1415);", .{ .number = 3.1415 });
//     try testInterpreter("(((((3.1415)))));", .{ .number = 3.1415 });
//
//     {
//         const alctr = std.testing.allocator;
//         // note the removed quotes
//         const result = stringValueFromSlice("fee fie foh fum", alctr);
//         defer result.deinit();
//
//         try testInterpreter("(\"fee fie foh fum\");", result);
//         try testInterpreter("(((((\"fee fie foh fum\")))));", result);
//     }
// }
//
// test "interpret: unary operations" {
//     try testInterpreter("!true;", .{ .booln = false });
//     try testInterpreter("!false;", .{ .booln = true });
//     try testInterpreter("!!true;", .{ .booln = true });
//     try testInterpreter("!!false;", .{ .booln = false });
//
//     try testInterpreter("-1;", .{ .number = -1.0 });
//     try testInterpreter("-1.1;", .{ .number = -1.1 });
//     try testInterpreter("-3.1415;", .{ .number = -3.1415 });
// }
//
// test "interpret: binary operations" {
//     try testInterpreter("1 + 1;", .{ .number = 2 });
//     try testInterpreter("1 - 1;", .{ .number = 0 });
//     try testInterpreter("2 * 2;", .{ .number = 4 });
//     try testInterpreter("2 / 2;", .{ .number = 1 });
//
//     try testInterpreter("1 > 0;", .{ .booln = true });
//     try testInterpreter("1 >= 0;", .{ .booln = true });
//     try testInterpreter("1 >= 1;", .{ .booln = true });
//     try testInterpreter("1 < 2;", .{ .booln = true });
//     try testInterpreter("1 <= 2;", .{ .booln = true });
//     try testInterpreter("1 <= 1;", .{ .booln = true });
//     try testInterpreter("1 == 1;", .{ .booln = true });
//     try testInterpreter("1 != 2;", .{ .booln = true });
//
//     {
//         const alctr = std.testing.allocator;
//         const result = stringValueFromSlice("hello_world", alctr);
//         defer result.deinit();
//
//         try testInterpreter("\"hello_\" + \"world\";", result);
//     }
// }

fn testInterpreterOutput(
    comptime text: []const u8,
    result: []const u8,
) !void {
    const alctr = std.testing.allocator;

    var lexer = lex.Lexer.init(text, alctr);
    defer lexer.deinit();

    const tokens = try lexer.scanTokens();
    defer alctr.free(tokens);

    var parser = prs.Parser.init(tokens, alctr);
    defer parser.deinit();

    const stmts = try parser.parse();
    defer alctr.free(stmts);

    var out = std.ArrayList(u8).init(alctr);
    defer out.deinit();

    var interpreter = Interpreter.init(alctr);
    defer interpreter.deinit();

    var ctx = .{ .alctr = alctr, .out = out.writer() };

    for (stmts) |stmt| {
        const interpreted_result = try parser.pool.getStmt(stmt).acceptVisitor(parser.pool, ctx, &interpreter);
        defer interpreted_result.deinit();
    }

    try std.testing.expectEqualStrings(result, out.items);
}

test "interpret: global variable declarations" {
    const txt =
        \\var a = 5;
        \\var b = 10;
        \\var c = 15;
        \\print (a + b) + 2 * c;
    ;

    const output =
        \\45.0000
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpret: global variable declarations 2" {
    const txt =
        \\var a = 5;
        \\var b = 10;
        \\var c = 1;
        \\print (a + b) + 2 * c;
        \\var c = 15;
        \\var a = 15;
        \\print (a + b) + 2 * c;
    ;

    const output =
        \\17.0000
        \\55.0000
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpret: global variable declarations 3" {
    const txt =
        \\var a = 5; print a;
        \\var b = 10; print b;
    ;

    const output =
        \\5.0000
        \\10.0000
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpret: global variable declarations 4" {
    const txt =
        \\var a = 5;
        \\print a;
        \\var a = 10;
        \\print a;
    ;

    const output =
        \\5.0000
        \\10.0000
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpret: global variable assignment" {
    const txt =
        \\var a = 5;
        \\print a;
        \\a = 10;
        \\print a * 2;
    ;

    const output =
        \\5.0000
        \\20.0000
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpret: global variable assignment 2" {
    const txt =
        \\var a = 5;
        \\print a;
        \\print a = 10;
        \\print a;
    ;

    const output =
        \\5.0000
        \\10.0000
        \\10.0000
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpret: global variable assignment 3" {
    const txt =
        \\var a = 5;
        \\var b = 10;
        \\print a;
        \\print b;
        \\a = b = 3;
        \\print a;
        \\print b;
    ;

    const output =
        \\5.0000
        \\10.0000
        \\3.0000
        \\3.0000
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpret: global variable assignment 4" {
    const txt =
        \\var a = 5;
        \\var b = 10;
        \\print a;
        \\print b;
        \\print a = b = 3 + 1;
        \\print a;
        \\print b;
    ;

    const output =
        \\5.0000
        \\10.0000
        \\4.0000
        \\4.0000
        \\4.0000
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpret: global variable assignment 5" {
    const txt =
        \\var a = 5;
        \\var b = 10;
        \\print a;
        \\print b;
        \\print a = (b = 3) + 1;
        \\print a;
        \\print b;
    ;

    const output =
        \\5.0000
        \\10.0000
        \\4.0000
        \\4.0000
        \\3.0000
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "environment: define/get" {
    var alctr = std.testing.allocator;
    var env = Environment.init(alctr);
    defer env.deinit();

    const value = Value{ .number = 123.456 };

    env.define("foo", value);
    const lookup = try env.get("foo");
    try std.testing.expectEqual(value, lookup);
}

test "environment: define/define/get" {
    var alctr = std.testing.allocator;
    var env = Environment.init(alctr);
    defer env.deinit();

    const value1 = Value{ .number = 123.456 };
    const value2 = Value{ .number = 789 };

    env.define("foo", value1);
    {
        const lookup = try env.get("foo");
        try std.testing.expectEqual(value1, lookup);
    }
    env.define("foo", value2);
    {
        const lookup = try env.get("foo");
        try std.testing.expectEqual(value2, lookup);
    }
}

test "environment: define/assign/get" {
    var alctr = std.testing.allocator;
    var env = Environment.init(alctr);
    defer env.deinit();

    const value1 = Value{ .number = 123.456 };
    const value2 = Value{ .number = 789 };

    env.define("foo", value1);
    {
        const lookup = try env.get("foo");
        try std.testing.expectEqual(value1, lookup);
    }
    try env.assign("foo", value2);
    {
        const lookup = try env.get("foo");
        try std.testing.expectEqual(value2, lookup);
    }
}

test "environment: parents" {
    var alctr = std.testing.allocator;
    var root = Environment.init(alctr);
    defer root.deinit();
    var leaf = Environment.init(alctr);
    defer leaf.deinit();
    leaf.parent = &root;

    const value1 = Value{ .number = 123 };
    const value2 = Value{ .number = 456 };

    root.define("foo", value1);
    leaf.define("bar", value2);

    {
        const lookup = try root.get("foo");
        try std.testing.expectEqual(value1, lookup);
    }
    {
        const lookup = try leaf.get("foo");
        try std.testing.expectEqual(value1, lookup);
    }
    {
        // not in root scope
        try std.testing.expectError(InterpreterError.UndefinedVariable, root.get("bar"));
    }
    {
        const lookup = try leaf.get("bar");
        try std.testing.expectEqual(value2, lookup);
    }
}

test "environment: shadowing" {
    var alctr = std.testing.allocator;
    var root = Environment.init(alctr);
    defer root.deinit();
    var leaf = Environment.init(alctr);
    defer leaf.deinit();
    leaf.parent = &root;

    const value1 = Value{ .number = 123 };
    const value2 = Value{ .number = 456 };

    root.define("foo", value1);
    leaf.define("foo", value2); // shadowing value1

    {
        const lookup = try root.get("foo");
        try std.testing.expectEqual(value1, lookup);
    }
    {
        const lookup = try leaf.get("foo");
        try std.testing.expectEqual(value2, lookup);
    }
}

test "interpreter: scopes 1" {
    const txt =
        \\{
        \\var a = 2;
        \\print a;
        \\}
    ;

    const output =
        \\2.0000
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpreter: scopes 2" {
    const txt =
        \\{
        \\var a = "inner a";
        \\print a;
        \\}
    ;

    const output =
        \\inner a
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpreter: scopes 3" {
    const txt =
        \\{
        \\var a = "inner a";
        \\var b = a;
        \\print a + b;
        \\}
    ;

    const output =
        \\inner ainner a
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpreter: scopes 4" {
    const txt =
        \\var a = "global a";
        \\{
        \\  var a = "outer a";
        \\  {
        \\    var a = "inner a";
        \\    print a;
        \\  }
        \\  print a;
        \\}
        \\print a;
    ;

    const output =
        \\inner a
        \\outer a
        \\global a
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpreter: scopes 5" {
    const txt =
        \\var a = "global a";
        \\var b = "global b";
        \\var c = "global c";
        \\{
        \\  var a = "outer a";
        \\  var b = "outer b";
        \\  {
        \\    var a = "inner a";
        \\    print a;
        \\    print b;
        \\    print c;
        \\  }
        \\  print a;
        \\  print b;
        \\  print c;
        \\}
        \\print a;
        \\print b;
        \\print c;
    ;

    const output =
        \\inner a
        \\outer b
        \\global c
        \\outer a
        \\outer b
        \\global c
        \\global a
        \\global b
        \\global c
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpreter: if statements 1" {
    const txt =
        \\if (true) {
        \\print "it true";
        \\}
    ;

    const output =
        \\it true
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpreter: if statements 2" {
    const txt =
        \\if (false) {
        \\print "it true";
        \\} else {
        \\print "it false";
        \\}
    ;

    const output =
        \\it false
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpreter: if statements 3" {
    const txt =
        \\var truthy = 999;
        \\if (truthy) {
        \\print "it true";
        \\} else {
        \\print "it false";
        \\}
    ;

    const output =
        \\it true
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpreter: logical 1" {
    const txt =
        \\print "hi" or 2;
        \\print nil or "yes";
        \\print nil or nil or 2 or 3;
    ;

    const output =
        \\hi
        \\yes
        \\2.0000
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpreter: logical 2" {
    const txt =
        \\print nil and 2;
        \\print "yes" and nil;
        \\print "yes" and "yes" and nil;
    ;

    const output =
        \\(nil)
        \\(nil)
        \\(nil)
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpreter: while loops 1" {
    const txt =
        \\var b = 0;
        \\while (b < 3) {
        \\  print b;
        \\  b = b + 1;
        \\}
    ;

    const output =
        \\0.0000
        \\1.0000
        \\2.0000
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpreter: loops 2" {
    const txt =
        \\for (var b = 0; b < 3; b = b + 1) {
        \\  print b;
        \\}
    ;

    const output =
        \\0.0000
        \\1.0000
        \\2.0000
        \\
    ;

    try testInterpreterOutput(txt, output);
}

test "interpreter: loops 3" {
    const txt =
        \\var a = 0;
        \\var temp;
        \\
        \\for (var b = 1; a < 10000; b = temp + b) {
        \\  print a;
        \\  temp = a;
        \\  a = b;
        \\}
    ;

    const output =
        \\0.0000
        \\1.0000
        \\1.0000
        \\2.0000
        \\3.0000
        \\5.0000
        \\8.0000
        \\13.0000
        \\21.0000
        \\34.0000
        \\55.0000
        \\89.0000
        \\144.0000
        \\233.0000
        \\377.0000
        \\610.0000
        \\987.0000
        \\1597.0000
        \\2584.0000
        \\4181.0000
        \\6765.0000
        \\
    ;

    try testInterpreterOutput(txt, output);
}

const std = @import("std");
const log = std.log.scoped(.interpret);

const ux = @import("ux.zig");
const prs = @import("parse.zig");
const lex = @import("lex.zig");
