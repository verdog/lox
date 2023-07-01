pub const std_options = struct {
    pub const log_level = if (@import("builtin").mode == .Debug) .debug else .info;
};

pub fn main() !void {
    defer if (!gpa.detectLeaks()) log.debug("no leaks", .{});

    if (std.os.argv.len > 2) {
        try ux.out.print("Usage: {s} <script>\n", .{std.os.argv[0]});
        log.err("invalid arguments {s}", .{std.os.argv});
        std.os.exit(64);
    } else if (std.os.argv.len == 2) {
        try runFile(std.mem.sliceTo(std.os.argv[1], '\x00'));
    } else {
        try runPrompt();
    }
}

fn runFile(path: []const u8) !void {
    const file = std.fs.cwd().openFile(path, .{}) catch |e| switch (e) {
        error.FileNotFound => {
            ux.err.print("File not found: {s}\n", .{path}) catch {};
            log.err("file not found: {s}", .{path});
            return e;
        },
        else => return e,
    };
    defer file.close();

    const bytes = file.readToEndAlloc(heap, 1024 * 1024 * 1024) catch |e| switch (e) {
        error.FileTooBig => {
            ux.err.print("Only input files up to 1GB are supported\n", .{}) catch {};
            log.err("file too big: {s}", .{path});
            return e;
        },
        else => return e,
    };
    defer heap.free(bytes);

    _ = try run(bytes);
}

fn runPrompt() !void {
    var input_buffer = [_]u8{'\x00'} ** 512;

    while (true) {
        try ux.out.print("> ", .{});
        try ux.stdout_buffer.flush();
        const maybe_line = try ux.in.readUntilDelimiterOrEof(&input_buffer, '\n');
        if (maybe_line) |line| {
            _ = run(line) catch |e|
                log.info("failed to run line: {s}, {}", .{ line, e });
        } else {
            break;
        }
    }
}

fn run(bytes: []const u8) !ux.Result {
    log.debug("run({s})", .{bytes});

    var lexer = lex.Lexer.init(bytes, heap);
    defer lexer.deinit();

    const tokens = try lexer.scanTokens();
    defer heap.free(tokens);

    var parser = prs.Parser.init(tokens, heap);
    defer parser.deinit();

    const expr = try parser.parse();
    const string = prs.printAst(parser.pool.buf.items[expr.index], parser.pool, heap);
    defer heap.free(string);

    log.debug("{s}", .{string});
    return .{};
}

test "run all tests" {
    std.testing.refAllDecls(@This());
}

const std = @import("std");
const ux = @import("ux.zig");
const lex = @import("lex.zig");
const prs = @import("parse.zig");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var heap = gpa.allocator();
const log = std.log.scoped(.main);
