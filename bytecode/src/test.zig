//! runs tests from the official lox repository

const Expected = struct {
    output: []const u8,
    compile_error: []const u8,
    runtime_error: []const u8,

    pub fn deinit(e: Expected, alctr: std.mem.Allocator) void {
        alctr.free(e.output);
        alctr.free(e.compile_error);
        alctr.free(e.runtime_error);
    }
};

fn matches(source: []const u8, needle: []const u8) bool {
    return std.mem.eql(u8, source[0..@min(needle.len, source.len)], needle);
}

fn parse_test(text: []const u8, alctr: std.mem.Allocator) Expected {
    const expect_prefix = "// expect: ";
    const line_error_prefix = "// [line ";
    const c_line_error_prefix = "// [c line ";
    const error_prefix = "// Error";
    const runtime_error_prefix = "// expect runtime error: ";

    var output_buffer = std.ArrayList(u8).init(alctr);
    var compile_error_buffer = std.ArrayList(u8).init(alctr);
    var runtime_error_buffer = std.ArrayList(u8).init(alctr);

    var line: usize = 1;
    var i: usize = 0;

    while (i < text.len) : (i += 1) {
        if (matches(text[i..], expect_prefix)) {
            i += expect_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, text, i, '\n') orelse text.len;
            output_buffer.appendSlice(text[i..j]) catch @panic("OOM");
            output_buffer.append('\n') catch @panic("OOM");
            i = j;
        } else if (matches(text[i..], line_error_prefix)) {
            i += line_error_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, text, i, '\n') orelse text.len;
            compile_error_buffer.appendSlice("[line ") catch @panic("OOM");
            compile_error_buffer.appendSlice(text[i..j]) catch @panic("OOM");
            compile_error_buffer.append('\n') catch @panic("OOM");
            i = j;
        } else if (matches(text[i..], error_prefix)) {
            i += error_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, text, i, '\n') orelse text.len;
            compile_error_buffer.writer().print("[line {}] Error", .{line}) catch @panic("OOM");
            compile_error_buffer.appendSlice(text[i..j]) catch @panic("OOM");
            compile_error_buffer.append('\n') catch @panic("OOM");
            i = j;
        } else if (matches(text[i..], c_line_error_prefix)) {
            i += c_line_error_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, text, i, '\n') orelse text.len;
            compile_error_buffer.appendSlice("[line ") catch @panic("OOM");
            compile_error_buffer.appendSlice(text[i..j]) catch @panic("OOM");
            compile_error_buffer.append('\n') catch @panic("OOM");
            i = j;
        } else if (matches(text[i..], runtime_error_prefix)) {
            i += runtime_error_prefix.len;
            const j = std.mem.indexOfScalarPos(u8, text, i, '\n') orelse text.len;
            runtime_error_buffer.appendSlice(text[i..j]) catch @panic("OOM");
            // Append start of stack trace to runtime error message
            runtime_error_buffer.writer().print("\n[line {}]", .{line}) catch @panic("OOM");
            i = j;
        }

        if (i < text.len and text[i] == '\n') line += 1;
    }

    return .{
        .output = output_buffer.toOwnedSlice() catch @panic("OOM"),
        .compile_error = compile_error_buffer.toOwnedSlice() catch @panic("OOM"),
        .runtime_error = runtime_error_buffer.toOwnedSlice() catch @panic("OOM"),
    };
}

fn run_test(text: []const u8) !void {
    var vm: VM = undefined;
    vm.init_in_place(std.testing.allocator);
    defer vm.deinit();

    const expected = parse_test(text, std.testing.allocator);
    defer expected.deinit(std.testing.allocator);

    var out_buf = std.ArrayList(u8).init(std.testing.allocator);
    defer out_buf.deinit();

    var err_buf = std.ArrayList(u8).init(std.testing.allocator);
    defer err_buf.deinit();

    var out = out_buf.writer();
    var err = err_buf.writer();
    var outs = .{ .out = out, .err = err };

    vm.interpret(text, std.testing.allocator, outs) catch |e| switch (e) {
        error.compile_error => {
            try std.testing.expectEqualStrings(expected.compile_error, out_buf.items);
            return;
        },
        error.runtime_error => {
            // only check that the error message starts with expected, since we can't know
            // at compile time how deep the actual stack trace will be
            try std.testing.expectEqualStrings(
                expected.runtime_error,
                err_buf.items[0..expected.runtime_error.len],
            );
            return;
        },
    };

    if (expected.compile_error.len == 0 and expected.runtime_error.len == 0) {
        // check runtime output
        try std.testing.expectEqualStrings(expected.output, out_buf.items);
    } else {
        // should have errored
        return error.TestUnexpectedResult;
    }
}

test "./if/fun_in_then.lox" {
    try run_test(@embedFile("./test/if/fun_in_then.lox"));
}

test "./if/var_in_else.lox" {
    try run_test(@embedFile("./test/if/var_in_else.lox"));
}

test "./if/fun_in_else.lox" {
    try run_test(@embedFile("./test/if/fun_in_else.lox"));
}

test "./if/var_in_then.lox" {
    try run_test(@embedFile("./test/if/var_in_then.lox"));
}

test "./if/else.lox" {
    try run_test(@embedFile("./test/if/else.lox"));
}

test "./if/class_in_then.lox" {
    try run_test(@embedFile("./test/if/class_in_then.lox"));
}

test "./if/class_in_else.lox" {
    try run_test(@embedFile("./test/if/class_in_else.lox"));
}

test "./if/dangling_else.lox" {
    try run_test(@embedFile("./test/if/dangling_else.lox"));
}

test "./if/if.lox" {
    try run_test(@embedFile("./test/if/if.lox"));
}

test "./if/truth.lox" {
    try run_test(@embedFile("./test/if/truth.lox"));
}

test "./method/parse.lox" {
    try run_test(@embedFile("./test/method/parse.lox"));
}

test "./method/not_found.lox" {
    try run_test(@embedFile("./test/method/not_found.lox"));
}

test "./method/too_many_parameters.lox" {
    try run_test(@embedFile("./test/method/too_many_parameters.lox"));
}

test "./method/arity.lox" {
    try run_test(@embedFile("./test/method/arity.lox"));
}

test "./method/empty_block.lox" {
    try run_test(@embedFile("./test/method/empty_block.lox"));
}

test "./method/refer_to_name.lox" {
    try run_test(@embedFile("./test/method/refer_to_name.lox"));
}

test "./method/missing_arguments.lox" {
    try run_test(@embedFile("./test/method/missing_arguments.lox"));
}

test "./method/too_many_arguments.lox" {
    try run_test(@embedFile("./test/method/too_many_arguments.lox"));
}

test "./method/print_bound_method.lox" {
    try run_test(@embedFile("./test/method/print_bound_method.lox"));
}

test "./method/extra_arguments.lox" {
    try run_test(@embedFile("./test/method/extra_arguments.lox"));
}

test "./for/fun_in_body.lox" {
    try run_test(@embedFile("./test/for/fun_in_body.lox"));
}

test "./for/statement_initializer.lox" {
    try run_test(@embedFile("./test/for/statement_initializer.lox"));
}

test "./for/return_closure.lox" {
    try run_test(@embedFile("./test/for/return_closure.lox"));
}

test "./for/statement_condition.lox" {
    try run_test(@embedFile("./test/for/statement_condition.lox"));
}

test "./for/var_in_body.lox" {
    try run_test(@embedFile("./test/for/var_in_body.lox"));
}

test "./for/statement_increment.lox" {
    try run_test(@embedFile("./test/for/statement_increment.lox"));
}

test "./for/scope.lox" {
    try run_test(@embedFile("./test/for/scope.lox"));
}

test "./for/class_in_body.lox" {
    try run_test(@embedFile("./test/for/class_in_body.lox"));
}

test "./for/syntax.lox" {
    try run_test(@embedFile("./test/for/syntax.lox"));
}

test "./for/closure_in_body.lox" {
    try run_test(@embedFile("./test/for/closure_in_body.lox"));
}

test "./for/return_inside.lox" {
    try run_test(@embedFile("./test/for/return_inside.lox"));
}

test "./inheritance/inherit_from_number.lox" {
    try run_test(@embedFile("./test/inheritance/inherit_from_number.lox"));
}

test "./inheritance/inherit_methods.lox" {
    try run_test(@embedFile("./test/inheritance/inherit_methods.lox"));
}

test "./inheritance/set_fields_from_base_class.lox" {
    try run_test(@embedFile("./test/inheritance/set_fields_from_base_class.lox"));
}

test "./inheritance/inherit_from_function.lox" {
    try run_test(@embedFile("./test/inheritance/inherit_from_function.lox"));
}

test "./inheritance/constructor.lox" {
    try run_test(@embedFile("./test/inheritance/constructor.lox"));
}

test "./inheritance/parenthesized_superclass.lox" {
    try run_test(@embedFile("./test/inheritance/parenthesized_superclass.lox"));
}

test "./inheritance/inherit_from_nil.lox" {
    try run_test(@embedFile("./test/inheritance/inherit_from_nil.lox"));
}

test "./operator/add_bool_string.lox" {
    try run_test(@embedFile("./test/operator/add_bool_string.lox"));
}

test "./operator/subtract_nonnum_num.lox" {
    try run_test(@embedFile("./test/operator/subtract_nonnum_num.lox"));
}

test "./operator/equals.lox" {
    try run_test(@embedFile("./test/operator/equals.lox"));
}

test "./operator/subtract.lox" {
    try run_test(@embedFile("./test/operator/subtract.lox"));
}

test "./operator/greater_nonnum_num.lox" {
    try run_test(@embedFile("./test/operator/greater_nonnum_num.lox"));
}

test "./operator/multiply_num_nonnum.lox" {
    try run_test(@embedFile("./test/operator/multiply_num_nonnum.lox"));
}

test "./operator/not.lox" {
    try run_test(@embedFile("./test/operator/not.lox"));
}

test "./operator/less_nonnum_num.lox" {
    try run_test(@embedFile("./test/operator/less_nonnum_num.lox"));
}

test "./operator/add_nil_nil.lox" {
    try run_test(@embedFile("./test/operator/add_nil_nil.lox"));
}

test "./operator/add.lox" {
    try run_test(@embedFile("./test/operator/add.lox"));
}

test "./operator/negate.lox" {
    try run_test(@embedFile("./test/operator/negate.lox"));
}

test "./operator/comparison.lox" {
    try run_test(@embedFile("./test/operator/comparison.lox"));
}

test "./operator/multiply_nonnum_num.lox" {
    try run_test(@embedFile("./test/operator/multiply_nonnum_num.lox"));
}

test "./operator/equals_method.lox" {
    try run_test(@embedFile("./test/operator/equals_method.lox"));
}

test "./operator/greater_or_equal_nonnum_num.lox" {
    try run_test(@embedFile("./test/operator/greater_or_equal_nonnum_num.lox"));
}

test "./operator/less_or_equal_num_nonnum.lox" {
    try run_test(@embedFile("./test/operator/less_or_equal_num_nonnum.lox"));
}

test "./operator/subtract_num_nonnum.lox" {
    try run_test(@embedFile("./test/operator/subtract_num_nonnum.lox"));
}

test "./operator/greater_num_nonnum.lox" {
    try run_test(@embedFile("./test/operator/greater_num_nonnum.lox"));
}

test "./operator/add_num_nil.lox" {
    try run_test(@embedFile("./test/operator/add_num_nil.lox"));
}

test "./operator/add_bool_nil.lox" {
    try run_test(@embedFile("./test/operator/add_bool_nil.lox"));
}

test "./operator/divide.lox" {
    try run_test(@embedFile("./test/operator/divide.lox"));
}

test "./operator/less_or_equal_nonnum_num.lox" {
    try run_test(@embedFile("./test/operator/less_or_equal_nonnum_num.lox"));
}

test "./operator/divide_nonnum_num.lox" {
    try run_test(@embedFile("./test/operator/divide_nonnum_num.lox"));
}

test "./operator/not_class.lox" {
    try run_test(@embedFile("./test/operator/not_class.lox"));
}

test "./operator/add_string_nil.lox" {
    try run_test(@embedFile("./test/operator/add_string_nil.lox"));
}

test "./operator/multiply.lox" {
    try run_test(@embedFile("./test/operator/multiply.lox"));
}

test "./operator/not_equals.lox" {
    try run_test(@embedFile("./test/operator/not_equals.lox"));
}

test "./operator/negate_nonnum.lox" {
    try run_test(@embedFile("./test/operator/negate_nonnum.lox"));
}

test "./operator/divide_num_nonnum.lox" {
    try run_test(@embedFile("./test/operator/divide_num_nonnum.lox"));
}

test "./operator/greater_or_equal_num_nonnum.lox" {
    try run_test(@embedFile("./test/operator/greater_or_equal_num_nonnum.lox"));
}

test "./operator/add_bool_num.lox" {
    try run_test(@embedFile("./test/operator/add_bool_num.lox"));
}

test "./operator/less_num_nonnum.lox" {
    try run_test(@embedFile("./test/operator/less_num_nonnum.lox"));
}

test "./operator/equals_class.lox" {
    try run_test(@embedFile("./test/operator/equals_class.lox"));
}

test "./unexpected_character.lox" {
    try run_test(@embedFile("./test/unexpected_character.lox"));
}

test "./empty_file.lox" {
    try run_test(@embedFile("./test/empty_file.lox"));
}

test "./return/return_nil_if_no_value.lox" {
    try run_test(@embedFile("./test/return/return_nil_if_no_value.lox"));
}

test "./return/after_else.lox" {
    try run_test(@embedFile("./test/return/after_else.lox"));
}

test "./return/in_method.lox" {
    try run_test(@embedFile("./test/return/in_method.lox"));
}

test "./return/after_while.lox" {
    try run_test(@embedFile("./test/return/after_while.lox"));
}

test "./return/in_function.lox" {
    try run_test(@embedFile("./test/return/in_function.lox"));
}

test "./return/after_if.lox" {
    try run_test(@embedFile("./test/return/after_if.lox"));
}

test "./return/at_top_level.lox" {
    try run_test(@embedFile("./test/return/at_top_level.lox"));
}

test "./variable/in_nested_block.lox" {
    try run_test(@embedFile("./test/variable/in_nested_block.lox"));
}

test "./variable/scope_reuse_in_different_blocks.lox" {
    try run_test(@embedFile("./test/variable/scope_reuse_in_different_blocks.lox"));
}

test "./variable/use_local_in_initializer.lox" {
    try run_test(@embedFile("./test/variable/use_local_in_initializer.lox"));
}

test "./variable/use_false_as_var.lox" {
    try run_test(@embedFile("./test/variable/use_false_as_var.lox"));
}

test "./variable/redefine_global.lox" {
    try run_test(@embedFile("./test/variable/redefine_global.lox"));
}

test "./variable/collide_with_parameter.lox" {
    try run_test(@embedFile("./test/variable/collide_with_parameter.lox"));
}

test "./variable/shadow_and_local.lox" {
    try run_test(@embedFile("./test/variable/shadow_and_local.lox"));
}

test "./variable/duplicate_local.lox" {
    try run_test(@embedFile("./test/variable/duplicate_local.lox"));
}

test "./variable/shadow_local.lox" {
    try run_test(@embedFile("./test/variable/shadow_local.lox"));
}

test "./variable/use_this_as_var.lox" {
    try run_test(@embedFile("./test/variable/use_this_as_var.lox"));
}

test "./variable/undefined_global.lox" {
    try run_test(@embedFile("./test/variable/undefined_global.lox"));
}

test "./variable/duplicate_parameter.lox" {
    try run_test(@embedFile("./test/variable/duplicate_parameter.lox"));
}

test "./variable/redeclare_global.lox" {
    try run_test(@embedFile("./test/variable/redeclare_global.lox"));
}

test "./variable/uninitialized.lox" {
    try run_test(@embedFile("./test/variable/uninitialized.lox"));
}

test "./variable/shadow_global.lox" {
    try run_test(@embedFile("./test/variable/shadow_global.lox"));
}

test "./variable/local_from_method.lox" {
    try run_test(@embedFile("./test/variable/local_from_method.lox"));
}

test "./variable/early_bound.lox" {
    try run_test(@embedFile("./test/variable/early_bound.lox"));
}

test "./variable/unreached_undefined.lox" {
    try run_test(@embedFile("./test/variable/unreached_undefined.lox"));
}

test "./variable/in_middle_of_block.lox" {
    try run_test(@embedFile("./test/variable/in_middle_of_block.lox"));
}

test "./variable/undefined_local.lox" {
    try run_test(@embedFile("./test/variable/undefined_local.lox"));
}

test "./variable/use_nil_as_var.lox" {
    try run_test(@embedFile("./test/variable/use_nil_as_var.lox"));
}

test "./variable/use_global_in_initializer.lox" {
    try run_test(@embedFile("./test/variable/use_global_in_initializer.lox"));
}

test "./comments/unicode.lox" {
    try run_test(@embedFile("./test/comments/unicode.lox"));
}

test "./comments/only_line_comment_and_line.lox" {
    try run_test(@embedFile("./test/comments/only_line_comment_and_line.lox"));
}

test "./comments/line_at_eof.lox" {
    try run_test(@embedFile("./test/comments/line_at_eof.lox"));
}

test "./comments/only_line_comment.lox" {
    try run_test(@embedFile("./test/comments/only_line_comment.lox"));
}

test "./print/missing_argument.lox" {
    try run_test(@embedFile("./test/print/missing_argument.lox"));
}

test "./this/this_in_top_level_function.lox" {
    try run_test(@embedFile("./test/this/this_in_top_level_function.lox"));
}

test "./this/this_in_method.lox" {
    try run_test(@embedFile("./test/this/this_in_method.lox"));
}

test "./this/this_at_top_level.lox" {
    try run_test(@embedFile("./test/this/this_at_top_level.lox"));
}

test "./this/closure.lox" {
    try run_test(@embedFile("./test/this/closure.lox"));
}

test "./this/nested_closure.lox" {
    try run_test(@embedFile("./test/this/nested_closure.lox"));
}

test "./this/nested_class.lox" {
    try run_test(@embedFile("./test/this/nested_class.lox"));
}

test "./constructor/default.lox" {
    try run_test(@embedFile("./test/constructor/default.lox"));
}

test "./constructor/default_arguments.lox" {
    try run_test(@embedFile("./test/constructor/default_arguments.lox"));
}

test "./constructor/return_value.lox" {
    try run_test(@embedFile("./test/constructor/return_value.lox"));
}

test "./constructor/early_return.lox" {
    try run_test(@embedFile("./test/constructor/early_return.lox"));
}

test "./constructor/call_init_early_return.lox" {
    try run_test(@embedFile("./test/constructor/call_init_early_return.lox"));
}

test "./constructor/missing_arguments.lox" {
    try run_test(@embedFile("./test/constructor/missing_arguments.lox"));
}

test "./constructor/call_init_explicitly.lox" {
    try run_test(@embedFile("./test/constructor/call_init_explicitly.lox"));
}

test "./constructor/return_in_nested_function.lox" {
    try run_test(@embedFile("./test/constructor/return_in_nested_function.lox"));
}

test "./constructor/init_not_method.lox" {
    try run_test(@embedFile("./test/constructor/init_not_method.lox"));
}

test "./constructor/arguments.lox" {
    try run_test(@embedFile("./test/constructor/arguments.lox"));
}

test "./constructor/extra_arguments.lox" {
    try run_test(@embedFile("./test/constructor/extra_arguments.lox"));
}

test "./while/fun_in_body.lox" {
    try run_test(@embedFile("./test/while/fun_in_body.lox"));
}

test "./while/return_closure.lox" {
    try run_test(@embedFile("./test/while/return_closure.lox"));
}

test "./while/var_in_body.lox" {
    try run_test(@embedFile("./test/while/var_in_body.lox"));
}

test "./while/class_in_body.lox" {
    try run_test(@embedFile("./test/while/class_in_body.lox"));
}

test "./while/syntax.lox" {
    try run_test(@embedFile("./test/while/syntax.lox"));
}

test "./while/closure_in_body.lox" {
    try run_test(@embedFile("./test/while/closure_in_body.lox"));
}

test "./while/return_inside.lox" {
    try run_test(@embedFile("./test/while/return_inside.lox"));
}

test "./bool/not.lox" {
    try run_test(@embedFile("./test/bool/not.lox"));
}

test "./bool/equality.lox" {
    try run_test(@embedFile("./test/bool/equality.lox"));
}

test "./field/call_nonfunction_field.lox" {
    try run_test(@embedFile("./test/field/call_nonfunction_field.lox"));
}

test "./field/set_on_string.lox" {
    try run_test(@embedFile("./test/field/set_on_string.lox"));
}

test "./field/set_evaluation_order.lox" {
    try run_test(@embedFile("./test/field/set_evaluation_order.lox"));
}

test "./field/get_on_num.lox" {
    try run_test(@embedFile("./test/field/get_on_num.lox"));
}

test "./field/call_function_field.lox" {
    try run_test(@embedFile("./test/field/call_function_field.lox"));
}

test "./field/get_on_string.lox" {
    try run_test(@embedFile("./test/field/get_on_string.lox"));
}

test "./field/get_on_bool.lox" {
    try run_test(@embedFile("./test/field/get_on_bool.lox"));
}

test "./field/set_on_class.lox" {
    try run_test(@embedFile("./test/field/set_on_class.lox"));
}

test "./field/many.lox" {
    try run_test(@embedFile("./test/field/many.lox"));
}

test "./field/set_on_function.lox" {
    try run_test(@embedFile("./test/field/set_on_function.lox"));
}

test "./field/set_on_nil.lox" {
    try run_test(@embedFile("./test/field/set_on_nil.lox"));
}

test "./field/get_on_nil.lox" {
    try run_test(@embedFile("./test/field/get_on_nil.lox"));
}

test "./field/method_binds_this.lox" {
    try run_test(@embedFile("./test/field/method_binds_this.lox"));
}

test "./field/get_on_function.lox" {
    try run_test(@embedFile("./test/field/get_on_function.lox"));
}

test "./field/on_instance.lox" {
    try run_test(@embedFile("./test/field/on_instance.lox"));
}

test "./field/set_on_num.lox" {
    try run_test(@embedFile("./test/field/set_on_num.lox"));
}

test "./field/get_and_set_method.lox" {
    try run_test(@embedFile("./test/field/get_and_set_method.lox"));
}

test "./field/undefined.lox" {
    try run_test(@embedFile("./test/field/undefined.lox"));
}

test "./field/set_on_bool.lox" {
    try run_test(@embedFile("./test/field/set_on_bool.lox"));
}

test "./field/method.lox" {
    try run_test(@embedFile("./test/field/method.lox"));
}

test "./field/get_on_class.lox" {
    try run_test(@embedFile("./test/field/get_on_class.lox"));
}

test "./closure/closed_closure_in_function.lox" {
    try run_test(@embedFile("./test/closure/closed_closure_in_function.lox"));
}

test "./closure/unused_closure.lox" {
    try run_test(@embedFile("./test/closure/unused_closure.lox"));
}

test "./closure/assign_to_closure.lox" {
    try run_test(@embedFile("./test/closure/assign_to_closure.lox"));
}

test "./closure/close_over_function_parameter.lox" {
    try run_test(@embedFile("./test/closure/close_over_function_parameter.lox"));
}

test "./closure/reference_closure_multiple_times.lox" {
    try run_test(@embedFile("./test/closure/reference_closure_multiple_times.lox"));
}

test "./closure/close_over_method_parameter.lox" {
    try run_test(@embedFile("./test/closure/close_over_method_parameter.lox"));
}

test "./closure/reuse_closure_slot.lox" {
    try run_test(@embedFile("./test/closure/reuse_closure_slot.lox"));
}

test "./closure/nested_closure.lox" {
    try run_test(@embedFile("./test/closure/nested_closure.lox"));
}

test "./closure/unused_later_closure.lox" {
    try run_test(@embedFile("./test/closure/unused_later_closure.lox"));
}

test "./closure/shadow_closure_with_local.lox" {
    try run_test(@embedFile("./test/closure/shadow_closure_with_local.lox"));
}

test "./closure/assign_to_shadowed_later.lox" {
    try run_test(@embedFile("./test/closure/assign_to_shadowed_later.lox"));
}

test "./closure/close_over_later_variable.lox" {
    try run_test(@embedFile("./test/closure/close_over_later_variable.lox"));
}

test "./closure/open_closure_in_function.lox" {
    try run_test(@embedFile("./test/closure/open_closure_in_function.lox"));
}

// test "./benchmark/string_equality.lox" {
//     try run_test(@embedFile("./test/benchmark/string_equality.lox"));
// }

// test "./benchmark/zoo.lox" {
//     try run_test(@embedFile("./test/benchmark/zoo.lox"));
// }

// test "./benchmark/properties.lox" {
//     try run_test(@embedFile("./test/benchmark/properties.lox"));
// }

// test "./benchmark/invocation.lox" {
//     try run_test(@embedFile("./test/benchmark/invocation.lox"));
// }

// test "./benchmark/zoo_batch.lox" {
//     try run_test(@embedFile("./test/benchmark/zoo_batch.lox"));
// }

// test "./benchmark/fib.lox" {
//     try run_test(@embedFile("./test/benchmark/fib.lox"));
// }

// test "./benchmark/trees.lox" {
//     try run_test(@embedFile("./test/benchmark/trees.lox"));
// }

// test "./benchmark/method_call.lox" {
//     try run_test(@embedFile("./test/benchmark/method_call.lox"));
// }

// test "./benchmark/binary_trees.lox" {
//     try run_test(@embedFile("./test/benchmark/binary_trees.lox"));
// }

// test "./benchmark/equality.lox" {
//     try run_test(@embedFile("./test/benchmark/equality.lox"));
// }

// test "./benchmark/instantiation.lox" {
//     try run_test(@embedFile("./test/benchmark/instantiation.lox"));
// }

test "./call/bool.lox" {
    try run_test(@embedFile("./test/call/bool.lox"));
}

test "./call/nil.lox" {
    try run_test(@embedFile("./test/call/nil.lox"));
}

test "./call/object.lox" {
    try run_test(@embedFile("./test/call/object.lox"));
}

test "./call/num.lox" {
    try run_test(@embedFile("./test/call/num.lox"));
}

test "./call/string.lox" {
    try run_test(@embedFile("./test/call/string.lox"));
}

test "./precedence.lox" {
    try run_test(@embedFile("./test/precedence.lox"));
}

test "./assignment/local.lox" {
    try run_test(@embedFile("./test/assignment/local.lox"));
}

test "./assignment/prefix_operator.lox" {
    try run_test(@embedFile("./test/assignment/prefix_operator.lox"));
}

test "./assignment/infix_operator.lox" {
    try run_test(@embedFile("./test/assignment/infix_operator.lox"));
}

test "./assignment/to_this.lox" {
    try run_test(@embedFile("./test/assignment/to_this.lox"));
}

test "./assignment/grouping.lox" {
    try run_test(@embedFile("./test/assignment/grouping.lox"));
}

test "./assignment/undefined.lox" {
    try run_test(@embedFile("./test/assignment/undefined.lox"));
}

test "./assignment/syntax.lox" {
    try run_test(@embedFile("./test/assignment/syntax.lox"));
}

test "./assignment/associativity.lox" {
    try run_test(@embedFile("./test/assignment/associativity.lox"));
}

test "./assignment/global.lox" {
    try run_test(@embedFile("./test/assignment/global.lox"));
}

test "./class/local_reference_self.lox" {
    try run_test(@embedFile("./test/class/local_reference_self.lox"));
}

test "./class/local_inherit_self.lox" {
    try run_test(@embedFile("./test/class/local_inherit_self.lox"));
}

test "./class/local_inherit_other.lox" {
    try run_test(@embedFile("./test/class/local_inherit_other.lox"));
}

test "./class/inherit_self.lox" {
    try run_test(@embedFile("./test/class/inherit_self.lox"));
}

test "./class/inherited_method.lox" {
    try run_test(@embedFile("./test/class/inherited_method.lox"));
}

test "./class/empty.lox" {
    try run_test(@embedFile("./test/class/empty.lox"));
}

test "./class/reference_self.lox" {
    try run_test(@embedFile("./test/class/reference_self.lox"));
}

test "./nil/literal.lox" {
    try run_test(@embedFile("./test/nil/literal.lox"));
}

test "./string/unterminated.lox" {
    try run_test(@embedFile("./test/string/unterminated.lox"));
}

test "./string/literals.lox" {
    try run_test(@embedFile("./test/string/literals.lox"));
}

test "./string/error_after_multiline.lox" {
    try run_test(@embedFile("./test/string/error_after_multiline.lox"));
}

test "./string/multiline.lox" {
    try run_test(@embedFile("./test/string/multiline.lox"));
}

test "./number/decimal_point_at_eof.lox" {
    try run_test(@embedFile("./test/number/decimal_point_at_eof.lox"));
}

test "./number/trailing_dot.lox" {
    try run_test(@embedFile("./test/number/trailing_dot.lox"));
}

test "./number/literals.lox" {
    try run_test(@embedFile("./test/number/literals.lox"));
}

test "./number/nan_equality.lox" {
    try run_test(@embedFile("./test/number/nan_equality.lox"));
}

test "./number/leading_dot.lox" {
    try run_test(@embedFile("./test/number/leading_dot.lox"));
}

test "./limit/too_many_upvalues.lox" {
    try run_test(@embedFile("./test/limit/too_many_upvalues.lox"));
}

test "./limit/too_many_constants.lox" {
    try run_test(@embedFile("./test/limit/too_many_constants.lox"));
}

test "./limit/too_many_locals.lox" {
    try run_test(@embedFile("./test/limit/too_many_locals.lox"));
}

test "./limit/no_reuse_constants.lox" {
    try run_test(@embedFile("./test/limit/no_reuse_constants.lox"));
}

test "./limit/stack_overflow.lox" {
    try run_test(@embedFile("./test/limit/stack_overflow.lox"));
}

test "./limit/loop_too_large.lox" {
    try run_test(@embedFile("./test/limit/loop_too_large.lox"));
}

test "./function/parameters.lox" {
    try run_test(@embedFile("./test/function/parameters.lox"));
}

test "./function/local_recursion.lox" {
    try run_test(@embedFile("./test/function/local_recursion.lox"));
}

test "./function/local_mutual_recursion.lox" {
    try run_test(@embedFile("./test/function/local_mutual_recursion.lox"));
}

test "./function/empty_body.lox" {
    try run_test(@embedFile("./test/function/empty_body.lox"));
}

test "./function/body_must_be_block.lox" {
    try run_test(@embedFile("./test/function/body_must_be_block.lox"));
}

test "./function/too_many_parameters.lox" {
    try run_test(@embedFile("./test/function/too_many_parameters.lox"));
}

test "./function/missing_arguments.lox" {
    try run_test(@embedFile("./test/function/missing_arguments.lox"));
}

test "./function/too_many_arguments.lox" {
    try run_test(@embedFile("./test/function/too_many_arguments.lox"));
}

test "./function/recursion.lox" {
    try run_test(@embedFile("./test/function/recursion.lox"));
}

test "./function/print.lox" {
    try run_test(@embedFile("./test/function/print.lox"));
}

test "./function/extra_arguments.lox" {
    try run_test(@embedFile("./test/function/extra_arguments.lox"));
}

test "./function/missing_comma_in_parameters.lox" {
    try run_test(@embedFile("./test/function/missing_comma_in_parameters.lox"));
}

test "./function/mutual_recursion.lox" {
    try run_test(@embedFile("./test/function/mutual_recursion.lox"));
}

test "./function/nested_call_with_arguments.lox" {
    try run_test(@embedFile("./test/function/nested_call_with_arguments.lox"));
}

// test "./super/super_without_name.lox" {
//     try run_test(@embedFile("./test/super/super_without_name.lox"));
// }

// test "./super/call_other_method.lox" {
//     try run_test(@embedFile("./test/super/call_other_method.lox"));
// }

// test "./super/super_in_inherited_method.lox" {
//     try run_test(@embedFile("./test/super/super_in_inherited_method.lox"));
// }

// test "./super/call_same_method.lox" {
//     try run_test(@embedFile("./test/super/call_same_method.lox"));
// }

// test "./super/super_in_top_level_function.lox" {
//     try run_test(@embedFile("./test/super/super_in_top_level_function.lox"));
// }

// test "./super/super_without_dot.lox" {
//     try run_test(@embedFile("./test/super/super_without_dot.lox"));
// }

// test "./super/this_in_superclass_method.lox" {
//     try run_test(@embedFile("./test/super/this_in_superclass_method.lox"));
// }

// test "./super/super_at_top_level.lox" {
//     try run_test(@embedFile("./test/super/super_at_top_level.lox"));
// }

// test "./super/parenthesized.lox" {
//     try run_test(@embedFile("./test/super/parenthesized.lox"));
// }

// test "./super/no_superclass_bind.lox" {
//     try run_test(@embedFile("./test/super/no_superclass_bind.lox"));
// }

// test "./super/constructor.lox" {
//     try run_test(@embedFile("./test/super/constructor.lox"));
// }

// test "./super/missing_arguments.lox" {
//     try run_test(@embedFile("./test/super/missing_arguments.lox"));
// }

// test "./super/closure.lox" {
//     try run_test(@embedFile("./test/super/closure.lox"));
// }

// test "./super/no_superclass_method.lox" {
//     try run_test(@embedFile("./test/super/no_superclass_method.lox"));
// }

// test "./super/bound_method.lox" {
//     try run_test(@embedFile("./test/super/bound_method.lox"));
// }

// test "./super/no_superclass_call.lox" {
//     try run_test(@embedFile("./test/super/no_superclass_call.lox"));
// }

// test "./super/extra_arguments.lox" {
//     try run_test(@embedFile("./test/super/extra_arguments.lox"));
// }

// test "./super/super_in_closure_in_inherited_method.lox" {
//     try run_test(@embedFile("./test/super/super_in_closure_in_inherited_method.lox"));
// }

// test "./super/reassign_superclass.lox" {
//     try run_test(@embedFile("./test/super/reassign_superclass.lox"));
// }

// test "./super/indirectly_inherited.lox" {
//     try run_test(@embedFile("./test/super/indirectly_inherited.lox"));
// }

test "./regression/40.lox" {
    try run_test(@embedFile("./test/regression/40.lox"));
}

test "./regression/394.lox" {
    try run_test(@embedFile("./test/regression/394.lox"));
}

test "./logical_operator/or_truth.lox" {
    try run_test(@embedFile("./test/logical_operator/or_truth.lox"));
}

test "./logical_operator/or.lox" {
    try run_test(@embedFile("./test/logical_operator/or.lox"));
}

test "./logical_operator/and.lox" {
    try run_test(@embedFile("./test/logical_operator/and.lox"));
}

test "./logical_operator/and_truth.lox" {
    try run_test(@embedFile("./test/logical_operator/and_truth.lox"));
}

test "./block/empty.lox" {
    try run_test(@embedFile("./test/block/empty.lox"));
}

test "./block/scope.lox" {
    try run_test(@embedFile("./test/block/scope.lox"));
}

const std = @import("std");
const dbg = @import("debug.zig");
const VM = @import("VM.zig");
