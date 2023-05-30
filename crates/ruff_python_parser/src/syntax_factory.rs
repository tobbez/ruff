use ruff_text_size::TextRange;
use rustpython_ast::{
    Alias, Arguments, Boolop, Constant, Excepthandler, Expr, ExprBinOp, ExprBoolOp, ExprConstant,
    ExprContext, ExprDict, ExprIfExp, ExprLambda, ExprName, ExprNamedExpr, ExprSet, ExprUnaryOp,
    Identifier, Keyword, MatchCase, ModExpression, ModModule, Operator, Stmt, StmtAnnAssign,
    StmtAssert, StmtAssign, StmtAsyncFor, StmtAsyncFunctionDef, StmtAsyncWith, StmtAugAssign,
    StmtBreak, StmtClassDef, StmtContinue, StmtDelete, StmtExpr, StmtFor, StmtFunctionDef,
    StmtGlobal, StmtIf, StmtImport, StmtImportFrom, StmtMatch, StmtNonlocal, StmtPass, StmtRaise,
    StmtReturn, StmtTry, StmtTryStar, StmtWhile, StmtWith, TypeIgnore, Unaryop, Withitem,
};
use std::borrow::Cow;

#[derive(Default)]
pub struct SyntaxFactory;

impl SyntaxFactory {
    pub fn new_module(
        &mut self,
        range: TextRange,
        body: Vec<Stmt>,
        type_ignores: Vec<TypeIgnore>,
    ) -> ModModule {
        ModModule {
            range,
            body,
            type_ignores,
        }
    }

    pub fn new_expression_module(&mut self, range: TextRange, body: Expr) -> ModExpression {
        ModExpression {
            range,
            body: Box::new(body),
        }
    }

    pub fn new_function_statement(
        &mut self,
        range: TextRange,
        decorator_list: Vec<Expr>,
        name: Identifier,
        parameters: Arguments,
        body: Vec<Stmt>,
        returns: Option<Expr>,
    ) -> StmtFunctionDef {
        StmtFunctionDef {
            range,
            name,
            args: Box::new(parameters),
            body,
            decorator_list,
            returns: returns.map(Box::new),
            type_comment: None,
        }
    }

    pub fn new_async_function_statement(
        &mut self,
        range: TextRange,
        decorator_list: Vec<Expr>,
        name: Identifier,
        parameters: Arguments,
        body: Vec<Stmt>,
        returns: Option<Expr>,
    ) -> StmtAsyncFunctionDef {
        StmtAsyncFunctionDef {
            range,
            name,
            args: Box::new(parameters),
            body,
            decorator_list,
            returns: returns.map(Box::new),
            type_comment: None,
        }
    }

    pub fn new_class_statement(
        &mut self,
        range: TextRange,
        decorator_list: Vec<Expr>,
        name: Identifier,
        bases: Vec<Expr>,
        keywords: Vec<Keyword>,
        body: Vec<Stmt>,
    ) -> StmtClassDef {
        StmtClassDef {
            range,
            name,
            bases,
            keywords,
            body,
            decorator_list,
        }
    }

    pub fn new_return_statement(&mut self, range: TextRange, value: Option<Expr>) -> StmtReturn {
        StmtReturn {
            range,
            value: value.map(Box::new),
        }
    }

    pub fn new_delete_statement(&mut self, range: TextRange, targets: Vec<Expr>) -> StmtDelete {
        StmtDelete { range, targets }
    }

    pub fn new_assignment_statement(
        &mut self,
        range: TextRange,
        targets: Vec<Expr>,
        value: Expr,
    ) -> StmtAssign {
        StmtAssign {
            range,
            targets,
            value: Box::new(value),
            type_comment: None,
        }
    }

    pub fn new_augmented_assignment_statement(
        &mut self,
        range: TextRange,
        target: Expr,
        operator: Operator,
        value: Expr,
    ) -> StmtAugAssign {
        StmtAugAssign {
            range,
            target: Box::new(target),
            op: operator,
            value: Box::new(value),
        }
    }

    pub fn new_annotated_assignment_statement(
        &mut self,
        range: TextRange,
        target: Expr,
        annotation: Expr,
        value: Option<Expr>,
        simple: bool,
    ) -> StmtAnnAssign {
        StmtAnnAssign {
            range,
            target: Box::new(target),
            annotation: Box::new(annotation),
            value: value.map(Box::new),
            simple,
        }
    }

    pub fn new_for_statement(
        &mut self,
        range: TextRange,
        target: Expr,
        iterator: Expr,
        body: Vec<Stmt>,
        alternate: Vec<Stmt>,
    ) -> StmtFor {
        StmtFor {
            range,
            target: Box::new(target),
            iter: Box::new(iterator),
            body,
            orelse: alternate,
            type_comment: None,
        }
    }

    fn new_async_for_statement(
        &mut self,
        range: TextRange,
        target: Expr,
        iterator: Expr,
        body: Vec<Stmt>,
        alternate: Vec<Stmt>,
    ) -> StmtAsyncFor {
        StmtAsyncFor {
            range,
            target: Box::new(target),
            iter: Box::new(iterator),
            body,
            orelse: alternate,
            type_comment: None,
        }
    }

    pub fn new_while_statement(
        &mut self,
        range: TextRange,
        test: Expr,
        body: Vec<Stmt>,
        alternate: Vec<Stmt>,
    ) -> StmtWhile {
        StmtWhile {
            range,
            test: Box::new(test),
            body,
            orelse: alternate,
        }
    }

    pub fn new_if_statement(
        &mut self,
        range: TextRange,
        test: Expr,
        consequent: Vec<Stmt>,
        alternate: Vec<Stmt>,
    ) -> StmtIf {
        StmtIf {
            range,
            test: Box::new(test),
            body: consequent,
            orelse: alternate,
        }
    }

    pub fn new_with_statement(
        &mut self,
        range: TextRange,
        items: Vec<Withitem>,
        body: Vec<Stmt>,
    ) -> StmtWith {
        StmtWith {
            range,
            items,
            body,
            type_comment: None,
        }
    }

    pub fn new_async_with_statement(
        &mut self,
        range: TextRange,
        items: Vec<Withitem>,
        body: Vec<Stmt>,
    ) -> StmtAsyncWith {
        StmtAsyncWith {
            range,
            items,
            body,
            type_comment: None,
        }
    }

    pub fn new_match_statement(
        &mut self,
        range: TextRange,
        subject: Expr,
        cases: Vec<MatchCase>,
    ) -> StmtMatch {
        StmtMatch {
            range,
            subject: Box::new(subject),
            cases: cases,
        }
    }

    pub fn new_raise_statement(
        &mut self,
        range: TextRange,
        exception: Option<Expr>,
        cause: Option<Expr>,
    ) -> StmtRaise {
        StmtRaise {
            range,
            exc: exception.map(Box::new),
            cause: cause.map(Box::new),
        }
    }

    pub fn new_try_statement(
        &mut self,
        range: TextRange,
        body: Vec<Stmt>,
        handlers: Vec<Excepthandler>,
        alternate: Vec<Stmt>,
        finally_body: Vec<Stmt>,
    ) -> StmtTry {
        StmtTry {
            range,
            body,
            handlers,
            orelse: alternate,
            finalbody: finally_body,
        }
    }

    pub fn new_try_star_statement(
        &mut self,
        range: TextRange,
        body: Vec<Stmt>,
        handlers: Vec<Excepthandler>,
        alternate: Vec<Stmt>,
        finally_body: Vec<Stmt>,
    ) -> StmtTryStar {
        StmtTryStar {
            range,
            body,
            handlers,
            orelse: alternate,
            finalbody: finally_body,
        }
    }

    pub fn new_assert_statement(
        &mut self,
        range: TextRange,
        test: Expr,
        message: Option<Expr>,
    ) -> StmtAssert {
        StmtAssert {
            range,
            test: Box::new(test),
            msg: message.map(Box::new),
        }
    }

    pub fn new_import_statement(&mut self, range: TextRange, names: Vec<Alias>) -> StmtImport {
        StmtImport { range, names }
    }

    pub fn new_import_from_statement(
        &mut self,
        range: TextRange,
        names: Vec<Alias>,
        module: Option<Identifier>,
    ) -> StmtImportFrom {
        StmtImportFrom {
            range,
            module,
            names,
            level: None,
        }
    }

    pub fn new_global_statement(&mut self, range: TextRange, names: Vec<Identifier>) -> StmtGlobal {
        StmtGlobal { range, names }
    }

    pub fn new_non_local_statement(
        &mut self,
        range: TextRange,
        names: Vec<Identifier>,
    ) -> StmtNonlocal {
        StmtNonlocal { range, names }
    }

    pub fn new_expression_statement(&mut self, range: TextRange, expression: Expr) -> StmtExpr {
        StmtExpr {
            range,
            value: expression.into(),
        }
    }

    pub fn new_pass_statement(&mut self, range: TextRange) -> StmtPass {
        StmtPass { range }
    }

    pub fn new_break_statement(&mut self, range: TextRange) -> StmtBreak {
        StmtBreak { range }
    }

    pub fn new_continue_statement(&mut self, range: TextRange) -> StmtContinue {
        StmtContinue { range }
    }

    pub fn new_boolean_operation_expression(
        &mut self,
        range: TextRange,
        operation: Boolop,
        values: Vec<Expr>,
    ) -> ExprBoolOp {
        ExprBoolOp {
            range,
            op: operation,
            values,
        }
    }

    pub fn new_named_expression(
        &mut self,
        range: TextRange,
        target: Expr,
        value: Expr,
    ) -> ExprNamedExpr {
        ExprNamedExpr {
            range,
            target: target.into(),
            value: value.into(),
        }
    }

    pub fn new_binary_operation_expression(
        range: TextRange,
        left: Expr,
        operation: Operator,
        right: Expr,
    ) -> ExprBinOp {
        ExprBinOp {
            range,
            left: left.into(),
            op: operation,
            right: right.into(),
        }
    }

    pub fn new_unary_expression(
        &mut self,
        range: TextRange,
        operator: Unaryop,
        operand: Expr,
    ) -> ExprUnaryOp {
        ExprUnaryOp {
            range,
            op: operator,
            operand: operand.into(),
        }
    }

    pub fn new_lambda_expression(
        &mut self,
        range: TextRange,
        parameters: Arguments,
        body: Expr,
    ) -> ExprLambda {
        ExprLambda {
            range,
            args: parameters.into(),
            body: body.into(),
        }
    }

    pub fn new_if_expression(
        &mut self,
        range: TextRange,
        test: Expr,
        consequent: Expr,
        alternate: Expr,
    ) -> ExprIfExp {
        ExprIfExp {
            range,
            test: test.into(),
            body: consequent.into(),
            orelse: alternate.into(),
        }
    }

    pub fn new_dict_expression(
        &mut self,
        range: TextRange,
        entries: Vec<(Option<Expr>, Expr)>,
    ) -> ExprDict {
        let mut keys = Vec::with_capacity(entries.len());
        let mut values = Vec::with_capacity(entries.len());

        for (key, value) in entries {
            keys.push(key);
            values.push(value);
        }

        ExprDict {
            range,
            keys,
            values,
        }
    }

    pub fn new_set_expression(&mut self, range: TextRange, values: Vec<Expr>) -> ExprSet {
        ExprSet {
            range,
            elts: values,
        }
    }

    pub fn new_name_expression(
        &mut self,
        range: TextRange,
        identifier: Identifier,
        context: ExprContext,
    ) -> ExprName {
        ExprName {
            range,
            id: identifier,
            ctx: context,
        }
    }

    pub fn new_string_literal_expression(
        &mut self,
        range: TextRange,
        value: Cow<str>,
    ) -> ExprConstant {
        ExprConstant {
            range,
            value: Constant::Str(value.into()),
            kind: None,
        }
    }
}
