#pragma once

#include "ast.h"

static expr_t *inner_expr(expr_t *expr) {
  switch (expr->kind) {
  case EXPR_EXPR: {
    return inner_expr(expr->expr);
  } break;
  default: return expr;
  }
}

symbol_t *get_expr_sym(expr_t *expr, scope_t *scope);

expr_t *get_access_expr(
    block_t *block, expr_t *expr, block_t **out_block, symbol_t **out_sym);

bool is_expr_const(expr_t *expr, scope_t *scope);

bool resolve_expr_int(expr_t *expr, scope_t *scope, int64_t *result);
