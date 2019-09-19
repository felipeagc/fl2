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
