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

static symbol_t *get_expr_sym(block_t *block, expr_t *expr) {
  switch (expr->kind) {
  case EXPR_PRIMARY: {
    switch (expr->primary.kind) {
    case PRIMARY_INT:
    case PRIMARY_FLOAT:
    case PRIMARY_PRIMITIVE_TYPE:
    case PRIMARY_STRING: break;

    case PRIMARY_IDENT: {
      symbol_t *sym = scope_get(&block->scope, expr->primary.string);
      if (sym) return sym;
    } break;
    }

  } break;

  case EXPR_EXPR: {
    return get_expr_sym(block, expr->expr);
  } break;

  case EXPR_ACCESS: {
    symbol_t *sym = get_expr_sym(block, expr->access.left);
    if (sym) {
      switch (sym->kind) {
      case SYMBOL_CONST_DECL: {
        expr_t *inner = inner_expr(&sym->const_decl->expr);

        switch (inner->kind) {
        case EXPR_IMPORT: {
          return get_expr_sym(&inner->import.ast->block, expr->access.right);
        } break;

        default: break;
        }

      } break;

      default: break;
      }
    }

  } break;

  case EXPR_UNARY: {
    // TODO
  } break;

  case EXPR_BINARY: {
    // TODO
  } break;

  case EXPR_PROC_CALL: {
    return get_expr_sym(block, expr->proc_call.expr);
  } break;

  case EXPR_PROC: {
  } break;

  case EXPR_PROC_PTR: {
  } break;

  case EXPR_STRUCT: {
  } break;

  case EXPR_IMPORT: {
  } break;

  case EXPR_BLOCK: {
  } break;
  }

  return NULL;
}

