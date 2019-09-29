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

static symbol_t *get_expr_sym(scope_t *scope, expr_t *expr) {
  switch (expr->kind) {
  case EXPR_PRIMARY: {
    switch (expr->primary.kind) {
    case PRIMARY_INT:
    case PRIMARY_FLOAT:
    case PRIMARY_PRIMITIVE_TYPE:
    case PRIMARY_STRING:
    case PRIMARY_CSTRING: break;

    case PRIMARY_IDENT: {
      symbol_t *sym = scope_get(scope, expr->primary.string);
      if (sym) return sym;
    } break;
    }

  } break;

  case EXPR_EXPR: {
    return get_expr_sym(scope, expr->expr);
  } break;

  case EXPR_ACCESS: {
    symbol_t *sym = get_expr_sym(scope, expr->access.left);
    if (sym) {
      switch (sym->kind) {
      case SYMBOL_CONST_DECL: {
        expr_t *inner = inner_expr(&sym->const_decl->expr);

        switch (inner->kind) {
        case EXPR_IMPORT: {
          return get_expr_sym(
              &inner->import.ast->block.scope, expr->access.right);
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
    return get_expr_sym(scope, expr->proc_call.expr);
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

static expr_t *get_access_expr(
    block_t *block, expr_t *expr, block_t **out_block, symbol_t **out_sym) {
  if (out_block) *out_block = block;

  switch (expr->kind) {
  case EXPR_ACCESS: {
    symbol_t *sym = get_expr_sym(&block->scope, expr->access.left);

    if (!sym) break;
    if (out_sym) *out_sym = sym;

    switch (sym->kind) {
    case SYMBOL_CONST_DECL: {
      expr_t *decl_expr = inner_expr(&sym->const_decl->expr);

      switch (decl_expr->kind) {
      case EXPR_IMPORT: {
        return get_access_expr(
            &decl_expr->import.ast->block, expr->access.right, out_block, out_sym);
      } break;

      default: break;
      }
    } break;

    default: break;
    }
  } break;

  default: return expr;
  }

  return NULL;
}

