#include "expr.h"

symbol_t *get_expr_sym(expr_t *expr, scope_t *scope) {
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
    return get_expr_sym(expr->expr, scope);
  } break;

  case EXPR_ACCESS: {
    symbol_t *sym = get_expr_sym(expr->access.left, scope);
    if (sym) {
      switch (sym->kind) {
      case SYMBOL_CONST_DECL: {
        expr_t *inner = inner_expr(&sym->const_decl->expr);

        switch (inner->kind) {
        case EXPR_IMPORT: {
          return get_expr_sym(
              expr->access.right, &inner->import.ast->block.scope);
        } break;

        default: break;
        }

      } break;

      default: break;
      }
    }
  } break;

  case EXPR_UNARY: {
  } break;

  case EXPR_BINARY: {
  } break;

  case EXPR_PROC_CALL: {
    return get_expr_sym(expr->proc_call.expr, scope);
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

expr_t *get_access_expr(
    block_t *block, expr_t *expr, block_t **out_block, symbol_t **out_sym) {
  if (out_block) *out_block = block;

  switch (expr->kind) {
  case EXPR_ACCESS: {
    symbol_t *sym = get_expr_sym(expr->access.left, &block->scope);

    if (!sym) break;
    if (out_sym) *out_sym = sym;

    switch (sym->kind) {
    case SYMBOL_CONST_DECL: {
      expr_t *decl_expr = inner_expr(&sym->const_decl->expr);

      switch (decl_expr->kind) {
      case EXPR_IMPORT: {
        return get_access_expr(
            &decl_expr->import.ast->block,
            expr->access.right,
            out_block,
            out_sym);
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

bool is_expr_const(expr_t *expr, scope_t *scope) {
  switch (expr->kind) {
  case EXPR_PRIMARY: {
    switch (expr->primary.kind) {
    case PRIMARY_INT:
    case PRIMARY_FLOAT:
    case PRIMARY_PRIMITIVE_TYPE:
    case PRIMARY_STRING:
    case PRIMARY_CSTRING: return true;

    case PRIMARY_IDENT: {
      symbol_t *sym = scope_get(scope, expr->primary.string);
      if (sym) {
        switch (sym->kind) {
        case SYMBOL_CONST_DECL: return true;
        default: break;
        }
      }
    } break;
    }
  } break;

  case EXPR_EXPR: return is_expr_const(expr->expr, scope);

  case EXPR_ACCESS: {
    symbol_t *sym = get_expr_sym(expr->access.left, scope);
    if (sym) {
      switch (sym->kind) {
      case SYMBOL_CONST_DECL: {
        expr_t *inner = inner_expr(&sym->const_decl->expr);

        switch (inner->kind) {
        case EXPR_IMPORT: {
          return is_expr_const(
              expr->access.right, &inner->import.ast->block.scope);
        } break;

        default: break;
        }

      } break;

      default: break;
      }
    }
  } break;

  case EXPR_STRUCT:
  case EXPR_PROC:
  case EXPR_PROC_PTR:
  case EXPR_IMPORT: return true;

  case EXPR_PROC_CALL: return false;

  case EXPR_UNARY: {
    switch (expr->unary.kind) {
    case UNOP_DEREF:
    case UNOP_ADDRESS: return false;
    default: break;
    }

    return is_expr_const(expr->right, scope);
  } break;

  case EXPR_BINARY: {

    return is_expr_const(expr->left, scope) &&
           is_expr_const(expr->right, scope);
  } break;

  case EXPR_BLOCK: return false;
  }

  return false;
}

