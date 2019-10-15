#include "expr.h"

symbol_t *get_expr_sym(expr_t *expr, scope_t *scope) {
  switch (expr->kind) {
  case EXPR_PRIMARY: {
    switch (expr->primary.kind) {
    case PRIMARY_INT:
    case PRIMARY_FLOAT:
    case PRIMARY_BOOL:
    case PRIMARY_NULL:
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

  case EXPR_PROC_CALL: {
    return get_expr_sym(expr->proc_call.expr, scope);
  } break;

  case EXPR_MACRO_CALL: {
    return get_expr_sym(expr->macro_call.expr, scope);
  } break;

  case EXPR_SUBSCRIPT: return get_expr_sym(expr->left, scope);

  case EXPR_INTRIN:
  case EXPR_UNARY:
  case EXPR_BINARY:
  case EXPR_ARRAY_TYPE:
  case EXPR_ARRAY_LITERAL:
  case EXPR_PROC:
  case EXPR_PROC_PTR:
  case EXPR_MACRO:
  case EXPR_STRUCT:
  case EXPR_IMPORT:
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
    case PRIMARY_BOOL:
    case PRIMARY_NULL:
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

  case EXPR_SUBSCRIPT: {
    return is_expr_const(expr->left, scope) &&
           is_expr_const(expr->right, scope);
  } break;

  case EXPR_ARRAY_LITERAL: {
    For(elem, expr->array_lit.elems) {
      if (!is_expr_const(elem, scope)) return false;
    }
    return true;
  } break;

  case EXPR_STRUCT:
  case EXPR_PROC:
  case EXPR_MACRO:
  case EXPR_PROC_PTR:
  case EXPR_IMPORT:
  case EXPR_ARRAY_TYPE:
  case EXPR_INTRIN: return true;

  case EXPR_PROC_CALL: return false;
  case EXPR_MACRO_CALL: return false;

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

bool resolve_expr_int(expr_t *expr, scope_t *scope, int64_t *result) {
  switch (expr->kind) {
  case EXPR_PRIMARY: {
    switch (expr->primary.kind) {
    case PRIMARY_FLOAT:
    case PRIMARY_BOOL:
    case PRIMARY_NULL:
    case PRIMARY_PRIMITIVE_TYPE:
    case PRIMARY_STRING:
    case PRIMARY_CSTRING: break;

    case PRIMARY_INT: {
      *result = expr->primary.i64;
      return true;
    } break;

    case PRIMARY_IDENT: {
      symbol_t *sym = scope_get(scope, expr->primary.string);
      if (sym) {
        switch (sym->kind) {
        case SYMBOL_CONST_DECL: {
          return resolve_expr_int(&sym->const_decl->expr, sym->scope, result);
        } break;

        default: break;
        }
      }
    } break;
    }

  } break;

  case EXPR_EXPR: return resolve_expr_int(expr->expr, scope, result);

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

  case EXPR_SUBSCRIPT: {
    // TODO
    return false;
  } break;

  case EXPR_INTRIN: {
  } break;

  case EXPR_UNARY: {
  } break;

  case EXPR_BINARY: {
  } break;

  case EXPR_ARRAY_TYPE:
  case EXPR_ARRAY_LITERAL:
  case EXPR_PROC_CALL:
  case EXPR_MACRO_CALL:
  case EXPR_PROC:
  case EXPR_PROC_PTR:
  case EXPR_MACRO:
  case EXPR_STRUCT:
  case EXPR_IMPORT:
  case EXPR_BLOCK: break;
  }

  return false;
}
