#include "analyzer.h"

#include "dbg.h"
#include "expr.h"
#include "filesystem.h"
#include "parser.h"
#include "scanner.h"
#include "type.h"
#include <assert.h>
#include <stdio.h>

static void error(analyzer_t *a, pos_t pos, const char *fmt, ...) {
  sb_reset(&a->ctx->sb);

  va_list vl;
  va_start(vl, fmt);
  sb_vsprintf(&a->ctx->sb, fmt, vl);
  va_end(vl);

  error_t err = {
      .pos = pos,
      .msg = bump_strdup(&a->ctx->alloc, sb_build(&a->ctx->sb)),
  };

  APPEND(a->errors, err);
}

static void analyze_block_unordered(analyzer_t *a, block_t *block);
static void analyze_block_ordered(analyzer_t *a, block_t *block);

static bool expr_as_type(analyzer_t *a, block_t *block, expr_t *expr) {
  bool res = false;
  if (expr->as_type) return true;

  switch (expr->kind) {
  case EXPR_PRIMARY: {
    switch (expr->primary.kind) {
    case PRIMARY_INT:
    case PRIMARY_FLOAT:
    case PRIMARY_BOOL:
    case PRIMARY_NULL:
    case PRIMARY_STRING:
    case PRIMARY_CSTRING: break;

    case PRIMARY_PRIMITIVE_TYPE: {
      type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
      memset(ty, 0, sizeof(*ty));
      expr->as_type = ty;
      ty->kind      = TYPE_PRIMITIVE;
      ty->prim      = expr->primary.prim_type;
      res           = true;
    } break;

    case PRIMARY_IDENT: {
      symbol_t *sym = scope_get(&block->scope, expr->primary.string);
      if (sym) {
        switch (sym->kind) {
        case SYMBOL_CONST_DECL: {
          res = expr_as_type(a, block, &sym->const_decl->expr);
          if (res) {
            expr->as_type                       = sym->const_decl->expr.as_type;
            sym->const_decl->expr.as_type->name = sym->const_decl->name;
          }
          return res;
        } break;

        default: break;
        }
      }
    } break;
    }

  } break;

  case EXPR_INTRIN: break;

  case EXPR_ARRAY_TYPE: {
    assert(expr->array.size_expr); // TODO: temporary

    if (!is_expr_const(expr->array.size_expr, &block->scope)) return false;

    int64_t size;
    if (!resolve_expr_int(expr->array.size_expr, &block->scope, &size))
      return false;

    if (size < 1) return false;

    if (!expr_as_type(a, block, expr->array.sub_expr)) return false;

    type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
    memset(ty, 0, sizeof(*ty));
    expr->as_type = ty;

    ty->kind    = TYPE_ARRAY;
    ty->subtype = expr->array.sub_expr->as_type;
    ty->size    = (size_t)size;

    res = true;
  } break;

  case EXPR_EXPR: {
    res           = expr_as_type(a, block, expr->expr);
    expr->as_type = expr->expr->as_type;
    return res;
  } break;

  case EXPR_ACCESS: {
    block_t *expr_block = NULL;
    expr_t *inner       = get_access_expr(block, expr, &expr_block, NULL);

    if (inner) {
      res           = expr_as_type(a, expr_block, inner);
      expr->as_type = inner->as_type;
    }
  } break;

  case EXPR_UNARY: {
    switch (expr->unary.kind) {
    case UNOP_DEREF: {
      res = true;

      type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
      memset(ty, 0, sizeof(*ty));
      expr->as_type = ty;

      ty->kind = TYPE_PTR;

      res |= expr_as_type(a, block, expr->right);
      if (res) ty->subtype = expr->right->as_type;
    } break;

    default: break;
    }
  } break;

  case EXPR_BINARY: {
    // TODO
  } break;

  case EXPR_PROC_CALL: {
  } break;

  case EXPR_PROC: {
  } break;

  case EXPR_PROC_PTR: {
    res = true;

    For(param, expr->proc.sig.params) {
      res |= expr_as_type(a, block, &param->type_expr);
      param->type = param->type_expr.as_type;
    }

    For(return_type, expr->proc.sig.return_types) {
      res |= expr_as_type(a, block, return_type);
    }

    if (!res) return res;

    type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
    memset(ty, 0, sizeof(*ty));
    expr->as_type = ty;
    ty->kind      = TYPE_PROC;
    ty->proc_sig  = &expr->proc.sig;
  } break;

  case EXPR_STRUCT: {
    type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
    memset(ty, 0, sizeof(*ty));
    expr->as_type = ty;
    ty->kind      = TYPE_STRUCT;
    ty->str       = &expr->str;
    res           = true;
  } break;

  case EXPR_IMPORT: {
  } break;

  case EXPR_BLOCK: {
  } break;

  case EXPR_SUBSCRIPT: {
  } break;
  }

  return res;
}

static symbol_t *symbol_check_expr(
    analyzer_t *a,
    block_t *operand_block,
    block_t *operation_block,
    expr_t *expr) {
  if (operation_block == NULL) operation_block = operand_block;

  expr = inner_expr(expr);
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
      symbol_t *sym = scope_get(&operation_block->scope, expr->primary.string);

      if (!sym) {
        error(a, expr->pos, "invalid identifier");
        break;
      }

      proc_t *sym_proc   = scope_proc(sym->scope);
      proc_t *block_proc = scope_proc(&operand_block->scope);

      if (sym_proc != NULL && block_proc != NULL) {
        if (sym_proc != block_proc) {
          if (sym->kind == SYMBOL_LOCAL_VAR) {
            error(
                a,
                expr->pos,
                "outside identifier not accessible from this procedure");
          }
        }
      }

      return sym;
    } break;
    }

  } break;

  case EXPR_EXPR: {
    return symbol_check_expr(a, operation_block, NULL, expr->expr);
  } break;

  case EXPR_INTRIN: {
    switch (expr->intrin.kind) {
    case INTRIN_SIZEOF:
    case INTRIN_ASSERT: {
      For(param, expr->intrin.params) {
        symbol_check_expr(a, operation_block, NULL, param);
      }
    } break;
    }
  } break;

  case EXPR_ACCESS: {
    block_t *expr_block = NULL;
    expr_t *inner = get_access_expr(operand_block, expr, &expr_block, NULL);

    if (inner) {
      return symbol_check_expr(a, operand_block, expr_block, inner);
    }
  } break;

  case EXPR_UNARY: {
    symbol_check_expr(a, operand_block, NULL, expr->right);
  } break;

  case EXPR_BINARY: {
    symbol_check_expr(a, operand_block, NULL, expr->left);
    symbol_check_expr(a, operand_block, NULL, expr->right);
  } break;

  case EXPR_SUBSCRIPT: {
    symbol_check_expr(a, operand_block, NULL, expr->left);
    symbol_check_expr(a, operand_block, NULL, expr->right);
  } break;

  case EXPR_PROC_CALL: {
    symbol_t *proc_sym =
        symbol_check_expr(a, operation_block, NULL, expr->proc_call.expr);
    proc_signature_t *proc_sig = NULL;

    if (!proc_sym) {
      expr_t *inner = inner_expr(expr->proc_call.expr);
      if (inner->kind == EXPR_PROC) proc_sig = &inner->proc.sig;
    }

    if (proc_sym) {
      switch (proc_sym->kind) {
      case SYMBOL_CONST_DECL: {
        if (proc_sym->const_decl->type->kind == TYPE_PROC) {
          proc_sig = proc_sym->const_decl->type->proc_sig;
        }
      } break;

      case SYMBOL_GLOBAL_VAR:
      case SYMBOL_LOCAL_VAR: {
        if (proc_sym->var_decl->type) {
          if (proc_sym->var_decl->type->kind == TYPE_PROC) {
            proc_sig = proc_sym->var_decl->type->proc_sig;
          }
        }
      } break;

      default: break;
      }
    }

    if (proc_sig == NULL) {
      error(
          a,
          expr->proc_call.expr->pos,
          "expression does not refer to a procedure");
      break;
    }

    expr->proc_call.sig = proc_sig;

    if (proc_sig->flags & PROC_FLAG_VARIADIC) {
      if (proc_sig->params.count > expr->proc_call.params.count) {
        error(
            a,
            expr->pos,
            "wrong variadic procedure parameter count, expected %zu or more, "
            "got %zu",
            proc_sig->params.count,
            expr->proc_call.params.count);
        break;
      }
    } else {
      if (proc_sig->params.count != expr->proc_call.params.count) {
        error(
            a,
            expr->pos,
            "wrong procedure parameter count, expected %zu, got %zu",
            proc_sig->params.count,
            expr->proc_call.params.count);
        break;
      }
    }
  } break;

  case EXPR_PROC_PTR:
  case EXPR_PROC: {
    scope_init(
        &expr->proc.block.scope,
        &operand_block->scope,
        expr->proc.block.stmts.count + expr->proc.sig.params.count);

    expr->proc.block.scope.proc = &expr->proc;

    For(param, expr->proc.sig.params) {
      // Check procedure declaration parameters

      if (param->name.count > 0) {
        if (scope_get_local(&expr->proc.block.scope, param->name)) {
          error(
              a,
              expr->pos,
              "duplicate parameter declaration: '%.*s'",
              (int)param->name.count,
              param->name.buf);
          continue;
        }
      }

      if (!expr_as_type(a, operand_block, &param->type_expr)) {
        error(a, param->type_expr.pos, "expression does not represent a type");
      }

      param->type = param->type_expr.as_type;

      if (param->name.count > 0) {
        symbol_t *sym = scope_add(&expr->proc.block.scope, a->ctx, param->name);
        sym->kind     = SYMBOL_LOCAL_VAR;
        sym->var_decl = param;
        param->sym    = sym;
      }
    }

    for (size_t i = 0; i < expr->proc.sig.return_types.count; i++) {
      // Check procedure return types

      expr_t *return_expr = &expr->proc.sig.return_types.buf[i];

      if (!expr_as_type(a, operand_block, return_expr)) {
        error(a, return_expr->pos, "expression does not represent a type");
      }
    }

    if ((expr->proc.sig.flags & (PROC_FLAG_NO_BODY | PROC_FLAG_INLINE)) ==
        (PROC_FLAG_NO_BODY | PROC_FLAG_INLINE)) {
      error(a, expr->pos, "inline procedures must have a body");
      break;
    }

    if (expr->proc.sig.conv == PROC_CONV_C && expr->proc.name.count > 0) {
      // TODO: remove this
      strbuf_t name       = expr->proc.name;
      proc_t *extern_proc = table_get(&a->ctx->extern_table, name);
      if (extern_proc) {
        error(a, expr->pos, "duplicate extern procedure");
        break;
      }
      table_set(&a->ctx->extern_table, name, &expr->proc);
    }

    if (strbuf_cmp(STR("main"), expr->proc.name)) {
      if (a->ctx->main_proc) {
        error(a, expr->pos, "duplicate main procedure");
        break;
      }
      a->ctx->main_proc = &expr->proc;
    }
  } break;

  case EXPR_ARRAY_TYPE: {
    if (expr->array.size_expr)
      symbol_check_expr(a, operation_block, NULL, expr->array.size_expr);

    assert(expr->array.sub_expr);
    symbol_check_expr(a, operation_block, NULL, expr->array.sub_expr);
  } break;

  case EXPR_STRUCT: {
  } break;

  case EXPR_IMPORT: {
  } break;

  case EXPR_BLOCK: {
    scope_init(
        &expr->block.scope, &operand_block->scope, expr->block.stmts.count);
  } break;
  }

  return NULL;
}

static void type_check_expr(
    analyzer_t *a,
    block_t *operand_block,
    block_t *operation_block,
    expr_t *expr,
    type_t *expected_type) {
  expr_as_type(a, operand_block, expr);

  if (operation_block == NULL) operation_block = operand_block;

  memset(&expr->type, 0, sizeof(expr->type));

  switch (expr->kind) {
  case EXPR_PRIMARY: {
    switch (expr->primary.kind) {
    case PRIMARY_INT: {
      type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
      memset(ty, 0, sizeof(*ty));
      expr->type = ty;

      ty->kind = TYPE_PRIMITIVE;

      if (expected_type && expected_type->kind == TYPE_PRIMITIVE &&
          expected_type->prim > PRIM_TYPE_NUM_BEGIN &&
          expected_type->prim < PRIM_TYPE_NUM_END) {
        ty->prim = expected_type->prim;
      } else {
        ty->prim = PRIM_TYPE_I64;
      }
    } break;

    case PRIMARY_FLOAT: {
      type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
      memset(ty, 0, sizeof(*ty));
      expr->type = ty;

      ty->kind = TYPE_PRIMITIVE;

      if (expected_type && expected_type->kind == TYPE_PRIMITIVE &&
          expected_type->prim > PRIM_TYPE_FLOAT_BEGIN &&
          expected_type->prim < PRIM_TYPE_FLOAT_END) {
        ty->prim = expected_type->prim;
      } else {
        ty->prim = PRIM_TYPE_F64;
      }
    } break;

    case PRIMARY_BOOL: {
      type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
      memset(ty, 0, sizeof(*ty));
      expr->type = ty;

      ty->kind = TYPE_PRIMITIVE;
      ty->prim = PRIM_TYPE_BOOL;
    } break;

    case PRIMARY_NULL: {
      type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
      memset(ty, 0, sizeof(*ty));
      expr->type = ty;

      ty->kind = TYPE_PTR;
      if (expected_type) {
        if (expected_type->kind == TYPE_PTR) {
          ty->subtype = expected_type->subtype;
        }
      }
    } break;

    case PRIMARY_STRING: {
      type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
      memset(ty, 0, sizeof(*ty));
      expr->type = ty;

      ty->kind = TYPE_STRING;
    } break;

    case PRIMARY_CSTRING: {
      type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
      memset(ty, 0, sizeof(*ty));
      expr->type = ty;

      ty->kind    = TYPE_PTR;
      ty->subtype = bump_alloc(&a->ctx->alloc, sizeof(type_t));
      memset(ty->subtype, 0, sizeof(*ty->subtype));
      ty->subtype->kind = TYPE_PRIMITIVE;
      ty->subtype->prim = PRIM_TYPE_U8;
    } break;

    case PRIMARY_PRIMITIVE_TYPE: {
      type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
      memset(ty, 0, sizeof(*ty));
      expr->type = ty;

      ty->kind = TYPE_TYPE;
    } break;

    case PRIMARY_IDENT: {
      symbol_t *sym = get_expr_sym(expr, &operation_block->scope);
      if (!sym) break;

      switch (sym->kind) {
      case SYMBOL_GLOBAL_VAR:
      case SYMBOL_LOCAL_VAR: {
        expr->type = sym->var_decl->type;
      } break;

      case SYMBOL_CONST_DECL: {
        expr->type = sym->const_decl->type;
      } break;

      default: break;
      }

    } break;
    }
  } break;

  case EXPR_EXPR: {
    type_check_expr(a, operand_block, NULL, expr->expr, expected_type);
    expr->type = expr->expr->type;
    return;
  } break;

  case EXPR_INTRIN: {
    switch (expr->intrin.kind) {
    case INTRIN_SIZEOF: {
      type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
      memset(ty, 0, sizeof(*ty));
      ty->kind = TYPE_PRIMITIVE;
      if (expected_type && expected_type->kind == TYPE_PRIMITIVE &&
          expected_type->prim > PRIM_TYPE_NUM_BEGIN &&
          expected_type->prim < PRIM_TYPE_NUM_END) {
        ty->prim = expected_type->prim;
      } else {
        ty->prim = PRIM_TYPE_I64;
      }

      expr->type = ty;

      For(param, expr->intrin.params) {
        type_check_expr(a, operand_block, NULL, param, NULL);
      }
    } break;
    case INTRIN_ASSERT: {
      static type_t void_type;
      void_type.kind = TYPE_PRIMITIVE;
      void_type.prim = PRIM_TYPE_VOID;
      expr->type     = &void_type;

      For(param, expr->intrin.params) {
        type_check_expr(a, operand_block, NULL, param, NULL);

        if (!((param->type->kind == TYPE_PRIMITIVE &&
               param->type->prim > PRIM_TYPE_INT_BEGIN &&
               param->type->prim < PRIM_TYPE_INT_END) ||
              param->type->kind == TYPE_PTR)) {
          error(a, param->pos, "invalid parameter for assert");
        }
      }
    } break;
    }
  } break;

  case EXPR_ACCESS: {
    block_t *expr_block = NULL;
    expr_t *inner = get_access_expr(operand_block, expr, &expr_block, NULL);

    if (inner) {
      type_check_expr(a, operand_block, expr_block, inner, expected_type);
      expr->type = inner->type;
    }
  } break;

  case EXPR_UNARY: {
    switch (expr->unary.kind) {
    case UNOP_DEREF: {
      type_check_expr(a, operand_block, NULL, expr->right, NULL);

      if (!expr->right->type) break;

      if (expr->right->type->kind == TYPE_TYPE) {
        type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
        memset(ty, 0, sizeof(*ty));
        expr->type = ty;

        ty->kind = TYPE_TYPE;
        break;
      }

      if (expr->right->type->kind != TYPE_PTR) {
        sb_reset(&a->ctx->sb);
        print_type(&a->ctx->sb, expr->right->type);
        strbuf_t type_name = bump_strdup(&a->ctx->alloc, sb_build(&a->ctx->sb));

        error(
            a,
            expr->right->pos,
            "can only dereference pointer types, got type: '%.*s'",
            (int)type_name.count,
            type_name.buf);
        break;
      }

      expr->type = expr->right->type->subtype;
    } break;

    case UNOP_ADDRESS: {
      type_t *sub_expected_type = NULL;
      if (expected_type && expected_type->kind == TYPE_PTR) {
        sub_expected_type = expected_type->subtype;
      }

      type_check_expr(a, operand_block, NULL, expr->right, sub_expected_type);
      if (!expr->right->type) break;

      type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
      memset(ty, 0, sizeof(*ty));
      expr->type = ty;

      ty->kind    = TYPE_PTR;
      ty->subtype = expr->right->type;
    } break;

    case UNOP_NOT: {
      // TODO: unimplemented
    } break;
    }
  } break;

  case EXPR_BINARY: {
    // TODO
  } break;

  case EXPR_SUBSCRIPT: {
    type_check_expr(a, operand_block, NULL, expr->left, NULL);
    if (expr->left->type->kind != TYPE_ARRAY) {
      error(a, expr->left->pos, "can't index a non array type");
      break;
    }

    type_check_expr(a, operand_block, NULL, expr->right, NULL);
    if (!(expr->right->type->kind == TYPE_PRIMITIVE &&
          expr->right->type->prim > PRIM_TYPE_INT_BEGIN &&
          expr->right->type->prim < PRIM_TYPE_INT_END)) {
      error(a, expr->right->pos, "can only use integers as indices");
      break;
    }

    assert(expr->left->type->kind == TYPE_ARRAY);
    assert(expr->left->type->subtype);
    expr->type = expr->left->type->subtype;
  } break;

  case EXPR_PROC_CALL: {
    type_check_expr(a, operation_block, NULL, expr->proc_call.expr, NULL);

    if (expr->proc_call.sig == NULL) break;

    proc_signature_t *proc_sig = expr->proc_call.sig;

    expr_slice_t return_types = proc_sig->return_types;

    if (return_types.count >= 1) {
      expr_as_type(a, operand_block, &return_types.buf[0]);
      expr->type = return_types.buf[0].as_type;
    } else {
      static type_t void_type;
      void_type.kind = TYPE_PRIMITIVE;
      void_type.prim = PRIM_TYPE_VOID;
      expr->type     = &void_type;
    }
  } break;

  case EXPR_PROC_PTR: {
    type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
    memset(ty, 0, sizeof(*ty));
    expr->type = ty;

    ty->kind     = TYPE_TYPE;
    ty->proc_sig = &expr->proc.sig;
  } break;

  case EXPR_PROC: {
    type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
    memset(ty, 0, sizeof(*ty));
    expr->type = ty;

    ty->kind     = TYPE_PROC;
    ty->proc_sig = &expr->proc.sig;
  } break;

  case EXPR_STRUCT: {
    static type_t ty = {.kind = TYPE_TYPE};
    expr->type       = &ty;
  } break;

  case EXPR_ARRAY_TYPE: {
    static type_t ty = {.kind = TYPE_TYPE};
    expr->type       = &ty;

    type_check_expr(a, operand_block, NULL, expr->array.sub_expr, &ty);

    static type_t size_ty = {.kind = TYPE_PRIMITIVE, .prim = PRIM_TYPE_U64};
    type_check_expr(a, operand_block, NULL, expr->array.size_expr, &size_ty);
  } break;

  case EXPR_IMPORT: {
    static type_t ty = {.kind = TYPE_NAMESPACE};
    expr->type       = &ty;
  } break;

  case EXPR_BLOCK: {
    static type_t ty = {.kind = TYPE_PRIMITIVE, .prim = PRIM_TYPE_VOID};
    expr->type       = &ty;
  } break;
  }

  if (!expr->type) {
    // TODO: remove this error message, but keep the return
    // This message is only for debugging purposes
    error(a, expr->pos, "undefined type");
    return;
  }

  if (expected_type) {
    if (!equivalent_types(expected_type, expr->type)) {
      sb_reset(&a->ctx->sb);
      print_type(&a->ctx->sb, expected_type);
      strbuf_t expected_name =
          bump_strdup(&a->ctx->alloc, sb_build(&a->ctx->sb));

      sb_reset(&a->ctx->sb);
      print_type(&a->ctx->sb, expr->type);
      strbuf_t actual_name = bump_strdup(&a->ctx->alloc, sb_build(&a->ctx->sb));

      error(
          a,
          expr->pos,
          "type mismatch, expected: '%.*s', instead got '%.*s'",
          (int)expected_name.count,
          expected_name.buf,
          (int)actual_name.count,
          actual_name.buf);
    }
  }
}

static void add_stmt(analyzer_t *a, block_t *block, stmt_t *stmt) {
  switch (stmt->kind) {
  case STMT_CONST_DECL: {
    if (scope_get(&block->scope, stmt->const_decl.name)) {
      error(a, stmt->pos, "duplicate declaration");
    }

    symbol_t *sym   = scope_add(&block->scope, a->ctx, stmt->const_decl.name);
    sym->kind       = SYMBOL_CONST_DECL;
    sym->const_decl = &stmt->const_decl;
    stmt->const_decl.sym = sym;

    expr_t *expr = inner_expr(&stmt->const_decl.expr);
    switch (expr->kind) {
    case EXPR_PROC: {
      expr->proc.name = stmt->const_decl.name;
    } break;

    default: break;
    }
  } break;

  case STMT_VAR_DECL: {
    if (scope_get_local(&block->scope, stmt->var_decl.name)) {
      error(a, stmt->pos, "duplicate declaration");
      break;
    }

    symbol_t *sym = scope_add(&block->scope, a->ctx, stmt->var_decl.name);
    sym->kind     = (scope_proc(&block->scope) == NULL) ? SYMBOL_GLOBAL_VAR
                                                    : SYMBOL_LOCAL_VAR;
    sym->var_decl      = &stmt->var_decl;
    stmt->var_decl.sym = sym;
  } break;

  default: break;
  }
}

static bool import_is_circular(ast_t *ast, ast_t *origin) {
  if (ast == origin) return true;

  For(import, ast->imports) {
    if (import_is_circular(*import, origin)) return true;
  }

  return false;
}

static inline error_set_t
process_import(analyzer_t *a, expr_t *expr, import_t *import) {
  char *dir = get_file_dir(a->ast->file->abs_path.buf);

  strbuf_t full_path;
  full_path.count = strlen(dir) + import->path.count;
  full_path.cap   = full_path.count + 1;
  full_path.buf   = bump_alloc(&a->ctx->alloc, full_path.cap);

  snprintf(
      full_path.buf,
      full_path.cap,
      "%s%.*s",
      dir,
      (int)import->path.count,
      import->path.buf);

  free(dir);

  // Check if import is circular
  file_t *file = table_get(&a->ctx->file_table, full_path);
  if (file) {
    if (import_is_circular(file->ast, a->ast)) {
      error_set_t result;
      memset(&result, 0, sizeof(result));
      error_t err = {
          .pos = expr->pos,
          .msg = STR("circular import detected"),
      };
      APPEND(result.errors, err);
      return result;
    }
  }

  import->ast = bump_alloc(&a->ctx->alloc, sizeof(ast_t));
  APPEND(a->ast->imports, import->ast);

  return ctx_process_file(a->ctx, full_path, &import->ast, &expr->pos);
}

static void add_import(analyzer_t *a, block_t *block, stmt_t *stmt) {
  switch (stmt->kind) {
  case STMT_CONST_DECL: {
    expr_t *expr = inner_expr(&stmt->const_decl.expr);

    switch (expr->kind) {
    case EXPR_IMPORT: {
      error_set_t result = process_import(a, expr, &expr->import);
      if (result.errors.count > 0) {
        For(err, result.errors) APPEND(a->errors, *err);
        break;
      }

    } break;

    default: break;
    }
  } break;

  case STMT_USING: {
    expr_t *expr = inner_expr(&stmt->expr);

    switch (expr->kind) {
    case EXPR_IMPORT: {
      error_set_t result = process_import(a, expr, &expr->import);
      if (result.errors.count > 0) {
        For(err, result.errors) APPEND(a->errors, *err);
        break;
      }

      APPEND(block->scope.siblings, expr->import.ast->block.scope);
    } break;

    default: break;
    }
  } break;

  default: break;
  }
}

static void symbol_check_stmt(analyzer_t *a, block_t *block, stmt_t *stmt) {
  switch (stmt->kind) {
  case STMT_CONST_DECL: {
    if (stmt->const_decl.typed) {
      symbol_check_expr(a, block, NULL, &stmt->const_decl.type_expr);
    }

    symbol_check_expr(a, block, NULL, &stmt->const_decl.expr);

    if (!is_expr_const(&stmt->const_decl.expr, &block->scope)) {
      error(a, stmt->pos, "can't assign non constant expression to a constant");
      break;
    }
  } break;

  case STMT_VAR_DECL: {
    if (stmt->var_decl.flags & VAR_DECL_HAS_TYPE) {
      symbol_check_expr(a, block, NULL, &stmt->var_decl.type_expr);
    }

    if (stmt->var_decl.flags & VAR_DECL_HAS_EXPR) {
      symbol_check_expr(a, block, NULL, &stmt->var_decl.expr);
    }
  } break;

  case STMT_VAR_ASSIGN: {
    symbol_t *assigned_sym =
        symbol_check_expr(a, block, NULL, &stmt->var_assign.assigned);

    if (assigned_sym) {
      switch (assigned_sym->kind) {
      case SYMBOL_GLOBAL_VAR:
      case SYMBOL_LOCAL_VAR: break;

      default: {
        error(a, stmt->pos, "can only assign to a variable");
      } break;
      }
    }

    symbol_check_expr(a, block, NULL, &stmt->var_assign.expr);
  } break;

  case STMT_EXPR: {
    symbol_check_expr(a, block, NULL, &stmt->expr);
  } break;

  case STMT_RETURN: {
    proc_t *proc = scope_proc(&block->scope);
    if (!proc) {
      error(a, stmt->pos, "return statement can only go inside procedures");
      break;
    }

    if (proc->sig.return_types.count != stmt->ret.exprs.count) {
      error(
          a,
          stmt->pos,
          "return statement must return %zu value(s), instead got %zu",
          proc->sig.return_types.count,
          stmt->ret.exprs.count);
      break;
    }

    For(expr, stmt->ret.exprs) { symbol_check_expr(a, block, NULL, expr); }
  } break;

  case STMT_USING: {
    symbol_t *sym      = symbol_check_expr(a, block, NULL, &stmt->expr);
    expr_t *using_expr = inner_expr(&stmt->expr);

    switch (using_expr->kind) {
    case EXPR_IMPORT: break;

    default: {
      if (sym) {
        switch (sym->kind) {
        case SYMBOL_CONST_DECL: {
          expr_t *sym_expr = inner_expr(&sym->const_decl->expr);

          switch (sym_expr->kind) {
          case EXPR_IMPORT: {
            APPEND(block->scope.siblings, sym_expr->import.ast->block.scope);
          } break;

          default: {
            error(
                a,
                stmt->pos,
                "'using' expression does not refer to a valid symbol");
          } break;
          }

        } break;

        default: {
          error(
              a,
              stmt->pos,
              "'using' expression does not refer to a valid symbol");
        } break;
        }
      } else {
        error(a, stmt->pos, "'using' invalid expression");
      }
    } break;
    }

  } break;

  case STMT_DUMMY: break;
  }
}

static void type_check_stmt(analyzer_t *a, block_t *block, stmt_t *stmt) {
  switch (stmt->kind) {
  case STMT_CONST_DECL: {
    if (stmt->const_decl.typed) {
      if (!expr_as_type(a, block, &stmt->const_decl.type_expr)) {
        error(
            a,
            stmt->const_decl.type_expr.pos,
            "expression does not represent a type");
        break;
      }
    }

    type_t *expected_type = NULL;
    if (stmt->const_decl.typed)
      expected_type = stmt->const_decl.type_expr.as_type;

    type_check_expr(a, block, NULL, &stmt->const_decl.expr, expected_type);

    if (expected_type)
      stmt->const_decl.type = expected_type;
    else
      stmt->const_decl.type = stmt->const_decl.expr.type;

    if (!stmt->const_decl.type) break;

    if (expr_as_type(a, block, &stmt->const_decl.expr)) {
      // If the constant expression is a type, give the type a name
      stmt->const_decl.type->name = stmt->const_decl.name;
    }
  } break;

  case STMT_VAR_DECL: {
    type_t type;
    memset(&type, 0, sizeof(type));

    if (stmt->var_decl.flags & VAR_DECL_HAS_TYPE) {
      if (!expr_as_type(a, block, &stmt->var_decl.type_expr)) {
        error(
            a,
            stmt->var_decl.type_expr.pos,
            "expression does not represent a type");
        break;
      }
    }

    type_t *expected_type = NULL;
    if (stmt->var_decl.flags & VAR_DECL_HAS_TYPE)
      expected_type = stmt->var_decl.type_expr.as_type;

    if (stmt->var_decl.flags & VAR_DECL_HAS_EXPR)
      type_check_expr(a, block, NULL, &stmt->var_decl.expr, expected_type);

    if (expected_type)
      stmt->var_decl.type = expected_type;
    else if (stmt->var_decl.flags & VAR_DECL_HAS_EXPR)
      stmt->var_decl.type = stmt->var_decl.expr.type;

    if (!stmt->var_decl.type) break;

    switch (stmt->var_decl.type->kind) {
    case TYPE_PRIMITIVE: {
      if (stmt->var_decl.type->prim == PRIM_TYPE_VOID) {
        sb_reset(&a->ctx->sb);
        print_type(&a->ctx->sb, stmt->var_decl.type);
        strbuf_t type_name = bump_strdup(&a->ctx->alloc, sb_build(&a->ctx->sb));

        error(
            a,
            stmt->pos,
            "variable declared with invalid type: '%.*s'",
            (int)type_name.count,
            type_name.buf);
      }
    } break;

    case TYPE_UNDEFINED:
    case TYPE_TYPE:
    case TYPE_NAMESPACE: {
      sb_reset(&a->ctx->sb);
      print_type(&a->ctx->sb, stmt->var_decl.type);
      strbuf_t type_name = bump_strdup(&a->ctx->alloc, sb_build(&a->ctx->sb));

      error(
          a,
          stmt->pos,
          "variable declared with invalid type: '%.*s'",
          (int)type_name.count,
          type_name.buf);
    } break;

    default: break;
    }
  } break;

  case STMT_VAR_ASSIGN: {
    type_check_expr(a, block, NULL, &stmt->var_assign.assigned, NULL);
    type_check_expr(
        a, block, NULL, &stmt->var_assign.expr, stmt->var_assign.assigned.type);
  } break;

  case STMT_EXPR: {
    type_check_expr(a, block, NULL, &stmt->expr, NULL);
  } break;

  case STMT_RETURN: {
    proc_t *proc = scope_proc(&block->scope);
    if (proc) {
      if (proc->sig.return_types.count == stmt->ret.exprs.count) {
        for (size_t i = 0; i < proc->sig.return_types.count; i++) {
          expr_t *expr     = &stmt->ret.exprs.buf[i];
          expr_t *ret_type = &proc->sig.return_types.buf[i];
          type_check_expr(a, block, NULL, expr, ret_type->as_type);
        }
      }
    }
  } break;

  case STMT_USING: {
    type_check_expr(a, block, NULL, &stmt->expr, NULL);
  } break;

  case STMT_DUMMY: break;
  }
}

static void expr_analyze_children(analyzer_t *a, block_t *block, expr_t *expr) {
  switch (expr->kind) {
  case EXPR_PROC: {
    analyze_block_ordered(a, &expr->proc.block);
  } break;

  case EXPR_BLOCK: {
    analyze_block_ordered(a, &expr->block);
  } break;

  case EXPR_EXPR: {
    expr_analyze_children(a, block, expr->expr);
  } break;

  case EXPR_ACCESS: {
    block_t *expr_block = NULL;
    expr_t *inner       = get_access_expr(block, expr, &expr_block, NULL);

    if (inner) {
      expr_analyze_children(a, block, inner);
    }
  } break;

  case EXPR_PROC_CALL: {
    expr_analyze_children(a, block, expr->proc_call.expr);

    if (expr->proc_call.sig == NULL) break;
    proc_signature_t *proc_sig = expr->proc_call.sig;

    for (size_t i = 0; i < expr->proc_call.params.count; i++) {
      expr_t *param = &expr->proc_call.params.buf[i];

      type_t *expected = NULL;
      if (i < proc_sig->params.count) {
        expected = proc_sig->params.buf[i].type;
      }

      symbol_check_expr(a, block, NULL, param);
      type_check_expr(a, block, NULL, param, expected);
      expr_analyze_children(a, block, param);
    }
  } break;

  default: break;
  }
}

static void stmt_analyze_children(analyzer_t *a, block_t *block, stmt_t *stmt) {
  switch (stmt->kind) {
  case STMT_CONST_DECL: {
    if (stmt->const_decl.typed) {
      expr_analyze_children(a, block, &stmt->const_decl.type_expr);
    }
    expr_analyze_children(a, block, &stmt->const_decl.expr);
  } break;
  case STMT_VAR_DECL: {
    if (stmt->var_decl.flags & VAR_DECL_HAS_TYPE) {
      expr_analyze_children(a, block, &stmt->var_decl.type_expr);
    }
    if (stmt->var_decl.flags & VAR_DECL_HAS_EXPR) {
      expr_analyze_children(a, block, &stmt->var_decl.expr);
    }
  } break;
  case STMT_VAR_ASSIGN: {
    expr_analyze_children(a, block, &stmt->var_assign.assigned);
    expr_analyze_children(a, block, &stmt->var_assign.expr);
  } break;
  case STMT_USING: {
    expr_analyze_children(a, block, &stmt->expr);
  } break;
  case STMT_EXPR: {
    expr_analyze_children(a, block, &stmt->expr);
  } break;
  case STMT_RETURN: {
    For(expr, stmt->ret.exprs) { expr_analyze_children(a, block, expr); }
  } break;

  case STMT_DUMMY: break;
  }
}

static void analyze_block_unordered(analyzer_t *a, block_t *block) {
  For(stmt, block->stmts) { add_stmt(a, block, stmt); }
  For(stmt, block->stmts) { add_import(a, block, stmt); }

  if (a->errors.count > 0) return;

  For(stmt, block->stmts) { symbol_check_stmt(a, block, stmt); }
  For(stmt, block->stmts) {
    type_check_stmt(a, block, stmt);
    stmt_analyze_children(a, block, stmt);
  }
}

static void analyze_block_ordered(analyzer_t *a, block_t *block) {
  For(stmt, block->stmts) {
    add_stmt(a, block, stmt);
    add_import(a, block, stmt);

    if (a->errors.count > 0) return;

    symbol_check_stmt(a, block, stmt);
    type_check_stmt(a, block, stmt);
    stmt_analyze_children(a, block, stmt);
  }
}

void analyzer_init(analyzer_t *a, ctx_t *ctx) {
  memset(a, 0, sizeof(*a));
  a->ctx = ctx;
}

error_set_t analyzer_analyze(analyzer_t *a, ast_t *ast) {
  memset(&a->errors, 0, sizeof(a->errors));

  a->ast = ast;

  scope_init(&ast->block.scope, NULL, ast->block.stmts.count);
  analyze_block_unordered(a, &ast->block);

  return (error_set_t){
      .type   = RESULT_ANALYZER,
      .errors = a->errors,
  };
}
