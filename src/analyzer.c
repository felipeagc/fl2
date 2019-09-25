#include "analyzer.h"

#include "expr.h"
#include "filesystem.h"
#include "parser.h"
#include "scanner.h"
#include "type.h"
#include <assert.h>
#include <stdio.h>

#define dbg(str)                                                               \
  printf(                                                                      \
      "%s:%s:%u :: '%.*s'\n",                                                  \
      __FILE__,                                                                \
      __func__,                                                                \
      __LINE__,                                                                \
      (int)(str).count,                                                        \
      (str).buf)

#define dbgf(fmt, ...)                                                         \
  printf("%s:%s:%u :: " fmt "\n", __FILE__, __func__, __LINE__, __VA_ARGS__)

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
    case PRIMARY_STRING: break;

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

  case EXPR_EXPR: {
    res           = expr_as_type(a, block, expr->expr);
    expr->as_type = expr->expr->as_type;
    return res;
  } break;

  case EXPR_ACCESS: {
    symbol_t *sym = get_expr_sym(block, expr->access.left);

    if (sym) {
      switch (sym->kind) {
      case SYMBOL_CONST_DECL: {
        expr_t *decl_expr = inner_expr(&sym->const_decl->expr);

        switch (decl_expr->kind) {
        case EXPR_IMPORT: {
          res = expr_as_type(
              a, &decl_expr->import.ast->block, expr->access.right);
          expr->as_type = expr->access.right->as_type;
          return res;
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
  } break;

  case EXPR_PROC: {
  } break;

  case EXPR_PROC_PTR: {
    type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
    memset(ty, 0, sizeof(*ty));
    expr->as_type = ty;
    ty->kind      = TYPE_PROC;
    ty->proc_sig  = &expr->proc.sig;
    res           = true;
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
    case PRIMARY_PRIMITIVE_TYPE:
    case PRIMARY_STRING: break;

    case PRIMARY_IDENT: {
      symbol_t *sym = scope_get(&operand_block->scope, expr->primary.string);

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

  case EXPR_ACCESS: {
    symbol_t *sym =
        symbol_check_expr(a, operand_block, NULL, expr->access.left);
    if (sym) {
      switch (sym->kind) {
      case SYMBOL_CONST_DECL: {
        expr_t *inner = inner_expr(&sym->const_decl->expr);
        switch (inner->kind) {
        case EXPR_IMPORT: {
          return symbol_check_expr(
              a, operand_block, &inner->import.ast->block, expr->access.right);
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
    symbol_t *proc_sym =
        symbol_check_expr(a, operation_block, NULL, expr->proc_call.expr);
    proc_signature_t *proc_sig = NULL;

    if (proc_sym) {
      switch (proc_sym->kind) {
      case SYMBOL_CONST_DECL: {
        if (proc_sym->const_decl->type->kind == TYPE_PROC) {
          proc_sig = proc_sym->const_decl->type->proc_sig;
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

    if (proc_sig->params.count != expr->proc_call.params.count) {
      error(a, expr->pos, "wrong procedure parameter count");
    }

    For(param, expr->proc_call.params) {
      symbol_check_expr(a, operand_block, NULL, param);
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

    if ((expr->proc.sig.flags & (PROC_FLAG_EXTERN | PROC_FLAG_INLINE)) ==
        (PROC_FLAG_EXTERN | PROC_FLAG_INLINE)) {
      error(
          a,
          expr->pos,
          "procedure cannot be extern and inline at the same time");
      break;
    }

    if ((expr->proc.sig.flags & (PROC_FLAG_NO_BODY | PROC_FLAG_INLINE)) ==
        (PROC_FLAG_NO_BODY | PROC_FLAG_INLINE)) {
      error(a, expr->pos, "inline procedures must have a body");
      break;
    }

    if (expr->proc.sig.flags & PROC_FLAG_EXTERN) {
      proc_t *block_proc = scope_proc(&operand_block->scope);
      if (block_proc) {
        error(a, expr->pos, "extern procedure has to be top level");
      }
    }

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

    case PRIMARY_STRING: {
      type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
      memset(ty, 0, sizeof(*ty));
      expr->type = ty;

      ty->kind = TYPE_STRING;
    } break;

    case PRIMARY_PRIMITIVE_TYPE: {
      type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
      memset(ty, 0, sizeof(*ty));
      expr->type = ty;

      ty->kind = TYPE_TYPE;
    } break;

    case PRIMARY_IDENT: {
      symbol_t *sym = get_expr_sym(operand_block, expr);
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

  case EXPR_ACCESS: {
    symbol_t *sym = get_expr_sym(operand_block, expr->access.left);
    if (sym) {
      switch (sym->kind) {
      case SYMBOL_CONST_DECL: {
        expr_t *decl_expr = inner_expr(&sym->const_decl->expr);

        switch (decl_expr->kind) {
        case EXPR_IMPORT: {
          type_check_expr(
              a,
              operand_block,
              &decl_expr->import.ast->block,
              expr->access.right,
              expected_type);
          expr->type = expr->access.right->type;
          return;
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
    symbol_t *proc_sym = get_expr_sym(operation_block, expr->proc_call.expr);
    proc_signature_t *proc_sig = NULL;

    if (proc_sym) {
      switch (proc_sym->kind) {
      case SYMBOL_CONST_DECL: {
        if (proc_sym->const_decl->type->kind == TYPE_PROC) {
          proc_sig = proc_sym->const_decl->type->proc_sig;
        }
      } break;
      default: break;
      }
    }

    if (proc_sig == NULL) break;

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

    if (proc_sig->params.count == expr->proc_call.params.count) {
      for (size_t i = 0; i < proc_sig->params.count; i++) {
        type_check_expr(
            a,
            operand_block,
            NULL,
            &expr->proc_call.params.buf[i],
            proc_sig->params.buf[i].type);
      }
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
    type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
    memset(ty, 0, sizeof(*ty));
    expr->type = ty;

    ty->kind = TYPE_TYPE;
  } break;

  case EXPR_IMPORT: {
    type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
    memset(ty, 0, sizeof(*ty));
    expr->type = ty;

    ty->kind = TYPE_NAMESPACE;
  } break;

  case EXPR_BLOCK: {
    type_t *ty = bump_alloc(&a->ctx->alloc, sizeof(type_t));
    memset(ty, 0, sizeof(*ty));
    expr->type = ty;

    ty->kind = TYPE_PRIMITIVE;
    ty->prim = PRIM_TYPE_VOID;
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
    case EXPR_IMPORT: {
      char *dir = get_file_dir(a->ast->file->abs_path.buf);

      strbuf_t full_path;
      full_path.count = strlen(dir) + 1 + expr->import.path.count;
      full_path.cap   = full_path.count + 1;
      full_path.buf   = bump_alloc(&a->ctx->alloc, full_path.cap);

      snprintf(
          full_path.buf,
          full_path.cap,
          "%s/%.*s",
          dir,
          (int)expr->import.path.count,
          expr->import.path.buf);

      free(dir);

      expr->import.ast = bump_alloc(&a->ctx->alloc, sizeof(ast_t));

      error_set_t result =
          ctx_process_file(a->ctx, full_path, expr->import.ast);
      if (result.errors.count > 0) {
        For(err, result.errors) APPEND(a->errors, *err);
      }
    } break;

    default: break;
    }
  } break;

  case STMT_USING: {
    expr_t *expr = inner_expr(&stmt->expr);

    switch (expr->kind) {
    case EXPR_IMPORT: {
      char *dir = get_file_dir(a->ast->file->abs_path.buf);

      strbuf_t full_path;
      full_path.count = strlen(dir) + 1 + expr->import.path.count;
      full_path.cap   = full_path.count + 1;
      full_path.buf   = bump_alloc(&a->ctx->alloc, full_path.cap);

      snprintf(
          full_path.buf,
          full_path.cap,
          "%s/%.*s",
          dir,
          (int)expr->import.path.count,
          expr->import.path.buf);

      free(dir);

      ast_t *ast         = bump_alloc(&a->ctx->alloc, sizeof(ast_t));
      error_set_t result = ctx_process_file(a->ctx, full_path, ast);
      if (result.errors.count > 0) {
        For(err, result.errors) APPEND(a->errors, *err);
        break;
      }

      APPEND(block->scope.siblings, ast->block.scope);
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

static void symbol_check_stmt(analyzer_t *a, block_t *block, stmt_t *stmt) {
  switch (stmt->kind) {
  case STMT_CONST_DECL: {
    if (stmt->const_decl.typed) {
      symbol_check_expr(a, block, NULL, &stmt->const_decl.type_expr);
    }

    symbol_t *sym = symbol_check_expr(a, block, NULL, &stmt->const_decl.expr);

    if (sym) {
      switch (sym->kind) {
      case SYMBOL_GLOBAL_VAR:
      case SYMBOL_LOCAL_VAR: {
        error(a, stmt->pos, "constant cannot refer to a variable");
      } break;

      default: break;
      }
    }
  } break;

  case STMT_VAR_DECL: {
    if (stmt->var_decl.flags & VAR_DECL_HAS_TYPE) {
      symbol_check_expr(a, block, NULL, &stmt->var_decl.type_expr);
    }

    symbol_check_expr(a, block, NULL, &stmt->var_decl.expr);
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

static void
stmt_analyze_child_block(analyzer_t *a, block_t *block, stmt_t *stmt);

// Only analyzes children expressions to the expression passed in
// Does not analyze the expression itself, only useful for analyzing
// children scopes, etc
static void
expr_analyze_child_block(analyzer_t *a, block_t *block, expr_t *expr) {
  expr = inner_expr(expr);
  switch (expr->kind) {
  case EXPR_PROC: {
    analyze_block_ordered(a, &expr->proc.block);
  } break;

  case EXPR_BLOCK: {
    analyze_block_ordered(a, &expr->block);
  } break;

  default: break;
  }
}

static void
stmt_analyze_child_block(analyzer_t *a, block_t *block, stmt_t *stmt) {
  switch (stmt->kind) {
  case STMT_CONST_DECL: {
    expr_analyze_child_block(a, block, &stmt->const_decl.expr);
  } break;

  case STMT_VAR_DECL: {
    expr_analyze_child_block(a, block, &stmt->var_decl.expr);
  } break;

  case STMT_VAR_ASSIGN: {
    expr_analyze_child_block(a, block, &stmt->var_assign.expr);
  } break;

  case STMT_EXPR: {
    expr_analyze_child_block(a, block, &stmt->expr);
  } break;

  default: break;
  }
}

static void analyze_block_unordered(analyzer_t *a, block_t *block) {
  For(stmt, block->stmts) { add_stmt(a, block, stmt); }
  For(stmt, block->stmts) { symbol_check_stmt(a, block, stmt); }
  For(stmt, block->stmts) { type_check_stmt(a, block, stmt); }
  For(stmt, block->stmts) { stmt_analyze_child_block(a, block, stmt); }
}

static void analyze_block_ordered(analyzer_t *a, block_t *block) {
  For(stmt, block->stmts) {
    add_stmt(a, block, stmt);
    symbol_check_stmt(a, block, stmt);
    type_check_stmt(a, block, stmt);
    stmt_analyze_child_block(a, block, stmt);
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
