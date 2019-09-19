#include "analyzer.h"

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

static bool
expr_as_type(analyzer_t *a, block_t *block, expr_t *expr, type_t *type) {
  switch (expr->kind) {
  case EXPR_PRIMARY: {

    switch (expr->primary.kind) {
    case PRIMARY_INT:
    case PRIMARY_FLOAT: return false;

    case PRIMARY_PRIMITIVE_TYPE: {
      type->kind = TYPE_PRIMITIVE;
      type->prim = expr->primary.prim_type;
      return true;
    } break;

    case PRIMARY_IDENT: {
      symbol_t *sym = scope_get(&block->scope, expr->primary.ident);
      if (sym) {
        switch (sym->kind) {
        case SYMBOL_STRUCT: {
          type->kind = TYPE_STRUCT;
          type->str  = &sym->str;
          return true;
        } break;

        default: break;
        }
      }
    } break;
    }

  } break;

  case EXPR_EXPR: {
    return expr_as_type(a, block, expr->expr, type);
  } break;

  case EXPR_ACCESS: {
  } break;

  case EXPR_UNARY: {
    // TODO
  } break;

  case EXPR_BINARY: {
    // TODO
    return false;
  } break;

  case EXPR_PROC_CALL: {
    return false;
  } break;

  case EXPR_PROC: {
    // TODO: proc type
    return false;
  } break;

  case EXPR_STRUCT: {

  } break;

  case EXPR_IMPORT: {
    return false;
  } break;

  case EXPR_BLOCK: {
    return false;
  } break;
  }

  return false;
}

static symbol_t *
symbol_check_expr(analyzer_t *a, block_t *block, expr_t *expr) {
  switch (expr->kind) {
  case EXPR_PRIMARY: {

    switch (expr->primary.kind) {
    case PRIMARY_INT:
    case PRIMARY_FLOAT:
    case PRIMARY_PRIMITIVE_TYPE: break;

    case PRIMARY_IDENT: {
      symbol_t *sym = scope_get(&block->scope, expr->primary.ident);
      if (!sym) {
        error(a, expr->pos, "invalid identifier");
        break;
      }

      return sym;
    } break;
    }

  } break;

  case EXPR_EXPR: {
    return symbol_check_expr(a, block, expr->expr);
  } break;

  case EXPR_ACCESS: {
    symbol_t *sym = symbol_check_expr(a, block, expr->access.left);
    if (sym) {
      switch (sym->kind) {
      case SYMBOL_NAMESPACE: {
        return symbol_check_expr(a, &sym->ast.block, expr->access.right);
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
    symbol_check_expr(a, block, expr->proc_call.expr);
    For(param, expr->proc_call.params) { symbol_check_expr(a, block, param); }
    return NULL;
  } break;

  case EXPR_PROC: {

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

static void
type_check_expr(analyzer_t *a, block_t *block, expr_t *expr, type_t *out_type) {
  memset(out_type, 0, sizeof(*out_type));

  switch (expr->kind) {
  case EXPR_PRIMARY: {
    switch (expr->primary.kind) {
    case PRIMARY_INT: {
      out_type->kind = TYPE_PRIMITIVE;
      out_type->prim = PRIM_TYPE_I64;
    } break;

    case PRIMARY_FLOAT: {
      out_type->kind = TYPE_PRIMITIVE;
      out_type->prim = PRIM_TYPE_F64;
    } break;

    case PRIMARY_PRIMITIVE_TYPE: break;

    case PRIMARY_IDENT: {

    } break;
    }
  } break;

  case EXPR_EXPR: {
    return type_check_expr(a, block, expr->expr, out_type);
  } break;

  case EXPR_ACCESS: {
    symbol_t *sym = symbol_check_expr(a, block, expr->access.left);
    if (sym) {
      switch (sym->kind) {
      case SYMBOL_NAMESPACE: {
        return type_check_expr(
            a, &sym->ast.block, expr->access.right, out_type);
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

  case EXPR_STRUCT: {

  } break;

  case EXPR_IMPORT: {

  } break;

  case EXPR_BLOCK: {

  } break;
  }
}

static void add_stmt(analyzer_t *a, block_t *block, stmt_t *stmt) {
  switch (stmt->kind) {
  case STMT_CONST_DECL: {
    if (scope_get(&block->scope, stmt->const_decl.name)) {
      error(a, stmt->pos, "duplicate declaration");
    }

    symbol_t *sym = scope_add(&block->scope, a->ctx, stmt->const_decl.name);
    sym->kind     = SYMBOL_CONST;

    expr_t *expr = &stmt->const_decl.expr;

    switch (expr->kind) {
    case EXPR_IMPORT: {
      file_t *file = bump_alloc(&a->ctx->alloc, sizeof(file_t));

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

      if (!file_init(file, a->ctx, full_path)) {
        error(
            a,
            expr->pos,
            "invalid import path: '%.*s'",
            (int)expr->import.path.count,
            expr->import.path.buf);
        break;
      }

      sym->kind = SYMBOL_NAMESPACE;

      error_set_t result = ctx_process_file(a->ctx, file, &sym->ast);
      if (result.errors.count > 0) {
        For(err, result.errors) APPEND(a->errors, *err);
      }
    } break;

    case EXPR_PROC: {
      sym->kind = SYMBOL_PROC;
    } break;

    case EXPR_STRUCT: {
      sym->kind = SYMBOL_STRUCT;
      sym->str  = expr->str;
    } break;

    default: break;
    }
  } break;

  case STMT_USING: {
    expr_t *expr = &stmt->expr;

    switch (expr->kind) {
    case EXPR_IMPORT: {
      file_t *file = bump_alloc(&a->ctx->alloc, sizeof(file_t));

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

      if (!file_init(file, a->ctx, full_path)) {
        error(
            a,
            expr->pos,
            "invalid import path: '%.*s'",
            (int)expr->import.path.count,
            expr->import.path.buf);
        break;
      }

      ast_t *ast         = bump_alloc(&a->ctx->alloc, sizeof(ast_t));
      error_set_t result = ctx_process_file(a->ctx, file, ast);
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
  } break;

  default: break;
  }
}

static void symbol_check_stmt(analyzer_t *a, block_t *block, stmt_t *stmt) {
  switch (stmt->kind) {
  case STMT_CONST_DECL: {
    if (stmt->const_decl.typed) {
      symbol_check_expr(a, block, &stmt->const_decl.type_expr);
    }

    symbol_t *sym = symbol_check_expr(a, block, &stmt->const_decl.expr);

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
    if (stmt->var_decl.typed) {
      symbol_check_expr(a, block, &stmt->var_decl.type_expr);
    }

    symbol_t *sym = symbol_check_expr(a, block, &stmt->var_decl.expr);
    if (sym) {
      switch (sym->kind) {
      case SYMBOL_LOCAL_VAR: {
        proc_t *var_proc   = scope_proc(sym->scope);
        proc_t *block_proc = scope_proc(&block->scope);

        if (var_proc != block_proc) {
          error(a, stmt->pos, "cannot capture variables from outer functions");
        }
      } break;

      default: break;
      }
    }
  } break;

  case STMT_VAR_ASSIGN: {
    symbol_t *assigned_sym =
        symbol_check_expr(a, block, &stmt->var_assign.assigned);

    if (assigned_sym) {
      switch (assigned_sym->kind) {
      case SYMBOL_GLOBAL_VAR:
      case SYMBOL_LOCAL_VAR: break;

      default: {
        error(a, stmt->pos, "can only assign to a variable");
      } break;
      }

      switch (assigned_sym->kind) {
      case SYMBOL_LOCAL_VAR: {
        proc_t *var_proc   = scope_proc(assigned_sym->scope);
        proc_t *block_proc = scope_proc(&block->scope);

        if (var_proc != block_proc) {
          error(a, stmt->pos, "cannot capture variables from outer functions");
        }
      } break;

      default: break;
      }
    }

    symbol_t *sym = symbol_check_expr(a, block, &stmt->var_assign.expr);
    if (sym) {
      switch (sym->kind) {
      case SYMBOL_LOCAL_VAR: {
        proc_t *var_proc   = scope_proc(sym->scope);
        proc_t *block_proc = scope_proc(&block->scope);

        if (var_proc != block_proc) {
          error(a, stmt->pos, "cannot capture variables from outer functions");
        }
      } break;

      default: break;
      }
    }
  } break;

  case STMT_EXPR: {
    symbol_check_expr(a, block, &stmt->expr);
  } break;

  case STMT_USING: {
    symbol_t *sym = symbol_check_expr(a, block, &stmt->expr);

    switch (stmt->expr.kind) {
    case EXPR_IMPORT: break;

    default: {
      if (sym) {
        switch (sym->kind) {
        case SYMBOL_NAMESPACE: break;

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
      type_t type_type;
      if (!expr_as_type(a, block, &stmt->const_decl.type_expr, &type_type)) {
        error(
            a,
            stmt->const_decl.type_expr.pos,
            "expression does not represent a type");
      }
    }

    type_t type;
    type_check_expr(a, block, &stmt->const_decl.expr, &type);
  } break;

  case STMT_VAR_DECL: {
    if (stmt->var_decl.typed) {
      type_t type_type;
      if (!expr_as_type(a, block, &stmt->var_decl.type_expr, &type_type)) {
        error(
            a,
            stmt->var_decl.type_expr.pos,
            "expression does not represent a type");
      }
    }

    type_t type;
    type_check_expr(a, block, &stmt->var_decl.expr, &type);
  } break;

  case STMT_VAR_ASSIGN: {
    type_t expr_type;
    type_check_expr(a, block, &stmt->var_assign.expr, &expr_type);
    type_t assigned_type;
    type_check_expr(a, block, &stmt->var_assign.assigned, &assigned_type);

    if (!equivalent_types(&expr_type, &assigned_type)) {
      error(a, stmt->pos, "type mismatch in variable assignment");
    }
  } break;

  case STMT_EXPR: {
    type_t type;
    type_check_expr(a, block, &stmt->expr, &type);
  } break;

  case STMT_USING: {
    type_t type;
    type_check_expr(a, block, &stmt->expr, &type);
  } break;

  case STMT_DUMMY: break;
  }
}

static void
stmt_analyze_child_block(analyzer_t *a, block_t *block, stmt_t *stmt);

// Only analyzes children expressions to the expression passed in
// Does not analyze the expression itself, only useful for analyzing children
// scopes, etc
static void
expr_analyze_child_block(analyzer_t *a, block_t *block, expr_t *expr) {
  switch (expr->kind) {
  case EXPR_PROC: {
    scope_init(
        &expr->proc.block.scope,
        &block->scope,
        expr->proc.block.stmts.count + expr->proc.params.count);

    expr->proc.block.scope.proc = &expr->proc;

    For(param, expr->proc.params) {
      if (scope_get_local(&expr->proc.block.scope, param->name)) {
        error(
            a,
            expr->pos,
            "duplicate parameter declaration: '%.*s'",
            (int)param->name.count,
            param->name.buf);
        continue;
      }

      type_t type;
      if (!expr_as_type(a, block, &param->type, &type)) {
        error(a, param->type.pos, "expression does not represent a type");
      }

      symbol_t *sym = scope_add(&expr->proc.block.scope, a->ctx, param->name);
      sym->kind     = SYMBOL_LOCAL_VAR;
    }

    analyze_block_ordered(a, &expr->proc.block);
  } break;

  case EXPR_BLOCK: {
    scope_init(&expr->block.scope, &block->scope, expr->block.stmts.count);

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
