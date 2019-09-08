#include "analyzer.h"

#include "filesystem.h"
#include "parser.h"
#include "scanner.h"
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

static void analyze_block(analyzer_t *a, block_t *block, scope_t *parent);

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

    switch (sym->kind) {
    case SYMBOL_NAMESPACE: {
      return symbol_check_expr(a, &sym->ast.block, expr->access.right);
    } break;

    default: break;
    }
  } break;

  case EXPR_PROC_CALL: {
    return symbol_check_expr(a, block, expr->proc_call.expr);
  } break;

  case EXPR_PROC: {
    analyze_block(a, &expr->proc.block, &block->scope);
  } break;

  case EXPR_STRUCT: {

  } break;

  case EXPR_IMPORT: {

  } break;
  }

  return NULL;
}

static void add_const_stmt(analyzer_t *a, block_t *block, stmt_t *stmt) {
  switch (stmt->kind) {
  case STMT_USING: {

  } break;

  case STMT_CONST_DECL: {
    if (scope_get(&block->scope, stmt->const_decl.name)) {
      error(a, stmt->pos, "duplicate constant declaration");
      break;
    }

    symbol_t *sym = scope_add(&block->scope, a->ctx, stmt->const_decl.name);

    expr_t *expr = &stmt->const_decl.expr;

    if (expr->kind == EXPR_IMPORT) {
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
    }
  } break;

  default: break;
  }
}

static void
symbol_check_const_stmt(analyzer_t *a, block_t *block, stmt_t *stmt) {
  switch (stmt->kind) {
  case STMT_USING: {
    symbol_check_expr(a, block, &stmt->expr);
  } break;

  case STMT_CONST_DECL: {
    symbol_check_expr(a, block, &stmt->const_decl.expr);
  } break;

  case STMT_VAR_DECL: {
    symbol_check_expr(a, block, &stmt->var_decl.expr);
  } break;

  case STMT_VAR_ASSIGN: {
    symbol_check_expr(a, block, &stmt->var_assign.assigned);
    symbol_check_expr(a, block, &stmt->var_assign.expr);
  } break;
  }
}

static void analyze_block(analyzer_t *a, block_t *block, scope_t *parent) {
  scope_init(&block->scope, parent, block->stmts.count);

  For(stmt, block->stmts) { add_const_stmt(a, block, stmt); }
  For(stmt, block->stmts) { symbol_check_const_stmt(a, block, stmt); }
}

void analyzer_init(analyzer_t *a, ctx_t *ctx) {
  memset(a, 0, sizeof(*a));
  a->ctx = ctx;
}

error_set_t analyzer_analyze(analyzer_t *a, ast_t *ast) {
  memset(&a->errors, 0, sizeof(a->errors));

  a->ast = ast;

  analyze_block(a, &ast->block, NULL);

  return (error_set_t){
      .type   = RESULT_ANALYZER,
      .errors = a->errors,
  };
}
