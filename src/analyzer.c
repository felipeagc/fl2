#include "analyzer.h"

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

static void symbol_check_expr(analyzer_t *a, block_t *block, expr_t *expr) {
  switch (expr->kind) {
  case EXPR_PRIMARY: {

  } break;
  case EXPR_PROC: {
    analyze_block(a, &expr->proc.block, &block->scope);
  } break;
  case EXPR_STRUCT: {

  } break;
  case EXPR_IMPORT: {

  } break;
  }
}

static void add_const_stmt(analyzer_t *a, block_t *block, stmt_t *stmt) {
  switch (stmt->kind) {
  case STMT_USING: {

  } break;

  case STMT_CONST_DECL: {
    if (scope_get(&block->scope, stmt->const_decl.name)) {
      error(a, stmt->pos, "duplicate constant declaration");
    } else {
      scope_add(&block->scope, a->ctx, stmt->const_decl.name);
    }

    symbol_check_expr(a, block, &stmt->const_decl.expr);
  } break;

  default: break;
  }
}

static void analyze_block(analyzer_t *a, block_t *block, scope_t *parent) {
  scope_init(&block->scope, parent, block->stmts.count);

  For(stmt, block->stmts) { add_const_stmt(a, block, stmt); }
}

void analyzer_init(analyzer_t *a, ctx_t *ctx) {
  memset(a, 0, sizeof(*a));
  a->ctx = ctx;
}

error_set_t analyzer_analyze(analyzer_t *a, ast_t *ast) {
  memset(&a->errors, 0, sizeof(a->errors));

  analyze_block(a, &ast->block, NULL);

  return (error_set_t){
      .type   = RESULT_ANALYZER,
      .errors = a->errors,
  };
}
