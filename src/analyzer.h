#pragma once

#include "ast.h"
#include "context.h"
#include "position.h"
#include "result.h"

typedef struct analyzer_t {
  ctx_t *ctx;
  ast_t *ast;
  error_slice_t errors;
  pos_slice_t pos_stack;
} analyzer_t;

void analyzer_init(analyzer_t *a, ctx_t *ctx);

error_set_t analyzer_analyze(analyzer_t *a, ast_t *ast);
