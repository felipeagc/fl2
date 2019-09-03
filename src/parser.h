#pragma once

#include "ast.h"
#include "context.h"
#include "position.h"
#include "result.h"
#include "token.h"

typedef struct parser_t {
  ctx_t *ctx;

  file_t *file;
  error_slice_t errors;

  token_slice_t tokens;
  size_t pos;

  ast_t *ast;
} parser_t;

void parser_init(parser_t *p, ctx_t *ctx);

error_set_t
parser_parse(parser_t *p, file_t *file, token_slice_t tokens, ast_t *ast);
