#pragma once

#include "context.h"
#include "position.h"
#include "result.h"
#include "token.h"

typedef struct scanner_t {
  ctx_t *ctx;

  file_t *file;
  error_slice_t errors;
  token_slice_t tokens;

  size_t offset;
  size_t line;
  size_t col;
} scanner_t;

void scanner_init(scanner_t *s, ctx_t *ctx);

error_set_t scanner_scan(scanner_t *s, file_t *file, token_slice_t *tokens);
