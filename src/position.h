#pragma once

#include "strbuf.h"

typedef struct ctx_t ctx_t;
typedef struct ast_t ast_t;

typedef struct file_t {
  strbuf_t path;
  strbuf_t abs_path;
  strbuf_t content;
  ast_t *ast;
} file_t;

typedef struct pos_t {
  file_t *file;
  size_t offset;
  size_t len;
  size_t line;
  size_t col;
} pos_t;

typedef SLICE(pos_t) pos_slice_t;

bool file_init(file_t *file, ctx_t *ctx, strbuf_t path);

void file_destroy(file_t *file);
