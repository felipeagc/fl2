#pragma once

#include "bump_alloc.h"
#include "result.h"
#include "str_builder.h"
#include "position.h"

typedef struct ast_t ast_t;

typedef struct ctx_t {
  str_builder_t sb;
  bump_alloc_t alloc;
} ctx_t;

void ctx_init(ctx_t *ctx);

error_set_t ctx_process_file(ctx_t *ctx, file_t *file, ast_t *ast);

void ctx_destroy(ctx_t *ctx);
