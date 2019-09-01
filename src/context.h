#pragma once

#include "bump_alloc.h"
#include "str_builder.h"

typedef struct ctx_t {
  str_builder_t sb;
  bump_alloc_t alloc;
} ctx_t;

void ctx_init(ctx_t *ctx);

void ctx_destroy(ctx_t *ctx);
