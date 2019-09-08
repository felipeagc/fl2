#pragma once

#include "context.h"
#include "table.h"

typedef struct symbol_t symbol_t;

typedef struct scope_t {
  struct scope_t *parent;
  table_t table;
} scope_t;

void scope_init(scope_t *s, scope_t *parent, size_t size);

symbol_t *scope_add(scope_t *s, ctx_t *ctx, strbuf_t name);

symbol_t *scope_get(scope_t *s, strbuf_t name);

symbol_t *scope_get_local(scope_t *s, strbuf_t name);

void scope_destroy(scope_t *s);
