#pragma once

#include "context.h"
#include "table.h"

typedef struct scope_t {
  struct scope_t *parent;
  table_t table;
} scope_t;

typedef struct scope_entry_t {
  int a;
} scope_entry_t;

void scope_init(scope_t *s, scope_t *parent, size_t size);

scope_entry_t *scope_add(scope_t *s, ctx_t *ctx, strbuf_t name);

scope_entry_t *scope_get(scope_t *s, strbuf_t name);

void scope_destroy(scope_t *s);
