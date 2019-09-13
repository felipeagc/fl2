#pragma once

#include "context.h"
#include "table.h"

typedef struct symbol_t symbol_t;
typedef struct proc_t proc_t;

typedef struct scope_t scope_t;
typedef SLICE(scope_t) scope_slice_t;

typedef struct scope_t {
  struct scope_t *parent;
  table_t table;
  scope_slice_t siblings;
  proc_t *proc;
} scope_t;

void scope_init(scope_t *s, scope_t *parent, size_t size);

symbol_t *scope_add(scope_t *s, ctx_t *ctx, strbuf_t name);

symbol_t *scope_get(scope_t *s, strbuf_t name);

symbol_t *scope_get_local(scope_t *s, strbuf_t name);

scope_t *scope_top_parent(scope_t *s);

proc_t *scope_proc(scope_t *s);

void scope_destroy(scope_t *s);
