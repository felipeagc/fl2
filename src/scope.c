#include "scope.h"

#include <assert.h>

void scope_init(scope_t *s, scope_t *parent, size_t size) {
  memset(s, 0, sizeof(*s));
  table_init(&s->table, size);
  s->parent = parent;
}

scope_entry_t *scope_add(scope_t *s, ctx_t *ctx, strbuf_t name) {
  scope_entry_t *entry = bump_alloc(&ctx->alloc, sizeof(scope_entry_t));
  table_set(&s->table, name, entry);
  return entry;
}

scope_entry_t *scope_get(scope_t *s, strbuf_t name) {
  return table_get(&s->table, name);
}

void scope_destroy(scope_t *s) { table_destroy(&s->table); }
