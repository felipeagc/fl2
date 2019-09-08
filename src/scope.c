#include "scope.h"

#include "ast.h"
#include <assert.h>

void scope_init(scope_t *s, scope_t *parent, size_t size) {
  memset(s, 0, sizeof(*s));
  table_init(&s->table, size);
  s->parent = parent;
}

symbol_t *scope_add(scope_t *s, ctx_t *ctx, strbuf_t name) {
  symbol_t *symbol = bump_alloc(&ctx->alloc, sizeof(symbol_t));
  table_set(&s->table, name, symbol);
  return symbol;
}

symbol_t *scope_get(scope_t *s, strbuf_t name) {
  if (s == NULL) return NULL;

  symbol_t *sym = scope_get(s->parent, name);
  if (sym) return sym;

  For(sibling, s->siblings) {
    symbol_t *sym = scope_get(sibling, name);
    if (sym) return sym;
  }

  return table_get(&s->table, name);
}

symbol_t *scope_get_local(scope_t *s, strbuf_t name) {
  For(sibling, s->siblings) {
    symbol_t *sym = scope_get_local(sibling, name);
    if (sym) return sym;
  }

  return table_get(&s->table, name);
}

void scope_destroy(scope_t *s) { table_destroy(&s->table); }
