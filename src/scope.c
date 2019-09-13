#include "scope.h"

#include "ast.h"
#include <assert.h>

void scope_init(scope_t *s, scope_t *parent, size_t size) {
  memset(s, 0, sizeof(*s));
  size_t actual_size = size;
  if (actual_size == 0) actual_size = 10;
  table_init(&s->table, actual_size);
  s->parent = parent;
}

symbol_t *scope_add(scope_t *s, ctx_t *ctx, strbuf_t name) {
  symbol_t *symbol = bump_alloc(&ctx->alloc, sizeof(symbol_t));
  memset(symbol, 0, sizeof(*symbol));
  symbol->scope = s;
  table_set(&s->table, name, symbol);
  return symbol;
}

symbol_t *scope_get(scope_t *s, strbuf_t name) {
  if (s == NULL) return NULL;

  symbol_t *sym = NULL;

  For(sibling, s->siblings) {
    sym = scope_get(sibling, name);
    if (sym) return sym;
  }

  sym = table_get(&s->table, name);
  if (sym) return sym;

  return scope_get(s->parent, name);
}

symbol_t *scope_get_local(scope_t *s, strbuf_t name) {
  For(sibling, s->siblings) {
    symbol_t *sym = scope_get_local(sibling, name);
    if (sym) return sym;
  }

  return table_get(&s->table, name);
}

scope_t *scope_top_parent(scope_t *s) {
  if (s->parent == NULL) return s;
  return scope_top_parent(s->parent);
}

proc_t *scope_proc(scope_t *s) {
  if (s == NULL) return NULL;
  if (s->proc == NULL) return scope_proc(s->parent);
  return s->proc;
}

void scope_destroy(scope_t *s) { table_destroy(&s->table); }
