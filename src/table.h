#pragma once

#include "strbuf.h"
#include <stddef.h>

typedef struct table_node_t table_node_t;

typedef struct table_t {
  table_node_t *nodes;
  size_t capacity;
} table_t;

void table_init(table_t *table, size_t capacity);

// Returns the number of allocations that happened
int table_set(table_t *table, strbuf_t key, void *value);

void *table_get(table_t *table, strbuf_t key);

// Returns the number of frees that happened
int table_remove(table_t *table, strbuf_t key);

void table_destroy(table_t *table);
