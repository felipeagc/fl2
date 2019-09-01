#include "table.h"

#include <assert.h>
#include <stdlib.h>

typedef struct table_node_t {
  void *data;
  table_node_t *next;
  strbuf_t key;
} table_node_t;

static inline size_t hash(strbuf_t str) {
  size_t hash = 5381;
  size_t i    = 0;

  while (i < str.count)
    hash = ((hash << 5) + hash) + str.buf[i++]; /* hash * 33 + c */

  return hash;
}

static inline void node_init(table_node_t *node, strbuf_t key, void *value) {
  node->data = value;
  node->key  = key;
  node->next = NULL;
}

static inline void free_node(table_node_t *node) {
  if (!node) return;
  free_node(node->next);
  free(node);
}

void table_init(table_t *table, size_t capacity) {
  table->capacity = capacity;
  table->nodes    = calloc(table->capacity, sizeof(table_node_t));
}

int table_set(table_t *table, strbuf_t key, void *value) {
  size_t pos         = hash(key) % table->capacity;
  table_node_t *node = &table->nodes[pos];

  // First try to find the value
  while (node) {
    // Found existing slot
    if (strbuf_cmp(node->key, key)) {
      node->data = value;
      return 0;
    }

    // Found available slot
    if (node->key.count == 0 && node->data == NULL) {
      node->data = value;
      node->key  = key;
      return 0;
    }

    // Couldn't find anything, so allocate a new slot
    if (!node->next) {
      node->next = calloc(1, sizeof(table_node_t));
      node       = node->next;
      node_init(node, key, value);
      return 1;
    }

    node = node->next;
  }

  return 0;
}

void *table_get(table_t *table, strbuf_t key) {
  size_t pos         = hash(key) % table->capacity;
  table_node_t *node = &table->nodes[pos];

  while (!strbuf_cmp(node->key, key)) {
    if (node->next == NULL) return NULL;
    node = node->next;
  }

  return node->data;
}

int table_remove(table_t *table, strbuf_t key) {
  size_t pos         = hash(key) % table->capacity;
  table_node_t *root = &table->nodes[pos];
  table_node_t *node = root;

  while (!strbuf_cmp(node->key, key)) {
    if (node->next == NULL) return 0;
    node = node->next;
  }

  node->data = NULL;

  table_node_t *last = node;
  while (last->next) {
    table_node_t *parent = last;
    last                 = parent->next;
    if (!last->next) {
      node->data   = last->data;
      node->key    = last->key;
      parent->next = NULL;
      free_node(last);
      return 1;
    }
  }

  if (node != root) {
    free_node(node);
    return 1;
  }

  node->data      = NULL;
  node->key.count = 0;

  return 0;
}

void table_destroy(table_t *table) {
  for (size_t i = 0; i < table->capacity; ++i) {
    free_node(table->nodes[i].next);
  }
  free(table->nodes);
}
