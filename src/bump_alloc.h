#pragma once

#include "strbuf.h"
#include <stddef.h>

typedef struct bump_block_t {
  unsigned char *data;
  size_t size;
  size_t pos;
  struct bump_block_t *next;
} bump_block_t;

typedef struct bump_alloc_t {
  size_t block_size;
  size_t last_block_size;
  bump_block_t base_block;
  bump_block_t *last_block;
} bump_alloc_t;

void bump_init(bump_alloc_t *alloc, size_t block_size);

void *bump_alloc(bump_alloc_t *alloc, size_t size);

void *
bump_realloc(bump_alloc_t *alloc, void *ptr, size_t old_size, size_t size);

strbuf_t bump_strdup(bump_alloc_t *alloc, strbuf_t str);

char *bump_c_str(bump_alloc_t *alloc, strbuf_t str);

size_t bump_usage(bump_alloc_t *alloc);

void bump_destroy(bump_alloc_t *alloc);
