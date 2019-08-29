#include "bump_alloc.h"
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static void block_init(bump_block_t *block, size_t size) {
  block->data = malloc(size);
  block->size = size;
  block->pos  = 0;
  block->next = NULL;
}

static void block_destroy(bump_block_t *block) {
  if (block->next != NULL) {
    block_destroy(block->next);
    free(block->next);
    block->next = NULL;
  }

  free(block->data);
}

static void *block_alloc(bump_block_t *block, size_t size) {
  assert((block->size - block->pos) >= size);
  void *data = block->data + block->pos;
  block->pos += size;
  return data;
}

void bump_init(bump_alloc_t *alloc, size_t block_size) {
  alloc->block_size      = block_size;
  alloc->last_block_size = alloc->block_size;
  block_init(&alloc->base_block, block_size);
  alloc->last_block = &alloc->base_block;
}

void *bump_alloc(bump_alloc_t *alloc, size_t size) {
  if (size == 0) {
    return NULL;
  }

  size_t space = alloc->last_block->size - alloc->last_block->pos;
  if (space < size) {
    // Append new block
    alloc->last_block->next = malloc(sizeof(bump_block_t));
    alloc->last_block_size *= 2;
    alloc->last_block_size += size;
    block_init(alloc->last_block->next, alloc->last_block_size);
    alloc->last_block = alloc->last_block->next;
  }

  return block_alloc(alloc->last_block, size);
}

void *
bump_realloc(bump_alloc_t *alloc, void *ptr, size_t old_size, size_t size) {
  void *new_array = bump_alloc(alloc, size);
  memcpy(new_array, ptr, old_size);
  return new_array;
}

strbuf_t bump_strdup(bump_alloc_t *alloc, strbuf_t str) {
  strbuf_t s;
  s.cap   = str.count;
  s.count = str.count;
  s.buf   = bump_alloc(alloc, s.cap);
  for (size_t i = 0; i < str.count; i++)
    s.buf[i] = str.buf[i];
  return s;
}

char *bump_c_str(bump_alloc_t *alloc, strbuf_t str) {
  char *s;
  s = bump_alloc(alloc, str.count + 1);
  for (size_t i = 0; i < str.count; i++)
    s[i] = str.buf[i];
  s[str.count] = '\0';
  return s;
}

size_t bump_usage(bump_alloc_t *alloc) {
  size_t usage = 0;

  bump_block_t *block = &alloc->base_block;
  while (block) {
    usage += block->pos;
    block = block->next;
  }

  return usage;
}

void bump_destroy(bump_alloc_t *alloc) { block_destroy(&alloc->base_block); }
