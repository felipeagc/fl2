#pragma once

#include "strbuf.h"

typedef struct file_t {
  strbuf_t path;
  strbuf_t content;
} file_t;

typedef struct pos_t {
  file_t *file;
  size_t offset;
  size_t len;
  size_t line;
  size_t col;
} pos_t;

void file_init(file_t *file, strbuf_t path);

void file_destroy(file_t *file);
