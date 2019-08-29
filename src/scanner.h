#pragma once

#include "bump_alloc.h"
#include "position.h"
#include "result.h"
#include "str_builder.h"
#include "token.h"

typedef struct scanner_t {
  file_t *file;
  error_slice_t errors;
  token_slice_t tokens;

  str_builder_t sb;
  bump_alloc_t alloc;

  size_t offset;
  size_t line;
  size_t col;
} scanner_t;

void scanner_init(scanner_t *s);

result_t scanner_scan(scanner_t *s, file_t *file, token_slice_t *tokens);

void scanner_destroy(scanner_t *s);
