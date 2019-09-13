#pragma once

#include "position.h"
#include "str_builder.h"
#include "strbuf.h"

#define TRY(result)                                                            \
  do {                                                                         \
    bool res = result;                                                         \
    if (!res) return res;                                                      \
  } while (0);

typedef struct error_t {
  pos_t pos;
  strbuf_t msg;
} error_t;

typedef SLICE(error_t) error_slice_t;

typedef enum {
  RESULT_SCANNER,
  RESULT_PARSER,
  RESULT_ANALYZER,
  RESULT_CODEGEN,
} result_type_t;

typedef struct error_set_t {
  result_type_t type;
  error_slice_t errors;
} error_set_t;

void print_error(str_builder_t *sb, error_t *err);
