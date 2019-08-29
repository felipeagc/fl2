#pragma once

#include "strbuf.h"

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

typedef struct result_t {
  result_type_t type;
  error_slice_t errors;
} result_t;
