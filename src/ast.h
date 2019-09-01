#pragma once

#include "strbuf.h"
#include "table.h"

typedef struct proc_t {

} proc_t;

typedef struct struct_t {

} struct_t;

typedef struct import_t {
  strbuf_t path;
} import_t;

typedef struct decl_t {
  enum {
    DECL_IMPORT,
    DECL_PROC,
    DECL_STRUCT,
  } kind;

  strbuf_t name;

  union {
    import_t import;
    proc_t proc;
    struct_t str;
  };
} decl_t;

typedef SLICE(decl_t) decl_slice_t;

typedef struct scope_t {
  table_t table;
} scope_t;

typedef struct ast_t {
  scope_t scope;

  decl_slice_t decls;
} ast_t;
