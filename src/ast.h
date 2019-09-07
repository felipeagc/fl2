#pragma once

#include "position.h"
#include "scope.h"
#include "strbuf.h"

typedef struct expr_t expr_t;
typedef SLICE(expr_t) expr_slice_t;

typedef struct stmt_t stmt_t;
typedef SLICE(stmt_t) stmt_slice_t;

typedef struct block_t {
  scope_t scope;
  stmt_slice_t stmts;
} block_t;

typedef struct ident_t {
  strbuf_t string;
  struct ident_t *child;
} ident_t;

typedef struct proc_t {
  block_t block;
} proc_t;

typedef struct struct_t {

} struct_t;

typedef struct proc_call_t {
  ident_t ident;
  expr_slice_t params;
} proc_call_t;

typedef struct import_t {
  strbuf_t path;
} import_t;

typedef enum primitive_type_t {
  PRIMITIVE_TYPE_I8,
  PRIMITIVE_TYPE_I16,
  PRIMITIVE_TYPE_I32,
  PRIMITIVE_TYPE_I64,
  PRIMITIVE_TYPE_U8,
  PRIMITIVE_TYPE_U16,
  PRIMITIVE_TYPE_U32,
  PRIMITIVE_TYPE_U64,
  PRIMITIVE_TYPE_BOOL,
  PRIMITIVE_TYPE_VOID,
} primitive_type_t;

typedef struct primary_expr_t {
  enum {
    PRIMARY_INT,
    PRIMARY_FLOAT,
    PRIMARY_IDENT,
    PRIMARY_PROC_CALL,
    PRIMARY_PRIMITIVE_TYPE,
  } kind;

  union {
    int64_t i64;
    double f64;
    ident_t ident;
    proc_call_t proc_call;
    primitive_type_t prim_type;
  };
} primary_expr_t;

typedef struct expr_t {
  pos_t pos;

  enum {
    EXPR_PRIMARY,
    EXPR_STRUCT,
    EXPR_PROC,
    EXPR_IMPORT,
  } kind;

  union {
    primary_expr_t primary;
    struct_t str;
    proc_t proc;
    import_t import;
  };
} expr_t;

typedef struct var_decl_t {
  strbuf_t name;
  expr_t expr;
  expr_t type;
} var_decl_t;

typedef struct var_assign_t {
  expr_t assigned;
  expr_t expr;
} var_assign_t;

typedef struct const_decl_t {
  strbuf_t name;
  expr_t expr;
  expr_t type;
} const_decl_t;

typedef struct stmt_t {
  pos_t pos;
  enum {
    STMT_VAR_DECL,
    STMT_VAR_ASSIGN,
    STMT_CONST_DECL,
    STMT_USING,
  } kind;
  union {
    var_decl_t var_decl;
    var_assign_t var_assign;
    const_decl_t const_decl;
    struct {
      expr_t expr;
    };
  };
} stmt_t;

typedef struct ast_t {
  file_t *file;
  block_t block;
} ast_t;
