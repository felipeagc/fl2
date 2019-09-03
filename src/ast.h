#pragma once

#include "strbuf.h"
#include "table.h"

typedef struct expr_t expr_t;
typedef SLICE(expr_t) expr_slice_t;

typedef struct stmt_t stmt_t;
typedef SLICE(stmt_t) stmt_slice_t;

typedef struct ident_t {
  strbuf_t string;
  struct ident_t *child;
} ident_t;

typedef struct proc_t {
  stmt_slice_t stmts;
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

typedef struct primary_expr_t {
  enum {
    PRIMARY_INT,
    PRIMARY_FLOAT,
    PRIMARY_IDENT,
    PRIMARY_PROC_CALL,
  } kind;

  union {
    int64_t i64;
    double f64;
    ident_t ident;
    proc_call_t proc_call;
  };
} primary_expr_t;

typedef struct expr_t {
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
} var_decl_t;

typedef struct var_assign_t {
  expr_t assigned;
  expr_t expr;
} var_assign_t;

typedef struct const_decl_t {
  strbuf_t name;
  expr_t expr;
} const_decl_t;

typedef struct stmt_t {
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

typedef struct scope_t {
  table_t table;
} scope_t;

typedef struct ast_t {
  scope_t scope;

  stmt_slice_t stmts;
} ast_t;
