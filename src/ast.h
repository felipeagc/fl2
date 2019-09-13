#pragma once

#include "position.h"
#include "scope.h"
#include "strbuf.h"

typedef struct expr_t expr_t;
typedef SLICE(expr_t) expr_slice_t;

typedef struct stmt_t stmt_t;
typedef SLICE(stmt_t) stmt_slice_t;

typedef struct block_t block_t;
typedef SLICE(block_t) block_slice_t;

typedef struct block_t {
  scope_t scope;
  stmt_slice_t stmts;
} block_t;

typedef struct access_t {
  expr_t *left;
  expr_t *right;
} access_t;

typedef struct proc_param_t proc_param_t;
typedef SLICE(proc_param_t) proc_param_slice_t;

typedef enum proc_flags_t {
  PROC_FLAG_INLINE = 1 << 1,
  PROC_FLAG_EXTERN = 1 << 2,
} proc_flags_t;

typedef struct proc_t {
  proc_flags_t flags;
  proc_param_slice_t params;
  expr_slice_t return_types;
  block_t block;
} proc_t;

typedef struct struct_t {

} struct_t;

typedef struct proc_call_t {
  expr_t *expr;
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
    PRIMARY_PRIMITIVE_TYPE,
  } kind;

  union {
    int64_t i64;
    double f64;
    strbuf_t ident;
    primitive_type_t prim_type;
  };
} primary_expr_t;

typedef struct unary_op_t {
  enum {
    UNOP_DEREF,
    UNOP_NOT,
  } kind;
} unary_op_t;

typedef struct binary_op_t {
  enum {
    BINOP_ADD,
    BINOP_SUB,
    BINOP_MUL,
    BINOP_DIV,
    BINOP_MOD,

    BINOP_AND,
    BINOP_OR,
  } kind;
} binary_op_t;

typedef struct expr_t {
  pos_t pos;

  enum {
    EXPR_PRIMARY,
    EXPR_EXPR,
    EXPR_STRUCT,
    EXPR_PROC,
    EXPR_IMPORT,
    EXPR_ACCESS,
    EXPR_PROC_CALL,
    EXPR_UNARY,
    EXPR_BINARY,
  } kind;

  union {
    primary_expr_t primary;
    struct expr_t *expr;
    struct_t str;
    proc_t proc;
    import_t import;
    access_t access;
    proc_call_t proc_call;

    struct {
      union {
        unary_op_t unary;
        binary_op_t binary;
      };
      struct expr_t *left;
      struct expr_t *right;
    };
  };
} expr_t;

typedef struct proc_param_t {
  strbuf_t name;
  expr_t type;
} proc_param_t;

typedef struct var_decl_t {
  strbuf_t name;
  expr_t expr;
  expr_t type;
  bool typed;
} var_decl_t;

typedef struct var_assign_t {
  expr_t assigned;
  expr_t expr;
} var_assign_t;

typedef struct const_decl_t {
  strbuf_t name;
  expr_t expr;
  expr_t type;
  bool typed;
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

typedef struct symbol_t {
  enum {
    SYMBOL_NAMESPACE,
    SYMBOL_CONST,
    SYMBOL_GLOBAL_VAR,
    SYMBOL_LOCAL_VAR,
    SYMBOL_PROC,
    SYMBOL_STRUCT,
  } kind;

  scope_t *scope;

  union {
    ast_t ast;
  };
} symbol_t;

bool is_expr_type(expr_t *expr, scope_t *scope);
