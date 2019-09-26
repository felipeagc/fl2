#pragma once

#include "position.h"
#include "scope.h"
#include "strbuf.h"

typedef struct expr_t expr_t;
typedef SLICE(expr_t) expr_slice_t;

typedef struct stmt_t stmt_t;
typedef SLICE(stmt_t) stmt_slice_t;

typedef struct type_t type_t;

typedef struct LLVMOpaqueValue *LLVMValueRef;
typedef struct LLVMOpaqueType *LLVMTypeRef;

typedef struct block_t {
  scope_t scope;
  stmt_slice_t stmts;
} block_t;
typedef SLICE(block_t) block_slice_t;

typedef struct access_t {
  expr_t *left;
  expr_t *right;
} access_t;

typedef struct var_decl_t var_decl_t;
typedef SLICE(var_decl_t) var_decl_slice_t;

typedef enum proc_flags_t {
  PROC_FLAG_INLINE  = 0x01,
  PROC_FLAG_EXTERN  = 0x02,
  PROC_FLAG_NO_BODY = 0x04,
} proc_flags_t;

typedef struct proc_signature_t {
  proc_flags_t flags;
  var_decl_slice_t params;
  expr_slice_t return_types;
} proc_signature_t;

typedef struct proc_t {
  proc_signature_t sig;
  block_t block;
} proc_t;

typedef enum struct_flags_t {
  STRUCT_FLAG_PACKED = 0x01,
} struct_flags_t;

typedef struct struct_t {
  struct_flags_t flags;

} struct_t;

typedef struct proc_call_t {
  expr_t *expr;
  expr_slice_t params;
} proc_call_t;

typedef struct import_t {
  strbuf_t path;
  ast_t *ast;
} import_t;

typedef enum prim_type_t {
  PRIM_TYPE_NUM_BEGIN,
  PRIM_TYPE_I8,
  PRIM_TYPE_I16,
  PRIM_TYPE_I32,
  PRIM_TYPE_I64,
  PRIM_TYPE_U8,
  PRIM_TYPE_U16,
  PRIM_TYPE_U32,
  PRIM_TYPE_U64,
  PRIM_TYPE_FLOAT_BEGIN,
  PRIM_TYPE_F32,
  PRIM_TYPE_F64,
  PRIM_TYPE_FLOAT_END,
  PRIM_TYPE_NUM_END,
  PRIM_TYPE_BOOL,
  PRIM_TYPE_VOID,
} prim_type_t;

typedef struct primary_expr_t {
  enum {
    PRIMARY_INT,
    PRIMARY_FLOAT,
    PRIMARY_IDENT,
    PRIMARY_PRIMITIVE_TYPE,
    PRIMARY_STRING,
    PRIMARY_CSTRING,
  } kind;

  union {
    int64_t i64;
    double f64;
    strbuf_t string;
    prim_type_t prim_type;
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

typedef enum expr_kind_t {
  EXPR_PRIMARY,
  EXPR_EXPR,
  EXPR_STRUCT,
  EXPR_PROC,
  EXPR_PROC_PTR,
  EXPR_IMPORT,
  EXPR_ACCESS,
  EXPR_PROC_CALL,
  EXPR_UNARY,
  EXPR_BINARY,
  EXPR_BLOCK,
} expr_kind_t;

typedef struct type_t {
  enum {
    TYPE_UNDEFINED,
    TYPE_NAMESPACE, // import "asdasd.fl"
    TYPE_PRIMITIVE, // i32
    TYPE_TYPE,      // ([]*Hello)
    TYPE_STRING,    // "hello"
    TYPE_PTR,       // *i32
    TYPE_ARRAY,     // [3]i32
    TYPE_SLICE,     // []i32
    TYPE_STRUCT,    // struct {}
    TYPE_PROC,      // proc (a: i32) i32
  } kind;

  strbuf_t name;

  union {
    prim_type_t prim;
    struct {
      size_t size;
      struct type_t *subtype;
    };
    struct_t *str;
    proc_signature_t *proc_sig;
  };

  LLVMTypeRef ref;
} type_t;

typedef struct expr_t {
  pos_t pos;
  expr_kind_t kind;

  type_t *type;
  type_t *as_type;

  union {
    primary_expr_t primary;
    struct expr_t *expr;
    struct_t str;
    proc_t proc;
    import_t import;
    access_t access;
    proc_call_t proc_call;
    block_t block;

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

typedef enum var_decl_flags_t {
  VAR_DECL_HAS_TYPE = 0x01,
  VAR_DECL_HAS_EXPR = 0x02,
} var_decl_flags_t;

typedef struct var_decl_t {
  var_decl_flags_t flags;
  strbuf_t name;
  expr_t expr;
  expr_t type_expr;
  type_t *type;
  symbol_t *sym;
} var_decl_t;

typedef struct var_assign_t {
  expr_t assigned;
  expr_t expr;
} var_assign_t;

typedef struct const_decl_t {
  strbuf_t name;
  expr_t expr;
  expr_t type_expr;
  type_t *type;
  bool typed;
  symbol_t *sym;
} const_decl_t;

typedef struct return_t {
  expr_slice_t exprs;
} return_t;

typedef struct stmt_t {
  pos_t pos;
  enum {
    STMT_DUMMY,
    STMT_VAR_DECL,
    STMT_VAR_ASSIGN,
    STMT_CONST_DECL,
    STMT_USING,
    STMT_EXPR,
    STMT_RETURN,
  } kind;
  union {
    var_decl_t var_decl;
    var_assign_t var_assign;
    const_decl_t const_decl;
    struct {
      expr_t expr;
    };
    return_t ret;
  };
} stmt_t;

typedef struct ast_t {
  file_t *file;
  block_t block;
} ast_t;

typedef struct value_t {
  enum {
    VALUE_UNDEFINED,
    VALUE_PROC,
    VALUE_TYPE,
    VALUE_CONST,
    VALUE_GLOBAL_VAR,
    VALUE_LOCAL_VAR,
    VALUE_TMP_VAR,
  } kind;
  union {
    LLVMValueRef value;
    LLVMTypeRef type;
  };
} value_t;

typedef struct symbol_t {
  enum {
    SYMBOL_DUMMY,
    SYMBOL_CONST_DECL,
    SYMBOL_GLOBAL_VAR,
    SYMBOL_LOCAL_VAR,
  } kind;

  scope_t *scope;

  value_t value;

  union {
    const_decl_t *const_decl;
    var_decl_t *var_decl;
  };
} symbol_t;

