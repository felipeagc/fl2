#pragma once

#include "ast.h"
#include "strbuf.h"

typedef struct type_t type_t;
typedef SLICE(type_t) type_slice_t;

typedef struct type_t {
  enum {
    TYPE_UNDEFINED,
    TYPE_NAMESPACE, // import "asdasd.fl"
    TYPE_PRIMITIVE, // i32
    TYPE_TYPE,      // ([]*Hello)
    TYPE_PTR,       // *i32
    TYPE_ARRAY,     // [3]i32
    TYPE_SLICE,     // []i32
    TYPE_STRUCT,    // struct {}
    TYPE_PROC,      // proc (a: i32) i32
  } kind;

  union {
    prim_type_t prim;
    struct {
      size_t size;
      struct type_t *subtype;
    };
    struct_t *str;
    proc_signature_t *proc_sig;
  };
} type_t;

type_t *exact_types(type_t *received, type_t *expected);

type_t *equivalent_types(type_t *received, type_t *expected);
