#include "type.h"

#include <assert.h>

type_t *exact_types(type_t *received, type_t *expected) {
  if (received->kind != expected->kind) return NULL;

  switch (received->kind) {
  case TYPE_PRIMITIVE:
    if (received->prim != expected->prim) return NULL;
    break;
  case TYPE_PTR: return exact_types(received->subtype, expected->subtype);
  case TYPE_ARRAY: {
    if (received->size != expected->size) return NULL;
    return exact_types(received->subtype, expected->subtype);
  } break;
  case TYPE_SLICE: return exact_types(received->subtype, expected->subtype);
  case TYPE_STRUCT: {
    // TODO
    assert(0);
  } break;
  case TYPE_PROC: {
    // TODO
    assert(0);
  } break;
  case TYPE_TYPE: break;
  case TYPE_NAMESPACE: break;
  case TYPE_UNDEFINED: break;
  }

  return received;
}

type_t *equivalent_types(type_t *received, type_t *expected) {
  return exact_types(received, expected);
}

void print_type(str_builder_t *sb, type_t *type) {
  switch (type->kind) {
  case TYPE_UNDEFINED: sb_append(sb, STR("undefined")); return;
  case TYPE_NAMESPACE: sb_append(sb, STR("namespace")); return;
  case TYPE_TYPE: sb_append(sb, STR("type")); return;
  default: break;
  }

  if (type->name.count > 0) {
    sb_append(sb, type->name);
    sb_append(sb, STR(" ("));
  }

  switch (type->kind) {
  default: break;

  case TYPE_PRIMITIVE: {

    switch (type->prim) {
    case PRIM_TYPE_I8: sb_append(sb, STR("i8")); break;
    case PRIM_TYPE_I16: sb_append(sb, STR("i16")); break;
    case PRIM_TYPE_I32: sb_append(sb, STR("i32")); break;
    case PRIM_TYPE_I64: sb_append(sb, STR("i64")); break;
    case PRIM_TYPE_U8: sb_append(sb, STR("u8")); break;
    case PRIM_TYPE_U16: sb_append(sb, STR("u16")); break;
    case PRIM_TYPE_U32: sb_append(sb, STR("u32")); break;
    case PRIM_TYPE_U64: sb_append(sb, STR("u64")); break;
    case PRIM_TYPE_F32: sb_append(sb, STR("f32")); break;
    case PRIM_TYPE_F64: sb_append(sb, STR("f64")); break;
    case PRIM_TYPE_BOOL: sb_append(sb, STR("bool")); break;
    case PRIM_TYPE_VOID: sb_append(sb, STR("void")); break;
    }

  } break;
  case TYPE_PTR: {
    sb_append_char(sb, '*');
    print_type(sb, type->subtype);
  } break;
  case TYPE_ARRAY: {
    sb_sprintf(sb, "[%zu]", type->size);
    print_type(sb, type->subtype);
  } break;
  case TYPE_SLICE: {
    sb_append(sb, STR("[]"));
    print_type(sb, type->subtype);
  } break;
  case TYPE_STRUCT: {
    sb_append(sb, STR("struct"));
  } break;
  case TYPE_PROC: {
    sb_append(sb, STR("proc ()"));
  } break;
  }

  if (type->name.count > 0) {
    sb_append(sb, STR(")"));
  }
}
