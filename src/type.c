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
  case TYPE_UNDEFINED: break;
  }

  return received;
}

type_t *equivalent_types(type_t *received, type_t *expected) {
  return exact_types(received, expected);
}
