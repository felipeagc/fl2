#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#define For(var, slice)                                                        \
  for (__typeof__(slice.buf) var = slice.buf; var < (slice.buf + slice.count); \
       var++)

#define SLICE(type)                                                            \
  struct {                                                                     \
    type *buf;                                                                 \
    size_t count;                                                              \
    size_t cap;                                                                \
  }

#define APPEND(slice, value)                                                   \
  do {                                                                         \
    if ((slice).cap == 0) {                                                    \
      (slice).cap   = 2;                                                       \
      (slice).count = 0;                                                       \
      (slice).buf   = malloc(sizeof(*(slice).buf) * (slice).cap);              \
    }                                                                          \
    if (((slice).count + 1) >= (slice).cap) {                                  \
      (slice).cap *= 2;                                                        \
      (slice).buf = realloc((slice).buf, sizeof(*(slice).buf) * (slice).cap);  \
    }                                                                          \
    (slice).buf[((slice).count)] = (value);                                    \
    ((slice).count)++;                                                         \
  } while (0)

#define POP_LAST(slice)                                                        \
  do {                                                                         \
    if ((slice).count > 0) (slice).count -= 1;                                 \
  } while (0)

typedef SLICE(char) strbuf_t;

#define STR(lit)                                                               \
  (strbuf_t) { .buf = lit, .count = strlen(lit), .cap = strlen(lit) + 1 }

static inline bool strbuf_cmp(strbuf_t a, strbuf_t b) {
  if (a.count != b.count) return false;
  return strncmp(a.buf, b.buf, a.count) == 0;
}
