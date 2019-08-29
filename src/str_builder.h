#pragma once

#include "strbuf.h"
#include <stdarg.h>

typedef struct str_builder_t {
  strbuf_t str;
  strbuf_t scratch;
} str_builder_t;

void sb_init(str_builder_t *sb);

void sb_destroy(str_builder_t *sb);

void sb_append(str_builder_t *sb, strbuf_t str);

void sb_append_int(str_builder_t *sb, size_t val);

void sb_append_char(str_builder_t *sb, char c);

#ifdef __GNUC__
__attribute__((format(printf, 2, 3 )))
#endif
void sb_sprintf(str_builder_t *sb, const char *fmt, ...);

void sb_vsprintf(str_builder_t *sb, const char *fmt, va_list vl);

void sb_reset(str_builder_t *sb);

size_t sb_length(str_builder_t *sb);

strbuf_t sb_build(str_builder_t *sb);
