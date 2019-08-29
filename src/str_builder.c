#include "str_builder.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SB_CAP 1 << 14

void sb_init(str_builder_t *sb) {
  memset(sb, 0, sizeof(*sb));
  sb->str.cap   = SB_CAP;
  sb->str.buf   = malloc(sb->str.cap);
  sb->str.count = 0;

  sb->scratch.cap   = 1 << 12;
  sb->scratch.buf   = malloc(sb->scratch.cap);
  sb->scratch.count = 0;
}

void sb_destroy(str_builder_t *sb) {
  free(sb->str.buf);
  free(sb->scratch.buf);
}

void sb_append(str_builder_t *sb, strbuf_t str) {
  if (sb->str.count + str.count >= sb->str.cap) {
    sb->str.cap = (sb->str.cap * 2) + str.count;
    sb->str.buf = realloc(sb->str.buf, sb->str.cap);
  }
  strncpy(&sb->str.buf[sb->str.count], str.buf, str.count);
  sb->str.count += str.count;
}

void sb_append_int(str_builder_t *sb, size_t val) {
  sb->scratch.count = snprintf(sb->scratch.buf, sb->scratch.cap, "%zu", val);
  sb_append(sb, sb->scratch);
}

void sb_append_char(str_builder_t *sb, char c) {
  sb_append(sb, (strbuf_t){.buf = &c, .count = 1});
}

void sb_sprintf(str_builder_t *sb, const char *fmt, ...) {
  va_list vl;
  va_start(vl, fmt);
  sb->scratch.count = vsnprintf(sb->scratch.buf, sb->scratch.cap, fmt, vl);
  va_end(vl);
  sb_append(sb, sb->scratch);
}

void sb_vsprintf(str_builder_t *sb, const char *fmt, va_list vl) {
  sb->scratch.count = vsnprintf(sb->scratch.buf, sb->scratch.cap, fmt, vl);
  sb_append(sb, sb->scratch);
}

void sb_reset(str_builder_t *sb) { sb->str.count = 0; }

size_t sb_length(str_builder_t *sb) { return sb->str.count; }

strbuf_t sb_build(str_builder_t *sb) { return sb->str; }
