#include "result.h"

#include "colors.h"
#include <assert.h>
#include <math.h>
#include <stdio.h>

typedef enum {
  ERR_MAIN,
  ERR_FROM,
} err_kind_t;

#define INDENT_SIZE 3

#define INDENT()                                                               \
  do {                                                                         \
    for (int i = 0; i < INDENT_SIZE * indent; i++)                             \
      sb_append_char(sb, ' ');                                                 \
  } while (0)

static void print_pos(
    str_builder_t *sb,
    strbuf_t *msg,
    pos_t *pos,
    err_kind_t kind,
    size_t indent) {
  switch (kind) {
  case ERR_MAIN: {
    if (pos->file) {
      color(sb, COLOR_GREY);
      sb_sprintf(
          sb,
          "%.*s:%zu:%zu: ",
          (int)pos->file->path.count,
          pos->file->path.buf,
          pos->line,
          pos->col);
    }

    color(sb, COLOR_LIGHT_RED);
    sb_sprintf(sb, "error: %.*s\n", (int)msg->count, msg->buf);
    color(sb, COLOR_RESET);
  } break;

  case ERR_FROM: {
    assert(pos->file);

    color(sb, COLOR_BLUE);

    size_t full_indent = (indent * INDENT_SIZE);
    if (full_indent > 0) {
      for (size_t i = 0; i < full_indent - 2; i++) {
        sb_append_char(sb, '=');
      }
      sb_append_char(sb, '>');
      sb_append_char(sb, ' ');
    }

    sb_sprintf(sb, "from: ");
    color(sb, COLOR_GREY);
    sb_sprintf(
        sb,
        "%.*s:%zu:%zu:\n",
        (int)pos->file->path.count,
        pos->file->path.buf,
        pos->line,
        pos->col);
    color(sb, COLOR_RESET);
  } break;
  }

  if (pos->len > 0) {
    char *start = pos->file->content.buf;

    size_t min_line      = pos->line > 1 ? pos->line - 1 : pos->line;
    const size_t n_lines = 3;

    {
      size_t line_count = 1;
      while (*start) {
        if (line_count >= min_line) break;
        if (*start == '\n') line_count++;
        start++;
      }
    }

    const int ndigits = floor(log10(min_line + n_lines - 1)) + 1;

    for (size_t i = min_line; i < min_line + n_lines; i++) {
      INDENT();
      sb_sprintf(sb, " %.*lu |  ", ndigits, i);

      while (*start != '\n' && *start != '\0') {
        size_t offset = (size_t)(start - pos->file->content.buf);
        if (pos->offset <= offset && (pos->offset + pos->len) > offset) {
          color(sb, COLOR_LIGHT_MAGENTA);
        }

        sb_append_char(sb, *start);

        if (pos->offset <= offset && (pos->offset + pos->len) > offset) {
          color(sb, COLOR_RESET);
        }

        start++;
      }

      if (*start == '\n') start++;

      sb_append_char(sb, '\n');

      if (*start == '\0') break;
    }
  }
}

void print_error(str_builder_t *sb, error_t *err) {
  sb_reset(sb);

  print_pos(sb, &err->msg, &err->pos, ERR_MAIN, 0);

  For(pos, err->pos_stack) { print_pos(sb, NULL, pos, ERR_FROM, 1); }

  strbuf_t str = sb_build(sb);
  printf("%.*s", (int)str.count, str.buf);
}
