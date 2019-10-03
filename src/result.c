#include "result.h"

#include "colors.h"
#include <math.h>
#include <stdio.h>

void print_error(str_builder_t *sb, error_t *err) {
  sb_reset(sb);

  if (err->pos.file) {
    color(sb, COLOR_GREY);
    sb_sprintf(
        sb,
        "%.*s:%zu:%zu: ",
        (int)err->pos.file->path.count,
        err->pos.file->path.buf,
        err->pos.line,
        err->pos.col);
  }
  color(sb, COLOR_LIGHT_RED);
  sb_sprintf(sb, "error: %.*s", (int)err->msg.count, err->msg.buf);
  sb_sprintf(sb, "\n");
  color(sb, COLOR_RESET);

  if (err->pos.len > 0) {
    char *start = err->pos.file->content.buf;

    size_t min_line = err->pos.line - 1;
    size_t max_line = err->pos.line + 1;

    const int ndigits = floor(log10(max_line)) + 1;

    char *line_buf = start;

    size_t line_n = 1;
    while (line_n <= max_line &&
           line_buf < (&start[err->pos.file->content.count])) {
      if (line_n >= min_line && line_n <= max_line) {
        char *line_start = line_buf;
        int line_len     = 0;
        while (*line_buf != '\n') {
          line_buf++;
          line_len++;
        }

        sb_sprintf(sb, " %.*lu |  ", ndigits, line_n);

        for (size_t c = 0; c < line_len; c++) {
          size_t pos = (size_t)((&line_start[c]) - start);
          if (err->pos.offset <= pos &&
              (err->pos.offset + err->pos.len) > pos) {
            color(sb, COLOR_LIGHT_MAGENTA);
          }
          sb_append_char(sb, line_start[c]);
          if (err->pos.offset <= pos &&
              (err->pos.offset + err->pos.len) > pos) {
            color(sb, COLOR_RESET);
          }
        }
        sb_append_char(sb, '\n');
      }

      if (*line_buf == '\n') line_n++;
      line_buf++;
    }
  }

  strbuf_t str = sb_build(sb);
  printf("%.*s", (int)str.count, str.buf);
}
