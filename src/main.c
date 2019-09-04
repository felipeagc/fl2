#include "colors.h"
#include "context.h"
#include "parser.h"
#include "scanner.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

void print_error(str_builder_t *sb, error_t *err) {
  sb_reset(sb);

  color(sb, COLOR_GREY);
  sb_sprintf(
      sb,
      "%.*s:%zu:%zu: ",
      (int)err->pos.file->path.count,
      err->pos.file->path.buf,
      err->pos.line,
      err->pos.col);
  color(sb, COLOR_LIGHT_RED);
  sb_sprintf(sb, "error: %.*s:\n", (int)err->msg.count, err->msg.buf);
  color(sb, COLOR_RESET);

  char *start = err->pos.file->content.buf;

  size_t min_line = err->pos.line - 1;
  size_t max_line = err->pos.line + 1;

  const int ndigits = floor(log10(abs(max_line))) + 1;

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
        if (err->pos.offset <= pos && (err->pos.offset + err->pos.len) > pos) {
          color(sb, COLOR_LIGHT_MAGENTA);
        }
        sb_append_char(sb, line_start[c]);
        if (err->pos.offset <= pos && (err->pos.offset + err->pos.len) > pos) {
          color(sb, COLOR_RESET);
        }
      }
      sb_append_char(sb, '\n');
    }

    if (*line_buf == '\n') line_n++;
    line_buf++;
  }

  strbuf_t str = sb_build(sb);
  printf("%.*s", (int)str.count, str.buf);
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    printf("Valid usage: %s <source file>\n", argv[0]);
    exit(1);
  }

  ctx_t ctx;
  ctx_init(&ctx);

  file_t file;
  file_init(&file, STR(argv[1]));

  scanner_t scanner;
  scanner_init(&scanner, &ctx);

  token_slice_t tokens;
  error_set_t result = scanner_scan(&scanner, &file, &tokens);
  if (result.errors.count > 0) {
    For(err, result.errors) {
      print_error(&ctx.sb, err);
      /* printf( */
      /*     "%zu:%zu: %.*s\n", */
      /*     err->pos.line, */
      /*     err->pos.col, */
      /*     (int)err->msg.count, */
      /*     err->msg.buf); */
    }

    exit(1);
  }

  /* For(token, tokens) { */
  /*   strbuf_t tok_str = token_to_string(token); */
  /*   printf("%.*s\n", (int)tok_str.count, tok_str.buf); */
  /* } */

  parser_t parser;
  parser_init(&parser, &ctx);

  ast_t ast = {0};
  result = parser_parse(&parser, &file, tokens, &ast);

  if (result.errors.count > 0) {
    For(err, result.errors) {
      print_error(&ctx.sb, err);
      /* printf( */
      /*     "%zu:%zu: %.*s\n", */
      /*     err->pos.line, */
      /*     err->pos.col, */
      /*     (int)err->msg.count, */
      /*     err->msg.buf); */
    }

    exit(1);
  }

  printf("Success!\n");

  file_destroy(&file);
  ctx_destroy(&ctx);

  return 0;
}
