#include "context.h"
#include "parser.h"
#include "scanner.h"
#include <stdio.h>
#include <stdlib.h>

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
  result_t result = scanner_scan(&scanner, &file, &tokens);
  if (result.errors.count > 0) {
    For(err, result.errors) {
      printf(
          "%zu:%zu: %.*s\n",
          err->pos.line,
          err->pos.col,
          (int)err->msg.count,
          err->msg.buf);
    }

    exit(1);
  }

  /* For(token, tokens) { */
  /*   strbuf_t tok_str = token_to_string(token); */
  /*   printf("%.*s\n", (int)tok_str.count, tok_str.buf); */
  /* } */

  parser_t parser;
  parser_init(&parser, &ctx);

  ast_t ast;
  result = parser_parse(&parser, &file, tokens, &ast);

  if (result.errors.count > 0) {
    For(err, result.errors) {
      printf(
          "%zu:%zu: %.*s\n",
          err->pos.line,
          err->pos.col,
          (int)err->msg.count,
          err->msg.buf);
    }

    exit(1);
  }

  printf("Success!\n");

  file_destroy(&file);
  ctx_destroy(&ctx);

  return 0;
}
