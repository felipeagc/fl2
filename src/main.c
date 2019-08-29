#include "scanner.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  if (argc < 2) {
    printf("Valid usage: %s <source file>\n", argv[0]);
    exit(1);
  }

  file_t file;
  file_init(&file, STR(argv[1]));

  scanner_t scanner;
  scanner_init(&scanner);

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
  }

  For(token, tokens) {
    strbuf_t tok_str = token_to_string(token);
    printf("%.*s\n", (int)tok_str.count, tok_str.buf);
  }

  printf("Success!\n");

  scanner_destroy(&scanner);

  file_destroy(&file);

  return 0;
}
