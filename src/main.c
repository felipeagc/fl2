#include "analyzer.h"
#include "context.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  if (argc < 2) {
    printf("Valid usage: %s <source file>\n", argv[0]);
    exit(1);
  }

  ctx_t ctx;
  ctx_init(&ctx);

  ast_t ast;
  memset(&ast, 0, sizeof(ast));
  error_set_t result = ctx_process_file(&ctx, STR(argv[1]), &ast);
  if (result.errors.count > 0) {
    For(err, result.errors) { print_error(&ctx.sb, err); }
    exit(1);
  }

  ctx_destroy(&ctx);

  return 0;
}
