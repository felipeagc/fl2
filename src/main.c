#include "analyzer.h"
#include "context.h"
#include "filesystem.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  if (argc < 2) {
    printf("Valid usage: %s <source file>\n", argv[0]);
    exit(1);
  }

  ctx_t ctx;
  ctx_init(&ctx);

  char *fullpath = absolute_path(argv[1]);
  strbuf_t full_path;
  full_path.count = strlen(fullpath);
  full_path.cap   = full_path.count + 1;
  full_path.buf   = fullpath;

  error_set_t result = ctx_process_main_file(&ctx, full_path);
  if (result.errors.count > 0) {
    For(err, result.errors) { print_error(&ctx.sb, err); }
    exit(1);
  }

  ctx_destroy(&ctx);

  free(fullpath);

  return 0;
}
