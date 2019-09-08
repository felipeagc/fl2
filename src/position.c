#include "position.h"

#include "context.h"
#include "filesystem.h"
#include <stdio.h>
#include <stdlib.h>

bool file_init(file_t *file, ctx_t *ctx, strbuf_t path) {
  memset(file, 0, sizeof(*file));

  file->path = path;

  char *null_terminated_path = bump_alloc(&ctx->alloc, path.count + 1);
  strncpy(null_terminated_path, path.buf, path.count);
  null_terminated_path[path.count] = '\0';

  file->abs_path.buf = absolute_path(null_terminated_path);
  if (!file->abs_path.buf) return false;

  file->abs_path.count = strlen(file->abs_path.buf);
  file->abs_path.cap   = file->abs_path.count + 1;

  FILE *f = fopen(file->abs_path.buf, "r");
  if (!f) return false;

  fseek(f, 0, SEEK_END);
  file->content.cap = ftell(f);
  fseek(f, 0, SEEK_SET);

  file->content.count = file->content.cap;

  file->content.buf = malloc(file->content.cap);

  fread(file->content.buf, 1, file->content.cap, f);

  fclose(f);

  return true;
}

void file_destroy(file_t *file) {
  free(file->content.buf);
  free(file->abs_path.buf);
}
