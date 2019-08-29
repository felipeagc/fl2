#include "position.h"

#include <stdio.h>
#include <stdlib.h>

void file_init(file_t *file, strbuf_t path) {
  file->path = path;

  FILE *f = fopen(path.buf, "r");
  if (!f) {
    printf("Failed to open file: %s\n", path.buf);
    exit(1);
  }

  fseek(f, 0, SEEK_END);
  file->content.cap = ftell(f);
  fseek(f, 0, SEEK_SET);

  file->content.count = file->content.cap;

  file->content.buf = malloc(file->content.cap);

  fread(file->content.buf, 1, file->content.cap, f);

  fclose(f);
}

void file_destroy(file_t *file) { free(file->content.buf); }
