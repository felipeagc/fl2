#include "filesystem.h"

#include <stdlib.h>
#include <string.h>

#ifdef __unix__
#include <limits.h>
#include <unistd.h>
#endif

char *absolute_path(char *relative_path) {
#ifdef __unix__
  return realpath(relative_path, NULL);
#endif
}

char *get_file_dir(char *path) {
#ifdef __unix__
  char *abs = absolute_path(path);
  for (int i = strlen(abs) - 1; i >= 0; i--) {
    if (abs[i] == '/') {
      abs[i + 1] = '\0';
      break;
    }
  }
  return abs;
#endif
}
