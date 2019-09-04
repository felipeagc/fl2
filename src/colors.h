#pragma once

#include "str_builder.h"

typedef enum term_color_t {
  COLOR_RESET,

  COLOR_BLACK,
  COLOR_WHITE,
  COLOR_GREY,
  COLOR_RED,
  COLOR_GREEN,
  COLOR_YELLOW,
  COLOR_BLUE,
  COLOR_MAGENTA,
  COLOR_CYAN,
  COLOR_DARK_GREY,
  COLOR_LIGHT_BLUE,
  COLOR_LIGHT_GREEN,
  COLOR_LIGHT_CYAN,
  COLOR_LIGHT_RED,
  COLOR_LIGHT_MAGENTA,
  COLOR_LIGHT_YELLOW,
} term_color_t;

static inline void color(str_builder_t *sb, term_color_t color) {
  strbuf_t str = {0};
  switch (color) {
  case COLOR_RESET: str = STR("\033[0m"); break;

  case COLOR_BLACK: str = STR("\033[0;30m"); break;
  case COLOR_RED: str = STR("\033[0;31m"); break;
  case COLOR_GREEN: str = STR("\033[0;32m"); break;
  case COLOR_YELLOW: str = STR("\033[0;33m"); break;
  case COLOR_BLUE: str = STR("\033[0;34m"); break;
  case COLOR_MAGENTA: str = STR("\033[0;35m"); break;
  case COLOR_CYAN: str = STR("\033[0;36m"); break;
  case COLOR_GREY: str = STR("\033[0;37m"); break;

  case COLOR_DARK_GREY: str = STR("\033[1;30m"); break;
  case COLOR_LIGHT_RED: str = STR("\033[1;31m"); break;
  case COLOR_LIGHT_GREEN: str = STR("\033[1;32m"); break;
  case COLOR_LIGHT_YELLOW: str = STR("\033[1;33m"); break;
  case COLOR_LIGHT_BLUE: str = STR("\033[1;34m"); break;
  case COLOR_LIGHT_MAGENTA: str = STR("\033[1;35m"); break;
  case COLOR_LIGHT_CYAN: str = STR("\033[1;36m"); break;
  case COLOR_WHITE: str = STR("\033[1;37m"); break;
  }
  sb_append(sb, str);
}
