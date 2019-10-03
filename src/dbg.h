#pragma once

#define dbg(str)                                                               \
  printf(                                                                      \
      "%s:%s:%u :: '%.*s'\n",                                                  \
      __FILE__,                                                                \
      __func__,                                                                \
      __LINE__,                                                                \
      (int)(str).count,                                                        \
      (str).buf)

#define dbgf(fmt, ...)                                                         \
  printf("%s:%s:%u :: " fmt "\n", __FILE__, __func__, __LINE__, __VA_ARGS__)
