#include "context.h"

void ctx_init(ctx_t *ctx) {
  sb_init(&ctx->sb);
  bump_init(&ctx->alloc, 1 << 14);
}

void ctx_destroy(ctx_t *ctx) {
  sb_destroy(&ctx->sb);
  bump_destroy(&ctx->alloc);
}
