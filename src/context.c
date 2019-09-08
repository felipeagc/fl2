#include "context.h"

#include "analyzer.h"
#include "parser.h"
#include "scanner.h"

void ctx_init(ctx_t *ctx) {
  sb_init(&ctx->sb);
  bump_init(&ctx->alloc, 1 << 14);
}

error_set_t ctx_process_file(ctx_t *ctx, file_t *file, ast_t *ast) {
  scanner_t scanner;
  scanner_init(&scanner, ctx);

  token_slice_t tokens;
  error_set_t result = scanner_scan(&scanner, file, &tokens);
  if (result.errors.count > 0) return result;

  parser_t parser;
  parser_init(&parser, ctx);

  memset(ast, 0, sizeof(*ast));
  result = parser_parse(&parser, file, tokens, ast);
  if (result.errors.count > 0) return result;

  analyzer_t analyzer;
  analyzer_init(&analyzer, ctx);

  result = analyzer_analyze(&analyzer, ast);
  if (result.errors.count > 0) return result;

  return result;
}

void ctx_destroy(ctx_t *ctx) {
  sb_destroy(&ctx->sb);
  bump_destroy(&ctx->alloc);
}
