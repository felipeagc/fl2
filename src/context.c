#include "context.h"

#include "analyzer.h"
#include "codegen_llvm.h"
#include "filesystem.h"
#include "parser.h"
#include "scanner.h"
#include <assert.h>
#include <stdio.h>

void ctx_init(ctx_t *ctx) {
  memset(ctx, 0, sizeof(*ctx));
  sb_init(&ctx->sb);
  bump_init(&ctx->alloc, 1 << 14);
  table_init(&ctx->file_table, 521);
  table_init(&ctx->extern_table, 2048);
}

error_set_t ctx_process_main_file(ctx_t *ctx, strbuf_t path) {
  ast_t *ast = bump_alloc(&ctx->alloc, sizeof(ast_t));
  memset(ast, 0, sizeof(*ast));
  error_set_t result = ctx_process_file(ctx, path, ast);
  if (result.errors.count > 0) return result;

  if (!ctx->main_proc) {
    static error_slice_t main_file_err_slice = {0};
    static error_t main_file_err             = {0};

    main_file_err                            = (error_t){
        .pos = (pos_t){.file = ast->file},
        .msg = STR("missing main procedure"),
    };
    main_file_err_slice.buf   = &main_file_err;
    main_file_err_slice.count = 1;
    main_file_err_slice.cap   = 1;

    return (error_set_t){
        .type   = RESULT_ANALYZER,
        .errors = main_file_err_slice,
    };
  }

  llvm_t llvm;
  llvm_init(&llvm, ctx);

  return llvm_codegen(&llvm, ast);
}

error_set_t ctx_process_file(ctx_t *ctx, strbuf_t full_path, ast_t *ast) {
  assert((full_path.count + 1) == full_path.cap);

  file_t *file = table_get(&ctx->file_table, full_path);

  error_set_t result;
  memset(&result, 0, sizeof(result));

  if (!file) {
    file_t *file = bump_alloc(&ctx->alloc, sizeof(file_t));
    if (!file_init(file, ctx, full_path)) {
      printf(
          "Failed to open file: %.*s\n", (int)full_path.count, full_path.buf);
      exit(1);
    }

    table_set(&ctx->file_table, full_path, file);

    scanner_t scanner;
    scanner_init(&scanner, ctx);

    token_slice_t tokens;
    result = scanner_scan(&scanner, file, &tokens);
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
  }

  return result;
}

void ctx_destroy(ctx_t *ctx) {
  sb_destroy(&ctx->sb);
  bump_destroy(&ctx->alloc);
  table_destroy(&ctx->file_table);
  table_destroy(&ctx->extern_table);
}
