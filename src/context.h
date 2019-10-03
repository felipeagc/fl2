#pragma once

#include "bump_alloc.h"
#include "position.h"
#include "result.h"
#include "str_builder.h"
#include "table.h"

typedef struct ast_t ast_t;
typedef struct proc_t proc_t;

typedef struct ctx_t {
  str_builder_t sb;
  bump_alloc_t alloc;
  table_t file_table;
  table_t extern_table;
  proc_t *main_proc;
} ctx_t;

void ctx_init(ctx_t *ctx);

error_set_t ctx_process_main_file(ctx_t *ctx, strbuf_t path);

error_set_t ctx_process_file(ctx_t *ctx, strbuf_t full_path, ast_t **ast);

void ctx_destroy(ctx_t *ctx);
