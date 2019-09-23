#pragma once

#include "context.h"

typedef struct llvm_t {
  ctx_t *ctx;
} llvm_t;

void llvm_init(llvm_t *llvm, ctx_t *ctx);

error_set_t llvm_codegen(llvm_t *llvm, ast_t *ast);
