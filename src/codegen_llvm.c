#include "codegen_llvm.h"

#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/Core.h>
#include <llvm-c/DebugInfo.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>

void llvm_init(llvm_t* llvm, ctx_t *ctx) {
  llvm->ctx = ctx;
}

error_set_t llvm_codegen(llvm_t *llvm, ast_t *ast) {
  error_set_t result;
  memset(&result, 0, sizeof(result));

  return result;
}
