#include "codegen_llvm.h"

#include "ast.h"
#include "expr.h"
#include <assert.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/Core.h>
#include <llvm-c/DebugInfo.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <stdio.h>

typedef struct {
  LLVMModuleRef mod;
  LLVMBuilderRef builder;
} module_t;

static void mod_init(module_t *mod, const char *name) {
  mod->mod     = LLVMModuleCreateWithName(name);
  mod->builder = LLVMCreateBuilder();
}

static void mod_destroy(module_t *mod) { LLVMDisposeBuilder(mod->builder); }

static LLVMTypeRef llvm_type(llvm_t *llvm, type_t *type) {
  switch (type->kind) {
  case TYPE_PRIMITIVE: {
    switch (type->prim) {
    case PRIM_TYPE_I8:
    case PRIM_TYPE_U8: return LLVMInt8Type();
    case PRIM_TYPE_I16:
    case PRIM_TYPE_U16: return LLVMInt16Type();
    case PRIM_TYPE_I32:
    case PRIM_TYPE_U32: return LLVMInt32Type();
    case PRIM_TYPE_I64:
    case PRIM_TYPE_U64: return LLVMInt64Type();
    case PRIM_TYPE_F32: return LLVMFloatType();
    case PRIM_TYPE_F64: return LLVMDoubleType();
    case PRIM_TYPE_BOOL: return LLVMInt32Type();
    case PRIM_TYPE_VOID: return LLVMVoidType();

    case PRIM_TYPE_NUM_BEGIN:
    case PRIM_TYPE_FLOAT_BEGIN:
    case PRIM_TYPE_FLOAT_END:
    case PRIM_TYPE_NUM_END: assert(0);
    }
  } break;

  case TYPE_PTR: assert(0);
  case TYPE_SLICE: assert(0);
  case TYPE_ARRAY: assert(0);
  case TYPE_PROC: {
    size_t param_count = type->proc_sig->params.count;
    LLVMTypeRef *param_types =
        bump_alloc(&llvm->ctx->alloc, sizeof(LLVMTypeRef) * param_count);
    for (size_t i = 0; i < type->proc_sig->params.count; i++) {
      param_types[i] = llvm_type(llvm, &type->proc_sig->params.buf[0].type);
    }

    LLVMTypeRef return_type =
        llvm_type(llvm, &type->proc_sig->return_types.buf[0]);

    LLVMFunctionType(return_type, param_types, param_count, false);

  } break;
  case TYPE_STRUCT: {
    size_t element_count = 1;
    LLVMTypeRef *element_types =
        bump_alloc(&llvm->ctx->alloc, sizeof(LLVMTypeRef) * element_count);
    element_types[0]    = LLVMInt32Type();
    LLVMTypeRef llvm_ty = LLVMStructCreateNamed(
        LLVMGetGlobalContext(), bump_c_str(&llvm->ctx->alloc, type->name));

    LLVMStructSetBody(llvm_ty, element_types, element_count, false);

    return llvm_ty;
  } break;
  case TYPE_STRING: assert(0);

  case TYPE_NAMESPACE: assert(0);
  case TYPE_TYPE: assert(0);
  case TYPE_UNDEFINED: assert(0);
  }

  return NULL;
}

static void ast_add_types(llvm_t *llvm, module_t *mod, block_t *block) {
  For(stmt, block->stmts) {
    switch (stmt->kind) {
    case STMT_CONST_DECL: {
      expr_t *expr = inner_expr(&stmt->const_decl.expr);
      if (expr->type.kind == TYPE_TYPE) {
        LLVMTypeRef llvm_ty = llvm_type(llvm, &expr->as_type);
      }
    } break;

    case STMT_USING: {
      // TODO: generate imported stuff

    } break;

    default: break;
    }
  }
}

static void ast_add_procs(llvm_t *llvm, module_t *mod, block_t *block) {
  For(stmt, block->stmts) {
    switch (stmt->kind) {
    case STMT_CONST_DECL: {
      expr_t *expr = inner_expr(&stmt->const_decl.expr);
      if (expr->kind == EXPR_PROC) {

        proc_t *proc = &expr->proc;

        char *fun_name   = bump_c_str(&llvm->ctx->alloc, stmt->const_decl.name);
        LLVMValueRef fun = NULL;

        if (proc->sig.flags & PROC_FLAG_EXTERN) {
          fun = LLVMGetNamedFunction(mod->mod, fun_name);
        }

        if (fun == NULL) {
          LLVMTypeRef *param_types = bump_alloc(
              &llvm->ctx->alloc, sizeof(LLVMTypeRef) * proc->sig.params.count);
          for (size_t i = 0; i < proc->sig.params.count; i++) {
            param_types[i] = llvm_type(llvm, &proc->sig.params.buf[i].type);
          }

          LLVMTypeRef return_type =
              llvm_type(llvm, &proc->sig.return_types.buf[0]);

          LLVMTypeRef fun_type = LLVMFunctionType(
              return_type, param_types, proc->sig.params.count, false);
          fun = LLVMAddFunction(mod->mod, fun_name, fun_type);

          if (proc->sig.flags & PROC_FLAG_EXTERN) {
            LLVMSetLinkage(fun, LLVMExternalLinkage);
          }
        }
      }
    } break;

    case STMT_USING: {
      // TODO: generate imported stuff

    } break;

    default: break;
    }
  }
}

static void codegen_ast(llvm_t *llvm, module_t *mod, block_t *block) {
  ast_add_types(llvm, mod, block);
  ast_add_procs(llvm, mod, block);
  /* ast_codegen_stmts(llvm, mod, ast); */
}

void llvm_init(llvm_t *llvm, ctx_t *ctx) { llvm->ctx = ctx; }

error_set_t llvm_codegen(llvm_t *llvm, ast_t *ast) {
  error_set_t result;
  memset(&result, 0, sizeof(result));

  module_t mod;
  mod_init(&mod, "main");

  codegen_ast(llvm, &mod, &ast->block);

  printf("%s\n", LLVMPrintModuleToString(mod.mod));

  mod_destroy(&mod);

  return result;
}
