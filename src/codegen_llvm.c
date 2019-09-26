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

typedef struct {
  LLVMModuleRef mod;
  LLVMBuilderRef builder;
  LLVMTargetDataRef data;
} module_t;

static void mod_init(module_t *mod, const char *name) {
  mod->mod     = LLVMModuleCreateWithName(name);
  mod->builder = LLVMCreateBuilder();
  mod->data    = LLVMGetModuleDataLayout(mod->mod);
}

static void mod_destroy(module_t *mod) { LLVMDisposeBuilder(mod->builder); }

static inline LLVMValueRef load_val(module_t *mod, value_t *val) {
  if (val->kind == VALUE_LOCAL_VAR || val->kind == VALUE_GLOBAL_VAR) {
    return LLVMBuildLoad(mod->builder, val->value, "");
  }
  return val->value;
}

static LLVMTypeRef llvm_type(llvm_t *llvm, type_t *type) {
  if (type->ref) return type->ref;

  switch (type->kind) {
  case TYPE_PRIMITIVE: {
    switch (type->prim) {
    case PRIM_TYPE_I8:
    case PRIM_TYPE_U8: type->ref = LLVMInt8Type(); break;
    case PRIM_TYPE_I16:
    case PRIM_TYPE_U16: type->ref = LLVMInt16Type(); break;
    case PRIM_TYPE_I32:
    case PRIM_TYPE_U32: type->ref = LLVMInt32Type(); break;
    case PRIM_TYPE_I64:
    case PRIM_TYPE_U64: type->ref = LLVMInt64Type(); break;
    case PRIM_TYPE_F32: type->ref = LLVMFloatType(); break;
    case PRIM_TYPE_F64: type->ref = LLVMDoubleType(); break;
    case PRIM_TYPE_BOOL: type->ref = LLVMInt32Type(); break;
    case PRIM_TYPE_VOID: type->ref = LLVMVoidType(); break;

    case PRIM_TYPE_NUM_BEGIN:
    case PRIM_TYPE_FLOAT_BEGIN:
    case PRIM_TYPE_FLOAT_END:
    case PRIM_TYPE_NUM_END: assert(0);
    }
  } break;

  case TYPE_PTR: {
    return LLVMPointerType(llvm_type(llvm, type->subtype), 0);
  } break;

  case TYPE_SLICE: assert(0);
  case TYPE_ARRAY: assert(0);
  case TYPE_PROC: {
    size_t param_count = type->proc_sig->params.count;
    LLVMTypeRef *param_types =
        bump_alloc(&llvm->ctx->alloc, sizeof(LLVMTypeRef) * param_count);
    for (size_t i = 0; i < type->proc_sig->params.count; i++) {
      param_types[i] = llvm_type(llvm, type->proc_sig->params.buf[0].type);
    }

    LLVMTypeRef return_type = NULL;

    if (type->proc_sig->return_types.count > 0) {
      return_type =
          llvm_type(llvm, type->proc_sig->return_types.buf[0].as_type);
    } else {
      return_type = LLVMVoidType();
    }

    type->ref = LLVMPointerType(
        LLVMFunctionType(return_type, param_types, param_count, false), 0);
  } break;
  case TYPE_STRUCT: {
    size_t element_count = 1;
    LLVMTypeRef *element_types =
        bump_alloc(&llvm->ctx->alloc, sizeof(LLVMTypeRef) * element_count);
    element_types[0]    = LLVMInt32Type();
    LLVMTypeRef llvm_ty = LLVMStructCreateNamed(
        LLVMGetGlobalContext(), bump_c_str(&llvm->ctx->alloc, type->name));

    LLVMStructSetBody(llvm_ty, element_types, element_count, false);

    type->ref = llvm_ty;
  } break;
  case TYPE_STRING: assert(0);

  case TYPE_NAMESPACE: assert(0);
  case TYPE_TYPE: assert(0);
  case TYPE_UNDEFINED: assert(0);
  }

  return type->ref;
}

static void codegen_const_expr(
    llvm_t *llvm,
    module_t *mod,
    block_t *operand_block,
    block_t *operation_block,
    expr_t *expr,
    value_t *val) {
  if (operation_block == NULL) operation_block = operand_block;

  memset(val, 0, sizeof(*val));

  switch (expr->kind) {
  case EXPR_PRIMARY: {

    switch (expr->primary.kind) {
    case PRIMARY_INT:
    case PRIMARY_FLOAT: {

      if (expr->type->kind == TYPE_PRIMITIVE) {
        switch (expr->type->prim) {
        case PRIM_TYPE_I8:
        case PRIM_TYPE_I16:
        case PRIM_TYPE_I32:
        case PRIM_TYPE_I64: {
          switch (expr->primary.kind) {
          case PRIMARY_INT: {
            val->kind  = VALUE_CONST;
            val->value = LLVMConstInt(
                llvm_type(llvm, expr->type),
                (unsigned long long)expr->primary.i64,
                true);
          } break;

          case PRIMARY_FLOAT: assert(0);

          default: break;
          }
        } break;

        case PRIM_TYPE_U8:
        case PRIM_TYPE_U16:
        case PRIM_TYPE_U32:
        case PRIM_TYPE_U64: {
          switch (expr->primary.kind) {
          case PRIMARY_INT: {
            val->kind  = VALUE_CONST;
            val->value = LLVMConstInt(
                llvm_type(llvm, expr->type),
                (unsigned long long)expr->primary.i64,
                false);
          } break;

          case PRIMARY_FLOAT: assert(0);

          default: break;
          }
        } break;

        case PRIM_TYPE_F32:
        case PRIM_TYPE_F64: {
          switch (expr->primary.kind) {
          case PRIMARY_INT: {
            val->kind  = VALUE_CONST;
            val->value = LLVMConstReal(
                llvm_type(llvm, expr->type), (double)expr->primary.i64);
          } break;

          case PRIMARY_FLOAT: {
            val->kind = VALUE_CONST;
            val->value =
                LLVMConstReal(llvm_type(llvm, expr->type), expr->primary.f64);
          } break;

          default: break;
          }
        } break;

        case PRIM_TYPE_BOOL:
        case PRIM_TYPE_VOID:
        case PRIM_TYPE_FLOAT_END:
        case PRIM_TYPE_NUM_END:
        case PRIM_TYPE_NUM_BEGIN:
        case PRIM_TYPE_FLOAT_BEGIN: assert(0);
        }
      }

    } break;

    case PRIMARY_STRING: break;

    case PRIMARY_CSTRING: {
      LLVMValueRef glob = LLVMAddGlobal(
          mod->mod,
          LLVMArrayType(LLVMInt8Type(), expr->primary.string.count),
          "str");

      // set as internal linkage and constant
      LLVMSetLinkage(glob, LLVMInternalLinkage);
      LLVMSetGlobalConstant(glob, true);

      // Initialize with string:
      LLVMSetInitializer(
          glob,
          LLVMConstString(
              expr->primary.string.buf, expr->primary.string.count, true));

      LLVMValueRef zero       = LLVMConstInt(LLVMInt32Type(), 0, false);
      LLVMValueRef indices[2] = {zero, zero};

      val->kind  = VALUE_CONST;
      val->value = LLVMConstGEP(glob, indices, 2);
    } break;

    case PRIMARY_PRIMITIVE_TYPE: {
      // Not a runtime expression
      assert(0);
    } break;

    case PRIMARY_IDENT: {
      symbol_t *sym = scope_get(&operand_block->scope, expr->primary.string);
      assert(sym);

      if (sym->value.kind == VALUE_UNDEFINED) {
        codegen_const_expr(
            llvm,
            mod,
            operand_block,
            NULL,
            &sym->const_decl->expr,
            &sym->value);
      }

      *val = sym->value;
    } break;
    }

  } break;

  case EXPR_EXPR: {
    return codegen_const_expr(llvm, mod, operand_block, NULL, expr->expr, val);
  } break;

  case EXPR_PROC: break;

  case EXPR_ACCESS: {
    // TODO
  } break;

  case EXPR_PROC_CALL: {
    // Unimplemented
    assert(0);
  } break;

  case EXPR_UNARY: {
    // TODO
  } break;

  case EXPR_BINARY: {
    // TODO
  } break;

  case EXPR_BLOCK: break;

  case EXPR_STRUCT:
  case EXPR_IMPORT:
  case EXPR_PROC_PTR: {
    // This isn't a runtime expression
    assert(0);
  } break;
  }
}

static void codegen_expr(
    llvm_t *llvm,
    module_t *mod,
    block_t *operand_block,
    block_t *operation_block,
    expr_t *expr,
    value_t *val) {
  if (operation_block == NULL) operation_block = operand_block;

  memset(val, 0, sizeof(*val));

  switch (expr->kind) {
  case EXPR_PRIMARY: {

    switch (expr->primary.kind) {
    case PRIMARY_INT:
    case PRIMARY_FLOAT:
    case PRIMARY_STRING:
    case PRIMARY_CSTRING:
      return codegen_const_expr(llvm, mod, operand_block, NULL, expr, val);

    case PRIMARY_PRIMITIVE_TYPE: {
      // Not a runtime expression
      assert(0);
    } break;

    case PRIMARY_IDENT: {
      symbol_t *sym = scope_get(&operand_block->scope, expr->primary.string);
      assert(sym);

      if (sym->value.kind == VALUE_UNDEFINED) {
        switch (sym->kind) {
        case SYMBOL_GLOBAL_VAR:
        case SYMBOL_LOCAL_VAR: {
          codegen_expr(
              llvm,
              mod,
              operand_block,
              NULL,
              &sym->const_decl->expr,
              &sym->value);
        } break;

        case SYMBOL_CONST_DECL: {
          codegen_const_expr(
              llvm,
              mod,
              operand_block,
              NULL,
              &sym->const_decl->expr,
              &sym->value);
        } break;

        case SYMBOL_DUMMY: assert(0);
        }
      }

      *val = sym->value;
    } break;
    }

  } break;

  case EXPR_EXPR: {
    return codegen_expr(llvm, mod, operand_block, NULL, expr->expr, val);
  } break;

  case EXPR_PROC: break;

  case EXPR_ACCESS: {
    symbol_t *sym = get_expr_sym(&operand_block->scope, expr->access.left);
    assert(sym);

    switch (sym->kind) {
    case SYMBOL_CONST_DECL: {
      expr_t *decl_expr = inner_expr(&sym->const_decl->expr);

      switch (decl_expr->kind) {
      case EXPR_IMPORT: {
        codegen_expr(
            llvm,
            mod,
            operand_block,
            &decl_expr->import.ast->block,
            expr->access.right,
            &sym->value);
        *val = sym->value;
      } break;

      default: break;
      }
    } break;

    default: break;
    }
  } break;

  case EXPR_PROC_CALL: {
    symbol_t *sym = get_expr_sym(&operation_block->scope, expr->proc_call.expr);
    assert(sym);

    LLVMValueRef fun = load_val(mod, &sym->value);
    assert(fun);

    unsigned arg_count = (unsigned)expr->proc_call.params.count;
    LLVMValueRef *args =
        bump_alloc(&llvm->ctx->alloc, sizeof(LLVMValueRef) * arg_count);

    for (size_t i = 0; i < arg_count; i++) {
      value_t v;
      codegen_expr(
          llvm, mod, operand_block, NULL, &expr->proc_call.params.buf[i], &v);
      args[i] = v.value;
    }

    val->kind  = VALUE_TMP_VAR;
    val->value = LLVMBuildCall(mod->builder, fun, args, arg_count, "");
  } break;

  case EXPR_UNARY: {
    // TODO
  } break;

  case EXPR_BINARY: {
    // TODO
  } break;

  case EXPR_BLOCK: break;

  case EXPR_STRUCT:
  case EXPR_IMPORT:
  case EXPR_PROC_PTR: {
    // This isn't a runtime expression
    assert(0);
  } break;
  }
}

/*
 * Registers function prototypes and binds LLVM types to symbols
 */
static void pre_pass(llvm_t *llvm, module_t *mod, block_t *block) {
  For(stmt, block->stmts) {
    switch (stmt->kind) {
    case STMT_CONST_DECL: {
      const_decl_t *const_decl = &stmt->const_decl;
      expr_t *expr             = inner_expr(&stmt->const_decl.expr);

      switch (expr->kind) {
      case EXPR_PROC: {
        proc_t *proc = &expr->proc;

        char *fun_name   = bump_c_str(&llvm->ctx->alloc, const_decl->name);
        LLVMValueRef fun = NULL;

        if (proc->sig.flags & PROC_FLAG_EXTERN) {
          fun = LLVMGetNamedFunction(mod->mod, fun_name);
        }

        if (fun == NULL) {
          LLVMTypeRef *param_types = bump_alloc(
              &llvm->ctx->alloc, sizeof(LLVMTypeRef) * proc->sig.params.count);
          for (size_t i = 0; i < proc->sig.params.count; i++) {
            param_types[i] = llvm_type(llvm, proc->sig.params.buf[i].type);
          }

          LLVMTypeRef return_type = NULL;
          if (proc->sig.return_types.count > 0) {
            return_type =
                llvm_type(llvm, proc->sig.return_types.buf[0].as_type);
          } else {
            return_type = LLVMVoidType();
          }

          LLVMTypeRef fun_type = LLVMFunctionType(
              return_type, param_types, proc->sig.params.count, false);
          fun = LLVMAddFunction(mod->mod, fun_name, fun_type);

          if (proc->sig.flags & PROC_FLAG_EXTERN) {
            LLVMSetLinkage(fun, LLVMExternalLinkage);
          }
        }

        assert(fun);

        const_decl->sym->value.kind  = VALUE_PROC;
        const_decl->sym->value.value = fun;
      } break;

      case EXPR_IMPORT: {
        pre_pass(llvm, mod, &expr->import.ast->block);
      } break;

      default: {
        if (expr->type->kind == TYPE_TYPE) {
          stmt->const_decl.sym->value.kind = VALUE_TYPE;
          stmt->const_decl.sym->value.type = llvm_type(llvm, expr->as_type);
        }
      } break;
      }
    } break;

    case STMT_USING: {
      expr_t *expr = inner_expr(&stmt->expr);

      switch (expr->kind) {
      case EXPR_IMPORT: {
        pre_pass(llvm, mod, &expr->import.ast->block);
      } break;
      default: break;
      }
    } break;

    default: break;
    }
  }
}

static void codegen_stmts(llvm_t *llvm, module_t *mod, block_t *block) {
  For(stmt, block->stmts) {
    switch (stmt->kind) {
    case STMT_CONST_DECL: {
      const_decl_t *const_decl = &stmt->const_decl;

      expr_t *expr = inner_expr(&const_decl->expr);

      switch (expr->kind) {
      case EXPR_PROC: {
        proc_t *proc = &expr->proc;
        if (proc->sig.flags & PROC_FLAG_NO_BODY) break;

        assert(const_decl->sym->value.kind == VALUE_PROC);
        LLVMValueRef fun = const_decl->sym->value.value;

        for (size_t i = 0; i < proc->sig.params.count; i++) {
          var_decl_t *param = &proc->sig.params.buf[i];

          char *param_name        = bump_c_str(&llvm->ctx->alloc, param->name);
          LLVMValueRef llvm_param = LLVMGetParam(fun, i);
          LLVMSetValueName(llvm_param, param_name);

          if (param->sym) {
            // Parameter could be unnamed and thus have no symbol
            param->sym->value.kind  = VALUE_TMP_VAR;
            param->sym->value.value = llvm_param;
          }
        }

        LLVMBasicBlockRef entry    = LLVMAppendBasicBlock(fun, "entry");
        LLVMBasicBlockRef prev_pos = LLVMGetInsertBlock(mod->builder);

        LLVMPositionBuilderAtEnd(mod->builder, entry);
        pre_pass(llvm, mod, &expr->proc.block);
        codegen_stmts(llvm, mod, &expr->proc.block);

        if (proc->sig.return_types.count == 0) {
          if (!LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(mod->builder))) {
            LLVMBuildRetVoid(mod->builder); // Add void return
          }
        }

        LLVMPositionBuilderAtEnd(mod->builder, prev_pos);
      } break;

      case EXPR_IMPORT: {
        codegen_stmts(llvm, mod, &expr->import.ast->block);
      } break;

      default: {
        switch (expr->type->kind) {
        case TYPE_NAMESPACE:
        case TYPE_UNDEFINED:
        case TYPE_TYPE: break;

        default: {
          codegen_const_expr(
              llvm, mod, block, NULL, expr, &const_decl->sym->value);
        } break;
        }
      } break;
      }
    } break;

    case STMT_VAR_DECL: {
      var_decl_t *var_decl = &stmt->var_decl;

      switch (var_decl->sym->kind) {
      case SYMBOL_GLOBAL_VAR: {
        var_decl->sym->value.kind  = VALUE_GLOBAL_VAR;
        var_decl->sym->value.value = LLVMAddGlobal(
            mod->mod,
            llvm_type(llvm, var_decl->type),
            bump_c_str(&llvm->ctx->alloc, var_decl->name));
      } break;

      case SYMBOL_LOCAL_VAR: {
        LLVMTypeRef type_ref = llvm_type(llvm, var_decl->type);

        var_decl->sym->value.kind  = VALUE_LOCAL_VAR;
        var_decl->sym->value.value = LLVMBuildAlloca(
            mod->builder,
            type_ref,
            bump_c_str(&llvm->ctx->alloc, var_decl->name));

        if (var_decl->flags & VAR_DECL_HAS_EXPR) {
          value_t value;
          codegen_expr(llvm, mod, block, NULL, &var_decl->expr, &value);
          assert(value.value);

          LLVMBuildStore(
              mod->builder, load_val(mod, &value), var_decl->sym->value.value);
        } else {
          // Zero initialization
          static LLVMValueRef zero_val = NULL;
          if (!zero_val) zero_val = LLVMConstInt(LLVMInt8Type(), 0, false);

          LLVMBuildMemSet(
              mod->builder,
              var_decl->sym->value.value,
              zero_val,
              LLVMConstInt(
                  LLVMInt32Type(),
                  LLVMStoreSizeOfType(mod->data, type_ref),
                  false),
              LLVMPreferredAlignmentOfType(mod->data, type_ref));
        }
      } break;

      default: assert(0);
      }
    } break;

    case STMT_VAR_ASSIGN: {
      var_assign_t *var_assign = &stmt->var_assign;

      symbol_t *sym = get_expr_sym(&block->scope, &var_assign->assigned);
      assert(sym);
      assert(sym->kind == SYMBOL_GLOBAL_VAR || sym->kind == SYMBOL_LOCAL_VAR);

      value_t value;
      codegen_expr(llvm, mod, block, NULL, &var_assign->expr, &value);
      assert(value.value);

      LLVMBuildStore(mod->builder, load_val(mod, &value), sym->value.value);
    } break;

    case STMT_EXPR: {

    } break;

    case STMT_RETURN: {
      if (stmt->ret.exprs.count > 0) {
        value_t value;
        codegen_expr(llvm, mod, block, NULL, &stmt->ret.exprs.buf[0], &value);

        LLVMBuildRet(mod->builder, load_val(mod, &value));
      } else {
        LLVMBuildRetVoid(mod->builder);
      }
    } break;

    case STMT_USING: {

    } break;

    case STMT_DUMMY: break;
    }
  }
}

static void codegen_block(llvm_t *llvm, module_t *mod, block_t *block) {
  // Register types and procedures
  pre_pass(llvm, mod, block);

  // Generate instructions & values for statements inside block
  codegen_stmts(llvm, mod, block);
}

void llvm_init(llvm_t *llvm, ctx_t *ctx) { llvm->ctx = ctx; }

error_set_t llvm_codegen(llvm_t *llvm, ast_t *ast) {
  error_set_t result;
  memset(&result, 0, sizeof(result));

  module_t mod;
  mod_init(&mod, "main");

  codegen_block(llvm, &mod, &ast->block);

  printf("%s\n", LLVMPrintModuleToString(mod.mod));

  char *error = NULL;
  if (LLVMVerifyModule(mod.mod, LLVMReturnStatusAction, &error)) {
    printf("Failed to verify module:\n%s\n", error);
    exit(1);
  }

  mod_destroy(&mod);

  return result;
}
