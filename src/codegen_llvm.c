#include "codegen_llvm.h"

#include "ast.h"
#include "dbg.h"
#include "expr.h"
#include <assert.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/Core.h>
#include <llvm-c/DebugInfo.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <stdio.h>

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

static void pre_pass(llvm_t *llvm, module_t *mod, block_t *block);
static void codegen_stmts(llvm_t *llvm, module_t *mod, block_t *block);

static void codegen_const_expr(
    llvm_t *llvm,
    module_t *mod,
    block_t *operand_block,
    block_t *operation_block,
    expr_t *expr,
    value_t *val) {
  if (operation_block == NULL) operation_block = operand_block;

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
    } break;

    case PRIMARY_IDENT: {
      symbol_t *sym = scope_get(&operation_block->scope, expr->primary.string);
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

  case EXPR_INTRIN: {
    switch (expr->intrin.kind) {
    case INTRIN_SIZEOF: {
      expr_t *param = &expr->intrin.params.buf[0];

      type_t *type = NULL;

      assert(param->type);
      switch (param->type->kind) {
      case TYPE_TYPE: {
        assert(param->as_type);
        type = param->as_type;
      } break;
      default: {
        type = param->type;
      } break;
      }

      assert(type);

      val->kind  = VALUE_CONST;
      val->value = LLVMConstInt(
          llvm_type(llvm, expr->type),
          LLVMStoreSizeOfType(mod->data, llvm_type(llvm, type)),
          false);
    } break;

    default: assert(0);
    }
  } break;

  case EXPR_PROC: {
    proc_t *proc = &expr->proc;
    if (proc->sig.flags & PROC_FLAG_NO_BODY) break;

    if (val->kind == VALUE_UNDEFINED) {
      char *fun_name = "nested_proc";

      LLVMTypeRef *param_types = bump_alloc(
          &llvm->ctx->alloc, sizeof(LLVMTypeRef) * proc->sig.params.count);
      for (size_t i = 0; i < proc->sig.params.count; i++) {
        param_types[i] = llvm_type(llvm, proc->sig.params.buf[i].type);
      }

      LLVMTypeRef return_type = NULL;
      if (proc->sig.return_types.count > 0) {
        return_type = llvm_type(llvm, proc->sig.return_types.buf[0].as_type);
      } else {
        return_type = LLVMVoidType();
      }

      LLVMTypeRef fun_type = LLVMFunctionType(
          return_type, param_types, proc->sig.params.count, false);

      val->kind  = VALUE_PROC;
      val->value = LLVMAddFunction(mod->mod, fun_name, fun_type);

      if (proc->sig.flags & PROC_FLAG_EXTERN) {
        LLVMSetLinkage(val->value, LLVMExternalLinkage);
      }
    }

    assert(val->kind == VALUE_PROC);
    LLVMValueRef fun = val->value;

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

  case EXPR_BLOCK: {
  } break;

  case EXPR_IMPORT: {
    codegen_stmts(llvm, mod, &expr->import.ast->block);
  } break;

  case EXPR_STRUCT:
  case EXPR_PROC_PTR: {
    // This isn't a runtime expression
  } break;
  }
}

static void codegen_expr(
    llvm_t *llvm,
    module_t *mod,
    block_t *operand_block,
    block_t *operation_block,
    expr_t *expr,
    value_t *val);

static void is_expr_true(
    llvm_t *llvm,
    module_t *mod,
    block_t *operand_block,
    expr_t *expr,
    value_t *val) {
  value_t value;
  codegen_expr(llvm, mod, operand_block, NULL, expr, &value);

  LLVMValueRef value_ref = load_val(mod, &value);

  val->kind  = VALUE_TMP_VAR;
  val->value = LLVMBuildICmp(
      mod->builder,
      LLVMIntNE,
      value_ref,
      LLVMConstInt(LLVMTypeOf(value_ref), 0, false),
      "");
}

static void codegen_expr(
    llvm_t *llvm,
    module_t *mod,
    block_t *operand_block,
    block_t *operation_block,
    expr_t *expr,
    value_t *val) {
  if (operation_block == NULL) operation_block = operand_block;

  switch (expr->kind) {
  case EXPR_PRIMARY: {

    switch (expr->primary.kind) {
    case PRIMARY_INT:
    case PRIMARY_FLOAT:
    case PRIMARY_STRING:
    case PRIMARY_CSTRING:
    case PRIMARY_PRIMITIVE_TYPE:
      return codegen_const_expr(llvm, mod, operand_block, NULL, expr, val);

    case PRIMARY_IDENT: {
      symbol_t *sym = scope_get(&operation_block->scope, expr->primary.string);
      assert(sym);
      assert(sym->value.kind != VALUE_UNDEFINED);

      *val = sym->value;
    } break;
    }

  } break;

  case EXPR_EXPR:
    return codegen_expr(llvm, mod, operand_block, NULL, expr->expr, val);

  case EXPR_INTRIN: {
    switch (expr->intrin.kind) {
    case INTRIN_ASSERT: {
      value_t condition = {0};
      is_expr_true(
          llvm, mod, operand_block, &expr->intrin.params.buf[0], &condition);

      LLVMValueRef fun =
          LLVMGetBasicBlockParent(LLVMGetInsertBlock(mod->builder));
      assert(fun);

      LLVMBasicBlockRef then_bb  = LLVMAppendBasicBlock(fun, "then");
      LLVMBasicBlockRef merge_bb = LLVMAppendBasicBlock(fun, "merge");

      LLVMBuildCondBr(
          mod->builder, load_val(mod, &condition), merge_bb, then_bb);

      // Then
      {
        LLVMPositionBuilderAtEnd(mod->builder, then_bb);
        LLVMBuildUnreachable(mod->builder);
        then_bb = LLVMGetInsertBlock(mod->builder);
      }

      // Merge
      LLVMPositionBuilderAtEnd(mod->builder, merge_bb);
    } break;

    default:
      return codegen_const_expr(llvm, mod, operand_block, NULL, expr, val);
    }
  } break;

  case EXPR_PROC:
    return codegen_const_expr(llvm, mod, operand_block, NULL, expr, val);

  case EXPR_ACCESS: {
    block_t *expr_block = NULL;
    symbol_t *sym       = NULL;
    expr_t *inner = get_access_expr(operand_block, expr, &expr_block, &sym);

    if (inner) {
      codegen_expr(llvm, mod, operand_block, expr_block, inner, &sym->value);
      *val = sym->value;
    }
  } break;

  case EXPR_PROC_CALL: {
    value_t fun_val;
    memset(&fun_val, 0, sizeof(fun_val));

    codegen_expr(
        llvm, mod, operation_block, NULL, expr->proc_call.expr, &fun_val);
    assert(fun_val.kind != VALUE_UNDEFINED);

    LLVMValueRef fun = load_val(mod, &fun_val);
    assert(fun);

    unsigned arg_count = (unsigned)expr->proc_call.params.count;
    LLVMValueRef *args =
        bump_alloc(&llvm->ctx->alloc, sizeof(LLVMValueRef) * arg_count);

    for (size_t i = 0; i < arg_count; i++) {
      value_t v;
      memset(&v, 0, sizeof(v));

      codegen_expr(
          llvm, mod, operand_block, NULL, &expr->proc_call.params.buf[i], &v);
      args[i] = load_val(mod, &v);
    }

    val->kind  = VALUE_TMP_VAR;
    val->value = LLVMBuildCall(mod->builder, fun, args, arg_count, "");
  } break;

  case EXPR_UNARY: {
    value_t right_val;
    memset(&right_val, 0, sizeof(right_val));
    codegen_expr(llvm, mod, operand_block, NULL, expr->right, &right_val);

    switch (expr->unary.kind) {
    case UNOP_DEREF: {
      switch (right_val.kind) {
      case VALUE_LOCAL_VAR:
      case VALUE_GLOBAL_VAR: {
        val->kind  = VALUE_LOCAL_VAR;
        val->value = load_val(mod, &right_val);
      } break;

      case VALUE_CONST: {
        assert(expr->right->type->kind == TYPE_PTR);
        assert(expr->right->type->subtype->kind == TYPE_PRIMITIVE);
        assert(expr->right->type->subtype->prim == PRIM_TYPE_U8);

        // Dereferencing c string literals
        val->kind  = VALUE_TMP_VAR;
        val->value = LLVMBuildLoad(mod->builder, right_val.value, "");
      } break;

      default: {
        // Can't dereference these values
        assert(0);
      } break;
      }
    } break;

    case UNOP_ADDRESS: {
      switch (right_val.kind) {
      case VALUE_LOCAL_VAR:
      case VALUE_GLOBAL_VAR: {
        val->kind  = VALUE_TMP_VAR;
        val->value = right_val.value;
      } break;

      case VALUE_TMP_VAR:
      case VALUE_PROC:
      case VALUE_CONST: {
        value_t alloc_val;
        memset(&alloc_val, 0, sizeof(alloc_val));

        alloc_val.kind  = VALUE_LOCAL_VAR;
        alloc_val.value = LLVMBuildAlloca(
            mod->builder, llvm_type(llvm, expr->right->type), "");
        LLVMBuildStore(mod->builder, right_val.value, alloc_val.value);

        val->kind  = VALUE_TMP_VAR;
        val->value = alloc_val.value;
      } break;

      default: {
        // Can't take the address of these values
        assert(0);
      } break;
      }
    } break;

    case UNOP_NOT: {
      // TODO: unimplemented
    } break;
    }
  } break;

  case EXPR_BINARY: {
    // TODO
  } break;

  case EXPR_BLOCK: {
    codegen_stmts(llvm, mod, &expr->block);
  } break;

  case EXPR_STRUCT:
  case EXPR_IMPORT:
  case EXPR_PROC_PTR:
    return codegen_const_expr(llvm, mod, operand_block, NULL, expr, val);
  }
}

static void
pre_pass_expr(llvm_t *llvm, module_t *mod, expr_t *expr, value_t *val) {
  switch (expr->kind) {
  case EXPR_PROC: {
    proc_t *proc = &expr->proc;

    char *fun_name   = bump_c_str(&llvm->ctx->alloc, proc->name);
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
        return_type = llvm_type(llvm, proc->sig.return_types.buf[0].as_type);
      } else {
        return_type = LLVMVoidType();
      }

      LLVMTypeRef fun_type = LLVMFunctionType(
          return_type,
          param_types,
          proc->sig.params.count,
          proc->sig.flags & PROC_FLAG_VARIADIC);
      fun = LLVMAddFunction(mod->mod, fun_name, fun_type);

      if (proc->sig.flags & PROC_FLAG_EXTERN) {
        LLVMSetLinkage(fun, LLVMExternalLinkage);
      }
    }

    assert(fun);

    assert(val);
    val->kind  = VALUE_PROC;
    val->value = fun;
  } break;

  case EXPR_IMPORT: {
    pre_pass(llvm, mod, &expr->import.ast->block);
  } break;

  default: {
    if (expr->type->kind == TYPE_TYPE) {
      assert(val);
      val->kind = VALUE_TYPE;
      val->type = llvm_type(llvm, expr->as_type);
    }
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
      pre_pass_expr(
          llvm,
          mod,
          inner_expr(&stmt->const_decl.expr),
          &stmt->const_decl.sym->value);
    } break;

    case STMT_USING: {
      pre_pass_expr(llvm, mod, inner_expr(&stmt->expr), NULL);
    } break;

    default: break;
    }
  }
}

static void codegen_stmts(llvm_t *llvm, module_t *mod, block_t *block) {
  For(stmt, block->stmts) {
    switch (stmt->kind) {
    case STMT_CONST_DECL: {
      expr_t *expr = inner_expr(&stmt->const_decl.expr);
      codegen_const_expr(
          llvm, mod, block, NULL, expr, &stmt->const_decl.sym->value);
    } break;

    case STMT_VAR_DECL: {
      var_decl_t *var_decl = &stmt->var_decl;

      switch (var_decl->sym->kind) {
      case SYMBOL_GLOBAL_VAR: {
        LLVMValueRef glob = LLVMAddGlobal(
            mod->mod,
            llvm_type(llvm, var_decl->type),
            bump_c_str(&llvm->ctx->alloc, var_decl->name));

        LLVMSetLinkage(glob, LLVMInternalLinkage);
        LLVMSetGlobalConstant(glob, false);

        if (var_decl->flags & VAR_DECL_HAS_EXPR) {
          value_t const_value;
          memset(&const_value, 0, sizeof(const_value));

          codegen_const_expr(
              llvm, mod, block, NULL, &var_decl->expr, &const_value);
          assert(const_value.value);

          LLVMSetInitializer(glob, load_val(mod, &const_value));
        }

        var_decl->sym->value.kind  = VALUE_GLOBAL_VAR;
        var_decl->sym->value.value = glob;
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
          memset(&value, 0, sizeof(value));

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

      value_t assigned;
      memset(&assigned, 0, sizeof(assigned));
      codegen_expr(llvm, mod, block, NULL, &var_assign->assigned, &assigned);
      assert(
          assigned.kind == VALUE_LOCAL_VAR ||
          assigned.kind == VALUE_GLOBAL_VAR);

      value_t value;
      memset(&value, 0, sizeof(value));

      codegen_expr(llvm, mod, block, NULL, &var_assign->expr, &value);
      assert(value.value);

      LLVMBuildStore(mod->builder, load_val(mod, &value), assigned.value);
    } break;

    case STMT_EXPR: {
      value_t value;
      codegen_expr(llvm, mod, block, NULL, &stmt->expr, &value);
    } break;

    case STMT_RETURN: {
      if (stmt->ret.exprs.count > 0) {
        value_t value;
        memset(&value, 0, sizeof(value));

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

  /* printf("%s\n", LLVMPrintModuleToString(mod.mod)); */

  char *error = NULL;
  if (LLVMVerifyModule(mod.mod, LLVMReturnStatusAction, &error)) {
    printf("Failed to verify module:\n%s\n", error);
    exit(1);
  }

  LLVMExecutionEngineRef engine;
  error = NULL;

  LLVMLinkInMCJIT();
  LLVMInitializeNativeTarget();
  LLVMInitializeNativeAsmPrinter();
  if (LLVMCreateExecutionEngineForModule(&engine, mod.mod, &error) != 0) {
    fprintf(stderr, "failed to create execution engine\n");
    abort();
  }

  if (error) {
    fprintf(stderr, "error: %s\n", error);
    LLVMDisposeMessage(error);
    exit(EXIT_FAILURE);
  }

  void (*main_func)() = (void (*)())LLVMGetFunctionAddress(engine, "main");
  if (main_func) main_func();

  mod_destroy(&mod);

  return result;
}
