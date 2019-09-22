#include "parser.h"
#include "token.h"
#include <assert.h>
#include <stdio.h>

static inline bool is_at_end(parser_t *p) {
  return p->pos >= p->tokens.count || p->tokens.buf[p->pos].type == TOKEN_EOF;
}

static inline token_t *peek(parser_t *p) { return &p->tokens.buf[p->pos]; }

static inline token_t *look(parser_t *p, size_t offset) {
  if (p->pos + offset >= p->tokens.count) return &p->tokens.buf[p->pos];
  return &p->tokens.buf[p->pos + offset];
}

static inline token_t *next(parser_t *p) {
  if (!is_at_end(p)) p->pos++;
  return &p->tokens.buf[p->pos - 1];
}

static bool error(parser_t *p, pos_t pos, const char *fmt, ...) {
  sb_reset(&p->ctx->sb);

  va_list vl;
  va_start(vl, fmt);
  sb_vsprintf(&p->ctx->sb, fmt, vl);
  va_end(vl);

  error_t err = {
      .pos = pos,
      .msg = bump_strdup(&p->ctx->alloc, sb_build(&p->ctx->sb)),
  };

  APPEND(p->errors, err);

  return false;
}

static inline token_t *consume(parser_t *p, token_type_t tok_type) {
  if (peek(p)->type != tok_type) {
    error(
        p,
        peek(p)->pos,
        "unexpected token, expected '%s'",
        TOKEN_STRINGS[tok_type]);
    return NULL;
  } else {
    return next(p);
  }
}

#define SKIP_TO(tok_type)                                                      \
  while (peek(p)->type != tok_type) {                                          \
    if (is_at_end(p)) return res;                                              \
    next(p);                                                                   \
  }

/* static inline void push_checkpoint(parser_t *p) { */
/*   APPEND(p->checkpoints, p->pos); */
/* } */

/* static inline void pop_checkpoint(parser_t *p, bool success) { */
/*   assert(p->checkpoints.count > 0); */

/*   if (!success) p->pos = p->checkpoints.buf[p->checkpoints.count - 1]; */

/*   p->checkpoints.count--; */
/* } */

static bool parse_stmt(parser_t *p, stmt_t *stmt);
static bool parse_expr(parser_t *p, expr_t *expr);

static bool parse_import(parser_t *p, import_t *import) {
  bool res = true;

  if (!consume(p, TOKEN_IMPORT)) res = false;

  token_t *tok = consume(p, TOKEN_STRING);

  if (tok)
    import->path = tok->string;
  else
    res = false;

  return res;
}

static bool parse_struct(parser_t *p, struct_t *str) {
  bool res = true;

  if (!consume(p, TOKEN_STRUCT)) res = false;
  if (!consume(p, TOKEN_LCURLY)) res = false;
  if (!consume(p, TOKEN_RCURLY)) res = false;

  return res;
}

static bool parse_proc(parser_t *p, proc_t *proc) {
  bool res = true;

  memset(proc, 0, sizeof(*proc));

  if (!consume(p, TOKEN_PROC)) res = false;

  if (peek(p)->type == TOKEN_EXTERN) {
    next(p);
    proc->sig.flags |= PROC_FLAG_EXTERN;
  }

  if (peek(p)->type == TOKEN_INLINE) {
    next(p);
    proc->sig.flags |= PROC_FLAG_INLINE;
  }

  if (!consume(p, TOKEN_LPAREN)) res = false;

  while (peek(p)->type != TOKEN_RPAREN) {
    // Parse arguments

    var_decl_t param;
    memset(&param, 0, sizeof(param));
    param.flags |= VAR_DECL_HAS_TYPE;

    token_t *name = consume(p, TOKEN_IDENT);
    if (!name) {
      res = false;
      SKIP_TO(TOKEN_RPAREN);
      break;
    }

    param.name = name->string;

    if (!consume(p, TOKEN_COLON)) res = false;

    if (!parse_expr(p, &param.type_expr)) res = false;

    if (res) {
      APPEND(proc->sig.params, param);
    }

    if (peek(p)->type != TOKEN_RPAREN) {
      if (!consume(p, TOKEN_COMMA)) {
        res = false;
        SKIP_TO(TOKEN_RPAREN);
      }
    }
  }

  if (!consume(p, TOKEN_RPAREN)) res = false;

  while (peek(p)->type != TOKEN_LCURLY) {
    expr_t return_type;
    if (!parse_expr(p, &return_type)) {
      res = false;
      SKIP_TO(TOKEN_LCURLY);
      break;
    }

    if (res) {
      APPEND(proc->sig.return_types, return_type);
    }

    if (peek(p)->type != TOKEN_LCURLY) {
      if (!consume(p, TOKEN_COMMA)) {
        res = false;
        SKIP_TO(TOKEN_LCURLY);
        break;
      }
    }
  }

  if (!consume(p, TOKEN_LCURLY)) res = false;

  while (peek(p)->type != TOKEN_RCURLY && !is_at_end(p)) {
    stmt_t stmt;
    memset(&stmt, 0, sizeof(stmt));
    if (parse_stmt(p, &stmt)) {
      APPEND(proc->block.stmts, stmt);
    } else {
      res = false;
    }
  }

  if (!consume(p, TOKEN_RCURLY)) res = false;

  return res;
}

static bool parse_primary(parser_t *p, primary_expr_t *primary) {
  bool res = true;

  token_t *tok = peek(p);
  switch (tok->type) {
  case TOKEN_FLOAT: {
    next(p);
    primary->kind = PRIMARY_FLOAT;
    primary->f64  = tok->f64;
  } break;

  case TOKEN_INT: {
    next(p);
    primary->kind = PRIMARY_INT;
    primary->i64  = tok->i64;
  } break;

  case TOKEN_IDENT: {
    next(p);

    primary->kind  = PRIMARY_IDENT;
    primary->ident = tok->string;
  } break;

  case TOKEN_I8:
  case TOKEN_I16:
  case TOKEN_I32:
  case TOKEN_I64:
  case TOKEN_U8:
  case TOKEN_U16:
  case TOKEN_U32:
  case TOKEN_U64:
  case TOKEN_F32:
  case TOKEN_F64:
  case TOKEN_BOOL:
  case TOKEN_VOID: {
    next(p);

    primary->kind = PRIMARY_PRIMITIVE_TYPE;

    switch (tok->type) {
    case TOKEN_I8: primary->prim_type = PRIM_TYPE_I8; break;
    case TOKEN_I16: primary->prim_type = PRIM_TYPE_I16; break;
    case TOKEN_I32: primary->prim_type = PRIM_TYPE_I32; break;
    case TOKEN_I64: primary->prim_type = PRIM_TYPE_I64; break;
    case TOKEN_U8: primary->prim_type = PRIM_TYPE_U8; break;
    case TOKEN_U16: primary->prim_type = PRIM_TYPE_U16; break;
    case TOKEN_U32: primary->prim_type = PRIM_TYPE_U32; break;
    case TOKEN_U64: primary->prim_type = PRIM_TYPE_U64; break;
    case TOKEN_F32: primary->prim_type = PRIM_TYPE_F32; break;
    case TOKEN_F64: primary->prim_type = PRIM_TYPE_F64; break;
    case TOKEN_BOOL: primary->prim_type = PRIM_TYPE_BOOL; break;
    case TOKEN_VOID: primary->prim_type = PRIM_TYPE_VOID; break;
    default: assert(0);
    }

  } break;

  default: {
    next(p);
    res = error(p, tok->pos, "unexpected token, expected primary");
  } break;
  }

  return res;
}

static bool parse_expr_expr(parser_t *p, expr_t *expr) {
  bool res = true;

  expr->pos = peek(p)->pos;

  if (peek(p)->type == TOKEN_LPAREN) {
    next(p);

    expr->kind = EXPR_EXPR;
    expr->expr = bump_alloc(&p->ctx->alloc, sizeof(expr_t));
    if (!parse_expr(p, expr->expr)) res = false;

    if (!consume(p, TOKEN_RPAREN)) res = false;
  } else {
    expr->kind = EXPR_PRIMARY;
    if (!parse_primary(p, &expr->primary)) res = false;
  }

  expr->pos.len = peek(p)->pos.offset - expr->pos.offset;

  return res;
}

static bool parse_proc_call(parser_t *p, expr_t *expr) {
  bool res = true;

  if (!parse_expr_expr(p, expr)) res = false;

  while (peek(p)->type == TOKEN_LPAREN) {
    next(p);

    expr_t *new_expr = bump_alloc(&p->ctx->alloc, sizeof(expr_t));
    *new_expr        = *expr;

    expr->kind           = EXPR_PROC_CALL;
    expr->proc_call.expr = new_expr;
    memset(&expr->proc_call.params, 0, sizeof(expr->proc_call.params));

    while (peek(p)->type != TOKEN_RPAREN && !is_at_end(p)) {
      expr_t param;
      memset(&param, 0, sizeof(param));
      if (!parse_expr(p, &param))
        res = false;
      else
        APPEND(expr->proc_call.params, param);

      if (peek(p)->type != TOKEN_RPAREN) {
        if (!consume(p, TOKEN_COMMA)) res = false;
      }
    }

    if (!consume(p, TOKEN_RPAREN)) res = false;

    expr->pos.len = peek(p)->pos.offset - expr->pos.offset;
  }

  return res;
}

static bool parse_access(parser_t *p, expr_t *expr) {
  bool res = true;

  if (!parse_proc_call(p, expr)) res = false;

  if (peek(p)->type == TOKEN_DOT) {
    next(p);

    expr_t *left = bump_alloc(&p->ctx->alloc, sizeof(expr_t));
    *left        = *expr;

    expr_t *right = bump_alloc(&p->ctx->alloc, sizeof(expr_t));
    right->pos    = peek(p)->pos;
    if (!parse_access(p, right)) res = false;
    right->pos.len = peek(p)->pos.offset - right->pos.offset;

    expr->kind         = EXPR_ACCESS;
    expr->access.left  = left;
    expr->access.right = right;
  }

  return res;
}

static bool parse_struct_proc_import(parser_t *p, expr_t *expr) {
  switch (peek(p)->type) {
  case TOKEN_PROC: {
    expr->kind = EXPR_PROC;
    return parse_proc(p, &expr->proc);
  } break;
  case TOKEN_STRUCT: {
    expr->kind = EXPR_STRUCT;
    return parse_struct(p, &expr->str);
  } break;
  case TOKEN_IMPORT: {
    expr->kind = EXPR_IMPORT;
    return parse_import(p, &expr->import);
  } break;
  default: {
    return parse_access(p, expr);
  } break;
  }
}

static bool parse_block_expr(parser_t *p, expr_t *expr) {
  bool res = true;

  switch (peek(p)->type) {
  case TOKEN_LCURLY: {
    next(p);

    memset(&expr->block, 0, sizeof(expr->block));

    expr->kind = EXPR_BLOCK;
    while (peek(p)->type != TOKEN_RCURLY && !is_at_end(p)) {
      stmt_t stmt;
      memset(&stmt, 0, sizeof(stmt));
      if (parse_stmt(p, &stmt)) {
        APPEND(expr->block.stmts, stmt);
      } else {
        res = false;
      }
    }

    if (!consume(p, TOKEN_RCURLY)) res = false;
  } break;

  default: {
    return parse_struct_proc_import(p, expr);
  } break;
  }

  return res;
}

static bool parse_expr(parser_t *p, expr_t *expr) {
  memset(expr, 0, sizeof(*expr));
  expr->pos     = peek(p)->pos;
  bool res      = parse_block_expr(p, expr);
  expr->pos.len = peek(p)->pos.offset - expr->pos.offset;
  return res;
}

static bool parse_decl_or_assign_or_stmt_expr(parser_t *p, stmt_t *stmt) {
  bool res = true;

  if (look(p, 0)->type == TOKEN_IDENT && look(p, 1)->type == TOKEN_COLON) {
    // Declaration
    token_t *name_tok = consume(p, TOKEN_IDENT);

    if (!consume(p, TOKEN_COLON)) res = false;

    expr_t type_expr;
    bool got_type = false;

    token_t *tok = peek(p);
    switch (tok->type) {
    case TOKEN_ASSIGN:
    case TOKEN_COLON: next(p); break;
    default: {
      if (!parse_expr(p, &type_expr))
        res = false;
      else {
        got_type = true;
      }

      tok = peek(p);
      switch (tok->type) {
      case TOKEN_ASSIGN:
      case TOKEN_COLON:
      case TOKEN_SEMI: next(p); break;
      default: {
        return error(p, tok->pos, "unexpected token, expected ':' or '='");
      } break;
      }
    } break;
    }

    if (tok->type == TOKEN_SEMI) {
      // Variable declaration
      stmt->kind          = STMT_VAR_DECL;
      stmt->var_decl.name = name_tok->string;

      stmt->var_decl.type_expr = type_expr;
      if (got_type) stmt->var_decl.flags |= VAR_DECL_HAS_TYPE;

      return res;
    }

    expr_t expr;
    if (!parse_expr(p, &expr)) res = false;

    switch (tok->type) {
    case TOKEN_ASSIGN:
    case TOKEN_SEMI: {
      // Variable declaration
      stmt->kind          = STMT_VAR_DECL;
      stmt->var_decl.name = name_tok->string;

      stmt->var_decl.flags |= VAR_DECL_HAS_EXPR;

      stmt->var_decl.expr      = expr;
      stmt->var_decl.type_expr = type_expr;
      if (got_type) stmt->var_decl.flags |= VAR_DECL_HAS_TYPE;

      if (!consume(p, TOKEN_SEMI)) {
        res = false;
        while (next(p)->type != TOKEN_SEMI) {
          if (is_at_end(p)) return res;
        }
      }
    } break;
    case TOKEN_COLON: {
      // Constant declaration
      stmt->kind            = STMT_CONST_DECL;
      stmt->const_decl.name = name_tok->string;

      stmt->const_decl.expr      = expr;
      stmt->const_decl.type_expr = type_expr;
      stmt->const_decl.typed     = got_type;

      switch (stmt->const_decl.expr.kind) {
      case EXPR_STRUCT:
      case EXPR_BLOCK:
      case EXPR_PROC: break;
      default: {
        if (!consume(p, TOKEN_SEMI)) {
          res = false;
          while (next(p)->type != TOKEN_SEMI) {
            if (is_at_end(p)) return res;
          }
        }
      } break;
      }

    } break;
    default: {
      consume(p, TOKEN_SEMI);
    } break;
    }

  } else {
    expr_t expr;
    memset(&expr, 0, sizeof(expr));
    if (!parse_expr(p, &expr)) res = false;

    switch (peek(p)->type) {
    case TOKEN_ASSIGN: {
      // Variable assignment
      next(p);

      stmt->kind                = STMT_VAR_ASSIGN;
      stmt->var_assign.assigned = expr;
      if (!parse_expr(p, &stmt->var_assign.expr)) res = false;

      if (!consume(p, TOKEN_SEMI)) res = false;
    } break;
    default: {
      // Expression statement
      stmt->kind = STMT_EXPR;
      stmt->expr = expr;

      switch (stmt->expr.kind) {
      case EXPR_PROC:
      case EXPR_STRUCT:
      case EXPR_BLOCK: break;
      default: {
        if (!consume(p, TOKEN_SEMI)) res = false;
      } break;
      }
    } break;
    }
  }

  return res;
}

static bool parse_stmt(parser_t *p, stmt_t *stmt) {
  bool res     = true;
  token_t *tok = peek(p);

  memset(stmt, 0, sizeof(*stmt));

  stmt->pos = tok->pos;

  switch (tok->type) {
  case TOKEN_USING: {
    next(p);

    stmt->kind = STMT_USING;
    if (!parse_expr(p, &stmt->expr)) res = false;

    switch (stmt->expr.kind) {
    case EXPR_STRUCT:
    case EXPR_PROC:
    case EXPR_BLOCK: break;
    default: {
      if (!consume(p, TOKEN_SEMI)) res = false;
    } break;
    }
  } break;
  default: {
    if (!parse_decl_or_assign_or_stmt_expr(p, stmt)) res = false;
  } break;
  }

  stmt->pos.len = peek(p)->pos.offset - stmt->pos.offset;

  return res;
}

void parser_init(parser_t *p, ctx_t *ctx) {
  memset(p, 0, sizeof(*p));
  p->ctx = ctx;
}

error_set_t
parser_parse(parser_t *p, file_t *file, token_slice_t tokens, ast_t *ast) {
  memset(&p->errors, 0, sizeof(p->errors));
  memset(&p->checkpoints, 0, sizeof(p->checkpoints));
  p->file   = file;
  p->tokens = tokens;
  p->ast    = ast;

  ast->file = file;

  while (!is_at_end(p)) {
    stmt_t stmt;
    memset(&stmt, 0, sizeof(stmt));
    if (parse_stmt(p, &stmt)) APPEND(ast->block.stmts, stmt);
  }

  return (error_set_t){
      .type   = RESULT_PARSER,
      .errors = p->errors,
  };
}
