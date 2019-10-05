#include "parser.h"
#include "token.h"
#include <assert.h>
#include <stdio.h>

static inline bool is_at_end(parser_t *p) {
  return p->pos >= p->tokens.count || p->tokens.buf[p->pos].type == TOKEN_EOF;
}

static inline token_t *peek(parser_t *p) { return &p->tokens.buf[p->pos]; }
static inline token_t *prev(parser_t *p) {
  if (p->pos > 0)
    return &p->tokens.buf[p->pos - 1];
  else
    return &p->tokens.buf[p->pos];
}

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
        prev(p)->pos,
        "unexpected token, expected '%s' after token",
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

static bool parse_proc(parser_t *p, expr_t *expr) {
  bool res = true;

  expr->kind = EXPR_PROC;

  memset(&expr->proc, 0, sizeof(expr->proc));

  if (!consume(p, TOKEN_PROC)) res = false;

  if (peek(p)->type == TOKEN_MUL) {
    if (!consume(p, TOKEN_MUL)) res = false;
    expr->kind = EXPR_PROC_PTR;
  }

  if (peek(p)->type == TOKEN_INLINE) {
    next(p);
    expr->proc.sig.flags |= PROC_FLAG_INLINE;
  }

  if (peek(p)->type == TOKEN_STRING) {
    token_t *tok = next(p);
    if (strbuf_cmp(tok->string, STR("c"))) {
      expr->proc.sig.conv = PROC_CONV_C;
    } else {
      error(p, tok->pos, "invalid calling convention");
    }
  }

  if (!consume(p, TOKEN_LPAREN)) res = false;

  while (peek(p)->type != TOKEN_RPAREN) {
    // Parse arguments

    if (peek(p)->type == TOKEN_ELLIPSIS) {
      next(p);
      expr->proc.sig.flags |= PROC_FLAG_VARIADIC;
      break;
    }

    var_decl_t param;
    memset(&param, 0, sizeof(param));
    param.flags |= VAR_DECL_HAS_TYPE;

    expr_t tmp_expr;
    if (!parse_expr(p, &tmp_expr)) res = false;

    if (peek(p)->type == TOKEN_COLON) {
      if (!consume(p, TOKEN_COLON)) res = false;

      if (tmp_expr.kind == EXPR_PRIMARY &&
          tmp_expr.primary.kind == PRIMARY_IDENT) {
        param.name = tmp_expr.primary.string;
      } else {
        error(p, tmp_expr.pos, "expected parameter identifier, got expression");
      }

      if (!parse_expr(p, &param.type_expr)) res = false;
    } else {
      param.type_expr = tmp_expr;
    }

    if (res) {
      APPEND(expr->proc.sig.params, param);
    }

    if (peek(p)->type != TOKEN_RPAREN) {
      if (!consume(p, TOKEN_COMMA)) {
        res = false;
        SKIP_TO(TOKEN_RPAREN);
      }
    }
  }

  if (!consume(p, TOKEN_RPAREN)) res = false;

  if (peek(p)->type == TOKEN_ARROW) {
    next(p);

    if (peek(p)->type == TOKEN_LPAREN) {
      while (1) {
        expr_t return_type_expr;
        if (!parse_expr(p, &return_type_expr)) {
          res = false;
          next(p);
          break;
        }

        if (res) APPEND(expr->proc.sig.return_types, return_type_expr);

        if (peek(p)->type == TOKEN_COMMA) {
          if (!consume(p, TOKEN_COMMA)) res = false;
        } else {
          break;
        }
      }

      if (!consume(p, TOKEN_RPAREN)) res = false;
    } else {
      expr_t return_type_expr;
      if (!parse_expr(p, &return_type_expr)) {
        res = false;
        next(p);
      }

      if (res) APPEND(expr->proc.sig.return_types, return_type_expr);
    }
  }

  if (peek(p)->type == TOKEN_LCURLY) {
    if (!consume(p, TOKEN_LCURLY)) res = false;
  } else {
    expr->proc.sig.flags |= PROC_FLAG_NO_BODY;
  }

  if (!(expr->proc.sig.flags & PROC_FLAG_NO_BODY) && expr->kind == EXPR_PROC) {
    while (peek(p)->type != TOKEN_RCURLY && !is_at_end(p)) {
      stmt_t stmt;
      memset(&stmt, 0, sizeof(stmt));
      if (parse_stmt(p, &stmt)) {
        APPEND(expr->proc.block.stmts, stmt);
      } else {
        res = false;
      }
    }

    if (!consume(p, TOKEN_RCURLY)) res = false;
  }

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

  case TOKEN_TRUE: {
    next(p);
    primary->kind    = PRIMARY_BOOL;
    primary->boolean = true;
  } break;

  case TOKEN_FALSE: {
    next(p);
    primary->kind    = PRIMARY_BOOL;
    primary->boolean = false;
  } break;

  case TOKEN_NULL: {
    next(p);
    primary->kind = PRIMARY_NULL;
  } break;

  case TOKEN_IDENT: {
    next(p);

    primary->kind   = PRIMARY_IDENT;
    primary->string = tok->string;
  } break;

  case TOKEN_STRING: {
    next(p);

    primary->kind   = PRIMARY_STRING;
    primary->string = tok->string;

    while (peek(p)->type == TOKEN_STRING) {
      token_t *t = next(p);
      strbuf_t new_str;
      new_str.count = primary->string.count + t->string.count;
      new_str.cap   = new_str.count;
      new_str.buf   = bump_alloc(&p->ctx->alloc, new_str.cap);
      memcpy(new_str.buf, primary->string.buf, primary->string.count);
      memcpy(
          &new_str.buf[primary->string.count], t->string.buf, t->string.count);
      primary->string = new_str;
    }
  } break;

  case TOKEN_CSTRING: {
    next(p);

    primary->kind   = PRIMARY_CSTRING;
    primary->string = tok->string;

    while (peek(p)->type == TOKEN_STRING) {
      assert(primary->string.buf[primary->string.count - 1] == '\0');

      token_t *t = next(p);
      strbuf_t new_str;
      new_str.count = primary->string.count + t->string.count;
      new_str.cap   = new_str.count;
      new_str.buf   = bump_alloc(&p->ctx->alloc, new_str.cap);
      memcpy(new_str.buf, primary->string.buf, primary->string.count - 1);
      memcpy(
          &new_str.buf[primary->string.count - 1],
          t->string.buf,
          t->string.count + 1);
      new_str.buf[new_str.count - 1] = '\0';
      primary->string                = new_str;
    }
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
    memset(right, 0, sizeof(*right));
    right->pos = peek(p)->pos;
    if (!parse_access(p, right)) res = false;
    right->pos.len = peek(p)->pos.offset - right->pos.offset;

    expr->kind         = EXPR_ACCESS;
    expr->access.left  = left;
    expr->access.right = right;
  }

  return res;
}

static bool parse_unary(parser_t *p, expr_t *expr) {
  bool res = true;

  switch (peek(p)->type) {
  case TOKEN_MUL: {
    // Dereference
    next(p);

    expr->kind       = EXPR_UNARY;
    expr->unary.kind = UNOP_DEREF;

    expr->right = bump_alloc(&p->ctx->alloc, sizeof(expr_t));
    memset(expr->right, 0, sizeof(*expr->right));
    if (!parse_expr(p, expr->right)) res = false;
  } break;

  case TOKEN_AMPERSAND: {
    // Address
    next(p);

    expr->kind       = EXPR_UNARY;
    expr->unary.kind = UNOP_ADDRESS;

    expr->right = bump_alloc(&p->ctx->alloc, sizeof(expr_t));
    memset(expr->right, 0, sizeof(*expr->right));
    if (!parse_expr(p, expr->right)) res = false;
  } break;

  default: {
    res |= parse_access(p, expr);
  } break;
  }

  return res;
}

static bool parse_struct_proc_import(parser_t *p, expr_t *expr) {
  switch (peek(p)->type) {
  case TOKEN_PROC: {
    return parse_proc(p, expr);
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
    return parse_unary(p, expr);
  } break;
  }
}

static bool parse_intrinsic(parser_t *p, expr_t *expr) {
  bool res = true;

  switch (peek(p)->type) {
  case TOKEN_INTRIN_SIZEOF: expr->intrin.kind = INTRIN_SIZEOF; break;
  case TOKEN_INTRIN_ASSERT: expr->intrin.kind = INTRIN_ASSERT; break;

  default: return parse_struct_proc_import(p, expr);
  }

  expr->kind = EXPR_INTRIN;

  switch (expr->intrin.kind) {
  case INTRIN_SIZEOF:
  case INTRIN_ASSERT: {
    next(p);

    if (!consume(p, TOKEN_LPAREN)) res = false;

    memset(&expr->intrin.params, 0, sizeof(expr->intrin.params));

    expr_t param;
    memset(&param, 0, sizeof(param));
    if (!parse_expr(p, &param))
      res = false;
    else
      APPEND(expr->intrin.params, param);

    if (!consume(p, TOKEN_RPAREN)) res = false;
  } break;
  }

  return res;
}

static bool parse_array_type(parser_t *p, expr_t *expr) {
  bool res = true;

  switch (peek(p)->type) {
  case TOKEN_LBRACK: {
    next(p);

    expr->kind = EXPR_ARRAY_TYPE;

    if (peek(p)->type != TOKEN_RBRACK) {
      expr->array.size_expr = bump_alloc(&p->ctx->alloc, sizeof(expr_t));
      memset(expr->array.size_expr, 0, sizeof(*expr->array.size_expr));
      if (!parse_expr(p, expr->array.size_expr)) res = false;
    }

    if (!consume(p, TOKEN_RBRACK)) res = false;

    expr->array.sub_expr = bump_alloc(&p->ctx->alloc, sizeof(expr_t));
    memset(expr->array.sub_expr, 0, sizeof(*expr->array.sub_expr));
    if (!parse_array_type(p, expr->array.sub_expr)) res = false;
  } break;

  default: return parse_intrinsic(p, expr);
  }

  return res;
}

static bool parse_array_literal(parser_t *p, expr_t *expr) {
  bool res = true;

  if (parse_array_type(p, expr)) {
    if (expr->kind == EXPR_ARRAY_TYPE) {
      if (peek(p)->type == TOKEN_LCURLY) {
        next(p);
        expr->kind = EXPR_ARRAY_LITERAL;

        while (peek(p)->type != TOKEN_RCURLY) {
          expr_t elem;
          memset(&elem, 0, sizeof(elem));
          if (!parse_expr(p, &elem))
            res = false;
          else
            APPEND(expr->array.elems, elem);

          if (peek(p)->type != TOKEN_RCURLY) {
            if (!consume(p, TOKEN_COMMA)) res = false;
          }
        }

        if (!consume(p, TOKEN_RCURLY)) res = false;
      }
    }
  } else {
    res = false;
  }

  return res;
}

static bool parse_subscript_expr(parser_t *p, expr_t *expr) {
  bool res = true;

  if (!parse_array_literal(p, expr)) res = false;

  while (peek(p)->type == TOKEN_LBRACK) {
    next(p);

    expr_t *left = bump_alloc(&p->ctx->alloc, sizeof(expr_t));
    *left        = *expr;

    expr->kind = EXPR_SUBSCRIPT;
    expr->left = left;

    expr_t *right = bump_alloc(&p->ctx->alloc, sizeof(expr_t));
    memset(right, 0, sizeof(*right));
    if (!parse_expr(p, right)) res = false;

    expr->right = right;

    if (!consume(p, TOKEN_RBRACK)) res = false;
  }

  return res;
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
    return parse_subscript_expr(p, expr);
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
    memset(&type_expr, 0, sizeof(type_expr));

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
      case EXPR_BLOCK: break;

      case EXPR_PROC: {
        if (!(stmt->const_decl.expr.proc.sig.flags & PROC_FLAG_NO_BODY)) break;
      };
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
  case TOKEN_RETURN: {
    next(p);

    stmt->kind = STMT_RETURN;

    bool first = true;
    while (peek(p)->type != TOKEN_SEMI) {
      if (!first) {
        if (!consume(p, TOKEN_COMMA)) res = false;
      }

      expr_t expr;
      if (!parse_expr(p, &expr))
        res = false;
      else
        APPEND(stmt->ret.exprs, expr);
      first = false;
    }

    if (!consume(p, TOKEN_SEMI)) res = false;
  } break;
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
