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
  if (!consume(p, TOKEN_LPAREN)) res = false;
  
  if (!consume(p, TOKEN_RPAREN)) res = false;

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
    ident_t ident;
    ident.string = tok->string;
    ident.child  = NULL;

    ident_t *last_ident = &ident;

    while (peek(p)->type == TOKEN_DOT && !is_at_end(p)) {
      next(p);

      tok = consume(p, TOKEN_IDENT);
      if (!tok) {
        res = false;
        break;
      }

      last_ident->child         = bump_alloc(&p->ctx->alloc, sizeof(ident_t));
      last_ident->child->string = tok->string;
      last_ident->child->child  = NULL;

      last_ident = last_ident->child;
    }

    primary->kind  = PRIMARY_IDENT;
    primary->ident = ident;

    if (peek(p)->type == TOKEN_LPAREN) {
      primary->kind            = PRIMARY_PROC_CALL;
      primary->proc_call.ident = ident;

      // TODO: proc call
      if (!consume(p, TOKEN_LPAREN)) {
        res = false;
        SKIP_TO(TOKEN_SEMI);
      }

      if (!consume(p, TOKEN_RPAREN)) {
        res = false;
        SKIP_TO(TOKEN_SEMI);
      }
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
  case TOKEN_BOOL:
  case TOKEN_VOID: {
    next(p);

    primary->kind = PRIMARY_PRIMITIVE_TYPE;

    switch (tok->type) {
    case TOKEN_I8: primary->prim_type = PRIMITIVE_TYPE_I8; break;
    case TOKEN_I16: primary->prim_type = PRIMITIVE_TYPE_I16; break;
    case TOKEN_I32: primary->prim_type = PRIMITIVE_TYPE_I32; break;
    case TOKEN_I64: primary->prim_type = PRIMITIVE_TYPE_I64; break;
    case TOKEN_U8: primary->prim_type = PRIMITIVE_TYPE_U8; break;
    case TOKEN_U16: primary->prim_type = PRIMITIVE_TYPE_U16; break;
    case TOKEN_U32: primary->prim_type = PRIMITIVE_TYPE_U32; break;
    case TOKEN_U64: primary->prim_type = PRIMITIVE_TYPE_U64; break;
    case TOKEN_BOOL: primary->prim_type = PRIMITIVE_TYPE_BOOL; break;
    case TOKEN_VOID: primary->prim_type = PRIMITIVE_TYPE_VOID; break;
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
    expr->kind = EXPR_PRIMARY;
    return parse_primary(p, &expr->primary);
  } break;
  }
}

static bool parse_expr(parser_t *p, expr_t *expr) {
  memset(expr, 0, sizeof(*expr));
  expr->pos     = peek(p)->pos;
  bool res      = parse_struct_proc_import(p, expr);
  expr->pos.len = peek(p)->pos.offset - expr->pos.offset;
  return res;
}

static bool parse_decl_or_assign(parser_t *p, stmt_t *stmt) {
  bool res = true;

  if (look(p, 0)->type == TOKEN_IDENT && look(p, 1)->type == TOKEN_COLON) {
    // Declaration
    token_t *name_tok = consume(p, TOKEN_IDENT);
    if (!name_tok) res = false;

    if (!consume(p, TOKEN_COLON)) res = false;

    token_t *tok = peek(p);
    switch (tok->type) {
    case TOKEN_ASSIGN:
    case TOKEN_COLON: next(p); break;
    default: {
      expr_t type_expr;
      if (!parse_expr(p, &type_expr)) res = false;

      switch (peek(p)->type) {
      case TOKEN_ASSIGN:
      case TOKEN_COLON: next(p); break;
      default: {
        return error(p, tok->pos, "unexpected token, expected ':' or '='");
      } break;
      }
    } break;
    }

    expr_t expr;
    if (!parse_expr(p, &expr)) res = false;

    switch (tok->type) {
    case TOKEN_ASSIGN: {
      // Variable declaration
      stmt->kind = STMT_VAR_DECL;
      if (name_tok) stmt->var_decl.name = name_tok->string;

      stmt->var_decl.expr = expr;

      if (!consume(p, TOKEN_SEMI)) {
        res = false;
        while (next(p)->type != TOKEN_SEMI) {
          if (is_at_end(p)) return res;
        }
      }
    } break;
    case TOKEN_COLON: {
      // Constant declaration
      stmt->kind = STMT_CONST_DECL;
      if (name_tok) stmt->const_decl.name = name_tok->string;

      stmt->const_decl.expr = expr;

      switch (stmt->const_decl.expr.kind) {
      case EXPR_STRUCT:
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
    stmt->kind = STMT_VAR_ASSIGN;

    if (!parse_expr(p, &stmt->var_assign.assigned)) res = false;

    if (!consume(p, TOKEN_ASSIGN)) res = false;

    if (!parse_expr(p, &stmt->var_assign.expr)) res = false;

    if (!consume(p, TOKEN_SEMI)) res = false;
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
    case EXPR_PROC: break;
    default: {
      if (!consume(p, TOKEN_SEMI)) res = false;
    } break;
    }
  } break;
  default: {
    if (!parse_decl_or_assign(p, stmt)) res = false;
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
