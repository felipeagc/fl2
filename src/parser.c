#include "parser.h"
#include "token.h"
#include <stdio.h>

static inline bool is_at_end(parser_t *p) { return p->pos >= p->tokens.count; }

static inline token_t *peek(parser_t *p) { return &p->tokens.buf[p->pos]; }

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

static bool parse_stmt(parser_t *p, stmt_t *stmt);

static bool parse_import(parser_t *p) {
  bool res = true;

  decl_t decl = {0};
  decl.kind   = DECL_IMPORT;

  token_t *tok = next(p);
  if (tok->type != TOKEN_IMPORT) res = error(p, tok->pos, "expected 'import'");

  tok = next(p);
  if (tok->type != TOKEN_IDENT)
    res = error(p, tok->pos, "expected import identifier");

  decl.name = tok->string;

  tok = next(p);
  if (tok->type != TOKEN_STRING)
    res = error(p, tok->pos, "expected import path");

  decl.import.path = tok->string;

  tok = next(p);
  if (tok->type != TOKEN_SEMI) res = error(p, tok->pos, "expected ';'");

  APPEND(p->ast->decls, decl);

  return res;
}

static bool parse_struct(parser_t *p, struct_t *str) {
  bool res = true;

  token_t *tok = next(p);
  if (tok->type != TOKEN_STRUCT) res = error(p, tok->pos, "expected 'struct'");

  tok = next(p);
  if (tok->type != TOKEN_LCURLY) res = error(p, tok->pos, "expected '{'");

  tok = next(p);
  if (tok->type != TOKEN_RCURLY) res = error(p, tok->pos, "expected '}'");

  return res;
}

static bool parse_proc(parser_t *p, proc_t *proc) {
  bool res = true;

  memset(proc, 0, sizeof(*proc));

  token_t *tok = next(p);
  if (tok->type != TOKEN_PROC) res = error(p, tok->pos, "expected 'proc'");

  tok = next(p);
  if (tok->type != TOKEN_LPAREN) res = error(p, tok->pos, "expected '('");

  tok = next(p);
  if (tok->type != TOKEN_RPAREN) res = error(p, tok->pos, "expected '{'");

  tok = next(p);
  if (tok->type != TOKEN_LCURLY) res = error(p, tok->pos, "expected '{'");

  while (peek(p)->type != TOKEN_RCURLY) {
    stmt_t stmt = {0};
    if (parse_stmt(p, &stmt)) {
      APPEND(proc->stmts, stmt);
    } else {
      res = false;
    }
  }

  tok = next(p);
  if (tok->type != TOKEN_RCURLY) res = error(p, tok->pos, "expected '}'");

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
  case TOKEN_STRUCT: {
    primary->kind = PRIMARY_STRUCT;
    TRY(parse_struct(p, &primary->str));
  } break;
  case TOKEN_PROC: {
    primary->kind = PRIMARY_PROC;
    TRY(parse_proc(p, &primary->proc));
  } break;
  case TOKEN_IDENT: {
    next(p);
    ident_t ident;
    ident.string = tok->string;
    ident.child  = NULL;

    ident_t *last_ident = &ident;

    while (peek(p)->type == TOKEN_DOT) {
      next(p);

      tok = next(p);
      if (tok->type != TOKEN_IDENT) {
        res = error(p, tok->pos, "unexpected token, expected identifier");
        break;
      }

      last_ident->child         = bump_alloc(&p->ctx->alloc, sizeof(ident_t));
      last_ident->child->string = tok->string;
      last_ident->child->child  = NULL;

      last_ident = last_ident->child;
    }

    if (peek(p)->type == TOKEN_LPAREN) {
      next(p);

      tok = next(p);
      if (tok->type != TOKEN_RPAREN) res = error(p, tok->pos, "expected ')'");
    } else {
      primary->kind  = PRIMARY_IDENT;
      primary->ident = ident;
    }
  } break;
  default: {
    res = error(p, tok->pos, "unexpected token, expected primary");
  } break;
  }

  return res;
}

static bool parse_expr(parser_t *p, expr_t *expr) {
  expr->kind = EXPR_PRIMARY;
  return parse_primary(p, &expr->primary);
}

static bool parse_var_decl_or_assign(parser_t *p, stmt_t *stmt) {
  bool res = true;

  expr_t assigned;
  TRY(parse_expr(p, &assigned));

  token_t *tok = next(p);
  switch (tok->type) {
  case TOKEN_COLON: {
    stmt->kind              = STMT_VAR_DECL;
    stmt->var_decl.assigned = assigned;

    tok = next(p);
    if (tok->type != TOKEN_ASSIGN) res = error(p, tok->pos, "expected '='");

    TRY(parse_expr(p, &stmt->var_decl.expr));

    tok = next(p);
    if (tok->type != TOKEN_SEMI) res = error(p, tok->pos, "expected ';'");
  } break;
  case TOKEN_ASSIGN: {
    stmt->kind                = STMT_VAR_ASSIGN;
    stmt->var_assign.assigned = assigned;

  } break;
  default: {
    res = error(
        p,
        tok->pos,
        "unexpected token, expected variable declaration or assignment");
  } break;
  }

  return res;
}

static bool parse_stmt(parser_t *p, stmt_t *stmt) {
  bool res     = true;
  token_t *tok = peek(p);

  switch (tok->type) {
  case TOKEN_IDENT: {
    TRY(parse_var_decl_or_assign(p, stmt));
  } break;
  default: {
    next(p);
    res = error(p, tok->pos, "unexpected token, expected statement");
  } break;
  }

  return res;
}

static bool parse_decl(parser_t *p) {
  bool res = true;

  token_t *tok = peek(p);

  switch (tok->type) {
  case TOKEN_IMPORT: {
    TRY(parse_import(p));
  } break;
  case TOKEN_IDENT: {
    tok         = next(p);
    decl_t decl = {0};
    decl.name   = tok->string;
    decl.kind   = DECL_EXPR;

    tok = next(p);
    if (tok->type != TOKEN_COLON) res = error(p, tok->pos, "expected ':'");

    tok = next(p);
    if (tok->type != TOKEN_COLON) res = error(p, tok->pos, "expected ':'");

    TRY(parse_expr(p, &decl.expr));
    APPEND(p->ast->decls, decl);
  } break;
  default: {
    next(p);
    res = error(p, tok->pos, "unexpected token, expected declaration");
  } break;
  }

  return res;
}

void parser_init(parser_t *p, ctx_t *ctx) {
  memset(p, 0, sizeof(*p));
  p->ctx = ctx;
}

error_set_t
parser_parse(parser_t *p, file_t *file, token_slice_t tokens, ast_t *ast) {
  memset(&p->errors, 0, sizeof(p->errors));
  p->file   = file;
  p->tokens = tokens;
  p->ast    = ast;

  while (!is_at_end(p)) {
    parse_decl(p);
  }

  return (error_set_t){
      .type   = RESULT_PARSER,
      .errors = p->errors,
  };
}
