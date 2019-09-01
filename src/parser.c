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

static bool parse_import(parser_t *p) {
  decl_t decl = {0};
  decl.kind   = DECL_IMPORT;

  token_t *tok = peek(p);
  if (tok->type != TOKEN_IMPORT) {
    return error(p, tok->pos, "expected 'import'");
  }
  next(p);

  tok = peek(p);
  if (tok->type != TOKEN_IDENT) {
    return error(p, tok->pos, "expected import identifier");
  }
  next(p);

  decl.name = tok->string;

  tok = peek(p);
  if (tok->type != TOKEN_STRING) {
    return error(p, tok->pos, "expected import path");
  }
  next(p);

  decl.import.path = tok->string;

  tok = peek(p);
  if (tok->type != TOKEN_SEMI) {
    return error(p, tok->pos, "expected ';'");
  }
  next(p);

  APPEND(p->ast->decls, decl);

  return true;
}

static bool parse_struct(parser_t *p, struct_t *str) {
  token_t *tok = peek(p);
  if (tok->type != TOKEN_STRUCT) return error(p, tok->pos, "expected 'struct'");
  next(p);

  tok = peek(p);
  if (tok->type != TOKEN_LCURLY) return error(p, tok->pos, "expected '{'");
  next(p);

  tok = peek(p);
  if (tok->type != TOKEN_RCURLY) return error(p, tok->pos, "expected '}'");
  next(p);

  return true;
}

static bool parse_proc(parser_t *p, proc_t *proc) {
  token_t *tok = peek(p);
  if (tok->type != TOKEN_PROC) return error(p, tok->pos, "expected 'proc'");
  next(p);

  tok = peek(p);
  if (tok->type != TOKEN_LPAREN) return error(p, tok->pos, "expected '('");
  next(p);

  tok = peek(p);
  if (tok->type != TOKEN_RPAREN) return error(p, tok->pos, "expected ')'");
  next(p);

  tok = peek(p);
  if (tok->type != TOKEN_LCURLY) return error(p, tok->pos, "expected '{'");
  next(p);

  tok = peek(p);
  if (tok->type != TOKEN_RCURLY) return error(p, tok->pos, "expected '}'");
  next(p);

  return true;
}

static bool parse_decl(parser_t *p) {
  token_t *tok = peek(p);

  switch (tok->type) {
  case TOKEN_IMPORT: return parse_import(p);
  case TOKEN_IDENT: {
    tok         = next(p);
    decl_t decl = {0};
    decl.name   = tok->string;

    tok = next(p);
    if (tok->type != TOKEN_COLON) {
      return error(p, tok->pos, "expected ':'");
    }

    tok = next(p);
    if (tok->type != TOKEN_COLON) {
      return error(p, tok->pos, "expected ':'");
    }

    tok = peek(p);
    switch (tok->type) {
    case TOKEN_PROC: {
      parse_proc(p, &decl.proc);
      decl.kind = DECL_PROC;
    } break;
    case TOKEN_STRUCT: {
      parse_struct(p, &decl.str);
      decl.kind = DECL_STRUCT;
    } break;
    default: {
      next(p);
      return error(p, tok->pos, "unexpected token");
    } break;
    }

    APPEND(p->ast->decls, decl);
  } break;
  default: {
    next(p);
    return error(p, tok->pos, "unexpected token");
  } break;
  }

  return true;
}

void parser_init(parser_t *p, ctx_t *ctx) {
  memset(p, 0, sizeof(*p));
  p->ctx = ctx;
}

result_t
parser_parse(parser_t *p, file_t *file, token_slice_t tokens, ast_t *ast) {
  memset(&p->errors, 0, sizeof(p->errors));
  p->file   = file;
  p->tokens = tokens;
  p->ast    = ast;

  while (!is_at_end(p)) {
    if (!parse_decl(p)) {
    }
  }

  return (result_t){
      .type   = RESULT_PARSER,
      .errors = p->errors,
  };
}
