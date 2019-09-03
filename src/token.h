#pragma once

#include "position.h"
#include "strbuf.h"

typedef enum {
  TOKEN_EOF,

  TOKEN_INT,
  TOKEN_FLOAT,
  TOKEN_STRING,
  TOKEN_IDENT,

  TOKEN_LPAREN, // (
  TOKEN_RPAREN, // )
  TOKEN_LBRACK, // [
  TOKEN_RBRACK, // ]
  TOKEN_LCURLY, // {
  TOKEN_RCURLY, // }

  TOKEN_DOT,   // .
  TOKEN_COLON, // :
  TOKEN_SEMI,  // ;

  TOKEN_ADD, // +
  TOKEN_SUB, // -
  TOKEN_MUL, // *
  TOKEN_DIV, // /
  TOKEN_MOD, // %

  TOKEN_EQL,    // ==
  TOKEN_LSS,    // <
  TOKEN_GTR,    // >
  TOKEN_ASSIGN, // =
  TOKEN_NOT,    // !

  TOKEN_NEQ, // !=
  TOKEN_LEQ, // <=
  TOKEN_GEQ, // >=

  TOKEN_U8,
  TOKEN_U16,
  TOKEN_U32,
  TOKEN_U64,
  TOKEN_I8,
  TOKEN_I16,
  TOKEN_I32,
  TOKEN_I64,

  TOKEN_BOOL,
  TOKEN_VOID,

  // Keywords:
  TOKEN_FOR,
  TOKEN_BREAK,
  TOKEN_CONTINUE,
  TOKEN_DEFER,
  TOKEN_RETURN,
  TOKEN_PROC,
  TOKEN_EXTERN,
  TOKEN_PACKED,
  TOKEN_STRUCT,
  TOKEN_UNION,
  TOKEN_IF,
  TOKEN_ELSE,
  TOKEN_IMPORT,
  TOKEN_USING,
} token_type_t;

extern char *TOKEN_STRINGS[];

typedef struct {
  token_type_t type;
  pos_t pos;
  union {
    strbuf_t string;
    int64_t i64;
    double f64;
  };
} token_t;

typedef SLICE(token_t) token_slice_t;

static inline strbuf_t token_to_string(token_t *token) {
  strbuf_t str;

  switch (token->type) {
  case TOKEN_IDENT: {
    str = token->string;
  } break;
  default: {
    str.buf   = &token->pos.file->content.buf[token->pos.offset];
    str.count = token->pos.len;
    str.cap   = token->pos.len;
  } break;
  }

  return str;
}
