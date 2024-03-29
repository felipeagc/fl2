#include "scanner.h"

#include <stdbool.h>
#include <stdio.h>

static void error(scanner_t *s, pos_t pos, const char *fmt, ...) {
  sb_reset(&s->ctx->sb);

  va_list vl;
  va_start(vl, fmt);
  sb_vsprintf(&s->ctx->sb, fmt, vl);
  va_end(vl);

  error_t err = {
      .pos = pos,
      .msg = bump_strdup(&s->ctx->alloc, sb_build(&s->ctx->sb)),
  };

  APPEND(s->errors, err);
}

static inline bool is_at_end(scanner_t *s) {
  return s->offset >= s->file->content.count ||
         s->file->content.buf[s->offset] == '\0';
}

static inline char next(scanner_t *s) {
  if (is_at_end(s)) return '\0';
  s->col++;
  return s->file->content.buf[s->offset++];
}

static inline char peek(scanner_t *s) {
  if (is_at_end(s)) return '\0';
  return s->file->content.buf[s->offset];
}

static inline char peek_next(scanner_t *s, size_t offset) {
  if ((s->offset + offset) >= s->file->content.count ||
      s->file->content.buf[s->offset + offset] == '\0')
    return '\0';
  return s->file->content.buf[s->offset + offset];
}

static inline bool is_letter(char c) {
  return ('z' >= c && c >= 'a') || ('Z' >= c && c >= 'A');
}

static inline bool is_numeric(char c) { return '0' <= c && '9' >= c; }

static inline bool is_alphanum(char c) {
  return is_letter(c) || is_numeric(c) || c == '_';
}

static inline bool is_newline(char c) { return c == '\n'; }

static inline bool is_whitespace(char c) {
  return c == ' ' || c == '\t' || is_newline(c);
}

static inline void skip_whitespace(scanner_t *s) {
  while (is_whitespace(peek(s))) {
    if (is_at_end(s)) return;

    if (is_newline(peek(s))) {
      s->line++;
      s->col = 0;
    }
    next(s);
  }
}

static void scan_identifier(scanner_t *s) {
  sb_reset(&s->ctx->sb);

  pos_t pos  = {0};
  pos.file   = s->file;
  pos.offset = s->offset;
  pos.line   = s->line;
  pos.col    = s->col;

  while (!is_at_end(s) && is_alphanum(peek(s))) {
    sb_append_char(&s->ctx->sb, peek(s));
    next(s);
  }

  pos.len = s->offset - pos.offset;

  strbuf_t ident = sb_build(&s->ctx->sb);

  token_t token = {
      .type = TOKEN_IDENT,
      .pos  = pos,
  };

  if (strbuf_cmp(ident, STR("if"))) {
    token.type = TOKEN_IF;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("u8"))) {
    token.type = TOKEN_U8;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("u16"))) {
    token.type = TOKEN_U16;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("u32"))) {
    token.type = TOKEN_U32;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("u64"))) {
    token.type = TOKEN_U64;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("i8"))) {
    token.type = TOKEN_I8;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("i16"))) {
    token.type = TOKEN_I16;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("i32"))) {
    token.type = TOKEN_I32;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("i64"))) {
    token.type = TOKEN_I64;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("f32"))) {
    token.type = TOKEN_F32;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("f64"))) {
    token.type = TOKEN_F64;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("true"))) {
    token.type = TOKEN_TRUE;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("false"))) {
    token.type = TOKEN_FALSE;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("null"))) {
    token.type = TOKEN_NULL;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("void"))) {
    token.type = TOKEN_VOID;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("bool"))) {
    token.type = TOKEN_BOOL;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("inline"))) {
    token.type = TOKEN_INLINE;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("packed"))) {
    token.type = TOKEN_PACKED;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("defer"))) {
    token.type = TOKEN_DEFER;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("proc"))) {
    token.type = TOKEN_PROC;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("struct"))) {
    token.type = TOKEN_STRUCT;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("for"))) {
    token.type = TOKEN_FOR;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("break"))) {
    token.type = TOKEN_BREAK;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("continue"))) {
    token.type = TOKEN_CONTINUE;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("return"))) {
    token.type = TOKEN_RETURN;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("union"))) {
    token.type = TOKEN_UNION;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("else"))) {
    token.type = TOKEN_ELSE;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("import"))) {
    token.type = TOKEN_IMPORT;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("using"))) {
    token.type = TOKEN_USING;
    APPEND(s->tokens, token);
    return;
  }

  token.string = bump_strdup(&s->ctx->alloc, ident);
  APPEND(s->tokens, token);
}

static void scan_intrin(scanner_t *s) {
  sb_reset(&s->ctx->sb);

  pos_t pos  = {0};
  pos.file   = s->file;
  pos.offset = s->offset;
  pos.line   = s->line;
  pos.col    = s->col;

  next(s); // skip @

  while (!is_at_end(s) && is_alphanum(peek(s))) {
    sb_append_char(&s->ctx->sb, peek(s));
    next(s);
  }

  pos.len = s->offset - pos.offset;

  strbuf_t ident = sb_build(&s->ctx->sb);

  token_t token = {
      .pos = pos,
  };

  if (strbuf_cmp(ident, STR("sizeof"))) {
    token.type = TOKEN_INTRIN_SIZEOF;
    APPEND(s->tokens, token);
    return;
  } else if (strbuf_cmp(ident, STR("assert"))) {
    token.type = TOKEN_INTRIN_ASSERT;
    APPEND(s->tokens, token);
    return;
  } else {
    error(s, token.pos, "invalid intrinsic token");
    return;
  }

  APPEND(s->tokens, token);
}

static void scan_string(scanner_t *s, bool c_string) {
  sb_reset(&s->ctx->sb);

  token_t token = {
      .pos = (pos_t){.file   = s->file,
                     .offset = s->offset,
                     .line   = s->line,
                     .col    = s->col},
  };

  token.type = c_string ? TOKEN_CSTRING : TOKEN_STRING;

  if (c_string) next(s); // skip c

  next(s); // Skip quote

  while (!is_at_end(s)) {
    if (peek(s) == '\\') {
      next(s);

      switch (peek(s)) {
      case 'a': sb_append_char(&s->ctx->sb, '\a'); break;
      case 'b': sb_append_char(&s->ctx->sb, '\b'); break;
      case 'f': sb_append_char(&s->ctx->sb, '\f'); break;
      case 'n': sb_append_char(&s->ctx->sb, '\n'); break;
      case 'r': sb_append_char(&s->ctx->sb, '\r'); break;
      case 't': sb_append_char(&s->ctx->sb, '\t'); break;
      case 'v': sb_append_char(&s->ctx->sb, '\v'); break;
      case '\\': sb_append_char(&s->ctx->sb, '\\'); break;
      case '\'': sb_append_char(&s->ctx->sb, '\''); break;
      case '"': sb_append_char(&s->ctx->sb, '"'); break;
      case '?': sb_append_char(&s->ctx->sb, '\?'); break;
      case '0': sb_append_char(&s->ctx->sb, '\0'); break;
      default: {
        sb_append_char(&s->ctx->sb, '\\');
        sb_append_char(&s->ctx->sb, peek(s));
      } break;
      }

      next(s);
      continue;
    }

    if (peek(s) == '\"') {
      break;
    }

    sb_append_char(&s->ctx->sb, next(s));
  }

  if (c_string) sb_append_char(&s->ctx->sb, '\0');

  next(s);

  token.string  = bump_strdup(&s->ctx->alloc, sb_build(&s->ctx->sb));
  token.pos.len = s->offset - token.pos.offset;

  APPEND(s->tokens, token);
}

static void scan_number(scanner_t *s) {
  sb_reset(&s->ctx->sb);

  token_t token    = {0};
  token.pos.file   = s->file;
  token.pos.offset = s->offset;
  token.pos.line   = s->line;
  token.pos.col    = s->col;

  bool has_dot = false;

  while (!is_at_end(s) && (is_numeric(peek(s)) || peek(s) == '.')) {
    sb_append_char(&s->ctx->sb, peek(s));
    if (peek(s) == '.') has_dot = true;
    next(s);
  }

  sb_append_char(&s->ctx->sb, '\0');

  char *str = sb_build(&s->ctx->sb).buf;

  if (has_dot) {
    token.type = TOKEN_FLOAT;
    token.f64  = strtod(str, NULL);
  } else {
    token.type = TOKEN_INT;
    token.i64  = strtol(str, NULL, 10);
  }

  token.pos.len = s->offset - token.pos.offset;

  APPEND(s->tokens, token);
}

static void scan_token(scanner_t *s) {
  skip_whitespace(s);

  char ch = peek(s);

  if (ch == '\0') return;

  token_t token    = {0};
  token.pos.file   = s->file;
  token.pos.offset = s->offset;
  token.pos.len    = 1;
  token.pos.line   = s->line;
  token.pos.col    = s->col;

  switch (ch) {
  case '(': {
    next(s);
    token.type = TOKEN_LPAREN;
    APPEND(s->tokens, token);
  } break;
  case ')': {
    next(s);
    token.type = TOKEN_RPAREN;
    APPEND(s->tokens, token);
  } break;
  case '[': {
    next(s);
    token.type = TOKEN_LBRACK;
    APPEND(s->tokens, token);
  } break;
  case ']': {
    next(s);
    token.type = TOKEN_RBRACK;
    APPEND(s->tokens, token);
  } break;
  case '{': {
    next(s);
    token.type = TOKEN_LCURLY;
    APPEND(s->tokens, token);
  } break;
  case '}': {
    next(s);
    token.type = TOKEN_RCURLY;
    APPEND(s->tokens, token);
  } break;

  case '.': {
    next(s);
    token.type = TOKEN_DOT;

    if (peek_next(s, 0) == '.' && peek_next(s, 1) == '.') {
      next(s);
      next(s);
      token.pos.len = 3;
      token.type    = TOKEN_ELLIPSIS;
    }

    APPEND(s->tokens, token);
  } break;
  case ',': {
    next(s);
    token.type = TOKEN_COMMA;
    APPEND(s->tokens, token);
  } break;
  case ':': {
    next(s);
    token.type = TOKEN_COLON;
    APPEND(s->tokens, token);
  } break;
  case ';': {
    next(s);
    token.type = TOKEN_SEMI;
    APPEND(s->tokens, token);
  } break;

  case '+': {
    next(s);
    token.type = TOKEN_ADD;
    APPEND(s->tokens, token);
  } break;
  case '-': {
    next(s);
    if (peek(s) == '>') {
      next(s);
      token.type    = TOKEN_ARROW;
      token.pos.len = 2;
      APPEND(s->tokens, token);
      break;
    }
    token.type = TOKEN_SUB;
    APPEND(s->tokens, token);
  } break;
  case '*': {
    next(s);
    token.type = TOKEN_MUL;
    APPEND(s->tokens, token);
  } break;
  case '/': {
    next(s);
    if (peek(s) == '/') {
      // Comment
      while (!is_newline(next(s))) {
      }
      break;
    }

    token.type = TOKEN_DIV;
    APPEND(s->tokens, token);
  } break;
  case '%': {
    next(s);
    token.type = TOKEN_MOD;
    APPEND(s->tokens, token);
  } break;

  case '&': {
    next(s);
    token.type = TOKEN_AMPERSAND;
    APPEND(s->tokens, token);
  } break;

  case '=': {
    next(s);
    if (peek(s) == '=') {
      next(s);
      token.type    = TOKEN_EQL;
      token.pos.len = 2;
      APPEND(s->tokens, token);
      break;
    }

    token.type = TOKEN_ASSIGN;
    APPEND(s->tokens, token);
  } break;

  case '!': {
    next(s);
    if (peek(s) == '=') {
      next(s);
      token.type    = TOKEN_NEQ;
      token.pos.len = 2;
      APPEND(s->tokens, token);
      break;
    }

    token.type = TOKEN_NOT;
    APPEND(s->tokens, token);
  } break;

  case '<': {
    next(s);
    if (peek(s) == '=') {
      next(s);
      token.type    = TOKEN_LEQ;
      token.pos.len = 2;
      APPEND(s->tokens, token);
      break;
    }

    token.type = TOKEN_LSS;
    APPEND(s->tokens, token);
  } break;

  case '>': {
    next(s);
    if (peek(s) == '=') {
      next(s);
      token.type    = TOKEN_GEQ;
      token.pos.len = 2;
      APPEND(s->tokens, token);
      break;
    }

    token.type = TOKEN_GTR;
    APPEND(s->tokens, token);
  } break;

  case '"': {
    scan_string(s, false);
  } break;

  case '@': {
    scan_intrin(s);
  } break;

  default: {
    if (is_letter(ch)) {
      if (ch == 'c' && peek_next(s, 1) == '\"') {
        scan_string(s, true);
        break;
      }
      scan_identifier(s);
      break;
    }

    if (is_numeric(ch)) {
      scan_number(s);
      break;
    }

    error(
        s,
        (pos_t){
            .file   = s->file,
            .offset = s->offset,
            .len    = 1,
            .line   = s->line,
            .col    = s->col,
        },
        "unexpected character: '%c'",
        peek(s));

    next(s);
  } break;
  }
}

void scanner_init(scanner_t *s, ctx_t *ctx) {
  memset(s, 0, sizeof(*s));
  s->ctx = ctx;
}

error_set_t scanner_scan(scanner_t *s, file_t *file, token_slice_t *tokens) {
  memset(&s->tokens, 0, sizeof(s->tokens));
  memset(&s->errors, 0, sizeof(s->errors));

  s->offset = 0;
  s->line   = 1;
  s->col    = 1;
  s->file   = file;

  while (!is_at_end(s)) {
    scan_token(s);
  }

  token_t eof_tok = {.type = TOKEN_EOF,
                     .pos  = (pos_t){
                         .file   = s->file,
                         .offset = file->content.count,
                         .len    = 0,
                         .line   = s->line,
                         .col    = s->col,
                     }};

  APPEND(s->tokens, eof_tok);

  *tokens = s->tokens;

  return (error_set_t){
      .type   = RESULT_SCANNER,
      .errors = s->errors,
  };
}
