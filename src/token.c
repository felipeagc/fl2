#include "token.h"

char *TOKEN_STRINGS[] = {
    [TOKEN_EOF] = "EOF",

    [TOKEN_INT]     = "integer",
    [TOKEN_FLOAT]   = "float",
    [TOKEN_STRING]  = "\"string\"",
    [TOKEN_CSTRING] = "c\"string\"",
    [TOKEN_IDENT]   = "identifier",

    [TOKEN_LPAREN] = "(",
    [TOKEN_RPAREN] = ")",
    [TOKEN_LBRACK] = "[",
    [TOKEN_RBRACK] = "]",
    [TOKEN_LCURLY] = "{",
    [TOKEN_RCURLY] = "}",

    [TOKEN_DOT]   = ".",
    [TOKEN_COMMA] = ",",
    [TOKEN_COLON] = ":",
    [TOKEN_SEMI]  = ";",

    [TOKEN_ELLIPSIS] = "...",

    [TOKEN_ADD] = "+",
    [TOKEN_SUB] = "-",
    [TOKEN_MUL] = "*",
    [TOKEN_DIV] = "/",
    [TOKEN_MOD] = "%",

    [TOKEN_AMPERSAND] = "&",

    [TOKEN_EQL]    = "==",
    [TOKEN_LSS]    = "<",
    [TOKEN_GTR]    = ">",
    [TOKEN_ASSIGN] = "=",
    [TOKEN_NOT]    = "!",

    [TOKEN_NEQ] = "!=",
    [TOKEN_LEQ] = "<=",
    [TOKEN_GEQ] = ">=",

    [TOKEN_ARROW] = "->",

    [TOKEN_U8]  = "u8",
    [TOKEN_U16] = "u16",
    [TOKEN_U32] = "u32",
    [TOKEN_U64] = "u64",
    [TOKEN_I8]  = "i8",
    [TOKEN_I16] = "i16",
    [TOKEN_I32] = "i32",
    [TOKEN_I64] = "i64",
    [TOKEN_F32] = "f32",
    [TOKEN_F64] = "f64",

    [TOKEN_BOOL]        = "bool",
    [TOKEN_VOID]        = "void",
    [TOKEN_STRING_TYPE] = "string",

    [TOKEN_INTRIN_SIZEOF] = "@sizeof",
    [TOKEN_INTRIN_ASSERT] = "@assert",

    [TOKEN_FOR]      = "for",
    [TOKEN_BREAK]    = "break",
    [TOKEN_CONTINUE] = "continue",
    [TOKEN_DEFER]    = "defer",
    [TOKEN_RETURN]   = "return",
    [TOKEN_PROC]     = "proc",
    [TOKEN_EXTERN]   = "extern",
    [TOKEN_PACKED]   = "packed",
    [TOKEN_STRUCT]   = "struct",
    [TOKEN_UNION]    = "union",
    [TOKEN_IF]       = "if",
    [TOKEN_ELSE]     = "else",
    [TOKEN_IMPORT]   = "import",
    [TOKEN_USING]    = "using",
};
