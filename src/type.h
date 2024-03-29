#pragma once

#include "ast.h"
#include "strbuf.h"

type_t *exact_types(type_t *received, type_t *expected);

type_t *equivalent_types(type_t *received, type_t *expected);

void print_type(str_builder_t *sb, type_t *type);
