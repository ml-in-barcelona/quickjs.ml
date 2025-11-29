/*
 * Shim functions for OCaml ctypes bindings
 */
#ifndef SHIMS_H
#define SHIMS_H

#include <stdint.h>

/* Unicode normalization shim - wraps unicode_normalize with standard realloc */
int unicode_normalize_shim(const uint32_t *src, int src_len, int n_type, uint32_t **pdst);

/* Free buffer allocated by unicode_normalize_shim */
void unicode_normalize_free(uint32_t *ptr);

#endif /* SHIMS_H */

