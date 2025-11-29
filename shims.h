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

/* Wrapper for lre_get_groupnames - casts away const for ctypes compatibility */
char *lre_get_groupnames_shim(const uint8_t *bc_buf);

/* Wrapper for js_atod - handles const char ** to char ** cast for ctypes */
double js_atod_shim(const char *str, char **pnext, int radix, int flags,
                    void *tmp_mem);

#endif /* SHIMS_H */

