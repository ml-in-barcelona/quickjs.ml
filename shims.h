/*
 * Shim functions for OCaml ctypes bindings
 */
#ifndef SHIMS_H
#define SHIMS_H

#include <stddef.h>
#include <stdint.h>

/* Monotonic clock in milliseconds (basis for exec deadlines). */
double lre_now_ms(void);

/* lre_compile with the C-stack guard armed. */
uint8_t *lre_compile_shim(int *plen, char *error_msg, int error_msg_size,
                          const char *buf, size_t buf_len, int re_flags,
                          void *opaque);

/* lre_exec with the C-stack guard armed and an optional deadline
   (NULL = no timeout, otherwise a pointer to an lre_now_ms() deadline). */
int lre_exec_shim(uint8_t **capture, const uint8_t *bc_buf,
                  const uint8_t *cbuf, int cindex, int clen, int cbuf_type,
                  double *deadline);

/* Free a bytecode buffer returned by lre_compile. */
void lre_bytecode_free(uint8_t *bc_buf);

/* Wrapper for lre_get_groupnames - casts away const for ctypes. */
char *lre_get_groupnames_shim(const uint8_t *bc_buf);

/* Unicode normalization shim - wraps unicode_normalize with realloc. */
int unicode_normalize_shim(const uint32_t *src, int src_len, int n_type,
                           uint32_t **pdst);

/* Free buffer allocated by unicode_normalize_shim. */
void unicode_normalize_free(uint32_t *ptr);

/* Look up a Unicode character range table by name.
   kind: 0 = Script, 1 = Script_Extensions, 2 = General_Category,
   3 = binary property.
   On success returns the number of points (always even) and stores the
   points buffer in *out_points (release with unicode_char_range_free;
   may be NULL when the result is empty). Points encode half-open
   intervals: [points[2i], points[2i+1]).
   Returns -1 on memory error, -2 when the name is unknown. */
int unicode_char_range_shim(int kind, const char *name, uint32_t **out_points);

/* Free buffer returned by unicode_char_range_shim. */
void unicode_char_range_free(uint32_t *points);

/* Wrapper for js_atod - reports consumed bytes as an offset. */
double js_atod_shim(const char *str, int *poffset, int radix, int flags,
                    void *tmp_mem);

#endif /* SHIMS_H */
