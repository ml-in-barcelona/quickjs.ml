#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include "cutils.h"
#include "libunicode.h"
#include "libregexp.h"
#include "dtoa.h"

int lre_check_stack_overflow(void *opaque, size_t alloca_size)
{
    return 0;
}

void *lre_realloc(void *opaque, void *ptr, size_t size)
{
    return realloc(ptr, size);
}

int lre_check_timeout(void *opaque)
{
    return 0;
}

/* Unicode normalization shim - wraps unicode_normalize with standard realloc */
int unicode_normalize_shim(const uint32_t *src, int src_len, int n_type, uint32_t **pdst)
{
    return unicode_normalize(pdst, src, src_len, (UnicodeNormalizationEnum)n_type,
                             NULL, lre_realloc);
}

/* Free buffer allocated by unicode_normalize_shim */
void unicode_normalize_free(uint32_t *ptr)
{
    free(ptr);
}

/* ctypes shims: ctypes can't express C's const qualifier, so these wrappers cast between const/non-const pointer types */

char *lre_get_groupnames_shim(const uint8_t *bc_buf)
{
    return (char *)lre_get_groupnames(bc_buf);
}

double js_atod_shim(const char *str, char **pnext, int radix, int flags,
                    JSATODTempMem *tmp_mem)
{
    return js_atod(str, (const char **)pnext, radix, flags, tmp_mem);
}
