/*
 * Shim functions for OCaml ctypes bindings.
 *
 * Provides:
 * - The three embedder callbacks libregexp requires (stack overflow check,
 *   timeout check, allocator).
 * - Wrappers around lre_compile/lre_exec that arm the stack guard and
 *   (for exec) an optional wall-clock deadline.
 * - Small helpers to work around things ctypes cannot express (const
 *   qualifiers, pointer-offset arithmetic across the FFI boundary).
 */
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>
#include "cutils.h"
#include "libunicode.h"
#include "libregexp.h"
#include "dtoa.h"
#include "shims.h"

/* ===========================================================================
   libregexp embedder callbacks
   =========================================================================== */

/* Amount of C stack the regexp compiler/executor is allowed to consume
   below the point where we entered it. Deeply nested patterns abort with a
   "stack overflow" compile error instead of smashing the process stack. */
#define LRE_STACK_BUDGET ((size_t)(1024 * 1024))

#if defined(_MSC_VER)
#define LRE_THREAD_LOCAL __declspec(thread)
#else
#define LRE_THREAD_LOCAL __thread
#endif

/* Lowest stack address the regexp engine may reach. 0 = guard not armed. */
static LRE_THREAD_LOCAL uintptr_t lre_stack_limit = 0;

static void lre_stack_guard_arm(void)
{
    char probe;
    uintptr_t sp = (uintptr_t)&probe;
    lre_stack_limit = sp > LRE_STACK_BUDGET ? sp - LRE_STACK_BUDGET : 1;
}

bool lre_check_stack_overflow(void *opaque, size_t alloca_size)
{
    char probe;
    uintptr_t sp = (uintptr_t)&probe;
    (void)opaque;
    if (lre_stack_limit == 0)
        return false;
    if (sp < alloca_size)
        return true;
    return (sp - alloca_size) < lre_stack_limit;
}

void *lre_realloc(void *opaque, void *ptr, size_t size)
{
    (void)opaque;
    /* Callers (cr_free, dbuf_free) rely on size == 0 meaning "free and
       return NULL". Plain realloc(NULL, 0) is malloc(0) on glibc: it
       returns a fresh minimal chunk that the callers discard, leaking one
       chunk per CharRange operation. QuickJS's own embedder implementation
       (js_realloc_rt) special-cases size == 0 the same way. */
    if (size == 0) {
        free(ptr);
        return NULL;
    }
    return realloc(ptr, size);
}

/* Monotonic clock in milliseconds. */
double lre_now_ms(void)
{
#if defined(CLOCK_MONOTONIC)
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0)
        return (double)ts.tv_sec * 1000.0 + (double)ts.tv_nsec / 1e6;
#endif
    return 0.0;
}

/* The opaque pointer passed through lre_exec is either NULL (no deadline)
   or a pointer to a double holding an lre_now_ms() deadline. */
int lre_check_timeout(void *opaque)
{
    if (opaque == NULL)
        return 0;
    return lre_now_ms() > *(const double *)opaque;
}

/* ===========================================================================
   libregexp wrappers
   =========================================================================== */

uint8_t *lre_compile_shim(int *plen, char *error_msg, int error_msg_size,
                          const char *buf, size_t buf_len, int re_flags,
                          void *opaque)
{
    lre_stack_guard_arm();
    return lre_compile(plen, error_msg, error_msg_size, buf, buf_len,
                       re_flags, opaque);
}

/* deadline: NULL for no timeout, otherwise an lre_now_ms() deadline. */
int lre_exec_shim(uint8_t **capture, const uint8_t *bc_buf,
                  const uint8_t *cbuf, int cindex, int clen, int cbuf_type,
                  double *deadline)
{
    lre_stack_guard_arm();
    return lre_exec(capture, bc_buf, cbuf, cindex, clen, cbuf_type, deadline);
}

/* Free a bytecode buffer returned by lre_compile (allocated via
   lre_realloc, i.e. plain realloc). */
void lre_bytecode_free(uint8_t *bc_buf)
{
    free(bc_buf);
}

/* ctypes cannot express const, so this casts the const away. */
char *lre_get_groupnames_shim(const uint8_t *bc_buf)
{
    return (char *)lre_get_groupnames(bc_buf);
}

/* ===========================================================================
   libunicode shims
   =========================================================================== */

int unicode_normalize_shim(const uint32_t *src, int src_len, int n_type,
                           uint32_t **pdst)
{
    return unicode_normalize(pdst, src, src_len,
                             (UnicodeNormalizationEnum)n_type, NULL,
                             lre_realloc);
}

void unicode_normalize_free(uint32_t *ptr)
{
    free(ptr);
}

/* Fills a CharRange from one of libunicode's property tables and hands the
   points buffer to the caller. The buffer is allocated with lre_realloc
   (plain realloc), so ownership can be transferred and released with
   free(). */
int unicode_char_range_shim(int kind, const char *name, uint32_t **out_points)
{
    CharRange cr;
    int ret;

    cr_init(&cr, NULL, lre_realloc);
    switch (kind) {
    case 0: ret = unicode_script(&cr, name, false); break;
    case 1: ret = unicode_script(&cr, name, true); break;
    case 2: ret = unicode_general_category(&cr, name); break;
    case 3: ret = unicode_prop(&cr, name); break;
    default: ret = -2; break;
    }
    if (ret) {
        cr_free(&cr);
        return ret;
    }
    *out_points = cr.points; /* ownership transferred */
    return cr.len;
}

void unicode_char_range_free(uint32_t *points)
{
    free(points);
}

/* ===========================================================================
   dtoa shims
   =========================================================================== */

/* Wraps js_atod, returning the number of bytes consumed through *poffset.
   Computing the offset on the C side avoids comparing pointers of two
   distinct OCaml->C copies of the same string. */
double js_atod_shim(const char *str, int *poffset, int radix, int flags,
                    void *tmp_mem)
{
    const char *next = str;
    double d = js_atod(str, &next, radix, flags, (JSATODTempMem *)tmp_mem);
    *poffset = (int)(next - str);
    return d;
}
