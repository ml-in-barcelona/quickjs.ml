#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

int lre_check_stack_overflow(void *opaque, size_t alloca_size)
{
    return 0;
}

void *lre_realloc(void *opaque, void *ptr, size_t size)
{
    return realloc(ptr, size);
}
