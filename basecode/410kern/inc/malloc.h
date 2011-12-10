#include <stdlib.h>
#include <types.h>

#ifndef _MALLOC_WRAPPERS_H_
#define _MALLOC_WRAPPERS_H_

#include <aan.h>

MAY_SLEEP void *malloc(size_t size);
MAY_SLEEP void *memalign(size_t alignment, size_t size);
MAY_SLEEP void *calloc(size_t nelt, size_t eltsize);
MAY_SLEEP void *realloc(void *buf, size_t new_size);
MAY_SLEEP void free(void *buf);
MAY_SLEEP void *smalloc(size_t size);
MAY_SLEEP void *smemalign(size_t alignment, size_t size);
MAY_SLEEP void sfree(void *buf, size_t size);

#endif /* _MALLOC_WRAPPERS_H_ */
