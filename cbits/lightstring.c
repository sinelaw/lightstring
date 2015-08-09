#include "lightstring.h"

#include <stdlib.h>
#include <string.h>

#define MAX_CHUNKS_PER_LS (10)

/** TODO:
 * implement trie that makes finding an existing chunk fast
 */

typedef struct _Chunk {
  unsigned int base;
  unsigned int count;
} Chunk;

struct _LightString {
  Chunk chunks[MAX_CHUNKS_PER_LS];
  unsigned int chunks_count;
  unsigned int length;
};

static LightString *glob_strings;
static unsigned int glob_strings__next_index;
static unsigned int glob_strings__max_count;
static char *glob_data;
static unsigned int glob_data__tail;
static unsigned int glob_data__size;

static LightString *alloc() {
  ASSERT(glob_strings__next_index + 1 < glob_strings__max_count);
  LightString *ls = &glob_strings[glob_strings__next_index];
  glob_strings__next_index++;
  return ls;
}

void init(unsigned int bytes, unsigned int max_strings_num)
{
  glob_data__size = bytes;
  glob_data = (char *)malloc(bytes);
  ASSERT(glob_data);
  glob_strings__max_count = max_strings_num;
  glob_strings = (LightString *)malloc(max_strings_num * (sizeof(LightString)));
  ASSERT(glob_strings);

  glob_strings__next_index = 0;
  glob_data__tail = 0;
}

void fini()
{
  ASSERT(glob_data);
  free(glob_data);
  free(glob_strings);
  glob_data = NULL;
  glob_strings = NULL;
  glob_strings__next_index = 0;
  glob_data__tail = 0;
  glob_data__size = 0;
  glob_strings__max_count = 0;
}

LightString *from_c_string(const char *c_str) {
  size_t size = strlen(c_str);
  unsigned int base;
  /* TODO: use a trie instead. */
  char *existing_chunk_start = strstr(glob_data, c_str);
  if (existing_chunk_start) {
    base = existing_chunk_start - glob_data;
  } else {
    ASSERT(size + glob_data__tail < glob_data__size);
    memcpy(glob_data + glob_data__tail, c_str, size);
    base = glob_data__tail;
    glob_data__tail += size;
  }

  LightString *ls = alloc();
  ls->chunks_count = 1;
  ls->chunks[0] = (Chunk) {
    .base = base,
    .count = size
  };
  ls->length = size;

  return ls;
}

void write_c_string(const LightString *ls, OUT char *dest)
{
  char *cur_dest = dest;
  for (unsigned int i = 0; i < ls->chunks_count; i++) {
    const unsigned int cur_size = ls->chunks[i].count;
    memcpy(cur_dest, glob_data + ls->chunks[i].base, cur_size);
    cur_dest += cur_size;
  }
  cur_dest[ls->length] = '\0';
}

LightString *concat(const LightString *a, const LightString *b)
{
  const unsigned int new_chunks_count = a->chunks_count + b->chunks_count;
  ASSERT(new_chunks_count < MAX_CHUNKS_PER_LS);

  LightString *ls = alloc();
  memcpy(ls, a, (sizeof(LightString)));
  for (unsigned int i = 0; i < b->chunks_count; i++) {
    memcpy(&ls->chunks[i + a->chunks_count], &b->chunks[i], (sizeof(Chunk)));
  }
  ls->chunks_count = new_chunks_count;
  ls->length = a->length + b->length;
  return ls;
}


unsigned int get_length(const LightString *ls)
{
  return ls->length;
}
