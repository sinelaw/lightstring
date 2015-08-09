#ifndef __LIGHTSTRING_H__
#define __LIGHTSTRING_H__

#define OUT

#include <stdio.h>

#define ASSERT(pred) do {                                               \
    if (!(pred)) {                                                      \
      fprintf(stderr, "%s:%d: error: ASSERTION FAILED: " #pred "\n", __FILE__, __LINE__); \
      abort();                                                          \
    }                                                                   \
  } while (0)

typedef struct _LightString LightString;

void init(unsigned int max_bytes, unsigned int max_strings_num);
void fini();

LightString *from_c_string(char *);
void write_c_string(LightString *, OUT char *);
unsigned int get_length(LightString *ls);

LightString *concat(LightString *, LightString *);

#endif
