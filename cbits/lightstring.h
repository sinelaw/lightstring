#ifndef __LIGHTSTRING_H__
#define __LIGHTSTRING_H__

#define OUT

#define ASSERT(pred) (pred ? abort() : 0)

typedef struct _LightString LightString;

void init(unsigned int max_bytes, unsigned int max_strings_num);
void fini();

LightString *from_c_string(char *);
void write_c_string(LightString *, OUT char *);

LightString *concat(LightString *, LightString *);

#endif
