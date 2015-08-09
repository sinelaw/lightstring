#ifdef  __LIGHTSTRING_H__
#define __LIGHTSTRING_H__

#define OUT

#define ASSERT(pred) (1/(pred))

typedef struct _LightString LightString;

void init(unsigned int max_bytes, unsigned int max_strings_num);
void fini();

LightString *fromCString(char *);
void writeCString(LightString *, OUT char *);

LightString *concat(LightString *, LightString *);

#endif
