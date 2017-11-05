#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern void register_chat(struct groupchat *gc, const char *thread_number, const char *board);
extern void loop(void);
extern void threads_catalog(irc_t *irc, const char *a2);
#ifdef __cplusplus
}
#endif

