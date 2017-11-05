#include <malloc.h>
#include <bitlbee.h>
#include <glib.h>
#include "HsFFI.h"
#include "c-lib.h"
#include "stub/Lib_stub_fixed.h"

static struct im_connection *ic = NULL;

struct plugin_info *init_plugin_info(void)
{
    static struct plugin_info info = {
        BITLBEE_ABI_VERSION_CODE,
        "bitlbee-4chan",
        "0.0.2",
        "Bitlbee plugin for 4chan",
        "",
        "https://github.com/"
    };

    return &info;
}


static void chan_init(account_t *acc)
{
}


gboolean chan_main_loop(gpointer data, gint fd, b_input_condition cond)
{
  struct im_connection *ic = data;
  loop();

  return TRUE;
}

static void chan_login(account_t *acc)
{
  ic = imcb_new(acc);
  b_timeout_add(5000, chan_main_loop, ic);
  // we need to tell bitlbee that we are connected
	imcb_connected(ic);
}

static void chan_logout(struct im_connection *ic)
{
}

static void chan_keepalive(struct im_connection *ic)
{
}

static void chan_chat_msg(struct groupchat *gc, char *msg, int flags)
{
  if(strcmp("start\n", msg) != 0){
    struct thread_info *info = gc->data;
    if(info != NULL){
      register_chat(gc, info->thread_number, info->board);
    }
  }
}

static void chan_chat_list(struct im_connection *ic, const char *server)
{

}

/**
 * A custom command to list the threads in the catalog of a board
 * */
static void chan_cmd_catalog(irc_t *irc, char **args){
  char *board = args[1];
  if(board == NULL){
    irc_rootmsg(irc, "USAGE: catalog BOARD");
  }else{
    irc_rootmsg(irc, "Looking for threads in %s...", board);
    threads_catalog(irc, board);
  }
}

static void chan_cmd_join_thread(irc_t *irc, char **args){
  char *board = args[1];
  char *thread = NULL;
  if(board == NULL){
    irc_rootmsg(irc, "USAGE: jthread BOARD THREAD_NUMBER");
  }else if(!(thread = args[2])){
    irc_rootmsg(irc, "USAGE: jthread BOARD THREAD_NUMBER");
  }else{
    if(ic == NULL){
      irc_rootmsg(irc, "There is no connection");
    }else{
      irc_rootmsg(irc, "Joining thread %s in board %s...", thread, board);
      struct groupchat *gc = imcb_chat_new(ic, thread);
      if(gc == NULL){
        irc_rootmsg(irc, "Failed to create a new chat room");
        return;
      }
      //we add info related to the thread for own use
      struct thread_info *info = g_malloc(sizeof(struct thread_info));
      info->thread_number = g_malloc(sizeof(strlen(thread)+1)*sizeof(char));
      info->board = g_malloc(sizeof(strlen(board)+1)*sizeof(char));
      strcpy(info->thread_number, thread);
      strcpy(info->board, board);
      gc->data = info;

      imcb_chat_name_hint(gc, thread);
      irc_rootmsg(irc, "Now /join #%s", thread);
    }
  }
}


static struct groupchat *chan_chat_join(
        struct im_connection *ic,
        const char *room,
        const char *nick,
        const char *password,
        set_t **sets)
{
  return NULL;
}

static int chan_buddy_msg(struct im_connection *ic, char *to, char *msg,
        int flags)
{
    return 0;
}


static gboolean chan_is_self(struct im_connection *ic, const char *who)
{
    return FALSE;
}


static GList *chan_away_states(struct im_connection *ic)
{
    return NULL;
}

static void chan_set_away(
        struct im_connection *ic,
        char *state,
        char *message)
{
}




G_MODULE_EXPORT void init_plugin(void)
{
    struct prpl *dpp;

    static const struct prpl pp = {
        .name           = "4chan",
        .init           = chan_init,
        .login          = chan_login,
        .logout         = chan_logout,
        .chat_msg       = chan_chat_msg,
        .chat_list      = chan_chat_list,
        .chat_join      = chan_chat_join,
        .buddy_msg      = chan_buddy_msg,
        .handle_cmp     = g_strcmp0,
        .handle_is_self = chan_is_self,
        .away_states    = chan_away_states,
        .set_away       = chan_set_away,
        .keepalive      = chan_keepalive
    };

    dpp = g_memdup(&pp, sizeof(pp));
    register_protocol(dpp);

    //custom commands
    root_command_add("catalog", 0, chan_cmd_catalog, 0);
    root_command_add("jthread", 0, chan_cmd_join_thread, 0);


    //init haskell 
    const char *name = "program";
    char **argv = g_malloc(sizeof(char*)*1);
    *argv = g_malloc(sizeof(char)*strlen(name) + 1);
    strcpy(*argv, name);
    int argc = 1;
    hs_init(&argc, &argv);
    g_free(*argv);
    g_free(argv);
}
