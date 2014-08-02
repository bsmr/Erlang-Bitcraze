/* =====================================================================
 * An Erlang/OTP NIF interface to access the Linux Joystick API
 * ===================================================================== */

#include <erl_nif.h>

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

#include <linux/joystick.h>

/* ---------------------------------------------------------------------
 * some constants
 * --------------------------------------------------------------------- */
static const char atom_ok[]        = "ok";
static const char atom_error[]     = "error";
static const char atom_no_event[]  = "noevent";

static const int max_pathname      = 128;
static const int max_joystick_name = 128;

/* ---------------------------------------------------------------------
 * export
 * --------------------------------------------------------------------- */
/*
extern static ERL_NIF_TERM js_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern static ERL_NIF_TERM js_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern static ERL_NIF_TERM js_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern static ERL_NIF_TERM js_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern static ERL_NIF_TERM js_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern static ERL_NIF_TERM js_axes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
extern static ERL_NIF_TERM js_buttons(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
 */

/* ---------------------------------------------------------------------
 * open a (joystick-)device
 * input: joystick device name (string)
 * result: {error, errno}
 *         {ok, fd}
 * --------------------------------------------------------------------- */
static ERL_NIF_TERM js_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  char pathname[max_pathname]; 
  
  ERL_NIF_TERM t_key;
  ERL_NIF_TERM t_val;
  ERL_NIF_TERM t_res;
  
  if (1 != argc) {
    return enif_make_badarg(env);
  }
  
  if (0 >= enif_get_string(env, argv[0], pathname, max_pathname, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }
    
  fd = open(pathname, O_RDONLY | O_NONBLOCK);
  if (-1 == fd) {
    t_key = enif_make_atom(env, atom_error);
    t_val = enif_make_int(env, errno);
  } else {
    t_key = enif_make_atom(env, atom_ok);
    t_val = enif_make_int(env, fd);
  }
  
  t_res = enif_make_tuple2(env, t_key, t_val);
  return t_res;
}

/* ---------------------------------------------------------------------
 * close a (joystick-)device
 * input: fd
 * result: {error, errno}
 *         ok
 * --------------------------------------------------------------------- */
static ERL_NIF_TERM js_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  
  ERL_NIF_TERM t_key;
  ERL_NIF_TERM t_val;
  ERL_NIF_TERM t_res;
  
  if (1 != argc) {
    return enif_make_badarg(env);
  }
  
  if (!enif_get_int(env, argv[0], &fd)) {
    return enif_make_badarg(env);
  }
    
  if (-1 == close(fd)) {
    t_key = enif_make_atom(env, atom_error);
    t_val = enif_make_int(env, errno);
    t_res = enif_make_tuple2(env, t_key, t_val);
  } else {
    t_res = enif_make_atom(env, atom_ok);
  }
  
  return t_res;
}

/* ---------------------------------------------------------------------
 * read a joystick event
 * input: fd
 * result: {error, errno}
 *         {ok, noevent}
 *         {ok, time, value, type, number}
 * --------------------------------------------------------------------- */
static ERL_NIF_TERM js_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  struct js_event jse;
  
  ERL_NIF_TERM t_key;
  ERL_NIF_TERM t_val;
  ERL_NIF_TERM t_time;
  ERL_NIF_TERM t_type;
  ERL_NIF_TERM t_number;
  ERL_NIF_TERM t_res;
  
  if (1 != argc) {
    return enif_make_badarg(env);
  }
  
  if (!enif_get_int(env, argv[0], &fd)) {
    return enif_make_badarg(env);
  }
    
  if (-1 == read(fd, &jse, sizeof(struct js_event))) {
    if (errno == EAGAIN) {
      t_key = enif_make_atom(env, atom_ok);
      t_val = enif_make_atom(env, atom_no_event);
    } else {
      t_key = enif_make_atom(env, atom_error);
      t_val = enif_make_int(env, errno);
    }
    t_res = enif_make_tuple2(env, t_key, t_val);
  } else {
    t_key    = enif_make_atom(env, atom_ok);
    t_time   = enif_make_int(env, jse.time);
    t_val    = enif_make_int(env, jse.value);
    t_type   = enif_make_int(env, jse.type);
    t_number = enif_make_int(env, jse.number);
    t_res    = enif_make_tuple5(env, t_key, t_time, t_val, t_type, t_number);
  }
  
  return t_res;
}

/* ---------------------------------------------------------------------
 * get joystick name
 * input: fd
 * result: {error, errno}
 *         {ok, Name}
 * --------------------------------------------------------------------- */
static ERL_NIF_TERM js_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  char name[128];  
  
  ERL_NIF_TERM t_key;
  ERL_NIF_TERM t_val;
  ERL_NIF_TERM t_res;
  
  if (1 != argc) {
    return enif_make_badarg(env);
  }
  
  if (!enif_get_int(env, argv[0], &fd)) {
    return enif_make_badarg(env);
  }
    
  if (-1 == ioctl(fd, JSIOCGNAME(sizeof(name)), name)) {
    t_key = enif_make_atom(env, atom_error);
    t_val = enif_make_int(env, errno);
  } else {
    t_key = enif_make_atom(env, atom_ok);
    t_val = enif_make_string(env, name, ERL_NIF_LATIN1);
  }
  
  t_res = enif_make_tuple2(env, t_key, t_val);
  return t_res;
}

/* ---------------------------------------------------------------------
 * get joystick version
 * input: fd
 * result: {error, errno}
 *         {ok, Version}
 * --------------------------------------------------------------------- */
static ERL_NIF_TERM js_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  int version;
  
  ERL_NIF_TERM t_key;
  ERL_NIF_TERM t_val;
  ERL_NIF_TERM t_res;
  
  if (1 != argc) {
    return enif_make_badarg(env);
  }
  
  if (!enif_get_int(env, argv[0], &fd)) {
    return enif_make_badarg(env);
  }
    
  if (-1 == ioctl(fd, JSIOCGVERSION, &version)) {
    t_key = enif_make_atom(env, atom_error);
    t_val = enif_make_int(env, errno);
  } else {
    t_key = enif_make_atom(env, atom_ok);
    t_val = enif_make_int(env, version);
  }
  
  t_res = enif_make_tuple2(env, t_key, t_val);
  return t_res;
}

/* ---------------------------------------------------------------------
 * get number of joystick axes
 * input: fd
 * result: {error, errno}
 *         {ok, Axes}
 * --------------------------------------------------------------------- */
static ERL_NIF_TERM js_axes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  char count_axes;
  
  ERL_NIF_TERM t_key;
  ERL_NIF_TERM t_val;
  ERL_NIF_TERM t_res;
  
  if (1 != argc) {
    return enif_make_badarg(env);
  }
  
  if (!enif_get_int(env, argv[0], &fd)) {
    return enif_make_badarg(env);
  }
    
  if (-1 == ioctl(fd, JSIOCGAXES, &count_axes)) {
    t_key = enif_make_atom(env, atom_error);
    t_val = enif_make_int(env, errno);
  } else {
    t_key = enif_make_atom(env, atom_ok);
    t_val = enif_make_int(env, count_axes);
  }
  
  t_res = enif_make_tuple2(env, t_key, t_val);
  return t_res;
}

/* ---------------------------------------------------------------------
 * get number of buttons
 * input: fd
 * result: {error, errno}
 *         {ok, Buttons}
 * --------------------------------------------------------------------- */
static ERL_NIF_TERM js_buttons(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  char count_buttons;
  
  ERL_NIF_TERM t_key;
  ERL_NIF_TERM t_val;
  ERL_NIF_TERM t_res;
  
  if (1 != argc) {
    return enif_make_badarg(env);
  }
  
  if (!enif_get_int(env, argv[0], &fd)) {
    return enif_make_badarg(env);
  }
    
  if (-1 == ioctl(fd, JSIOCGBUTTONS, &count_buttons)) {
    t_key = enif_make_atom(env, atom_error);
    t_val = enif_make_int(env, errno);
  } else {
    t_key = enif_make_atom(env, atom_ok);
    t_val = enif_make_int(env, count_buttons);
  }
  
  t_res = enif_make_tuple2(env, t_key, t_val);
  return t_res;
}

/* ---------------------------------------------------------------------
 * NIF mapping
 * --------------------------------------------------------------------- */
static ErlNifFunc nif_funcs[] = {
  { "jsnif_open",    1, js_open    },
  { "jsnif_close",   1, js_close   },
  { "jsnif_read",    1, js_read    },
  { "jsnif_name",    1, js_name    },
  { "jsnif_version", 1, js_version },
  { "jsnif_axes",    1, js_axes    },
  { "jsnif_buttons", 1, js_buttons }
};

/* ---------------------------------------------------------------------
 * NIF interface description
 * --------------------------------------------------------------------- */
ERL_NIF_INIT(joystick, nif_funcs, NULL, NULL, NULL, NULL)

/* =====================================================================
 * End Of File
 * ===================================================================== */
