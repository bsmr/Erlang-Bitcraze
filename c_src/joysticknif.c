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
 * open a (joystick-)device
 * input: joystick device name (string)
 * result: {error, errno}
 *         {ok, fd}
 * --------------------------------------------------------------------- */
static ERL_NIF_TERM open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  const char pathname[] = "/dev/input/js0";
  
  fd = open(pathname, O_RDONLY | O_NONBLOCK);
  if (-1 == fd) {
    /* error */
  } else {
    /* ok, good to go */
  }
}

/* ---------------------------------------------------------------------
 * close a (joystick-)device
 * input: fd
 * result: {error, errno}
 *         ok
 * --------------------------------------------------------------------- */
static ERL_NIF_TERM close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  if (-1 == close(fd)) {
    /* error: errno has error */
  } else {
    /* ok, closed */
  }
}

/* ---------------------------------------------------------------------
 * read a joystick event
 * input: fd
 * result: {error, errno}
 *         {ok, noevent}
 *         {ok, time, value, type, number}
 * --------------------------------------------------------------------- */
static ERL_NIF_TERM read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  struct js_event jse;
  if (-1 == read(fd, &jse, sizeof(struct js_event))) {
    if (errno == EAGAIN) {
      /* no event */
    } else {
      /* error */
    }
  } else {
    /* good read */
    /* fprintf(stderr, "js - axis unknown - %d - %d - %d - %d\n", jse.time, jse.value, jse.type, jse.number); */
  }
}

/* ---------------------------------------------------------------------
 * get joystick name
 * input: fd
 * result: {error, errno}
 *         {ok, Name}
 * --------------------------------------------------------------------- */
static ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  char name[128];
  
  if (-1 == ioctl(fd, JSIOCGNAME(sizeof(name)), name)) {
    fprintf(stderr, "Failed to get name of joystick!\n");
  } else {
    fprintf(stderr, "Joystick name: %s\n", name);
  }
}

/* ---------------------------------------------------------------------
 * get joystick version
 * input: fd
 * result: {error, errno}
 *         {ok, Version}
 * --------------------------------------------------------------------- */
static ERL_NIF_TERM version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  int version;
  if (-1 == ioctl(fd, JSIOCGVERSION, &version)) {
    fprintf(stderr, "Failed to get version of joystick driver!\n");
  } else {
    fprintf(stderr, "Joystick version: %d\n", version);
  }
}

/* ---------------------------------------------------------------------
 * get number of joystick axes
 * input: fd
 * result: {error, errno}
 *         {ok, Axes}
 * --------------------------------------------------------------------- */
static ERL_NIF_TERM axes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  char count_axes;
  if (-1 == ioctl(fd, JSIOCGAXES, &count_axes)) {
    fprintf(stderr, "Failed to get axes count!\n");
  } else {
    fprintf(stderr, "Axis count: %d\n", count_axes);
  }
}

/* ---------------------------------------------------------------------
 * get number of buttons
 * input: fd
 * result: {error, errno}
 *         {ok, Buttons}
 * --------------------------------------------------------------------- */
static ERL_NIF_TERM buttons(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  char count_buttons;
  if (-1 == ioctl(fd, JSIOCGBUTTONS, &count_buttons)) {
    fprintf(stderr, "Failed to get button count!\n");
  } else {
    fprintf(stderr, "Button count: %d\n", count_buttons);
  }
}

/* ---------------------------------------------------------------------
 * NIF mapping
 * --------------------------------------------------------------------- */
static ErlNifFunc nif_funcs[] = {
  { "jsnif_open",    1, open    },
  { "jsnif_close",   1, close   },
  { "jsnif_read",    1, read    },
  { "jsnif_name",    1, name    },
  { "jsnif_version", 1, version },
  { "jsnif_axes",    1, axes    },
  { "jsnif_buttons", 1, buttons }
};

/* ---------------------------------------------------------------------
 * NIF interface description
 * --------------------------------------------------------------------- */
ERL_NIF_INIT(joysticknif, nif_funcs, NULL, NULL, NULL, NULL)

/* =====================================================================
 * End Of File
 * ===================================================================== */
