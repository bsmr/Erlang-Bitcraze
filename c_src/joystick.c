/* #####################################################################
 *
 * An Erlang/OTP C Node to interface a joxstick.
 *
 * compile:
 * > gcc -s -Wall -O3 -o priv/joystick c_src/joystick.c
 *
 * This is currently just a hack, to see what a Sony Dualshock 3 Sixaxis
 * returns.
 *
 * Notes for "PLAYSTATION(R)3 Controller":
 * - ioctl() for version, number of axis, and number of buttons does not
 *   work!
 * - the 1st bulk of "events" (with JS_EVENT_INIT) reports the state for
 *   17 buttons (0..16) and 29 axis (0..28), but I could not see any
 *   events for axis 20..28, yet.
 *
 * ##################################################################### */

/* =====================================================================
 *
 * ===================================================================== */

/* ---------------------------------------------------------------------
 *
 * --------------------------------------------------------------------- */

/* ---------------------------------------------------------------------
 * header files
 * --------------------------------------------------------------------- */
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

#include <linux/joystick.h>

/* ---------------------------------------------------------------------
 * function prototypes
 * --------------------------------------------------------------------- */
int main(int argc, char** argv);
void hack(void);
void joystick_name_version(int fd);
void joystick_event(struct js_event e);
void joystick_event_button(struct js_event e);
void joystick_event_axis(struct js_event e);

/* ---------------------------------------------------------------------
 * main entry point
 * --------------------------------------------------------------------- */
int main(int argc, char** argv)
{
  /* --- parse arguments ----------------------------------------------- */
  /* --- setup environment --------------------------------------------- */
  /* --- loop until done ----------------------------------------------- */
  
  /* --- just for 1st testing ------------------------------------------ */
  hack();

  /* --- terminate gracefully ------------------------------------------ */
  return 0;
}

/* ---------------------------------------------------------------------
 * just a hack, to get a basic understanding of the joystick API
 * --------------------------------------------------------------------- */
void hack(void)
{
  int fdjs;
  int loop = 1;
  struct js_event jse;

  fdjs = open("/dev/input/js0", O_RDONLY | O_NONBLOCK);
  if (0 < fdjs) {
    
    joystick_name_version(fdjs);
    
    while (loop) {
      while (0 < read(fdjs, &jse, sizeof(struct js_event))) {
	joystick_event(jse);
      }
      
      /* check if we got an error */
      if (errno != EAGAIN) {
	/* TODO: add error handling/information */
      }
    }
    close(fdjs);
  } else {
    fprintf(stderr, "Error: failed to open joystick device!\n");
    /* TODO: add error information */
  }
}

/* ---------------------------------------------------------------------
 * get the joystick name and some more information
 * --------------------------------------------------------------------- */
void joystick_name_version(int fd)
{
  char name[128];
  int version;
  char count_axis;
  char count_buttons;
  
  if (0 < ioctl(fd, JSIOCGNAME(sizeof(name)), name)) {
    fprintf(stderr, "Joystick name: %s\n", name);
  } else {
    fprintf(stderr, "Failed to get name of joystick!\n");
  }

  if (0 < ioctl(fd, JSIOCGVERSION, &version)) {
    fprintf(stderr, "Joystick version: %d\n", version);
  } else {
    fprintf(stderr, "Failed to get version of joystick driver!\n");
  }

  if (0 < ioctl(fd, JSIOCGAXES, &count_axis)) {
    fprintf(stderr, "Axis count: %d\n", count_axis);
  } else {
    fprintf(stderr, "Failed to get axis count!\n");
  }

  if (0 < ioctl(fd, JSIOCGBUTTONS, &count_buttons)) {
    fprintf(stderr, "Button count: %d\n", count_buttons);
  } else {
    fprintf(stderr, "Failed to get button count!\n");
  }
}

/* ---------------------------------------------------------------------
 * handle a joystick event
 * --------------------------------------------------------------------- */
void joystick_event(struct js_event e)
{
  switch (e.type) {
  case JS_EVENT_AXIS:
    joystick_event_axis(e);
    break;
  case JS_EVENT_BUTTON:
    joystick_event_button(e);
    break;
  default:
    fprintf(stderr, "js - unknown - %d - %d - %d - %d\n", e.time, e.value, e.type, e.number);
  }
}

/* ---------------------------------------------------------------------
 * handle a joysitck button event
 * --------------------------------------------------------------------- */
void joystick_event_button(struct js_event e)
{
  switch (e.number) {
  case 0:
    /* SELECT */
    break;
  case 1:
    /* left joystick press */
    break;
  case 2:
    /* right joystick press */
    break;
  case 3:
    /* START */
    break;
  case 4:
    /* left up */
    break;
  case 5:
    /* left right */
    break;
  case 6:
    /* left down */
    break;
  case 7:
    /* left left */
    break;
  case 8:
    /* L2 */
    break;
  case 9:
    /* R2 */
    break;
  case 10:
    /* L1 */
    break;
  case 11:
    /* R1 */
    break;
  case 12:
    /* right green triangle */
    break;
  case 13:
    /* right red circle */
    break;
  case 14:
    /* right grey X */
    break;
  case 15:
    /* right pink square */
    break;
  case 16:
    /* PS */
    break;
  default:
    fprintf(stderr, "js - button unknown - %d - %d - %d - %d\n", e.time, e.value, e.type, e.number);
  }
}

/* ---------------------------------------------------------------------
 * handle a joystick axis event
 * --------------------------------------------------------------------- */
void joystick_event_axis(struct js_event e)
{
  switch (e.number) {
  case 0:
  case 1:
    /* left joystick */
    break;
  case 2:
  case 3:
    /* right joystick */
    break;
  case 4:
  case 5:
  case 6: /* controller  */
    /* posistion of the controller */
    break;
  case 8:
    /* left up */
    break;
  case 9:
    /* left right */
    break;
  case 10:
    /* left down */
    break;
  case 11:
    /* left left */
    break;
  case 12:
    /* L2 */
    break;
  case 13:
    /* R2 */
    break;
  case 14:
    /* L1 */
    break;
  case 15:
    /* R1 */
    break;
  case 16:
    /* right green triangle */
    break;
  case 17:
    /* right red circle */
    break;
  case 18:
    /* right grey X */
    break;
  case 19:
    /* right ping square */
    break;
  default:
    fprintf(stderr, "js - axis unknown - %d - %d - %d - %d\n", e.time, e.value, e.type, e.number);
  }
}

/* #####################################################################
 *
 * End Of File
 *
 * ##################################################################### */
