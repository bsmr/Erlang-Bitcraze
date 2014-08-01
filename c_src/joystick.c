/* #####################################################################
 *
 * A small test program for the Linux Joystick API.
 * This is just a hack, to see what a Sony Dualshock 3 Sixaxis returns.
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
      if (-1 == read(fdjs, &jse, sizeof(struct js_event))) {
	if (errno == EAGAIN) {
	  //fprintf(stderr, "no event\n");
	} else {
	  fprintf(stderr, "Error: read() failed with errno %d!\n", errno);
	  loop = 0;
	}
      } else {
	joystick_event(jse);
      }
    }
    if (-1 == close(fdjs)) {
      fprintf(stderr, "Error: failed to close() with errno %d!\n", errno);
    }
  } else {
    fprintf(stderr, "Error: failed to open joystick device wth errno %d!\n", errno);
    exit(1);
  }
}

/* ---------------------------------------------------------------------
 * get the joystick name and some more information
 * --------------------------------------------------------------------- */
void joystick_name_version(int fd)
{
  char name[128];
  int  version;
  char count_axis;
  char count_buttons;
  
  if (-1 == ioctl(fd, JSIOCGNAME(sizeof(name)), name)) {
    fprintf(stderr, "Failed to get name of joystick!\n");
  } else {
    fprintf(stderr, "Joystick name: %s\n", name);
  }

  if (-1 == ioctl(fd, JSIOCGVERSION, &version)) {
    fprintf(stderr, "Failed to get version of joystick driver!\n");
  } else {
    fprintf(stderr, "Joystick version: %d\n", version);
  }

  if (-1 == ioctl(fd, JSIOCGAXES, &count_axis)) {
    fprintf(stderr, "Failed to get axis count!\n");
  } else {
    fprintf(stderr, "Axis count: %d\n", count_axis);
  }

  if (-1 == ioctl(fd, JSIOCGBUTTONS, &count_buttons)) {
    fprintf(stderr, "Failed to get button count!\n");
  } else {
    fprintf(stderr, "Button count: %d\n", count_buttons);
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
    fprintf(stderr, "js - SELECT button - %d - %d - %d - %d\n", e.time, e.value, e.type, e.number);
    break;
  case 1:
    /* left joystick press */
    break;
  case 2:
    /* right joystick press */
    break;
  case 3:
    /* START */
    fprintf(stderr, "js - START button - %d - %d - %d - %d\n", e.time, e.value, e.type, e.number);
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
