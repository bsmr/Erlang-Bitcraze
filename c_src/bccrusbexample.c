/* #####################################################################
 *
 * Bitcraze Crazyradio USB Example
 *
 * ##################################################################### */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <libusb.h>

/* =====================================================================
 *
 * forward declarations
 *
 * ===================================================================== */

int main(int argc, char** argv);
void banner();
void discover_crazyradio();
bool is_crazyradio(libusb_device *device);

/* =====================================================================
 *
 * main entry point
 *
 * ===================================================================== */

int main(int argc, char** argv)
{
  bool showBanner = true;
  int r;

  /* parse arguments */
  
  /* display banner */
  if (showBanner == true) {
    banner();
  }

  /* do something */
  r = libusb_init(NULL);
  if (r < 0) {
    return r;
  }
  discover_crazyradio();
  libusb_exit(NULL);
  
  /* terminate... */
  return 0;
}

/* =====================================================================
 *
 * display the programs banner
 *
 * ===================================================================== */

void banner()
{
  puts("Bitcraze Crazyradio USB Example");
}

/* =====================================================================
 *
 * discover the Crazyradio USB dongle
 *
 * ===================================================================== */

void discover_crazyradio()
{
  libusb_device **list;
  libusb_device *found = NULL;
  ssize_t cnt = libusb_get_device_list(NULL, &list);
  ssize_t i = 0;
  int err = 0;

  if (cnt < 0) {
    fprintf(stderr, "Error: can not find any USB devices!\n");
    goto cleanup;
  }
  
  for (i = 0; i < cnt; i++) {
    libusb_device *device = list[i];
    if (is_crazyradio(device)) {
      found = device;
      break;
    }
  }
  
  if (found) {
    libusb_device_handle *handle;
    err = libusb_open(found, &handle);
    if (err) {
      fprintf(stderr, "Error: can not open Crazyflie!\n");
      goto cleanup;
    }
  }
  
 cleanup:
  libusb_free_device_list(list, 1);
}

/* =====================================================================
 *
 * check if USB device is the Crazyradio dongle
 *
 * ===================================================================== */

bool is_crazyradio(libusb_device *device)
{
  struct libusb_device_descriptor desc;
  
  if(device == NULL) return false;
  
  int r = libusb_get_device_descriptor(device, &desc);
  if (r < 0) {
    fprintf(stderr, "Error: failed to get device descriptor!\n");
    return false;
  }
  
  if(desc.idVendor != 0x1915) {
    return false;
  }
  
  if(desc.idProduct != 0x7777) {
    return false;
  }
  
  fprintf(stderr,
	  "Info: found Crazyradio: bus %d, device %d.\n",
	  libusb_get_bus_number(device),
	  libusb_get_device_address(device));
  
  return true;
}

/* #####################################################################
 *
 * End Of File
 *
 * ##################################################################### */
