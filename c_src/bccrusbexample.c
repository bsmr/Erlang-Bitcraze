/* #####################################################################
 *
 * Bitcraze Crazyradio USB Example
 *
 * ##################################################################### */

/* =====================================================================
 * header files
 * ===================================================================== */
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <libusb.h>

/* =====================================================================
 * forward declarations
 * ===================================================================== */
int main(int argc, char** argv);
void banner();
void discover_crazyradio();
bool is_crazyradio(libusb_device *device);

/* =====================================================================
 * constants
 * ===================================================================== */
static const int CrazyradioVersionID = 0x1915;
static const int CrazyradioProductID = 0x7777;

/* =====================================================================
 * main entry point
 * ===================================================================== */
int main(int argc, char** argv)
{
  int             r;
  bool            showBanner = true;
  libusb_context *context    = NULL;

  /* parse arguments */
  
  /* display banner */
  if (showBanner == true) {
    banner();
  }

  /* do something */
  r = libusb_init(&context);
  if (r == 0) {
    discover_crazyradio(context);
  }
  libusb_exit(context);
  
  /* terminate... */
  return 0;
}

/* =====================================================================
 * display the programs banner
 * ===================================================================== */
void banner()
{
  puts("Bitcraze Crazyradio USB Example");
}

/* =====================================================================
 * discover the Crazyradio USB dongle
 * ===================================================================== */
void discover_crazyradio(libusb_context *context)
{
  libusb_device **list;
  libusb_device *found = NULL;
  ssize_t cnt;
  ssize_t i = 0;
  int err = 0;
  
  cnt = libusb_get_device_list(context, &list);
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
 * check if USB device is the Crazyradio dongle
 * ===================================================================== */
bool is_crazyradio(libusb_device *device)
{
  struct libusb_device_descriptor desc;
  int version_major;
  int version_minor;
  uint8_t port_numbers[8];
  int port_count;
  int i;
  
  if(device == NULL) return false;
  
  int r = libusb_get_device_descriptor(device, &desc);
  if (r < 0) {
    fprintf(stderr, "Error: failed to get device descriptor!\n");
    return false;
  }
  
  if(desc.idVendor != CrazyradioVersionID) {
    return false;
  }
  
  if(desc.idProduct != CrazyradioProductID) {
    return false;
  }

  version_major = desc.bcdDevice >> 8;
  version_minor = desc.bcdDevice & 0x0FF;

  fprintf(stderr,
	  "Info: found Crazyradio: bus %d, device %d, version %x.%x.\n",
	  libusb_get_bus_number(device),
	  libusb_get_device_address(device),
          version_major, version_minor);
  
  port_count = libusb_get_port_numbers(device, port_numbers, 8);
  if (port_count == LIBUSB_ERROR_OVERFLOW) {
  } else {
    fprintf(stderr, "Info: port count = %d\n", port_count);
    for(i = 0; i < port_count; i++) {
      fprintf(stderr, " port[%d] = %d\n", i, port_numbers[i]);
    }
  }
  
  return true;
}

/* #####################################################################
 *
 * End Of File
 *
 * ##################################################################### */
