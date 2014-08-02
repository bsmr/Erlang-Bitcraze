#
# A basic Makefile for the "Erlang Bitcraze" project.
#

all:

EBINS		=	ebin/ebe.beam ebin/joystick.beam
PRIVS		=	priv/bccrusbexample priv/joystick.so
LIBUSB_CFLAGS	=	-I/usr/include/libusb-1.0
#LIBUSB_LIBS	=	-L/usr/lib/x86_64-linux-gnu -lusb-1.0
LIBUSB_LIBS	=	-lusb-1.0

ERL_ROOT	?=	/opt/erlang/otp/active/lib/erlang
ERL_INCS	=	$(ERL_ROOT)/usr/include
ERL_LIBS	=	$(ERL_ROOT)/usr/lib

create_directories:
	@mkdir -p "ebin"
	@mkdir -p "priv"
	@mkdir -p "obj"

compile:	create_directories $(EBINS) $(PRIVS)
	@erl -make

ebin/ebe.beam:	src/ebe.erl

ebin/joystick.beam:	src/joystick.erl priv/joystick.so

priv/bccrusbexample:	c_src/bccrusbexample.c
	gcc -s -Wall -O2 -o $@ $(LIBUSB_CFLAGS) $< $(LIBUSB_LIBS)

obj/joystick.o:	c_src/joystick.c
	gcc -fpic -shared -Wall -O2 -I$(ERL_INCS) -o $@ -c $<

priv/joystick.so:	obj/joystick.o
	gcc -s -fpic -shared -o $@ $^

clean:
	@rm -f *~
	@rm -f src/*~
	@rm -f c_src/*~

nuke: clean
	@rm -f ebin/*
	@rm -f priv/*
	@rm -f obj/*

ubuntu-dependencies:
	sudo apt-get install libusb-1.0-0-dev

#
# End Of File
#
