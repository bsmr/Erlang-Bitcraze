#
# A basic Makefile for the "Erlang Bitcraze" project.
#

all:

EBINS		=	ebin/ebe.beam
PRIVS		=	priv/bccrusbexample
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

compile:	create_directories $(EBINS) $(PRIVS)	\
		priv/joystick				\
		priv/joystick.so
	@erl -make

ebin/ebe.beam:	src/ebe.erl

priv/bccrusbexample:	c_src/bccrusbexample.c
	gcc -s -Wall -O2 -o priv/bccrusbexample $(LIBUSB_CFLAGS) c_src/bccrusbexample.c $(LIBUSB_LIBS)

obj/joystick.o:		c_src/joystick.c
	gcc -Wall -O2 -o obj/joystick.o -c c_src/joystick.c

obj/joysticknif.o:	c_src/joysticknif.c
	gcc -fpic -shared -Wall -O2 -I$(ERL_INCS) -o obj/joysticknif.o -c c_src/joysticknif.c

priv/joystick:	obj/joystick.o
	gcc -s -o priv/joystick obj/joystick.o

priv/joystick.so:	obj/joysticknif.o
	gcc -s -fpic -shared -o priv/joystick.so obj/joysticknif.o

run-joystick: priv/joystick
	@priv/joystick

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
