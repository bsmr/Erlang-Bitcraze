#
# A basic Makefile for the "Erlang Bitcraze" project.
#

all:

EBINS		=							\
			ebin/ebe.beam					\
			ebin/joystick.app				\
			ebin/joystick.beam				\
			ebin/joystick_app.beam				\
			ebin/joystick_sup.beam				\
			ebin/joystick_srv.beam				\
			ebin/joystick_example.beam

PRIVS		=	priv/bccrusbexample				\
			priv/joystick.so
LIBUSB_CFLAGS	=	-I/usr/include/libusb-1.0
#LIBUSB_LIBS	=	-L/usr/lib/x86_64-linux-gnu -lusb-1.0
LIBUSB_LIBS	=	-lusb-1.0

ERL_ROOT	?=	/opt/erlang/otp/active/lib/erlang
ERL_INCS	=	$(ERL_ROOT)/usr/include
ERL_LIBS	=	$(ERL_ROOT)/usr/lib

ebin/%.beam:	src/%.erl
	erlc -I include -o ebin -v $<

create_directories:
	@mkdir -p "ebin"
	@mkdir -p "priv"
	@mkdir -p "obj"

run:	compile
	erl -pa ebin -run joystick_example run

compile:	create_directories $(EBINS) $(PRIVS)

ebin/ebe.beam:	src/ebe.erl

ebin/joystick.app:		src/joystick.app.src
	cat <"$<" >"$@"
ebin/joystick.beam:		src/joystick.erl	priv/joystick.so
ebin/joystick_app.beam:		src/joystick_app.erl
ebin/joystick_sup.beam: 	src/joystick_sup.erl
ebin/joystick_srv.beam:		src/joystick_srv.erl
ebin/joystick_example.beam:	src/joystick_example.erl

priv/bccrusbexample:	c_src/bccrusbexample.c
	gcc -s -Wall -O2 -o $@ $(LIBUSB_CFLAGS) $< $(LIBUSB_LIBS)

obj/joystick.po:	c_src/joystick.c
	gcc -fpic -shared -Wall -O2 -I$(ERL_INCS) -o $@ -c $<

priv/joystick.so:	obj/joystick.po
	gcc -s -fpic -shared -o $@ $^

clean:
	@rm -f *~
	@rm -f src/*~
	@rm -f c_src/*~

nuke: clean
	@rm -f ebin/*
	@rm -f priv/*
	@rm -f obj/*
	@rm -f erl_crash.dump
	@rm -f core

ubuntu-dependencies:
	sudo apt-get install libusb-1.0-0-dev

#
# End Of File
#
