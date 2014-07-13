#
# A basic Makefile for the "Erlang Bitcraze" project.
#

all:

EBINS	=	ebin/ebe.beam

compile:	$(EBINS)
	@mkdir -p "ebin"
	@erl -make

ebin/ebe.beam:	src/ebe.erl

clean:
	@rm -f *~
	@rm -f src/*~

nuke: clean
	@rm -f ebin/*

#
# End Of File
#
