MYOS := $(subst -,,$(shell uname))

CFLAGS=-L../../$(MYOS)/lib

CC=gcc

all: syncfilter

syncfilter: syncfilter.o ../../$(MYOS)/lib/libctp.a

	gcc $(CFLAGS) -o syncfilter syncfilter.o -lctp
	cp  syncfilter ../../$(MYOS)/bin/syncfilter



