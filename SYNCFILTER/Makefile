MYOS := $(subst -,,$(shell uname))

CFLAGS=-L../../$(MYOS)/lib

CC=gcc

all: syncfilter

syncfilter: syncfilter.c ../../$(MYOS)/lib/libctp.a

	gcc $(CFLAGS) -o syncfilter syncfilter.c -lctp
	cp  syncfilter ../../$(MYOS)/bin/syncfilter
	rm syncfilter