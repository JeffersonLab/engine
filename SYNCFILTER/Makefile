MYOS := $(subst -,,$(shell uname))

CFLAGS=-L../../$(MYOS)/lib

CC=gcc

all: syncfilter

syncfilter: syncfilter.c ../../$(MYOS)/lib/libcoda.a

	gcc $(CFLAGS) -o syncfilter syncfilter.c -lcoda
	cp  syncfilter ../../$(MYOS)/bin/syncfilter
	rm syncfilter