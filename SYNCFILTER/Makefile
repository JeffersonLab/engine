#MYOS := $(subst -,,$(shell uname))

CFLAGS=-L../../$(MYOS)/lib

CC=gcc

all: syncfilter

syncfilter: syncfilter.c ../../$(MYOS)/lib/libcoda.a

	$(CC) $(CFLAGS) -o syncfilter syncfilter.c -lcoda
	$(CP) syncfilter ../../$(MYOS)/bin/syncfilter
	$(RM) syncfilter
