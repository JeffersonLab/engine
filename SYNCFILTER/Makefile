.DELETE_ON_ERROR: ;

include ../etc/Makefile.variables

.PHONY: all

override CFLAGS += -L../../$(MYOS)/lib

all: ../../$(MYOS)/bin/syncfilter

../../$(MYOS)/bin/syncfilter: syncfilter.c ../../$(MYOS)/lib/libcoda.a
	$(CC) $(CFLAGS) -o syncfilter syncfilter.c -lcoda
	$(CP) syncfilter ../../$(MYOS)/bin/syncfilter
	$(RM) syncfilter
