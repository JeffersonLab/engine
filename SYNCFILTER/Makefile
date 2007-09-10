.DELETE_ON_ERROR: ;

.PHONY: all

override CFLAGS += -L../../$(MYOS)/lib

all: syncfilter

syncfilter: syncfilter.c ../../$(MYOS)/lib/libcoda.a
	$(CC) $(CFLAGS) -o syncfilter syncfilter.c -lcoda
	$(CP) syncfilter ../../$(MYOS)/bin/syncfilter
	$(RM) syncfilter
