
disp_objs = glvolu.o 

Csoft=$(PWD)

RM        = rm -f 
SHELL     = /bin/sh

MYOS := $(subst -,,$(shell uname))
ifeq ($(MYOS),HPUX)
  ifneq (,$(findstring 09,$(shell uname -r)))
    HPUXVERSION := 09
  else
    HPUXVERSION := 10
  endif
  LIBROOT = $(Csoft)/../$(MYOS)$(HPUXVERSION)/lib
else
  LIBROOT = $(Csoft)/../$(MYOS)/lib
endif

ONEEVLIB = $(LIBROOT)/liboneev.a
ENGINELIB = $(LIBROOT)/libengine.a
UTILLIB = $(LIBROOT)/libutils.a
CTPLIB = $(LIBROOT)/libctp.a
CTPCLIENTLIB = $(LIBROOT)/libctpclient.a
TRACKINGLIB = $(LIBROOT)/libtracking.a
HTRACKINGLIB = $(LIBROOT)/libhtracking.a
STRACKINGLIB = $(LIBROOT)/libstracking.a
HACKLIB = $(LIBROOT)/libhack.a
CERNLIBS = -lgeant$(GEANTVER) -lpawlib -lgraflib -lgrafX11 -lpacklib -lmathlib
OURGENLIBS = $(ENGINELIB) $(HTRACKINGLIB) $(STRACKINGLIB) $(TRACKINGLIB) \
	$(HACKLIB) $(UTILLIB) $(CTPLIB) $(CTPCLIENTLIB) 
MAKEREG=$(Csoft)/../$(MYOS)/bin/makereg

ifeq ($(MYOS),HPUX)
  FFLAGS=+U77 +ppu -C +es -O +Onolimit +FPVZOU -I$(Csoft)/INCLUDE
  DISPFLAGS=+U77 +ppu +es -O +Onolimit +FPVZOU -I$(Csoft)/INCLUDE
  LDFLAGS=-Wl,-a archive
  OTHERLIBS = \
	-Wl,-L$(CODA)/HP_UX/lib \
	-Wl,-L$(CERN_ROOT)/lib -lpacklib $(CERNLIBS) \
	-Wl,-L/usr/lib/X11R5 -lX11 -lm 
  MAKEREG=$(Csoft)/../$(MYOS)$(HPUXVERSION)/bin/makereg
  OURLIBS := $(OURGENLIBS)
endif

ifeq ($(MYOS),ULTRIX)
  FFLAGS=-check_bounds
  DISPFLAGS=$(FFLAGS)
  LDFLAGS=
  OTHERLIBS = -L$(CODA)/ULTRIX/lib \
	-lana -lmsg -lcoda -L$(CERN_ROOT)/lib -lpacklib
  OURLIBS := $(OURGENLIBS)
endif

ifeq ($(MYOS),OSF1)
  FFLAGS=-non_shared -check_bounds -align dcommons
  DISPFLAGS=$(FFLAGS)
  LDFLAGS=
  OTHERLIBS = -L$(CERN_ROOT)/lib -lpacklib
  OURLIBS := $(OURGENLIBS)
endif

ifeq ($(MYOS),Linux)  

   F77=g77
   FFLAGS=-g -I$(Csoft)/INCLUDE -ffixed-line-length-132
   DISPFLAGS=$(FFLAGS)
   OTHERLIBS = -L$(CERN_ROOT)/lib -lpacklib -lc -lm -lnsl
   OURLIBS := $(OURGENLIBS) $(LIBROOT)/libport.a

endif

ifeq ($(MYOS),SunOS)
  FFLAGS=-e -O -I$(Csoft)/INCLUDE
  DISPFLAGS=$(FFLAGS)
  ifeq ($(OSTYPE),SunOS4)
    OTHERLIBS = -L$(CERN_ROOT)/lib $(CERNLIBS) -lnsl -lX11
  else
    OTHERLIBS = -L$(CERN_ROOT)/lib $(CERNLIBS) -lnsl -lsocket -lX11
  endif
  OURLIBS := $(OURGENLIBS)
  ifndef CERN_ROOT
    CERN_ROOT=/apps/cernlib/sun4_solaris2/97a
  endif
endif
 
ifeq ($(MYOS),AIX)
  F77=f77
  FFLAGS=-g -qfixed=132 -qextname -O -I$(Csoft)/INCLUDE
  DISPFLAGS=$(FFLAGS)
  OTHERLIBS = -L$(CERN_ROOT)/lib -lpacklib $(CERNLIBS) -lX11
  OURLIBS := $(OURGENLIBS) $(LIBROOT)/libport.a
endif

r_%.f : %.cmn $(MAKEREG)
	$(MAKEREG) $< -o $@ -e /dev/null

.PRECIOUS: r_%.f

all: engine_replay

display: evdisplay

glvolu.o: glvolu.f
	$(F77) $(FFLAGS) -c $(DISPFLAGS) $<

evdisplay.o: evdisplay.f
	$(F77) $(FFLAGS) -c $(DISPFLAGS) $<

%.o: %.f
	$(F77) $(FFLAGS) -c $< -o $@

engine_replay_$(MYOS): Makefile  ../ENGINE/O.$(MYOS)/engine.o  $(OURLIBS)
	echo $(MYVERSION)
	cp ../ENGINE/O.$(MYOS)/engine.o engine.o
	$(F77) -o $@ $(FFLAGS) engine.o $(OURLIBS) $(OTHERLIBS)
	$(RM) engine.o

engine_replay: engine_replay_$(MYOS)
	$(RM) ../../$(MYOS)/bin/engine_replay
#	ln -s $< $@
	cp engine_replay_$(MYOS) ../../$(MYOS)/bin/engine_replay
	$(RM) engine_replay_$(MYOS)
	echo ************** READ this ***********
	echo **** Please remember to compile code *****
	echo **** on Sun and Linux machines **********
	echo ********** READ THIS ***************
engine.o:
	cp ../ENGINE/O.$(MYOS)/engine.o engine.o

$(OURLIBS):
	cp ../ENGINE/O.$(MYOS)/engine.o engine.o

evdisplay: $(disp_objs) Makefile evdisplay.o $(disp_objs) $(ONEEVLIB) $(OURLIBS) 
	$(F77) -o $@ $(DISPFLAGS) evdisplay.o $(disp_objs) $(ONEEVLIB) $(OURLIBS) $(OTHERLIBS)

clean:
	$(RM) *.o r_*.f engine_replay engine_replay_$(MYOS) evdisplay









