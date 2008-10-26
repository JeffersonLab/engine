.DELETE_ON_ERROR: ; 

# To disable compilation of CTP Root Trees, make sure that the environment
# variable ROOTSYS is undefined, or uncomment "#ROOTSYS=" line below.
# The same must be done in CTP/Makefile.Unix

ROOTSYS=

disp_objs = glvolu.o 

Csoft=$(PWD)

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
CODALIB = $(LIBROOT)/libcoda.a
ifdef ROOTSYS
  CTPLIB = $(LIBROOT)/libctp_root.a
  CTPCLIENTLIB = $(LIBROOT)/libctpclient_root.a
  OTHERLIBS = $(shell $(ROOTSYS)/bin/root-config --libs)
else
  CTPLIB = $(LIBROOT)/libctp.a
  CTPCLIENTLIB = $(LIBROOT)/libctpclient.a
  OTHERLIBS=
endif
TRACKINGLIB = $(LIBROOT)/libtracking.a
HTRACKINGLIB = $(LIBROOT)/libhtracking.a
STRACKINGLIB = $(LIBROOT)/libstracking.a
BTRACKINGLIB = $(LIBROOT)/libbtracking.a
SANETRACKINGLIB = $(LIBROOT)/libsanetracking.a
F1TRIGGER = $(LIBROOT)/libf1trigger.a
SEM = $(LIBROOT)/libsem.a
HACKLIB = $(LIBROOT)/libhack.a
CERNLIBS = -lgeant$(GEANTVER) -lpawlib -lgraflib -lgrafX11 -lpacklib -lmathlib
OURGENLIBS = $(ENGINELIB) $(HTRACKINGLIB) $(STRACKINGLIB) $(TRACKINGLIB) \
	$(HACKLIB) $(UTILLIB) $(CODALIB) $(CTPLIB) $(CTPCLIENTLIB) \
	$(BTRACKINGLIB) $(SANETRACKINGLIB) $(F1TRIGGER) $(SEM)
MAKEREG=$(Csoft)/../$(MYOS)/bin/makereg

ifeq ($(MYOS),HPUX)
  FFLAGS=+U77 +ppu -C +es -O +Onolimit +FPVZOU -I$(Csoft)/INCLUDE
  DISPFLAGS=+U77 +ppu +es -O +Onolimit +FPVZOU -I$(Csoft)/INCLUDE
  LDFLAGS=-Wl,-a archive
  OTHERLIBS += \
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
  OTHERLIBS += -L$(CODA)/ULTRIX/lib \
	-lana -lmsg -lcoda -L$(CERN_ROOT)/lib -lpacklib
  OURLIBS := $(OURGENLIBS)
endif

ifeq ($(MYOS),OSF1)
  FFLAGS=-non_shared -check_bounds -align dcommons
  DISPFLAGS=$(FFLAGS)
  LDFLAGS=
  OTHERLIBS += -L$(CERN_ROOT)/lib -lpacklib
  OURLIBS := $(OURGENLIBS)
endif

ifeq ($(MYOS),Linux)  
   override FFLAGS += -I$(Csoft)/INCLUDE
   DISPFLAGS=$(FFLAGS)
   ifeq ($(MYREALOS),Darwin)
      OTHERLIBS += -L$(CERN_ROOT)/lib -lpacklib -lc -lm
   else
      OTHERLIBS += -L$(CERN_ROOT)/lib -lpacklib -lc -lm -lnsl
   endif 
   OURLIBS := $(OURGENLIBS) $(LIBROOT)/libport.a
endif

ifeq ($(MYOS),SunOS)
  FFLAGS=-e -O -I$(Csoft)/INCLUDE
  DISPFLAGS=$(FFLAGS)
  ifeq ($(MYOS),SunOS4)
    OTHERLIBS += -L$(CERN_ROOT)/lib $(CERNLIBS) -lnsl -lX11
  else
    OTHERLIBS += -L$(CERN_ROOT)/lib $(CERNLIBS) -lnsl -lsocket -lX11
  endif
  OURLIBS := $(OURGENLIBS)
  ifndef CERN_ROOT
    CERN_ROOT=/apps/cernlib/sun4_solaris2/97a
  endif
endif
 
ifeq ($(MYOS),AIX)
  FC=f77
  FFLAGS=-g -qfixed=132 -qextname -O -I$(Csoft)/INCLUDE
  DISPFLAGS=$(FFLAGS)
  OTHERLIBS += -L$(CERN_ROOT)/lib -lpacklib $(CERNLIBS) -lX11
  OURLIBS := $(OURGENLIBS) $(LIBROOT)/libport.a
endif

# There are no r_%.f files used or built -- this rule isn't used
r_%.f : %.cmn $(MAKEREG)
	$(MAKEREG) $< -o $@ -e /dev/null

.PHONY: all engine_replay clean
.PRECIOUS: r_%.f

all: engine_replay

display: evdisplay

glvolu.o: glvolu.f
	$(FC) $(FFLAGS) -c $(DISPFLAGS) $<

evdisplay.o: evdisplay.f
	$(FC) $(FFLAGS) -c $(DISPFLAGS) $<

%.o: %.f
	$(FC) $(FFLAGS) -c $< -o $@

engine_replay_$(MYOS): Makefile engine.o  $(OURLIBS)
	$(FC) -o $@ $(FFLAGS) engine.o $(OURLIBS) $(OTHERLIBS)

engine_replay: engine_replay_$(MYOS)
	$(RM) ../../$(MYOS)/bin/engine_replay
#	ln -s $< $@
	$(CP) engine_replay_$(MYOS) ../../$(MYOS)/bin/engine_replay
	$(RM) engine_replay_$(MYOS)

.INTERMEDIATE: engine.o
engine.o: ../ENGINE/O.$(MYOS)/engine.o
	$(CP) ../ENGINE/O.$(MYOS)/engine.o engine.o


$(OURLIBS):
	$(CP) ../ENGINE/O.$(MYOS)/engine.o engine.o

evdisplay: $(disp_objs) Makefile evdisplay.o $(disp_objs) $(ONEEVLIB) $(OURLIBS) 
	$(FC) -o $@ $(DISPFLAGS) evdisplay.o $(disp_objs) $(ONEEVLIB) $(OURLIBS) $(OTHERLIBS)

clean:
	$(RM) *.o r_*.f engine_replay engine_replay_$(MYOS) evdisplay
