#
# $Log$
# Revision 1.1  1994/07/07 15:25:03  cdaq
# Initial revision
#
#
include $(Csoft)/etc/Makefile

sources = usrmain.f usrprestart.f usrevent.f usrdump.f usrend.f usrpause.f \
	usrdownload.f usrgo.f

install-dirs := lib bin

bin_targets = engine_online

lib_targets := $(patsubst %.f, libonline.a(%.o), $(sources))

objects := $(patsubst %.f, %.o, $(sources))

ifeq ($(ARCH),HPUX)
  CC = gcc
  OTHERLIBS = -Wl,-L$(CODA)/HP_UX/lib \
	-lana -lmsg -lcoda -Wl,-L$(CERN_ROOT)/lib -lpacklib
endif

ifeq ($(ARCH),ULTRIX)
  CC = gcc
  OTHERLIBS = -L$(CODA)/ULTRIX/lib -lana -lmsg -lcoda -L$(CERN_ROOT)/lib -lpacklib
endif

bin_targets = engine_online

DEPLIBS = $(LIBROOT)/libengine.a \
	$(LIBROOT)/libhtracking.a $(LIBROOT)/libstracking.a \
	$(LIBROOT)/libtracking.a \
	$(LIBROOT)/libgmc.a $(LIBROOT)/libutils.a $(LIBROOT)/libctp.a

default:
	@echo "nothing to make"

$(LIBROOT)/libutils.a:
	@make -C $(Csoft)/SRC/UTILSUBS csoft

$(LIBROOT)/libgmc.a:
	@make -C $(Csoft)/SRC/GMC csoft

$(LIBROOT)/libtracking.a:
	@make -C $(Csoft)/SRC/TRACKING csoft

$(LIBROOT)/libctp.a:	
	@make -C $(Csoft)/SRC/CTP csoft


engine_online: $(objects) $(DEPLIBS)
	$(F77) $(FFLAGS) -o engine_online $(objects) $(DEPLIBS) $(OTHERLIBS)

include $(sources:.f=.d)
