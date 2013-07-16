#
# $Log: Makefile,v $
# Revision 1.6  2003/02/14 18:10:21  jones
# Add if statement for Alpha OSF1 (E. Brash)
#
# Revision 1.5  1996/01/17 16:48:24  cdaq
# (SAW) Add libengine.a to library list
#
# Revision 1.4  1995/05/24 14:04:15  cdaq
# (SAW) Add -lm library for CTP
#
# Revision 1.3  1995/01/27  21:09:22  cdaq
# (SAW) Remove RCS from include file rules
#
# Revision 1.2  1994/08/04  03:51:30  cdaq
# (SAW) Add libhack.a to library list
#
# Revision 1.1  1994/07/07  15:25:03  cdaq
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
	-lana -lmsg -lcoda -Wl,-L$(CERN_ROOT)/lib -lpacklib -lm
endif

ifeq ($(ARCH),ULTRIX)
  CC = gcc
  OTHERLIBS = -L$(CODA)/ULTRIX/lib -lana -lmsg -lcoda -L$(CERN_ROOT)/lib -lpacklib
endif

ifeq ($(ARCH),OSF1)
  CC = cc
  OTHERLIBS = -L$(CERN_ROOT)/lib -lpacklib
endif

bin_targets = engine_online

DEPLIBS = $(LIBROOT)/libengine.a \
	$(LIBROOT)/libhtracking.a $(LIBROOT)/libstracking.a \
	$(LIBROOT)/libengine.a \
	$(LIBROOT)/libtracking.a $(LIBROOT)/libhack.a \
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

%.cmn :: ../INCLUDE/%.cmn,v
	cp $< $@

include $(sources:.f=.d)
