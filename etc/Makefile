#----------------------------------------------------------------------------*
# Copyright (c) 1991, 1992  Southeastern Universities Research Association,  * 
#			    Continuous Electron Beam Accelerator Facility    *
#									     *
#   This software was developed under a United States Government license     *
#    described in the NOTICE file included as part of this distribution.     *
#									     *
#CEBAF Data Acquisition Group, 12000 Jefferson Ave., Newport News, VA 23606  *
#     heyescebaf.gov	Tel: (804) 249-7030    Fax: (804) 249-7363	     *
#----------------------------------------------------------------------------*
#Discription: follows this header.
#
#Author:
#	Graham Heyes
#	CEBAF Data Acquisition Group
#
#  Revision History:
#    $Log$
#    Revision 1.1  1996/02/02 14:14:46  saw
#    Initial revision
#
#    
#    93/12/14 saw Copied from $(Csoft)/etc
#
#----------------------------------------------------------------------------
# Generic Makefile add more directories here
#ifndef OSTYPE  # New tcsh sets this, let's force it.
  OSTYPE := $(subst -,,$(shell uname))
#endif

RM        = rm -f 
SHELL     = /bin/sh

ifeq ($(OSTYPE),HPUX)
EXTRA = -D_HPUX_SOURCE -Dhpux
FFLAGS=+ppu -C +es -O +Obb1000 +FPVZOU
#FFLAGS=+ppu -C +es -O -G +Obb1000 +FPVZOU
ARFLAGS=frv
endif

ifeq ($(OSTYPE),ULTRIX)
EXTRA = 
FFLAGS=-check_bounds -extend_source -g2
endif

ifeq ($(OSTYPE),Linux)
EXTRA =
FFLAGS=-g -f
endif

ifeq ($(OSTYPE),IRIX)
EXTRA =
FFLAGS=-check_bounds -extend_source -g2
LDFLAGS=
endif
ARCH = $(OSTYPE)

include $(Csoft)/etc/makefile.csoft.in
include $(Csoft)/etc/makefile.site.in
include $(sources:.f=.d) 
include $(sources:.c=.d) 

EVGENLIB   = $(Csoft)/$(OSTYPE)/lib/libevgen.a
LIBROOT   = $(Csoft)/$(OSTYPE)/lib
INCROOT   = $(Csoft)/$(OSTYPE)/include
OS     := $(shell echo $(OSTYPE) | tr "[A-Z]" "[a-z]")

parent := $(shell pwd)
MAKEDIRS:=$(addprefix $(parent)/,$(shell find . -type d -print | sed -e '/RCS/d'|xargs echo))


TIMESTAMP := "\"$(shell date +%h\ %d\ %Y)\""
CSOFT_VERSION := "\"$(shell grep define $(Csoft)/.version.h | sed -e 's|.*CSOFTVERSION ||')\""

CFLAGS    = $(INCS) $(DEFS) $(EXTRA) -D$(OSTYPE) -DOSTYPE="\"$(OSTYPE)\"" -DVERSION=$(CSOFT_VERSION) -DDAYTIME=$(TIMESTAMP)

# OS dependent declarations of library and include directories etc...

.PRECIOUS: *.d
.PHONY : CSOFT csoft clean

ifdef DEPMETHOD
## .o files don't depend on Makefile or .os$(OSTYPE) to make switching between
## os's faster
%.d: %.c .os$(OSTYPE) Makefile $(Csoft)/etc/Makefile
	echo remaking $@
	$(GCC) -MM $(CFLAGS) $<  > $@

%.d: %.f .os$(OSTYPE) Makefile $(Csoft)/etc/Makefile
	echo remaking $@
##	grep -i "[ \t]*include[ \t]*'" $< | sed -e "s/^[\t ]*[iI][nN][cC][lL][uU][dD][eE][ \t]*'/$<: /" | sed -e "s/'.*$$//" | sed -e 's/.f:/.o:/' > $@
	cat $< | sed -n -e \
	"s/^[ \t]*[Ii][Nn][Cc][Ll][Uu][Dd][Ee][ \t]*'/$<: /p" | \
	sed -e "s/'.*$$//" | sed -e 's/.f:/.o:/' | sed -e \
	's/$*.o/& $@/g' -e 's/:/: .os$(OSTYPE) Makefile /' > $@

else
%.d: %.c .os$(OSTYPE) Makefile $(Csoft)/etc/Makefile
	echo remaking $@
	$(GCC) -MM $(CFLAGS) $< | sed -e 's/$*.o/& $@/g' -e 's/:/: .os$(OSTYPE) Makefile /' > $@

#%.d: %.f .os$(OSTYPE) Makefile $(Csoft)/etc/Makefile
%.d: %.f .os$(OSTYPE)
	echo remaking $@
	( cat $< | sed -n -e \
	"s/^[ 	]*[Ii][Nn][Cc][Ll][Uu][Dd][Ee][ 	]*['\"]/$<: /p" | \
	sed -e "s/['\"].*$$//" | sed -e 's/.f:/.o:/' ; \
	echo $*.o: .os$(OSTYPE) ) > $@
# Don't "Makefile" to .d files anymore
endif

%.f : %.F
	$(F77) -F $<

(%.o): %.o
	@echo Adding $*.o to $@
	$(AR) $(ARFLAGS) $@ $*.o
ifeq ($(ARCH),ULTRIX)
	$(RANLIB) $@
endif

TARGET_PATH := $(Csoft)/$(OSPREFIX)$(SUBDIR)/
FILES       = $(addsuffix ",$(addprefix "$(TARGET_PATH),$($(SUBDIR)_targets)))


ifdef TARGET_PATH
$(TARGET_PATH)%: %	
	@echo "Making $@ from $< "
	rm -rf $@
	cp $< $@
endif

.os$(OSTYPE):
ifdef DEPMETHOD
	rm -f *.d .os* *.o
else
	rm -f *.d .os*
endif
	@echo $(OSTYPE) > .os$(OSTYPE)
	@echo remade marker .os$(OSTYPE)

defaulT:
	@echo "Nothing to do for $(SUBDIR)"

depend: .os$(OSTYPE)
	@ echo depending...
	@$(MAKE) $(sources:.f=.d) 
	@$(MAKE) $(sources:.c=.d) 

CLEAN clean::
	@ echo Cleaning `pwd`
	rm -f *.o *.d .os*
	@ echo Done.

ALL_CLEAN all_clean: 
	@  for makefile in `find . -name Makefile -print | sed -e '/Eiffel/d'  -e '/pre_/d'`; do \
	  echo ; \
	  echo Considering `dirname $$makefile` ... ; \
	  $(MAKE) -C `dirname $$makefile` clean ; \
        done

directory:
	@echo "Doing directory"
	@echo "SUBDIR=$(SUBDIR)"
	@echo "OSPREFIX=$(OSPREFIX)"
##	@echo "FILES=$(FILES)"
	@if test -d "$(TARGET_PATH)"; then echo ; else \
		mkdir -p $(TARGET_PATH) ; \
	fi;true
	$(MAKE) SUBDIR=$(SUBDIR) OSPREFIX=$(OSPREFIX) $(FILES)

CSOFT csoft: 
	@echo "CSOFT - Arch. $(ARCH) sys.vers $(OSTYPE)"
	@echo "Considering $(shell pwd) ... "
	@if test -z "$(install-dirs)"; then \
	  echo Nothing to do for $(OSTYPE); \
	else \
	for subdir in $(install-dirs) dummy; do \
            if test $$subdir != dummy; then \
	    $(MAKE) SUBDIR=$$subdir OSPREFIX=$(OSTYPE)/ directory ; \
            fi \
	done \
	fi;true
	@echo "Done $(shell pwd)"

ALL_CSOFT all_csoft: 
	@ for makefile in `find . -name Makefile -print | sed -e '/Eiffel/d'  -e '/pre_/d' -e '/dev_/d`; do \
	  echo ; \
	  $(MAKE) -C `dirname $$makefile` csoft ; \
        done
	@ echo Made csoft for $(OS)











