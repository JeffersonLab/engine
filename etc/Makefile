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
#    Revision 1.3  1998/12/07 22:09:50  saw
#    Remove dependency on $Csoft
#
#    Revision 1.1  1996/02/02 14:14:46  saw
#    Initial revision
#
#    
#    93/12/14 saw Copied from $(Csoft)/etc
#
#----------------------------------------------------------------------------
# Generic Makefile add more directories here
ifndef OSTYPE  # New tcsh sets this, let's force it.
  OSTYPE := $(subst -,,$(shell uname))
endif
ifeq ($(OSTYPE),linux-gnu)
  OSTYPE=Linux
endif

SHELL     = /bin/sh

include ../../etc/makefile.site.in
include $(sources:.f=.d) 
include $(sources:.c=.d) 

EVGENLIB   = ../../../$(OSTYPE)$(OSEXT)/lib/libevgen.a
LIBROOT   = ../../../$(OSTYPE)$(OSEXT)/lib
#INCROOT   = $(Csoft)/$(OSTYPE)/include
OS     := $(shell echo $(OSTYPE) | tr "[A-Z]" "[a-z]")

parent := $(shell pwd)
MAKEDIRS:=$(addprefix $(parent)/,$(shell find . -type d -print | sed -e '/RCS/d'|xargs echo))


TIMESTAMP := "\"$(shell date +%h\ %d\ %Y)\""
#CSOFT_VERSION := "\"$(shell grep define $(Csoft)/.version.h | sed -e 's|.*CSOFTVERSION ||')\""
CSOFT_VERSION := "Version 0"

CFLAGS    = $(INCS) $(DEFS) $(EXTRA) -D$(OSTYPE) -DOSTYPE="\"$(OSTYPE)\"" -DVERSION=$(CSOFT_VERSION) -DDAYTIME=$(TIMESTAMP)

# OS dependent declarations of library and include directories etc...

.PRECIOUS: *.d
.PHONY : CSOFT csoft clean

ifdef NFSDIRECTORY
DEPENDS_RULE.c =  $(GCC) -MM $(CFLAGS) $< |\
	(sed -e 's|$*.o|& $@|g' -e 's|:|: .os$(OSTYPE) Makefile |' ;\
	cat $< | sed -n -e \ "s|\#include \"|$@: ../|p" |\
	sed -e "s|\".*$$||" | sed -e 's|.d:|.o:|') >$@
else
DEPENDS_RULE.c =  $(GCC) -MM $(CFLAGS) $< |\
	 sed -e 's|$*.o|& $@|g' -e 's|:|: .os$(OSTYPE) Makefile |' > $@
endif
DEPENDS_RULE.f = 	( cat $< | sed -n -e \
	"s|^[ 	]*[Ii][Nn][Cc][Ll][Uu][Dd][Ee][ 	]*['\"]|$@: |p" | \
	sed -e "s|['\"].*$$||" | sed -e 's|.d:|.o:|' ; \
	echo $*.o: .os$(OSTYPE) ) > $@
COMPILE_RULE.f = $(F77) $(FFLAGS) -c $< -o $@ 
COMPILE_RULE.c = $(CC) $(CFLAGS) -c $< -o $@
# Don't "Makefile" to .d files anymore

ifdef NEWSTYLE
%.d: ../%.c
	echo remaking $@
	$(DEPENDS_RULE.c)

#r_%.d:
#	echo r_$*.o: r_$*.f > $@

%.d: ../%.f
	echo remaking $@
	$(DEPENDS_RULE.f)

%.o: ../%.f
	$(COMPILE_RULE.f)

%.o: ../%.c
	$(COMPILE_RULE.c)

else
%.d: %.c
	echo remaking $@
	$(DEPENDS_RULE.c)

#%.d: %.f .os$(OSTYPE)
%.d: %.f
	echo remaking $@
	$(DEPENDS_RULE.f)
#	( cat $< | sed -n -e \
#	"s/^[ 	]*[Ii][Nn][Cc][Ll][Uu][Dd][Ee][ 	]*['\"]/$<: /p" | \
#	sed -e "s/['\"].*$$//" | sed -e 's/.f:/.o:/' ; \
#	echo $*.o: .os$(OSTYPE) ) > $@

%.o: %.f
	$(COMPILE_RULE.f)
endif

#%.f : %.F
#	$(F77) -F $<

(%.o): %.o
	@echo Adding $*.o to $@
	$(AR) $(ARFLAGS) $@ $*.o
ifdef RANLIB
	$(RANLIB) $@
endif

TARGET_PATH := ../../../$(OSPREFIX)$(SUBDIR)/
FILES       = $(addsuffix ",$(addprefix "$(TARGET_PATH),$($(SUBDIR)_targets)))


ifdef TARGET_PATH
$(TARGET_PATH)%: %	
	@echo "Making $@ from $< "
	rm -rf $@
	cp $< $@
endif

.os$(OSTYPE):
	rm -f *.d .os*
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

CSOFT csoft: $(REGSOURCES)
	@echo "CSOFT - Arch. $(ARCH) sys.vers $(OSTYPE)$(OSEXT)"
	@echo "`pwd` $(REGSOURCES)"
	@echo "Considering $(shell pwd) ... "
	@if test -z "$(install-dirs)"; then \
	  echo Nothing to do for $(OSTYPE)$(OSEXT); \
	else \
	for subdir in $(install-dirs) dummy; do \
            if test $$subdir != dummy; then \
	    $(MAKE) SUBDIR=$$subdir OSPREFIX=$(OSTYPE)$(OSEXT)/ directory ; \
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
