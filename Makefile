#
# $Log:
#

.DELETE_ON_ERROR: ;

# If you're not using GCC version 3 or 4 and Linux, you'll proably need to make
# some changes to the makefiles and possibly the source code.
getversion = --version | head -1 | sed 's/.*) //' | sed 's/\..*//'
gccversion = $(shell gcc $(getversion))
g77flags = -Wimplicit

export #export all variables
unexport getversion gccversion g77flags message #except these ones

CFLAGS = -Wall -W -O -ggdb #-pg #-pedantic
#CFLAGS = -Wall -W
CXXFLAGS := $(CFLAGS)
FFLAGS = -O -ffixed-line-length-132 -ggdb -Wall -W -fbounds-check #-pg #-pedantic
CC = gcc
CXX = g++
ifeq ($(gccversion),4)
  ifeq ($(shell gfortran $(getversion)),4)
    FC = gfortran
    CFLAGS += -DgFortran # for cfortran.h
    CXXFLAGS += -DgFortran
  else
    # this happens if you're using a JLab RHEL3 system and have typed "use gcc/4.1.1"
    # JLab RHEL3 systems currently have GCC 4 but no gfortran.
    define message

  +--------------------------------------------------------------------------+
  | Warning: Using GNU C compiler version 4 but gfortran not found. Falling  |
  | back to g77 + GCC 3 for Fortran but still using GCC 4 for C and C++.     |
  +--------------------------------------------------------------------------+
    endef
    $(warning $(message))
    FC = g77
    FFLAGS += $(g77flags)
  endif
else
  FC = g77
  FFLAGS += $(g77flags)
endif

SHELL = /bin/sh
CP = cp -f
RM = rm -f

# This program has only been tested under Linux and Mac OS X lately
# For either Mac or Linux, this variable should be set to Linux
MYOS = Linux

# Once upon a time, there were variables named $(MYOS), $(OSTYPE), and $(ARCH)
# but they all tended to be set to the same value. We only use MYOS now, and
# it's always set to Linux.
#MYOS := $(subst -,,$(shell uname))
#ifeq ($(MYOS),SunOS)
#  OSTYPE = $(MYOS)
#else
#  ifeq ($(MYOS),HPUX)
#    OSTYPE = hpux10
#  else
#    ifeq ($(MYOS),AIX)
#      OSTYPE = aix
#    else
#      OSTYPE = $(MYOS)
#    endif
#  endif
#endif

.PHONY: all info coda ctp engine exe hack htracking include oneev online port \
	stracking syncfilter t20 tracking utilsubs clean distclean btracking

#Missing from all: oneev online t20
#There is no way to compile t20
#make fails when running on t20 or online
#oneev seems to work

all: info include utilsubs ctp coda tracking htracking stracking hack port \
	engine btracking syncfilter exe

info:
	@echo HOSTNAME = $(HOSTNAME)
	@echo ROOTSYS = $(ROOTSYS)
	@echo CERN_ROOT = $(CERN_ROOT)
	@echo NFSDIRECTORY = $(NFSDIRECTORY)
	@echo Using GCC $(gccversion) with CC = $(CC), CXX = $(CXX) and FC = $(FC)
	@echo
btracking:
	$(MAKE) -C BTRACKING
coda:
	$(MAKE) -C CODA
ctp:
	$(MAKE) -C CTP
engine:
	$(MAKE) -C ENGINE
exe:
	$(MAKE) -C EXE
hack:
	$(MAKE) -C HACK
htracking:
	$(MAKE) -C HTRACKING
include:
	$(MAKE) -C INCLUDE
oneev:
	$(MAKE) -C ONEEV
online:
	$(MAKE) -C ONLINE csoft
port:
	$(MAKE) -C PORT
stracking:
	$(MAKE) -C STRACKING
syncfilter:
	$(MAKE) -C SYNCFILTER
t20: ;
tracking:
	$(MAKE) -C TRACKING
utilsubs:
	$(MAKE) -C UTILSUBS

clean:
	-$(RM) */O.$(MYOS)/*.[do]
	-(cd ../$(MYOS)/lib; $(RM) libcoda.a libctpclient_root.a libctp_root.a \
	    libengine.a libhack.a libhtracking.a libport.a libstracking.a \
	    libtracking.a libutils.a )
	-(cd ../$(MYOS)/bin; $(RM) engine_replay makereg syncfilter)
	-$(RM) CTP/daVarRpc_svc.c CTP/daVarRpc_xdr.c CTP/daVarRpc_clnt.c CTP/daVarRpc.h

# This rule deletes almost everything possible. It makes no attempt to clean
# anything that would be created if NFSDIRECTORY were defined
distclean: clean
	-$(RM) -r */O.$(MYOS)
# r_*.f files are generated automatically and saved because they are .PRECIOUS
	-$(RM) */r_*.f
