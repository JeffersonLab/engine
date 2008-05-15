#
# $Log:
#

.DELETE_ON_ERROR: ;

# Put any variables needed by all makefiles in etc/Makefile.variables
include etc/Makefile.variables

.PHONY: all info coda ctp engine exe hack htracking include oneev online port \
	stracking syncfilter t20 tracking utilsubs clean distclean btracking \
	sanetracking

#Missing from all: oneev online t20
#There is no way to compile t20
#make fails when running on t20 or online
#oneev seems to work

all: info include utilsubs ctp coda tracking htracking stracking hack port \
	engine btracking sanetracking syncfilter exe 

info:
	@echo HOSTNAME = $(HOSTNAME)
	@echo ROOTSYS = $(ROOTSYS)
	@echo CERN_ROOT = $(CERN_ROOT)
	@echo NFSDIRECTORY = $(NFSDIRECTORY)
	@echo Using GCC $(gccversion) with CC = $(CC), CXX = $(CXX) and FC = $(FC)
	@echo
sanetracking:
	$(MAKE) -C SANE
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
	-rm ../$(MYOS)/lib/*.a
#	-(cd ../$(MYOS)/lib; $(RM) libcoda.a libctpclient_root.a libctp_root.a \
	    libengine.a libhack.a libhtracking.a libport.a libstracking.a \
	    libtracking.a libutils.a libbtracking.a libctp.a libctpclient.a \
            libsanetracking.a )
	-(cd ../$(MYOS)/bin; $(RM) engine_replay makereg syncfilter)
	-$(RM) CTP/daVarRpc_svc.c CTP/daVarRpc_xdr.c CTP/daVarRpc_clnt.c CTP/daVarRpc.h

# This rule deletes almost everything possible. It makes no attempt to clean
# anything that would be created if NFSDIRECTORY were defined
distclean: clean
	-$(RM) -r */O.$(MYOS)
# r_*.f files are generated automatically and saved because they are .PRECIOUS
	-$(RM) */r_*.f
