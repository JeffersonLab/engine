#
# $Log:
#

MYOS := $(subst -,,$(shell uname))
ifeq ($(MYOS),SunOS)
  OSTYPE = sunos
else
  ifeq ($(MYOS),HPUX)
    OSTYPE = hpux10
  else
    ifeq ($(MYOS),AIX)
      OSTYPE = aix
    else
      OSTYPE = $(MYOS)
    endif
  endif
endif

.PHONY: utilsubs ctp coda engine tracking stracking htracking hack \
	oneev coda port t20

all: include utilsubs ctp coda tracking htracking stracking hack port \
	engine t20 oneev

include:
	$(MAKE) -C INCLUDE

utilsubs:
	$(MAKE) -C UTILSUBS

ctp:
	$(MAKE) -C CTP

tracking:
	$(MAKE) -C TRACKING

stracking:
	$(MAKE) -C STRACKING

htracking:
	$(MAKE) -C HTRACKING

hack:
	$(MAKE) -C HACK

oneev:
	$(MAKE) -C ONEEV

engine:
	$(MAKE) -C ENGINE

online:
	$(MAKE) -C ONLINE csoft

coda:
	$(MAKE) -C CODA

port:
	$(MAKE) -C PORT

#broken
clean:
	-@rm UTILSUBS/O.$(OSTYPE)/*.[do]
	-@rm TRACKING/O.$(OSTYPE)/*.[do]
	-@rm STRACKING/O.$(OSTYPE)/*.[do]
	-@rm HTRACKING/O.$(OSTYPE)/*.[do]
	-@rm ENGINE/O.$(OSTYPE)/*.[do]
	-@rm CTP/O.$(OSTYPE)/*.[do]
	-@rm HACK/O.$(OSTYPE)/*.[do]
	-@rm ONEEV/O.$(OSTYPE)/*.[do]
	-@rm PORT/O.$(OSTYPE)/*.[do]
	-@rm CODA/O.$(OSTYPE)/*.[do]
#	-@rm ONLINE/O.$(OSTYPE)/*.[do]
	-@rm ../$(OSTYPE)/lib/*.a
	-@rm ../$(OSTYPE)/bin/*
	-@rm CTP/daVarRpc_svc.c 
	-@rm CTP/daVarRpc_xdr.c CTP/daVarRpc_clnt.c CTP/daVarRpc.h
#	$(MAKE) -C UTILSUBS clean
#	$(MAKE) -C ENGINE clean
#	$(MAKE) -C CTP clean
#	$(MAKE) -C TRACKING clean
#	$(MAKE) -C HTRACKING clean
#	$(MAKE) -C STRACKING clean
#	$(MAKE) -C GMC clean
