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
	rm UTILSUBS/$(OSTYPE)/*.d
	rm TRACKING/$(OSTYPE)/*.d
	rm STRACKING/$(OSTYPE)/*.d
	rm HTRACKING/$(OSTYPE)/*.d
	rm ENGINE/$(OSTYPE)/*.d
	rm CTP/$(OSTYPE)/*.d
	rm HACK/$(OSTYPE)/*.d
	rm ONEEV/$(OSTYPE)/*.d
	rm ONLINE/$(OSTYPE)/*.d
	rm CODA/$(OSTYPE)/$(OSTYPE)/*.d CODA/$(OSTYPE)/*.o
#	$(MAKE) -C UTILSUBS clean
#	$(MAKE) -C ENGINE clean
#	$(MAKE) -C CTP clean
#	$(MAKE) -C TRACKING clean
#	$(MAKE) -C HTRACKING clean
#	$(MAKE) -C STRACKING clean
#	$(MAKE) -C GMC clean



