-include Makefile.conf

.DEFAULT_GOAL:=all

Makefile.conf:Makefile
	@echo "export PATH:=${CURDIR}/bin:${PATH}" >${@}

all:
	gprbuild -p -j0 -P gnatcoll-json-tests.gpr 

test:
	ada2json tests/data.ads


install:
	gprinstall -p gnatcoll-json.gpr

uninstall:
	gprinstall -p gnatcoll-json.gpr --uninstall
