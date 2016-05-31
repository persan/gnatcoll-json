-include Makefile.conf
SHELL=/bin/bash
.DEFAULT_GOAL:=all

Makefile.conf:Makefile
	@echo "export PATH:=${CURDIR}/bin:${PATH}" >${@}

all:compile test
compile:
	gprbuild -p -j0 -P gnatcoll-json-tests.gpr
	gprbuild -p -j0 -P gnatcoll-json-util.gpr

test:
	gnatcoll-json-support-test-main
	check_version

install:
	gprinstall -p gnatcoll-json.gpr

uninstall:
	gprinstall -p gnatcoll-json.gpr --uninstall

check_clean:
	if [ -n "`git status`" ] ; then git status ; exit -1 ; fi

tag:all check_clean
	check_version
