-include Makefile.conf
SHELL=/bin/bash
export PATH:=${CURDIR}/bin:${PATH}
.DEFAULT_GOAL:=all
ifdef PREFIX
INSTALLFLAGS:=--prefix=$(shell readlink -f ${PREFIX})
endif


all:compile test
compile:
	gprbuild -p -j0 -P gnatcoll-json-tests.gpr
	gprbuild -p -j0 -P gnatcoll-json-util.gpr

test:
	gnatcoll-json-support-test-main

install:
	gprinstall ${INSTALLFLAGS} -p gnatcoll-json.gpr

uninstall:
	gprinstall ${INSTALLFLAGS} -p gnatcoll-json.gpr --uninstall

check_clean: # IGNORE
	@if [ -n "`git status --porcelain`" ] ; then git status ; exit -1 ; fi

tag:all check_clean
	@check_version >/dev/null
	@check_tags gnatcoll-JSON-v`check_version`
	git tag -f gnatcoll-JSON-v`check_version`
	git push --all
	git push --tags
