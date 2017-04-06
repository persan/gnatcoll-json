-include Makefile.conf

SHELL=/bin/bash
EXEC_DIR := $(shell gprinfo --exec-dir gnatcoll-json-tests.gpr)
export PATH:=${EXEC_DIR}:${PATH}
.DEFAULT_GOAL:=all

ifdef PREFIX
INSTALLFLAGS:=--prefix=$(shell readlink -f ${PREFIX})
endif


all: compile test

compile:
	gprbuild -p -j0 -P gnatcoll-json-tests.gpr
	gprbuild -p -j0 -P gnatcoll-json-util.gpr

test:compile
	gnatcoll-json-support-test-main

install:
	sudo `which gprinstall` ${INSTALLFLAGS} -p gnatcoll-json.gpr

uninstall:
	sudo `which gprinstall` ${INSTALLFLAGS} -p gnatcoll-json.gpr --uninstall

check_clean: # IGNORE
	@if [ -n "`git status --porcelain`" ] ; then git status ; exit -1 ; fi

tag:all check_clean
	@check_version >/dev/null
	@check_tags gnatcoll-JSON-v`check_version`
	git tag -f gnatcoll-JSON-v`check_version`
	git push --all
	git push --tags
clean:
	git clean -fdx
