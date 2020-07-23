#!/usr/bin/make -f

-include Makefile.conf


all: compile test
setup:Makefile.conf

PROJECT:=gnatcoll-json-builder.gpr
GPRBUILDFLAGS:=-p

Makefile.conf:  # IGNORE
	echo "export PREFIX?=$(shell dirname $(shell dirname $(shell which gnat)))" >${@}
	echo "export PATH:=${CURDIR}/bin:${PATH}" >>${@}


compile:
	gprbuild  -P ${PROJECT} ${GPRBUILDFLAGS}
.PHONY: test
test:
	${MAKE} -C test all

install:
	gprinstall  -P ${PROJECT} ${GPRBUILDFLAGS} --prefix=${DESTDIR}${PREFIX}

clean:
	git clean -xdf
