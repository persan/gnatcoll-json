#!/usr/bin/make -f

-include Makefile.conf


all: compile test
setup:Makefile.conf

PROJECT:=gnatcoll-json-builder.gpr
GPRBUILDFLAGS:=-p

Makefile.conf: Makefile # IGNORE 
	@echo "export PREFIX?=$(shell dirname $(shell dirname $(shell which gnat)))" >${@}
	@echo "export PATH:=${CURDIR}/bin:${PATH}" >>${@}
	@e1=`which gnatstudio`; e2=`which gps`;  if [ -s $${e1} ]; then \
		echo GNATSTUDIO=$${e1};\
	else\
		if [ -s $${e2} ] ; then\
			echo GNATSTUDIO=$${e2};\
		fi;\
	fi >>${@}

compile:
	gprbuild  -P ${PROJECT} ${GPRBUILDFLAGS}
.PHONY: test
test:
	${MAKE} -C test all

install:
	gprinstall  -P ${PROJECT} ${GPRBUILDFLAGS} --prefix=${DESTDIR}${PREFIX}

clean:
	git clean -xdf

gnatstudio gps:
	${GNATSTUDIO} -P gnatcoll-json-builder.gpr & 
	${GNATSTUDIO} -P test/codegen.gpr & 
