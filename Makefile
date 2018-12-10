
# helper programs
CAT := cat
ECHO  := echo
WHICH := which

# check for out-of-tree build
SOURCE_DIR := $(dir $(MAKEFILE_LIST))
ifeq ($(SOURCE_DIR),./)
  RBD=
  GNATCOLL_JSON_GPR=gnatcoll-json.gpr
  MAKEPREFIX=
else
  RBD=--relocate-build-tree
  GNATCOLL_JSON_GPR=$(SOURCE_DIR)/gnatcoll-json.gpr
  MAKEPREFIX=$(SOURCE_DIR)/
endif

TARGET := $(shell gcc -dumpmachine)
NORMALIZED_TARGET := $(subst normalized_target:,,$(wordlist 6,6,$(shell gprconfig  --config=ada --target=$(TARGET) --mi-show-compilers)))
ifeq ($(NORMALIZED_TARGET),)
  $(error No toolchain found for target "$(TARGET)")
endif

GNATCOLL_JSON_OS := $(if $(findstring darwin,$(NORMALIZED_TARGET)),osx,$(if $(findstring windows,$(NORMALIZED_TARGET)),windows,unix))

prefix ?= $(dir $(shell $(WHICH) gnatls))..
GNATCOLL_JSON_VERSION := $(shell $(CAT) $(SOURCE_DIR)/version_information)

BUILD         = PROD
PROCESSORS    = 0
BUILD_DIR     =
ENABLE_SHARED = yes
INTEGRATED    = no

all: build

# Load current setup if any
-include makefile.setup

GTARGET=--target=$(NORMALIZED_TARGET)

ifeq ($(ENABLE_SHARED), yes)
   LIBRARY_TYPES=static relocatable static-pic
else
   LIBRARY_TYPES=static
endif

ifeq ($(INTEGRATED), yes)
   integrated_install=/$(NORMALIZED_TARGET)
endif


GPR_VARS=-XGNATCOLL_JSON_VERSION=$(GNATCOLL_JSON_VERSION) \
	 -XGNATCOLL_JSON_OS=$(GNATCOLL_JSON_OS) \
	 -XBUILD=$(BUILD)

# Used to pass extra options to GPRBUILD, like -d for instance
GPRBUILD_OPTIONS=

BUILDER=gprbuild -P ${GNATCOLL_JSON_GPR} -p -m $(GTARGET) $(RBD) -j$(PROCESSORS) $(GPR_VARS) \
	$(GPRBUILD_OPTIONS)
INSTALLER=gprinstall -p -f $(GTARGET) $(GPR_VARS) \
	$(RBD) --sources-subdir=include/gnatcoll-json --prefix=$(prefix)$(integrated_install)
CLEANER=gprclean -q $(RBD) $(GTARGET)
UNINSTALLER=$(INSTALLER) -p -f --install-name=gnatcoll-json --uninstall


EXEC_DIR := $(shell gprinfo --exec-dir -r tests/gnatcoll-json-tests.gpr -XLIBRARY_TYPE=static 2>/dev/null)

ifeq ("${EXEC_DIR}","")
	EXEC_DIRS:=${CURDIR}/bin:${CURDIR}/tests/bin
endif


.DEFAULT_GOAL:=all

ifdef PREFIX
INSTALLFLAGS:=--prefix=$(shell readlink -f ${PREFIX})
endif


#########
# build #
#########

build: $(LIBRARY_TYPES:%=build-%)

build-%:
	$(BUILDER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* -XGPR_BUILD=$* \
		$(GPR_VARS)

###########
# Install #
###########

uninstall:
ifneq (,$(wildcard $(prefix)$(integrated_install)/share/gpr/manifests/gnatcoll))
	-$(UNINSTALLER) $(GNATCOLL_JSON_GPR)
endif

install: uninstall $(LIBRARY_TYPES:%=install-%)

install-%:
	$(INSTALLER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* -XGPR_BUILD=$* -f\
		--build-name=$* $(GPR_VARS) \
		--build-var=LIBRARY_TYPE --build-var=GNATCOLL_BUILD \
		--build-var=GNATCOLL_CORE_BUILD $(GNATCOLL_JSON_GPR)

###########
# Cleanup #
###########

clean: $(LIBRARY_TYPES:%=clean-%)

clean-%:
	-$(CLEANER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* -XGPR_BUILD=$* \
		$(GPR_VARS) $(GNATCOLL_JSON_GPR)

.PHONY: test-install
test-install:
	@sudo rm -rf _
	@mkdir  -p _
	make install PREFIX=${CURDIR}/_
	cd test-install ; GPR_PROJECT_PATH=$(dir $(shell find ${CURDIR}/_ -name gnatcoll-json.gpr))  gprbuild -p


#########
# setup #
#########

.SILENT: setup

makefile.setup:
	$(ECHO) "prefix=$(prefix)" > makefile.setup
	$(ECHO) "PATH=${EXEC_DIR}:${PATH}:$(dir $(shell which gnatls))" >> makefile.setup
	$(ECHO) "ENABLE_SHARED=$(ENABLE_SHARED)" >> makefile.setup
	$(ECHO) "INTEGRATED=$(INTEGRATED)" >> makefile.setup
	$(ECHO) "BUILD=$(BUILD)" >> makefile.setup
	$(ECHO) "PROCESSORS=$(PROCESSORS)" >> makefile.setup
	$(ECHO) "TARGET=$(TARGET)" >> makefile.setup
	$(ECHO) "SOURCE_DIR=$(SOURCE_DIR)" >> makefile.setup
	$(ECHO) "GNATCOLL_JSON_OS=$(GNATCOLL_JSON_OS)" >> makefile.setup
	$(ECHO) "GNATCOLL_JSON_VERSION=$(GNATCOLL_JSON_VERSION)" >> makefile.setup


# Let gprbuild handle parallelisation. In general, we don't support parallel
# runs in this Makefile, as concurrent gprinstall processes may crash.
.NOTPARALLEL:

check_clean: # IGNORE
	@if [ -n "`git status --porcelain`" ] ; then git status ; exit -1 ; fi
tools:
	gprbuild -P gnatcoll-json-util.gpr -XLIBRARY_TYPE=static
test:
	gprbuild -P tests/gnatcoll-json-tests.gpr -XLIBRARY_TYPE=static
	tests/bin/gnatcoll-json-support-test-main

tag:tools check_clean all
	@check_version >/dev/null
	@check_tags gnatcoll-json-v`check_version`
	git tag -f gnatcoll-json-v`check_version`
	git push --all
	git push --tags

clean:
	git clean -fdx
