
export PATH:=${CURDIR}/../bin:${PATH}

targets=simple-json.ads

all:${targets}
#m	gprbuild -p -j0

.PRECIOUS: simple.ads.xml

%.ads.xml: %.ads
	gnat2xml ${<} >${@}
	touch $<
%-json.ads:%.ads.xml
	gnatxml2json.py ${<}
	gprbuild -p -j0 -P codegen.gpr

