targets=simple-json.ads

all:${targets}

%.ads.xml: %.ads
	gnat2xml ${<} >${@}

%-json.ads:%.ads.xml
	../bin/gnatxml2json.py ${<}

