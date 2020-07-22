
export PATH:=${CURDIR}/../bin:${PATH}

targets=simple-json.ads

all:simple-json.ads

simple-json.ads:simple.ads $(wildcard ../bin/*)
	json-builder ${<}
