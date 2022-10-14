
all:test

test:
	gprbuild -P gnatcoll-json-tests.gpr -XLIBRARY_TYPE=static
	bin/gnatcoll-json-support-test-main


clean:
	git clean . -fdx
