
.PHONY: all ats c test clean install

all: ats c

ats:
	$(MAKE) -C ats all

c: ats
	$(MAKE) -C c all

test: ats
	$(MAKE) -C ats neuroasm-test
	./ats/neuroasm-test

clean:
	$(MAKE) -C ats clean
	$(MAKE) -C c clean

install: all
	$(MAKE) -C c install
