CC       = gcc
CFLAGS   = -std=c99 -O3 -Wall -Wextra -D_POSIX_C_SOURCE=200809L -fPIC
LDFLAGS  = -lm

EMACS_H := $(shell find /usr/include /usr/local/include \
                        $(HOME)/emacs_install \
                   -name "emacs-module.h" 2>/dev/null | head -1)
EMACS_INC := $(if $(EMACS_H),$(dir $(EMACS_H)),.)

CORE_LIB = ../ats/libneuro_core.a
CORE_INC = -I.

all: neuro_module.so neurod

$(CORE_LIB):
	$(MAKE) -C ../ats libneuro_core.a

neuro_module.so: neuro_module.c neuro_core.h $(CORE_LIB)
	$(CC) $(CFLAGS) -shared -I$(EMACS_INC) $(CORE_INC) \
	      neuro_module.c $(CORE_LIB) -o $@ $(LDFLAGS)
	chmod +x $@

neurod: neurod.c neuro_core.h $(CORE_LIB)
	$(CC) $(CFLAGS) $(CORE_INC) neurod.c $(CORE_LIB) -o $@ $(LDFLAGS)
	chmod +x $@

install: all
	install -d $(DESTDIR)$(PREFIX)/lib/neuroasm
	install -d $(DESTDIR)$(HOME)/.local/bin
	install -m 755 neuro_module.so $(DESTDIR)$(PREFIX)/lib/neuroasm/
	install -m 755 neurod          $(DESTDIR)$(PREFIX)/lib/neuroasm/

clean:
	rm -f neuro_module.so neurod
	$(MAKE) -C ../ats clean

.PHONY: all clean install
