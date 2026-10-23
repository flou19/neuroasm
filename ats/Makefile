PATSCC   = patscc
PATSOPT  = patsopt
CC       = gcc
AR       = ar
ATS_HOME = /usr/lib/ats2-postiats-0.4.2
ATS_RT   = $(ATS_HOME)/ccomp/runtime
CFLAGS   = -std=c99 -O3 -Wall -Wextra -fPIC -D_POSIX_C_SOURCE=200809L -I../c
ATS_CFLAGS = -std=c99 -O3 -w -fPIC -D_POSIX_C_SOURCE=200809L -DATS_DYNLOADFLAG=0 \
             -I../c -I$(ATS_RT) -I$(ATS_HOME)
ATSFLAGS = -DATS_MEMALLOC_LIBC -IATS ../c -DATS_DYNLOADFLAG=0
LIBS     = -lgc -lm

CORE_C   = ../c/neuro_core.c
CORE_O   = neuro_core_c.o
ATS_O    = neuro_core_dats.o
STUB_O   = dynload_stub.o

all: libneuro_core.a neuroasm-test

$(CORE_O): $(CORE_C) ../c/neuro_core.h
	$(CC) $(CFLAGS) -c $(CORE_C) -o $@

neuro_core_dats.c: neuro_core.dats neuro_core.sats
	$(PATSOPT) -o $@ -d neuro_core.dats

$(ATS_O): neuro_core_dats.c
	$(CC) $(ATS_CFLAGS) -c neuro_core_dats.c -o $@

$(STUB_O): neuro_core_dats.c
	FLAG=$$(grep 'dynloadflag' neuro_core_dats.c | grep -o '_057[^ )]*' | head -1); \
	printf 'int %s = 0;\n' "$$FLAG" > dynload_stub.c
	$(CC) $(CFLAGS) -c dynload_stub.c -o $@

libneuro_core.a: $(CORE_O) $(ATS_O) $(STUB_O)
	$(AR) rcs $@ $(CORE_O) $(ATS_O) $(STUB_O)

neuroasm-test: neuro_standalone.dats libneuro_core.a
	$(PATSCC) $(ATSFLAGS) -o $@ neuro_standalone.dats -L. -lneuro_core $(LIBS)
	chmod +x $@

clean:
	rm -f $(CORE_O) $(ATS_O) $(STUB_O) libneuro_core.a neuroasm-test \
	      neuro_core_dats.c dynload_stub.c *_dats.c *_dats.o

.PHONY: all clean
