Virtual 8086-compatible system with neural substrate and adversarial entity (Second User). Entity activation is triggered by filesystem interrogation exceeding a threat threshold. Easier: the game.

LAYOUT

neuroasm/
  ats/
    neuro_core.sats
    neuro_core.dats
  c/
    neuro_core.h
    neuro_module.c
    neurod.c
  src/
    n0.el  u0.el  f0.el  c0.el  e0.el  s0.el
    a0.el  d0.el  k0.el  y0.el  v0.el  w0.el
    b0.el  g0.el  r0.el

BUILD

gcc make libm patscc libgc-dev emacs-module.h

  make
  make test
  make clean

  emacs -Q --batch -L src -f batch-byte-compile src/*.el

RUN

  emacs -Q -nw -L src --eval '(progn (require '\''n0) (neuroasm))'

INTERFACE

FS
  SYS.566/{ENTITY.BIN,FRAGMENT.00[1-3]}
  RESEARCH/PROTOCOL.TXT          (REVEAL)

CMD
  LS CD PWD CAT RM MKDIR TOUCH TREE FIND HEXDUMP WRITE
  PS KILL KILLALL MONITOR SYS MEM PORTS
  D U E TRACE
  ISOLATE RELEASE FRAGMENTS COLLECT STATUS HISTORY
  RESEARCH REVEAL NEURAL ASM
  HELP CLS EXIT SAVE LOAD

THREAT
  RM ∈ SYS.566 → +2
  threat ≥ 3 → Second User + A* pursuit
  exit = E; capture = termination

RESOLUTION
  1. reach E
  2. ISOLATE USER2 (threat ≥ 2)
  3. ≥8 fragments → REVEAL → RESEARCH
  4. 60 cmds @ threat ≤ 1

DAEMON

  nc -U $TMPDIR/neurodos.sock

  PING VERSION STEP RUN REGS FLAGS GETREG SETREG
  DUMP WRITE CYCLES HALTED RESET