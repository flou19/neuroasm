#include "share/atspre_staload.hats"
staload "./neuro_core.sats"

%{^
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include "../c/neuro_core.h"

void ats_test_cpu(void) {
  Cpu8086 cpu;
  cpu_init(&cpu);
  cpu.mem[0x100] = 0xB8; cpu.mem[0x101] = 0x34; cpu.mem[0x102] = 0x12;
  cpu.mem[0x103] = 0xBB; cpu.mem[0x104] = 0x01; cpu.mem[0x105] = 0x00;
  cpu.mem[0x106] = 0x01; cpu.mem[0x107] = 0xC3;
  cpu.mem[0x108] = 0xF4;
  cpu.regs.ip = 0x100;
  char buf[512];
  printf("=== CPU Test ===\n");
  while (!cpu.halted) {
    NeuroErr e = cpu_step(&cpu);
    if (e) { printf("Error: %d\n", (int)e); break; }
    cpu_dump_regs(&cpu, buf, (int)sizeof(buf));
    printf("  %s\n", buf);
  }
  printf("Cycles: %llu\n\n", (unsigned long long)cpu.cycles);
}

void ats_test_neural(void) {
  Xorshift64 rng;
  xorshift64_init_time(&rng);
  NeuralCore nc;
  neural_init(&nc, &rng);
  float inputs[8]  = {0.5f, 1.0f, -0.5f, 0.2f, 0.0f, 0.0f, 0.0f, 0.0f};
  float outputs[4] = {0};
  neural_forward(&nc, inputs, outputs);
  printf("=== Neural Core Test ===\n");
  printf("Input:  [%.3f %.3f %.3f %.3f %.3f %.3f %.3f %.3f]\n",
         inputs[0], inputs[1], inputs[2], inputs[3],
         inputs[4], inputs[5], inputs[6], inputs[7]);
  printf("Output: [%.3f %.3f %.3f %.3f]\n",
         outputs[0], outputs[1], outputs[2], outputs[3]);
  int i;
  for (i = 0; i < 12; i++) neural_fragment_pulse(&nc, 0.1f);
  printf("Awakened: %s  Cores: %u\n\n",
         neural_is_awakened(&nc) ? "YES" : "NO", nc.cores_online);
}

void ats_test_mem(void) {
  PhysMem mem;
  phys_init(&mem);
  phys_write8(&mem, 0x1000, 0x00, 0xAB);
  phys_write16(&mem, 0x1000, 0x02, 0xDEAD);
  uint8_t v8; uint16_t v16;
  phys_read8(&mem, 0x1000, 0x00, &v8);
  phys_read16(&mem, 0x1000, 0x02, &v16);
  printf("=== Memory Test ===\n");
  printf("Wrote 0xAB -> read 0x%02X (expect AB)\n", v8);
  printf("Wrote 0xDEAD -> read 0x%04X (expect DEAD)\n", v16);
  printf("\n");
}

void ats_test_hexdump(void) {
  uint8_t data[32];
  int i;
  for (i = 0; i < 32; i++) data[i] = (uint8_t)(i * 7 + 0x41);
  char buf[2048];
  neuro_hex_dump(data, 32, 0x200, 8, buf, (int)sizeof(buf));
  printf("=== Hex Dump Test ===\n%s\n", buf);
}

void ats_test_rng(void) {
  Xorshift64 rng;
  xorshift64_init_time(&rng);
  printf("=== RNG Test ===\n");
  int i;
  for (i = 0; i < 8; i++)
    printf("  range(100): %u\n", xorshift64_range(&rng, 100));
  printf("\n");
}

void ats_test_ports(void) {
  Ports p;
  ports_init(&p);
  uint8_t v = 0;
  ports_outb(&p, 0x61, 0xA5);
  ports_inb(&p, 0x61, &v);
  printf("=== Ports Test ===\n");
  printf("Wrote 0xA5 to port 0x61 -> read 0x%02X (expect A5)\n", v);
  printf("PIT mode port 0x43 default: 0x%02X (expect 36)\n", p.data[0x43]);
  printf("\n");
}

void ats_test_idt(void) {
  Idt idt;
  idt_init(&idt);
  idt_defaults(&idt);
  uint16_t seg = 0, off = 0;
  idt_get(&idt, 0x21, &seg, &off);
  printf("=== IDT Test ===\n");
  printf("Vector 0x21 -> %04X:%04X (expect F000:2100)\n", seg, off);
  idt_set(&idt, 0x80, 0x1234, 0x5678);
  idt_get(&idt, 0x80, &seg, &off);
  printf("Vector 0x80 -> %04X:%04X (expect 1234:5678)\n", seg, off);
  printf("\n");
}
%}

extern fun ats_test_cpu (): void = "mac#ats_test_cpu"
extern fun ats_test_neural (): void = "mac#ats_test_neural"
extern fun ats_test_mem (): void = "mac#ats_test_mem"
extern fun ats_test_hexdump (): void = "mac#ats_test_hexdump"
extern fun ats_test_rng (): void = "mac#ats_test_rng"
extern fun ats_test_ports (): void = "mac#ats_test_ports"
extern fun ats_test_idt (): void = "mac#ats_test_idt"

implement main0 () = {
  val () = println! ("neuroasm ats core standalone test")
  val () = println! ("=================================")
  val () = println! ("")
  val () = ats_test_cpu ()
  val () = ats_test_neural ()
  val () = ats_test_mem ()
  val () = ats_test_hexdump ()
  val () = ats_test_rng ()
  val () = ats_test_ports ()
  val () = ats_test_idt ()
  val () = println! ("All tests complete.")
}
