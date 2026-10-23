%{#
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include "../c/neuro_core.h"
%}

typedef cpu8086   = $extype"Cpu8086"
typedef physmem   = $extype"PhysMem"
typedef ports     = $extype"Ports"
typedef idt       = $extype"Idt"
typedef neural    = $extype"NeuralCore"
typedef xorshift  = $extype"Xorshift64"
typedef neuroerr  = $extype"NeuroErr"

fun cpu_init (cpu: &cpu8086): void = "mac#cpu_init"
fun cpu_init_segs (
  cpu: &cpu8086, cs: uint16, ds: uint16, es: uint16,
  ss: uint16, sp: uint16, ip: uint16
): void = "mac#cpu_init_segs"
fun cpu_step (cpu: &cpu8086): neuroerr = "mac#cpu_step"
fun cpu_dump_regs (cpu: &cpu8086, buf: ptr, sz: int): int = "mac#cpu_dump_regs"
fun cpu_dump_flags (cpu: &cpu8086, buf: ptr, sz: int): int = "mac#cpu_dump_flags"
fun cpu_dump_mem (cpu: &cpu8086, addr: uint16, len: int, buf: ptr, bufsz: int): void = "mac#cpu_dump_mem"
fun cpu_write_mem (cpu: &cpu8086, addr: uint16, data: ptr, n: int): void = "mac#cpu_write_mem"

fun phys_init (m: &physmem): void = "mac#phys_init"
fun phys_read8 (m: &physmem, seg: uint16, off: uint16, out: &uint8): neuroerr = "mac#phys_read8"
fun phys_write8 (m: &physmem, seg: uint16, off: uint16, v: uint8): neuroerr = "mac#phys_write8"
fun phys_read16 (m: &physmem, seg: uint16, off: uint16, out: &uint16): neuroerr = "mac#phys_read16"
fun phys_write16 (m: &physmem, seg: uint16, off: uint16, v: uint16): neuroerr = "mac#phys_write16"

fun ports_init (p: &ports): void = "mac#ports_init"
fun ports_inb (p: &ports, port: uint16, out: &uint8): neuroerr = "mac#ports_inb"
fun ports_outb (p: &ports, port: uint16, v: uint8): neuroerr = "mac#ports_outb"

fun idt_init (idt: &idt): void = "mac#idt_init"
fun idt_defaults (idt: &idt): void = "mac#idt_defaults"
fun idt_set (idt: &idt, vec: uint8, seg: uint16, off: uint16): void = "mac#idt_set"
fun idt_get (idt: &idt, vec: uint8, seg: &uint16, off: &uint16): void = "mac#idt_get"
fun idt_load_from_mem (idt: &idt, m: &physmem): neuroerr = "mac#idt_load_from_mem"
fun idt_write_to_mem (idt: &idt, m: &physmem): neuroerr = "mac#idt_write_to_mem"

fun xorshift64_init_time (r: &xorshift): void = "mac#xorshift64_init_time"
fun xorshift64_f32 (r: &xorshift): float = "mac#xorshift64_f32"

fun neural_init (nc: &neural, rng: &xorshift): void = "mac#neural_init"
fun neural_forward (nc: &neural, inputs: ptr, outputs: ptr): void = "mac#neural_forward"
fun neural_fragment_pulse (nc: &neural, intensity: float): void = "mac#neural_fragment_pulse"
fun neural_is_awakened (nc: &neural): bool = "mac#neural_is_awakened"

fun neuro_hex_dump (data: ptr, len: int, base: uint16, cols: int, buf: ptr, bufsz: int): void = "mac#neuro_hex_dump"
fun neuro_now_ms (): uint64 = "mac#neuro_now_ms"
fun neuro_sleep_ms (ms: int): void = "mac#neuro_sleep_ms"
fun neuro_beep (times: int, ms: int): void = "mac#neuro_beep"
