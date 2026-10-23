staload "./neuro_core.sats"

%{^
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>
#include "../c/neuro_core.h"

uint64_t neuro_now_ms(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (uint64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}
void neuro_sleep_ms(int ms) {
  struct timespec ts;
  ts.tv_sec  = ms / 1000;
  ts.tv_nsec = (long)(ms % 1000) * 1000000L;
  nanosleep(&ts, NULL);
}
void neuro_beep(int times, int ms) {
  int i;
  for (i = 0; i < times; i++) {
    fputc('\a', stderr);
    fflush(stderr);
    neuro_sleep_ms(ms);
  }
}

void xorshift64_init_time(Xorshift64 *r) {
  r->state = (uint64_t)neuro_now_ms() * 6364136223846793005ULL
           ^ (uint64_t)(uintptr_t)r;
  if (!r->state) r->state = 0xDEADBEEFCAFEBABEULL;
}
float xorshift64_f32(Xorshift64 *r) {
  uint32_t v = (uint32_t)(xorshift64_next(r) >> 32);
  return (float)(v >> 8) / (float)(1 << 24);
}

static uint32_t phys_addr(uint16_t seg, uint16_t off) {
  return (((uint32_t)seg << 4) + off) & 0xFFFFF;
}
void phys_init(PhysMem *m) {
  memset(m->phys, 0, PHYS_MEM_SIZE);
  m->protect_low = true;
  m->dirty_pages = 0;
}
NeuroErr phys_read8(const PhysMem *m, uint16_t seg, uint16_t off, uint8_t *out) {
  uint32_t a = phys_addr(seg, off);
  if (m->protect_low && a < 0x400) return NEURO_ERR_KERNEL_PROTECTED;
  *out = m->phys[a];
  return NEURO_OK;
}
NeuroErr phys_write8(PhysMem *m, uint16_t seg, uint16_t off, uint8_t val) {
  uint32_t a = phys_addr(seg, off);
  if (m->protect_low && a < 0x400) return NEURO_ERR_KERNEL_PROTECTED;
  m->phys[a] = val;
  m->dirty_pages |= (1u << (a >> 16));
  return NEURO_OK;
}
NeuroErr phys_read16(const PhysMem *m, uint16_t seg, uint16_t off, uint16_t *out) {
  uint8_t lo, hi;
  NeuroErr e = phys_read8(m, seg, off, &lo);
  if (e) return e;
  e = phys_read8(m, seg, (uint16_t)(off + 1), &hi);
  if (e) return e;
  *out = make_u16(hi, lo);
  return NEURO_OK;
}
NeuroErr phys_write16(PhysMem *m, uint16_t seg, uint16_t off, uint16_t val) {
  NeuroErr e = phys_write8(m, seg, off, lo_byte(val));
  if (e) return e;
  return phys_write8(m, seg, (uint16_t)(off + 1), hi_byte(val));
}

void ports_init(Ports *p) {
  memset(p->data, 0, MAX_PORTS);
  memset(p->trap, 0, MAX_PORTS);
  p->data[0x40] = 0x00;
  p->data[0x41] = 0x00;
  p->data[0x42] = 0x00;
  p->data[0x43] = 0x36;
  p->data[0x61] = 0x00;
}
NeuroErr ports_inb(const Ports *p, uint16_t port, uint8_t *out) {
  if (p->trap[port]) return NEURO_ERR_INVALID_ADDR;
  *out = p->data[port];
  return NEURO_OK;
}
NeuroErr ports_outb(Ports *p, uint16_t port, uint8_t val) {
  if (p->trap[port]) return NEURO_ERR_INVALID_ADDR;
  p->data[port] = val;
  return NEURO_OK;
}

void idt_init(Idt *idt) { memset(idt->vectors, 0, sizeof(idt->vectors)); }
void idt_set(Idt *idt, uint8_t vec, uint16_t seg, uint16_t off) {
  idt->vectors[vec] = ((uint32_t)seg << 16) | off;
}
void idt_defaults(Idt *idt) {
  int i;
  for (i = 0; i < IDT_SIZE; i++)
    idt->vectors[i] = 0xF0000000u | ((uint32_t)i * 16);
  idt_set(idt, 0x00, 0xF000, 0x0010);
  idt_set(idt, 0x08, 0xF000, 0x0100);
  idt_set(idt, 0x09, 0xF000, 0x0200);
  idt_set(idt, 0x10, 0xF000, 0x1000);
  idt_set(idt, 0x13, 0xF000, 0x1300);
  idt_set(idt, 0x21, 0xF000, 0x2100);
}
void idt_get(const Idt *idt, uint8_t vec, uint16_t *seg, uint16_t *off) {
  uint32_t v = idt->vectors[vec];
  *seg = (uint16_t)(v >> 16);
  *off = (uint16_t)(v & 0xFFFF);
}
NeuroErr idt_load_from_mem(Idt *idt, const PhysMem *m) {
  int i;
  for (i = 0; i < IDT_SIZE; i++) {
    uint16_t off, seg;
    NeuroErr e = phys_read16(m, 0, (uint16_t)(i * 4), &off);
    if (e) return e;
    e = phys_read16(m, 0, (uint16_t)(i * 4 + 2), &seg);
    if (e) return e;
    idt->vectors[i] = ((uint32_t)seg << 16) | off;
  }
  return NEURO_OK;
}
NeuroErr idt_write_to_mem(const Idt *idt, PhysMem *m) {
  int i;
  for (i = 0; i < IDT_SIZE; i++) {
    uint16_t seg = (uint16_t)(idt->vectors[i] >> 16);
    uint16_t off = (uint16_t)(idt->vectors[i] & 0xFFFF);
    NeuroErr e = phys_write16(m, 0, (uint16_t)(i * 4), off);
    if (e) return e;
    e = phys_write16(m, 0, (uint16_t)(i * 4 + 2), seg);
    if (e) return e;
  }
  return NEURO_OK;
}

void neuro_hex_dump(const uint8_t *data, int len, uint16_t base, int cols, char *buf, int bufsz) {
  int pos = 0;
  int i, j;
  for (i = 0; i < len && pos < bufsz - 60; i += cols) {
    pos += snprintf(buf + pos, bufsz - pos, "%04X:", (uint16_t)(base + i));
    for (j = 0; j < cols && i + j < len; j++)
      pos += snprintf(buf + pos, bufsz - pos, " %02X", data[i + j]);
    for (j = (len - i < cols ? len - i : cols); j < cols; j++)
      pos += snprintf(buf + pos, bufsz - pos, "   ");
    pos += snprintf(buf + pos, bufsz - pos, "  ");
    for (j = 0; j < cols && i + j < len; j++) {
      uint8_t c = data[i + j];
      buf[pos++] = (c >= 0x20 && c < 0x7F) ? (char)c : '.';
    }
    buf[pos++] = '\n';
  }
  buf[pos] = 0;
}

static float tanh_approx(float x) {
  if (x > 4.0f) return 1.0f;
  if (x < -4.0f) return -1.0f;
  float x2 = x * x;
  return x * (27.0f + x2) / (27.0f + 9.0f * x2);
}

void neural_init(NeuralCore *nc, Xorshift64 *rng) {
  memset(nc, 0, sizeof(*nc));
  int topo[] = {8, 16, 12, 4};
  int l, n, w;
  nc->n_layers = 4;
  for (l = 0; l < 4; l++) nc->layer_sizes[l] = topo[l];
  for (l = 1; l < 4; l++) {
    int n_in = topo[l - 1];
    for (n = 0; n < topo[l]; n++) {
      nc->layers[l][n].n_inputs = n_in;
      nc->layers[l][n].bias = xorshift64_f32(rng) * 2.0f - 1.0f;
      for (w = 0; w < n_in && w < 64; w++)
        nc->layers[l][n].weights[w] = xorshift64_f32(rng) * 2.0f - 1.0f;
    }
  }
}

void neural_forward(NeuralCore *nc, const float *inputs, float *outputs) {
  float buf_a[64], buf_b[64];
  int l, n, w;
  memcpy(buf_a, inputs, (size_t)nc->layer_sizes[0] * sizeof(float));
  for (l = 1; l < nc->n_layers; l++) {
    float *src = buf_a, *dst = buf_b;
    int n_out = nc->layer_sizes[l];
    int n_in  = nc->layer_sizes[l - 1];
    for (n = 0; n < n_out; n++) {
      float s = nc->layers[l][n].bias;
      for (w = 0; w < n_in && w < 64; w++)
        s += nc->layers[l][n].weights[w] * src[w];
      nc->layers[l][n].activation = tanh_approx(s);
      dst[n] = nc->layers[l][n].activation;
    }
    memcpy(buf_a, buf_b, (size_t)n_out * sizeof(float));
  }
  {
    int n_out = nc->layer_sizes[nc->n_layers - 1];
    memcpy(outputs, buf_a, (size_t)n_out * sizeof(float));
  }
}

void neural_fragment_pulse(NeuralCore *nc, float intensity) {
  nc->research_progress += intensity * 0.1f;
  if (nc->research_progress >= 1.0f && !nc->awakened) {
    nc->awakened = true;
    nc->cores_online++;
  }
}

bool neural_is_awakened(const NeuralCore *nc) { return nc->awakened; }

void cpu_init(Cpu8086 *cpu) {
    memset(cpu, 0, sizeof(*cpu));
    cpu->regs.sp = 0xFFFE;
    cpu->regs.flags = 0x0202;
    cpu->regs.cs = 0xF000;
}

void cpu_init_segs(Cpu8086 *cpu, uint16_t cs, uint16_t ds, uint16_t es, uint16_t ss, uint16_t sp, uint16_t ip) {
    cpu_init(cpu);
    cpu->regs.cs = cs; cpu->regs.ds = ds;
    cpu->regs.es = es; cpu->regs.ss = ss;
    cpu->regs.sp = sp; cpu->regs.ip = ip;
}

static uint8_t cpu_fetch(Cpu8086 *cpu) {
    uint8_t b = cpu->mem[cpu->regs.ip];
    cpu->regs.ip++;
    return b;
}

static uint16_t cpu_fetch16(Cpu8086 *cpu) {
    uint8_t lo = cpu_fetch(cpu);
    uint8_t hi = cpu_fetch(cpu);
    return make_u16(hi, lo);
}

static NeuroErr cpu_push(Cpu8086 *cpu, uint16_t val) {
    if (cpu->regs.sp < 2) return NEURO_ERR_STACK_OVERFLOW;
    cpu->regs.sp -= 2;
    cpu->mem[cpu->regs.sp]     = lo_byte(val);
    cpu->mem[cpu->regs.sp + 1] = hi_byte(val);
    return NEURO_OK;
}

static NeuroErr cpu_pop(Cpu8086 *cpu, uint16_t *val) {
    if (cpu->regs.sp > 0xFFFE) return NEURO_ERR_STACK_UNDERFLOW;
    *val = make_u16(cpu->mem[cpu->regs.sp + 1], cpu->mem[cpu->regs.sp]);
    cpu->regs.sp += 2;
    return NEURO_OK;
}

static uint16_t *reg16(Cpu8086 *cpu, uint8_t r) {
    switch (r & 7) {
        case 0: return &cpu->regs.ax;
        case 1: return &cpu->regs.cx;
        case 2: return &cpu->regs.dx;
        case 3: return &cpu->regs.bx;
        case 4: return &cpu->regs.sp;
        case 5: return &cpu->regs.bp;
        case 6: return &cpu->regs.si;
        default: return &cpu->regs.di;
    }
}

static uint8_t get_reg8(Cpu8086 *cpu, uint8_t r) {
    uint16_t v = *reg16(cpu, r & 3);
    return (r & 4) ? hi_byte(v) : lo_byte(v);
}

static void set_reg8(Cpu8086 *cpu, uint8_t r, uint8_t val) {
    uint16_t *p = reg16(cpu, r & 3);
    *p = (r & 4) ? make_u16(val, lo_byte(*p)) : make_u16(hi_byte(*p), val);
}

static uint16_t alu_op(Regs8086 *R, int op, uint16_t a, uint16_t b, bool word) {
    unsigned w = word ? 16u : 8u;
    uint32_t mask = word ? 0xFFFFu : 0xFFu;
    uint32_t sign = 1u << (w - 1);
    uint32_t ua = a & mask, ub = b & mask, r = 0;
    uint32_t c = (op == 2 || op == 3) && (R->flags & FLAG_CF) ? 1u : 0u;
    bool cf = false, of = false, af = false;

    switch (op) {
    case 0: case 2:
        r = (ua + ub + c) & mask;
        cf = (ua + ub + c) > mask;
        of = (~(ua ^ ub) & (ua ^ r) & sign) != 0;
        af = ((ua ^ ub ^ r) & 0x10) != 0;
        break;
    case 3: case 5: case 7:
        r = (ua - ub - c) & mask;
        cf = ua < ub + c;
        of = ((ua ^ ub) & (ua ^ r) & sign) != 0;
        af = ((ua ^ ub ^ r) & 0x10) != 0;
        break;
    case 1: r = (ua | ub) & mask; break;
    case 4: r = (ua & ub) & mask; break;
    default: r = (ua ^ ub) & mask; break;
    }

    regs_set_flag(R, FLAG_CF, cf);
    regs_set_flag(R, FLAG_OF, of);
    regs_set_flag(R, FLAG_AF, af);
    regs_set_flag(R, FLAG_ZF, r == 0);
    regs_set_flag(R, FLAG_SF, (r & sign) != 0);
    regs_set_flag(R, FLAG_PF, flag_parity((uint8_t)r));
    return (uint16_t)r;
}

static NeuroErr cpu_alu_form(Cpu8086 *cpu, uint8_t op) {
    Regs8086 *R = &cpu->regs;
    int aluop = (op >> 3) & 7;
    int form = op & 7;
    bool word = (form & 1) != 0;

    if (form >= 4) {
        uint16_t a, b, r;
        if (word) { a = R->ax; b = cpu_fetch16(cpu); }
        else      { a = get_reg8(cpu, 0); b = cpu_fetch(cpu); }
        r = alu_op(R, aluop, a, b, word);
        if (aluop != 7) {
            if (word) R->ax = r; else set_reg8(cpu, 0, (uint8_t)r);
        }
        return NEURO_OK;
    }

    uint8_t modrm = cpu_fetch(cpu);
    int reg = (modrm >> 3) & 7, rm = modrm & 7;
    bool rm_is_dst = (form < 2);
    uint16_t rmv, regv;
    if (word) { rmv = *reg16(cpu, rm); regv = *reg16(cpu, reg); }
    else      { rmv = get_reg8(cpu, rm); regv = get_reg8(cpu, reg); }

    uint16_t a = rm_is_dst ? rmv : regv;
    uint16_t b = rm_is_dst ? regv : rmv;
    uint16_t r = alu_op(R, aluop, a, b, word);

    if (aluop != 7) {
        int dst = rm_is_dst ? rm : reg;
        if (word) *reg16(cpu, dst) = r; else set_reg8(cpu, dst, (uint8_t)r);
    }
    return NEURO_OK;
}

NeuroErr cpu_step(Cpu8086 *cpu) {
    if (cpu->halted) return NEURO_ERR_HALTED;
    uint8_t op = cpu_fetch(cpu);
    cpu->cycles++;
    Regs8086 *R = &cpu->regs;

    if (op < 0x40 && (op & 7) < 6) {
        return cpu_alu_form(cpu, op);
    }
    if (op >= 0xB0 && op <= 0xB7) {
        set_reg8(cpu, (uint8_t)(op - 0xB0), cpu_fetch(cpu));
        return NEURO_OK;
    }

    switch (op) {
    case 0x40: R->ax++; break;
    case 0x41: R->cx++; break;
    case 0x42: R->dx++; break;
    case 0x43: R->bx++; break;
    case 0x44: R->sp++; break;
    case 0x45: R->bp++; break;
    case 0x46: R->si++; break;
    case 0x47: R->di++; break;
    case 0x48: R->ax--; break;
    case 0x49: R->cx--; break;
    case 0x4A: R->dx--; break;
    case 0x4B: R->bx--; break;
    case 0x4C: R->sp--; break;
    case 0x4D: R->bp--; break;
    case 0x4E: R->si--; break;
    case 0x4F: R->di--; break;
    case 0x50: { NeuroErr e = cpu_push(cpu, R->ax); if (e) return e; break; }
    case 0x51: { NeuroErr e = cpu_push(cpu, R->cx); if (e) return e; break; }
    case 0x52: { NeuroErr e = cpu_push(cpu, R->dx); if (e) return e; break; }
    case 0x53: { NeuroErr e = cpu_push(cpu, R->bx); if (e) return e; break; }
    case 0x54: { NeuroErr e = cpu_push(cpu, R->sp); if (e) return e; break; }
    case 0x55: { NeuroErr e = cpu_push(cpu, R->bp); if (e) return e; break; }
    case 0x56: { NeuroErr e = cpu_push(cpu, R->si); if (e) return e; break; }
    case 0x57: { NeuroErr e = cpu_push(cpu, R->di); if (e) return e; break; }
    case 0x58: { NeuroErr e = cpu_pop(cpu, &R->ax); if (e) return e; break; }
    case 0x59: { NeuroErr e = cpu_pop(cpu, &R->cx); if (e) return e; break; }
    case 0x5A: { NeuroErr e = cpu_pop(cpu, &R->dx); if (e) return e; break; }
    case 0x5B: { NeuroErr e = cpu_pop(cpu, &R->bx); if (e) return e; break; }
    case 0x5C: { NeuroErr e = cpu_pop(cpu, &R->sp); if (e) return e; break; }
    case 0x5D: { NeuroErr e = cpu_pop(cpu, &R->bp); if (e) return e; break; }
    case 0x5E: { NeuroErr e = cpu_pop(cpu, &R->si); if (e) return e; break; }
    case 0x5F: { NeuroErr e = cpu_pop(cpu, &R->di); if (e) return e; break; }
    case 0x70: { int8_t d = (int8_t)cpu_fetch(cpu); if (regs_get_flag(R, FLAG_OF)) R->ip += d; break; }
    case 0x71: { int8_t d = (int8_t)cpu_fetch(cpu); if (!regs_get_flag(R, FLAG_OF)) R->ip += d; break; }
    case 0x72: { int8_t d = (int8_t)cpu_fetch(cpu); if (regs_get_flag(R, FLAG_CF)) R->ip += d; break; }
    case 0x73: { int8_t d = (int8_t)cpu_fetch(cpu); if (!regs_get_flag(R, FLAG_CF)) R->ip += d; break; }
    case 0x74: { int8_t d = (int8_t)cpu_fetch(cpu); if (regs_get_flag(R, FLAG_ZF)) R->ip += d; break; }
    case 0x75: { int8_t d = (int8_t)cpu_fetch(cpu); if (!regs_get_flag(R, FLAG_ZF)) R->ip += d; break; }
    case 0x78: { int8_t d = (int8_t)cpu_fetch(cpu); if (regs_get_flag(R, FLAG_SF)) R->ip += d; break; }
    case 0x79: { int8_t d = (int8_t)cpu_fetch(cpu); if (!regs_get_flag(R, FLAG_SF)) R->ip += d; break; }
    case 0x7C: { int8_t d = (int8_t)cpu_fetch(cpu); if (regs_get_flag(R, FLAG_SF) != regs_get_flag(R, FLAG_OF)) R->ip += d; break; }
    case 0x7D: { int8_t d = (int8_t)cpu_fetch(cpu); if (regs_get_flag(R, FLAG_SF) == regs_get_flag(R, FLAG_OF)) R->ip += d; break; }
    case 0x7E: { int8_t d = (int8_t)cpu_fetch(cpu); if (regs_get_flag(R, FLAG_ZF) || (regs_get_flag(R, FLAG_SF) != regs_get_flag(R, FLAG_OF))) R->ip += d; break; }
    case 0x7F: { int8_t d = (int8_t)cpu_fetch(cpu); if (!regs_get_flag(R, FLAG_ZF) && (regs_get_flag(R, FLAG_SF) == regs_get_flag(R, FLAG_OF))) R->ip += d; break; }
    case 0x89: {
        uint8_t modrm = cpu_fetch(cpu);
        uint16_t *dst = reg16(cpu, modrm & 7);
        uint16_t *src = reg16(cpu, (modrm >> 3) & 7);
        *dst = *src;
        break;
    }
    case 0x8B: {
        uint8_t modrm = cpu_fetch(cpu);
        uint16_t *dst = reg16(cpu, (modrm >> 3) & 7);
        uint16_t *src = reg16(cpu, modrm & 7);
        *dst = *src;
        break;
    }
    case 0x90: break;
    case 0x91: case 0x92: case 0x93: case 0x94:
    case 0x95: case 0x96: case 0x97: {
        uint16_t *rn = reg16(cpu, op - 0x90);
        uint16_t  tmp = cpu->regs.ax;
        cpu->regs.ax = *rn;
        *rn = tmp;
        break;
    }
    case 0xB8: R->ax = cpu_fetch16(cpu); break;
    case 0xB9: R->cx = cpu_fetch16(cpu); break;
    case 0xBA: R->dx = cpu_fetch16(cpu); break;
    case 0xBB: R->bx = cpu_fetch16(cpu); break;
    case 0xBC: R->sp = cpu_fetch16(cpu); break;
    case 0xBD: R->bp = cpu_fetch16(cpu); break;
    case 0xBE: R->si = cpu_fetch16(cpu); break;
    case 0xBF: R->di = cpu_fetch16(cpu); break;
    case 0xE2: {
        int8_t d = (int8_t)cpu_fetch(cpu);
        R->cx--;
        if (R->cx != 0) R->ip += d;
        break;
    }
    case 0xE8: {
        int16_t rel = (int16_t)cpu_fetch16(cpu);
        NeuroErr e = cpu_push(cpu, R->ip);
        if (e) return e;
        R->ip = (uint16_t)(R->ip + rel);
        break;
    }
    case 0xE9: {
        int16_t rel = (int16_t)cpu_fetch16(cpu);
        R->ip = (uint16_t)(R->ip + rel);
        break;
    }
    case 0xEB: {
        int8_t d = (int8_t)cpu_fetch(cpu);
        R->ip += d;
        break;
    }
    case 0xF4: cpu->halted = true; break;
    case 0xF8: regs_set_flag(R, FLAG_CF, false); break;
    case 0xF9: regs_set_flag(R, FLAG_CF, true); break;
    case 0xFA: regs_set_flag(R, FLAG_IF, false); break;
    case 0xFB: regs_set_flag(R, FLAG_IF, true); break;
    case 0xFC: regs_set_flag(R, FLAG_DF, false); break;
    case 0xFD: regs_set_flag(R, FLAG_DF, true); break;
    case 0xC3: {
        uint16_t addr;
        NeuroErr e = cpu_pop(cpu, &addr);
        if (e) return e;
        R->ip = addr;
        break;
    }
    case 0xCD: {
        uint8_t vec = cpu_fetch(cpu);
        cpu->irq_pending = (int)vec;
        if (vec == 0x20) cpu->halted = true;
        break;
    }
    case 0xF6: {
        uint8_t modrm = cpu_fetch(cpu);
        uint8_t ext = (modrm >> 3) & 7;
        if (ext == 0) {
            uint8_t imm = cpu_fetch(cpu);
            uint8_t src = lo_byte(*reg16(cpu, modrm & 7));
            uint8_t r = src & imm;
            regs_update_logic(R, r);
        }
        break;
    }
    case 0xF7: {
        uint8_t modrm = cpu_fetch(cpu);
        uint8_t ext = (modrm >> 3) & 7;
        if (ext == 0) {
            uint16_t imm = cpu_fetch16(cpu);
            uint16_t src = *reg16(cpu, modrm & 7);
            uint16_t r = src & imm;
            regs_update_logic(R, r);
        } else if (ext == 6) {
            uint16_t divisor = *reg16(cpu, modrm & 7);
            if (!divisor) return NEURO_ERR_DIV_ZERO;
            uint32_t dividend = ((uint32_t)R->dx << 16) | R->ax;
            R->ax = (uint16_t)(dividend / divisor);
            R->dx = (uint16_t)(dividend % divisor);
        }
        break;
    }
    case 0xFF: {
        uint8_t modrm = cpu_fetch(cpu);
        uint8_t ext = (modrm >> 3) & 7;
        if (ext == 2) {
            R->ip = *reg16(cpu, modrm & 7);
        } else if (ext == 4) {
            NeuroErr e = cpu_push(cpu, R->ip);
            if (e) return e;
            R->ip = *reg16(cpu, modrm & 7);
        } else if (ext == 0) {
            (*reg16(cpu, modrm & 7))++;
        } else if (ext == 1) {
            (*reg16(cpu, modrm & 7))--;
        }
        break;
    }
    default:
        return NEURO_ERR_INVALID_OPCODE;
    }
    return NEURO_OK;
}

int cpu_dump_regs(const Cpu8086 *cpu, char *buf, int sz) {
    const Regs8086 *R = &cpu->regs;
    return snprintf(buf, sz,
        "AX=%04X BX=%04X CX=%04X DX=%04X SI=%04X DI=%04X SP=%04X BP=%04X IP=%04X FL=%04X",
        R->ax, R->bx, R->cx, R->dx, R->si, R->di, R->sp, R->bp, R->ip, R->flags);
}

int cpu_dump_flags(const Cpu8086 *cpu, char *buf, int sz) {
    const Regs8086 *R = &cpu->regs;
    return snprintf(buf, sz, "%s%s%s%s%s%s%s%s%s",
        regs_get_flag(R, FLAG_CF) ? "CF " : "",
        regs_get_flag(R, FLAG_PF) ? "PF " : "",
        regs_get_flag(R, FLAG_AF) ? "AF " : "",
        regs_get_flag(R, FLAG_ZF) ? "ZF " : "",
        regs_get_flag(R, FLAG_SF) ? "SF " : "",
        regs_get_flag(R, FLAG_TF) ? "TF " : "",
        regs_get_flag(R, FLAG_IF) ? "IF " : "",
        regs_get_flag(R, FLAG_DF) ? "DF " : "",
        regs_get_flag(R, FLAG_OF) ? "OF " : "");
}

void cpu_dump_mem(const Cpu8086 *cpu, uint16_t addr, int len, char *buf, int bufsz) {
    neuro_hex_dump(cpu->mem + addr, len, addr, 8, buf, bufsz);
}

void cpu_write_mem(Cpu8086 *cpu, uint16_t addr, const uint8_t *data, int n) {
    int cap = PROC_MEM_SIZE - (int)addr;
    if (n > cap) n = cap;
    memcpy(cpu->mem + addr, data, n);
}

%}
