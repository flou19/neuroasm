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
    const uint8_t *p = m->phys + i * 4;
    idt->vectors[i] = ((uint32_t)make_u16(p[3], p[2]) << 16) | make_u16(p[1], p[0]);
  }
  return NEURO_OK;
}
NeuroErr idt_write_to_mem(const Idt *idt, PhysMem *m) {
  int i;
  for (i = 0; i < IDT_SIZE; i++) {
    uint8_t *p = m->phys + i * 4;
    uint16_t seg = (uint16_t)(idt->vectors[i] >> 16);
    uint16_t off = (uint16_t)(idt->vectors[i] & 0xFFFF);
    p[0] = lo_byte(off); p[1] = hi_byte(off);
    p[2] = lo_byte(seg); p[3] = hi_byte(seg);
  }
  m->dirty_pages |= 1u;
  return NEURO_OK;
}

static int hex_emit(char *buf, int bufsz, int pos, const char *str) {
  while (*str && pos < bufsz - 1) buf[pos++] = *str++;
  return pos;
}

static int hex_emit_char(char *buf, int bufsz, int pos, char c) {
  if (pos < bufsz - 1) buf[pos++] = c;
  return pos;
}

void neuro_hex_dump(const uint8_t *data, int len, uint16_t base, int cols, char *buf, int bufsz) {
  int pos = 0, i, j;
  char tmp[8];
  if (bufsz <= 0) return;
  if (cols < 1) cols = 1;
  if (cols > 16) cols = 16;
  for (i = 0; i < len; i += cols) {
    snprintf(tmp, sizeof tmp, "%04X:", (uint16_t)(base + i));
    pos = hex_emit(buf, bufsz, pos, tmp);
    for (j = 0; j < cols; j++) {
      if (i + j < len) {
        snprintf(tmp, sizeof tmp, " %02X", data[i + j]);
        pos = hex_emit(buf, bufsz, pos, tmp);
      } else {
        pos = hex_emit(buf, bufsz, pos, "   ");
      }
    }
    pos = hex_emit(buf, bufsz, pos, "  ");
    for (j = 0; j < cols && i + j < len; j++) {
      uint8_t c = data[i + j];
      pos = hex_emit_char(buf, bufsz, pos, (c >= 0x20 && c < 0x7F) ? (char)c : '.');
    }
    pos = hex_emit_char(buf, bufsz, pos, '\n');
  }
  buf[pos] = 0;
}

static float tanh_approx(float x) {
  if (x > 4.0f) return 1.0f;
  if (x < -4.0f) return -1.0f;
  float x2 = x * x;
  float r = x * (27.0f + x2) / (27.0f + 9.0f * x2);
  return r > 1.0f ? 1.0f : (r < -1.0f ? -1.0f : r);
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
  if (intensity < 0.0f) intensity = 0.0f;
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

typedef struct {
    bool     is_reg;
    uint8_t  reg;
    uint16_t addr;
} Operand;

static Operand reg_operand(uint8_t r) {
    Operand o;
    o.is_reg = true;
    o.reg = r;
    o.addr = 0;
    return o;
}

static Operand decode_rm(Cpu8086 *cpu, uint8_t modrm) {
    Regs8086 *R = &cpu->regs;
    uint8_t mod = modrm >> 6;
    uint8_t rm = modrm & 7;
    uint16_t base = 0, disp = 0;
    Operand o;
    if (mod == 3) return reg_operand(rm);
    switch (rm) {
        case 0: base = (uint16_t)(R->bx + R->si); break;
        case 1: base = (uint16_t)(R->bx + R->di); break;
        case 2: base = (uint16_t)(R->bp + R->si); break;
        case 3: base = (uint16_t)(R->bp + R->di); break;
        case 4: base = R->si; break;
        case 5: base = R->di; break;
        case 6: base = (mod == 0) ? 0 : R->bp; break;
        default: base = R->bx; break;
    }
    if (mod == 0 && rm == 6) disp = cpu_fetch16(cpu);
    else if (mod == 1) disp = (uint16_t)(int16_t)(int8_t)cpu_fetch(cpu);
    else if (mod == 2) disp = cpu_fetch16(cpu);
    o.is_reg = false;
    o.reg = 0;
    o.addr = (uint16_t)(base + disp);
    return o;
}

static uint16_t op_read(Cpu8086 *cpu, Operand o, bool word) {
    if (o.is_reg) return word ? *reg16(cpu, o.reg) : get_reg8(cpu, o.reg);
    if (word) return make_u16(cpu->mem[(uint16_t)(o.addr + 1)], cpu->mem[o.addr]);
    return cpu->mem[o.addr];
}

static void op_write(Cpu8086 *cpu, Operand o, bool word, uint16_t v) {
    if (o.is_reg) {
        if (word) *reg16(cpu, o.reg) = v;
        else set_reg8(cpu, o.reg, (uint8_t)v);
        return;
    }
    if (word) {
        cpu->mem[o.addr] = lo_byte(v);
        cpu->mem[(uint16_t)(o.addr + 1)] = hi_byte(v);
    } else {
        cpu->mem[o.addr] = (uint8_t)v;
    }
}

static NeuroErr cpu_alu_form(Cpu8086 *cpu, uint8_t op) {
    Regs8086 *R = &cpu->regs;
    int aluop = (op >> 3) & 7;
    int form = op & 7;
    bool word = (form & 1) != 0;

    if (form >= 4) {
        uint16_t imm = word ? cpu_fetch16(cpu) : cpu_fetch(cpu);
        Operand acc = reg_operand(0);
        uint16_t r = alu_op(R, aluop, op_read(cpu, acc, word), imm, word);
        if (aluop != 7) op_write(cpu, acc, word, r);
        return NEURO_OK;
    }

    uint8_t modrm = cpu_fetch(cpu);
    Operand rm = decode_rm(cpu, modrm);
    Operand rg = reg_operand((modrm >> 3) & 7);
    bool rm_is_dst = form < 2;
    Operand dst = rm_is_dst ? rm : rg;
    Operand src = rm_is_dst ? rg : rm;
    uint16_t r = alu_op(R, aluop, op_read(cpu, dst, word), op_read(cpu, src, word), word);
    if (aluop != 7) op_write(cpu, dst, word, r);
    return NEURO_OK;
}

static NeuroErr cpu_group1(Cpu8086 *cpu, uint8_t op) {
    Regs8086 *R = &cpu->regs;
    bool word = op != 0x80;
    uint8_t modrm = cpu_fetch(cpu);
    Operand o = decode_rm(cpu, modrm);
    uint16_t imm;
    if (op == 0x81) imm = cpu_fetch16(cpu);
    else if (op == 0x83) imm = (uint16_t)(int16_t)(int8_t)cpu_fetch(cpu);
    else imm = cpu_fetch(cpu);
    int aluop = (modrm >> 3) & 7;
    uint16_t r = alu_op(R, aluop, op_read(cpu, o, word), imm, word);
    if (aluop != 7) op_write(cpu, o, word, r);
    return NEURO_OK;
}

static uint16_t cpu_incdec(Regs8086 *R, uint16_t a, bool dec, bool word) {
    uint32_t mask = word ? 0xFFFFu : 0xFFu;
    uint32_t sign = word ? 0x8000u : 0x80u;
    uint32_t ua = a & mask;
    uint32_t r = dec ? ((ua - 1) & mask) : ((ua + 1) & mask);
    bool of = dec ? (ua == sign) : (ua == (sign - 1));
    bool af = ((ua ^ 1u ^ r) & 0x10) != 0;
    regs_set_flag(R, FLAG_OF, of);
    regs_set_flag(R, FLAG_AF, af);
    regs_set_flag(R, FLAG_ZF, r == 0);
    regs_set_flag(R, FLAG_SF, (r & sign) != 0);
    regs_set_flag(R, FLAG_PF, flag_parity((uint8_t)r));
    return (uint16_t)r;
}

static bool cpu_cond(const Regs8086 *R, uint8_t c) {
    bool cf = regs_get_flag(R, FLAG_CF);
    bool zf = regs_get_flag(R, FLAG_ZF);
    bool sf = regs_get_flag(R, FLAG_SF);
    bool of = regs_get_flag(R, FLAG_OF);
    bool pf = regs_get_flag(R, FLAG_PF);
    switch (c & 0xF) {
        case 0x0: return of;
        case 0x1: return !of;
        case 0x2: return cf;
        case 0x3: return !cf;
        case 0x4: return zf;
        case 0x5: return !zf;
        case 0x6: return cf || zf;
        case 0x7: return !cf && !zf;
        case 0x8: return sf;
        case 0x9: return !sf;
        case 0xA: return pf;
        case 0xB: return !pf;
        case 0xC: return sf != of;
        case 0xD: return sf == of;
        case 0xE: return zf || (sf != of);
        default:  return !zf && (sf == of);
    }
}

static uint16_t cpu_shift(Regs8086 *R, uint8_t ext, uint16_t v, unsigned count, bool word) {
    uint32_t mask = word ? 0xFFFFu : 0xFFu;
    uint32_t sign = word ? 0x8000u : 0x80u;
    uint32_t r = v & mask;
    if (count == 0) return (uint16_t)r;
    bool cf = regs_get_flag(R, FLAG_CF);
    uint32_t msb_in = r & sign;
    unsigned k;
    for (k = 0; k < count; k++) {
        switch (ext) {
            case 0: cf = (r & sign) != 0; r = ((r << 1) | (cf ? 1u : 0u)) & mask; break;
            case 1: cf = (r & 1) != 0; r = ((r >> 1) | (cf ? sign : 0u)) & mask; break;
            case 2: { bool old = cf; cf = (r & sign) != 0; r = ((r << 1) | (old ? 1u : 0u)) & mask; break; }
            case 3: { bool old = cf; cf = (r & 1) != 0; r = ((r >> 1) | (old ? sign : 0u)) & mask; break; }
            case 4: case 6: cf = (r & sign) != 0; r = (r << 1) & mask; break;
            case 5: cf = (r & 1) != 0; r = r >> 1; break;
            default: cf = (r & 1) != 0; r = ((r >> 1) | (r & sign)) & mask; break;
        }
    }
    regs_set_flag(R, FLAG_CF, cf);
    if (count == 1) {
        bool msb = (r & sign) != 0;
        bool nxt = (r & (sign >> 1)) != 0;
        bool of;
        switch (ext) {
            case 0: case 2: case 4: case 6: of = msb != cf; break;
            case 1: case 3: of = msb != nxt; break;
            case 5: of = msb_in != 0; break;
            default: of = false; break;
        }
        regs_set_flag(R, FLAG_OF, of);
    }
    if (ext >= 4) {
        regs_set_flag(R, FLAG_ZF, r == 0);
        regs_set_flag(R, FLAG_SF, (r & sign) != 0);
        regs_set_flag(R, FLAG_PF, flag_parity((uint8_t)r));
    }
    return (uint16_t)r;
}

static NeuroErr cpu_group2(Cpu8086 *cpu, uint8_t op) {
    bool word = (op & 1) != 0;
    uint8_t modrm = cpu_fetch(cpu);
    Operand o = decode_rm(cpu, modrm);
    unsigned count;
    if (op == 0xC0 || op == 0xC1) count = cpu_fetch(cpu);
    else if (op == 0xD0 || op == 0xD1) count = 1;
    else count = lo_byte(cpu->regs.cx);
    uint8_t ext = (modrm >> 3) & 7;
    uint16_t r = cpu_shift(&cpu->regs, ext, op_read(cpu, o, word), count, word);
    op_write(cpu, o, word, r);
    return NEURO_OK;
}

static void cpu_mul(Regs8086 *R, uint16_t v, bool word, bool sgn) {
    bool ovf;
    if (!word) {
        if (sgn) {
            int32_t p = (int8_t)lo_byte(R->ax) * (int8_t)(uint8_t)v;
            R->ax = (uint16_t)p;
            ovf = p != (int8_t)(uint8_t)p;
        } else {
            uint32_t p = lo_byte(R->ax) * (uint8_t)v;
            R->ax = (uint16_t)p;
            ovf = (p >> 8) != 0;
        }
    } else {
        if (sgn) {
            int64_t p = (int64_t)(int16_t)R->ax * (int16_t)v;
            R->ax = (uint16_t)p;
            R->dx = (uint16_t)((uint64_t)p >> 16);
            ovf = p != (int64_t)(int16_t)(uint16_t)p;
        } else {
            uint32_t p = (uint32_t)R->ax * v;
            R->ax = (uint16_t)p;
            R->dx = (uint16_t)(p >> 16);
            ovf = R->dx != 0;
        }
    }
    regs_set_flag(R, FLAG_CF, ovf);
    regs_set_flag(R, FLAG_OF, ovf);
}

static NeuroErr cpu_div(Regs8086 *R, uint16_t v, bool word, bool sgn) {
    if (!word) {
        uint8_t d = (uint8_t)v;
        if (!d) return NEURO_ERR_DIV_ZERO;
        if (sgn) {
            int32_t n = (int16_t)R->ax;
            int32_t dv = (int8_t)d;
            int32_t q = n / dv;
            int32_t rm = n % dv;
            if (q < -128 || q > 127) return NEURO_ERR_DIV_OVERFLOW;
            R->ax = (uint16_t)(((uint8_t)rm << 8) | (uint8_t)q);
        } else {
            uint32_t n = R->ax;
            uint32_t q = n / d;
            uint32_t rm = n % d;
            if (q > 0xFF) return NEURO_ERR_DIV_OVERFLOW;
            R->ax = (uint16_t)((rm << 8) | q);
        }
        return NEURO_OK;
    }
    if (!v) return NEURO_ERR_DIV_ZERO;
    if (sgn) {
        int64_t n = (int32_t)(((uint32_t)R->dx << 16) | R->ax);
        int64_t dv = (int16_t)v;
        int64_t q = n / dv;
        int64_t rm = n % dv;
        if (q < -32768 || q > 32767) return NEURO_ERR_DIV_OVERFLOW;
        R->ax = (uint16_t)q;
        R->dx = (uint16_t)rm;
    } else {
        uint64_t n = ((uint32_t)R->dx << 16) | R->ax;
        uint64_t q = n / v;
        uint64_t rm = n % v;
        if (q > 0xFFFF) return NEURO_ERR_DIV_OVERFLOW;
        R->ax = (uint16_t)q;
        R->dx = (uint16_t)rm;
    }
    return NEURO_OK;
}

static NeuroErr cpu_group3(Cpu8086 *cpu, bool word) {
    Regs8086 *R = &cpu->regs;
    uint8_t modrm = cpu_fetch(cpu);
    uint8_t ext = (modrm >> 3) & 7;
    Operand o = decode_rm(cpu, modrm);
    if (ext == 0) {
        uint16_t imm = word ? cpu_fetch16(cpu) : cpu_fetch(cpu);
        alu_op(R, 4, op_read(cpu, o, word), imm, word);
        return NEURO_OK;
    }
    if (ext == 1) return NEURO_ERR_INVALID_OPCODE;
    uint16_t v = op_read(cpu, o, word);
    switch (ext) {
        case 2: op_write(cpu, o, word, (uint16_t)~v); return NEURO_OK;
        case 3: op_write(cpu, o, word, alu_op(R, 5, 0, v, word)); return NEURO_OK;
        case 4: cpu_mul(R, v, word, false); return NEURO_OK;
        case 5: cpu_mul(R, v, word, true); return NEURO_OK;
        case 6: return cpu_div(R, v, word, false);
        default: return cpu_div(R, v, word, true);
    }
}

static NeuroErr cpu_group45(Cpu8086 *cpu, bool word) {
    Regs8086 *R = &cpu->regs;
    uint8_t modrm = cpu_fetch(cpu);
    uint8_t ext = (modrm >> 3) & 7;
    Operand o = decode_rm(cpu, modrm);
    NeuroErr e;
    if (!word && ext > 1) return NEURO_ERR_INVALID_OPCODE;
    switch (ext) {
        case 0: op_write(cpu, o, word, cpu_incdec(R, op_read(cpu, o, word), false, word)); return NEURO_OK;
        case 1: op_write(cpu, o, word, cpu_incdec(R, op_read(cpu, o, word), true, word)); return NEURO_OK;
        case 2: {
            uint16_t target = op_read(cpu, o, true);
            e = cpu_push(cpu, R->ip);
            if (e) return e;
            R->ip = target;
            return NEURO_OK;
        }
        case 4: R->ip = op_read(cpu, o, true); return NEURO_OK;
        case 6: return cpu_push(cpu, op_read(cpu, o, true));
        default: return NEURO_ERR_INVALID_OPCODE;
    }
}

NeuroErr cpu_step(Cpu8086 *cpu) {
    if (cpu->halted) return NEURO_ERR_HALTED;
    uint8_t op = cpu_fetch(cpu);
    cpu->cycles++;
    Regs8086 *R = &cpu->regs;
    NeuroErr e;

    if (op < 0x40 && (op & 7) < 6) return cpu_alu_form(cpu, op);

    if (op >= 0x40 && op <= 0x4F) {
        Operand o = reg_operand(op & 7);
        op_write(cpu, o, true, cpu_incdec(R, op_read(cpu, o, true), op >= 0x48, true));
        return NEURO_OK;
    }
    if (op >= 0x50 && op <= 0x57) return cpu_push(cpu, *reg16(cpu, op & 7));
    if (op >= 0x58 && op <= 0x5F) {
        uint16_t v;
        e = cpu_pop(cpu, &v);
        if (e) return e;
        *reg16(cpu, op & 7) = v;
        return NEURO_OK;
    }
    if (op >= 0x70 && op <= 0x7F) {
        int8_t d = (int8_t)cpu_fetch(cpu);
        if (cpu_cond(R, op)) R->ip = (uint16_t)(R->ip + d);
        return NEURO_OK;
    }
    if (op >= 0x91 && op <= 0x97) {
        uint16_t *rn = reg16(cpu, op & 7);
        uint16_t tmp = R->ax;
        R->ax = *rn;
        *rn = tmp;
        return NEURO_OK;
    }
    if (op >= 0xB0 && op <= 0xB7) {
        set_reg8(cpu, op & 7, cpu_fetch(cpu));
        return NEURO_OK;
    }
    if (op >= 0xB8 && op <= 0xBF) {
        *reg16(cpu, op & 7) = cpu_fetch16(cpu);
        return NEURO_OK;
    }
    if (op >= 0xE0 && op <= 0xE2) {
        int8_t d = (int8_t)cpu_fetch(cpu);
        bool take;
        R->cx--;
        take = R->cx != 0;
        if (op == 0xE0) take = take && !regs_get_flag(R, FLAG_ZF);
        else if (op == 0xE1) take = take && regs_get_flag(R, FLAG_ZF);
        if (take) R->ip = (uint16_t)(R->ip + d);
        return NEURO_OK;
    }

    switch (op) {
    case 0x80: case 0x81: case 0x83:
        return cpu_group1(cpu, op);
    case 0x84: case 0x85: {
        bool word = (op & 1) != 0;
        uint8_t modrm = cpu_fetch(cpu);
        Operand rm = decode_rm(cpu, modrm);
        Operand rg = reg_operand((modrm >> 3) & 7);
        alu_op(R, 4, op_read(cpu, rm, word), op_read(cpu, rg, word), word);
        return NEURO_OK;
    }
    case 0x86: case 0x87: {
        bool word = (op & 1) != 0;
        uint8_t modrm = cpu_fetch(cpu);
        Operand rm = decode_rm(cpu, modrm);
        Operand rg = reg_operand((modrm >> 3) & 7);
        uint16_t a = op_read(cpu, rm, word);
        uint16_t b = op_read(cpu, rg, word);
        op_write(cpu, rm, word, b);
        op_write(cpu, rg, word, a);
        return NEURO_OK;
    }
    case 0x88: case 0x89: case 0x8A: case 0x8B: {
        bool word = (op & 1) != 0;
        uint8_t modrm = cpu_fetch(cpu);
        Operand rm = decode_rm(cpu, modrm);
        Operand rg = reg_operand((modrm >> 3) & 7);
        if (op < 0x8A) op_write(cpu, rm, word, op_read(cpu, rg, word));
        else op_write(cpu, rg, word, op_read(cpu, rm, word));
        return NEURO_OK;
    }
    case 0x8D: {
        uint8_t modrm = cpu_fetch(cpu);
        Operand m = decode_rm(cpu, modrm);
        if (m.is_reg) return NEURO_ERR_INVALID_OPCODE;
        *reg16(cpu, (modrm >> 3) & 7) = m.addr;
        return NEURO_OK;
    }
    case 0x8F: {
        uint8_t modrm = cpu_fetch(cpu);
        Operand o = decode_rm(cpu, modrm);
        uint16_t v;
        if (((modrm >> 3) & 7) != 0) return NEURO_ERR_INVALID_OPCODE;
        e = cpu_pop(cpu, &v);
        if (e) return e;
        op_write(cpu, o, true, v);
        return NEURO_OK;
    }
    case 0x90: return NEURO_OK;
    case 0x98: R->ax = (uint16_t)(int16_t)(int8_t)lo_byte(R->ax); return NEURO_OK;
    case 0x99: R->dx = (R->ax & 0x8000) ? 0xFFFF : 0; return NEURO_OK;
    case 0x9C: return cpu_push(cpu, R->flags);
    case 0x9D: {
        uint16_t v;
        e = cpu_pop(cpu, &v);
        if (e) return e;
        R->flags = (uint16_t)(v | 0x0002);
        return NEURO_OK;
    }
    case 0x9E: R->flags = (uint16_t)((R->flags & ~0xD5u) | ((R->ax >> 8) & 0xD5u)); return NEURO_OK;
    case 0x9F: R->ax = (uint16_t)((R->ax & 0x00FF) | (((R->flags & 0xD5u) | 0x02u) << 8)); return NEURO_OK;
    case 0xA8: case 0xA9: {
        bool word = op == 0xA9;
        uint16_t imm = word ? cpu_fetch16(cpu) : cpu_fetch(cpu);
        Operand acc = reg_operand(0);
        alu_op(R, 4, op_read(cpu, acc, word), imm, word);
        return NEURO_OK;
    }
    case 0xC0: case 0xC1: case 0xD0: case 0xD1: case 0xD2: case 0xD3:
        return cpu_group2(cpu, op);
    case 0xC2: {
        uint16_t n = cpu_fetch16(cpu);
        uint16_t a;
        e = cpu_pop(cpu, &a);
        if (e) return e;
        R->ip = a;
        R->sp = (uint16_t)(R->sp + n);
        return NEURO_OK;
    }
    case 0xC3: {
        uint16_t a;
        e = cpu_pop(cpu, &a);
        if (e) return e;
        R->ip = a;
        return NEURO_OK;
    }
    case 0xC6: case 0xC7: {
        bool word = op == 0xC7;
        uint8_t modrm = cpu_fetch(cpu);
        Operand o = decode_rm(cpu, modrm);
        uint16_t imm;
        if (((modrm >> 3) & 7) != 0) return NEURO_ERR_INVALID_OPCODE;
        imm = word ? cpu_fetch16(cpu) : cpu_fetch(cpu);
        op_write(cpu, o, word, imm);
        return NEURO_OK;
    }
    case 0xCD: {
        uint8_t vec = cpu_fetch(cpu);
        cpu->irq_pending = (int)vec;
        if (vec == 0x20) cpu->halted = true;
        return NEURO_OK;
    }
    case 0xE3: {
        int8_t d = (int8_t)cpu_fetch(cpu);
        if (R->cx == 0) R->ip = (uint16_t)(R->ip + d);
        return NEURO_OK;
    }
    case 0xE8: {
        int16_t rel = (int16_t)cpu_fetch16(cpu);
        e = cpu_push(cpu, R->ip);
        if (e) return e;
        R->ip = (uint16_t)(R->ip + rel);
        return NEURO_OK;
    }
    case 0xE9: {
        int16_t rel = (int16_t)cpu_fetch16(cpu);
        R->ip = (uint16_t)(R->ip + rel);
        return NEURO_OK;
    }
    case 0xEB: {
        int8_t d = (int8_t)cpu_fetch(cpu);
        R->ip = (uint16_t)(R->ip + d);
        return NEURO_OK;
    }
    case 0xF4: cpu->halted = true; return NEURO_OK;
    case 0xF5: regs_set_flag(R, FLAG_CF, !regs_get_flag(R, FLAG_CF)); return NEURO_OK;
    case 0xF6: return cpu_group3(cpu, false);
    case 0xF7: return cpu_group3(cpu, true);
    case 0xF8: regs_set_flag(R, FLAG_CF, false); return NEURO_OK;
    case 0xF9: regs_set_flag(R, FLAG_CF, true); return NEURO_OK;
    case 0xFA: regs_set_flag(R, FLAG_IF, false); return NEURO_OK;
    case 0xFB: regs_set_flag(R, FLAG_IF, true); return NEURO_OK;
    case 0xFC: regs_set_flag(R, FLAG_DF, false); return NEURO_OK;
    case 0xFD: regs_set_flag(R, FLAG_DF, true); return NEURO_OK;
    case 0xFE: return cpu_group45(cpu, false);
    case 0xFF: return cpu_group45(cpu, true);
    default:
        return NEURO_ERR_INVALID_OPCODE;
    }
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
