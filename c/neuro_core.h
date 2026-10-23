#ifndef NEURO_CORE_H
#define NEURO_CORE_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#define PHYS_MEM_SIZE  (1 << 20)
#define PROC_MEM_SIZE  65536
#define MAX_PORTS      65536
#define MAX_PROCS      64
#define IDT_SIZE       256

#define FLAG_CF  (1u << 0)
#define FLAG_PF  (1u << 2)
#define FLAG_AF  (1u << 4)
#define FLAG_ZF  (1u << 6)
#define FLAG_SF  (1u << 7)
#define FLAG_TF  (1u << 8)
#define FLAG_IF  (1u << 9)
#define FLAG_DF  (1u << 10)
#define FLAG_OF  (1u << 11)

typedef struct {
    uint16_t ax, bx, cx, dx;
    uint16_t si, di, sp, bp;
    uint16_t ip, flags;
    uint16_t cs, ds, es, ss;
} Regs8086;

typedef struct {
    Regs8086 regs;
    uint8_t  mem[PROC_MEM_SIZE];
    bool     halted;
    uint64_t cycles;
    int      irq_pending;
} Cpu8086;

typedef struct {
    uint8_t  phys[PHYS_MEM_SIZE];
    bool     protect_low;
    uint32_t dirty_pages;
} PhysMem;

typedef struct {
    uint8_t  data[MAX_PORTS];
    bool     trap[MAX_PORTS];
} Ports;

typedef struct {
    uint32_t vectors[IDT_SIZE];
} Idt;

typedef struct {
    float    weights[64];
    float    bias;
    float    activation;
    int      n_inputs;
} Neuron;

typedef struct {
    Neuron   layers[4][64];
    int      layer_sizes[4];
    int      n_layers;
    float    research_progress;
    bool     awakened;
    uint32_t cores_online;
} NeuralCore;

typedef struct {
    uint64_t state;
} Xorshift64;

static inline uint64_t xorshift64_next(Xorshift64 *r) {
    r->state ^= r->state << 13;
    r->state ^= r->state >> 7;
    r->state ^= r->state << 17;
    return r->state;
}

static inline uint32_t xorshift64_range(Xorshift64 *r, uint32_t max) {
    if (!max) return 0;
    return (uint32_t)(xorshift64_next(r) % max);
}

static inline bool xorshift64_bool(Xorshift64 *r, uint32_t pct) {
    return (uint32_t)(xorshift64_next(r) % 100) < pct;
}

static inline uint16_t make_u16(uint8_t hi, uint8_t lo) { return ((uint16_t)hi << 8) | lo; }
static inline uint8_t  hi_byte(uint16_t v) { return (v >> 8) & 0xFF; }
static inline uint8_t  lo_byte(uint16_t v) { return v & 0xFF; }

static inline bool flag_zero(uint16_t v)       { return v == 0; }
static inline bool flag_sign(uint16_t v)       { return (v & 0x8000) != 0; }
static inline bool flag_parity(uint8_t v)      { v ^= v >> 4; v ^= v >> 2; v ^= v >> 1; return !(v & 1); }
static inline bool flag_carry_add(uint16_t a, uint16_t b) { return (uint32_t)a + b > 0xFFFF; }
static inline bool flag_carry_sub(uint16_t a, uint16_t b) { return a < b; }
static inline bool flag_overflow_add(uint16_t a, uint16_t b, uint16_t r) {
    return !((a ^ b) & 0x8000) && ((a ^ r) & 0x8000);
}
static inline bool flag_overflow_sub(uint16_t a, uint16_t b, uint16_t r) {
    return ((a ^ b) & 0x8000) && ((a ^ r) & 0x8000);
}
static inline bool flag_adjust_add(uint8_t a, uint8_t b) { return ((a & 0xF) + (b & 0xF)) > 0xF; }
static inline bool flag_adjust_sub(uint8_t a, uint8_t b) { return (a & 0xF) < (b & 0xF); }

static inline void regs_set_flag(Regs8086 *r, uint16_t f, bool on) {
    if (on) r->flags |= f; else r->flags &= ~f;
}
static inline bool regs_get_flag(const Regs8086 *r, uint16_t f) { return (r->flags & f) != 0; }

static inline void regs_update_add(Regs8086 *r, uint16_t a, uint16_t b, uint16_t res) {
    regs_set_flag(r, FLAG_CF, flag_carry_add(a, b));
    regs_set_flag(r, FLAG_OF, flag_overflow_add(a, b, res));
    regs_set_flag(r, FLAG_ZF, flag_zero(res));
    regs_set_flag(r, FLAG_SF, flag_sign(res));
    regs_set_flag(r, FLAG_PF, flag_parity(lo_byte(res)));
    regs_set_flag(r, FLAG_AF, flag_adjust_add(lo_byte(a), lo_byte(b)));
}

static inline void regs_update_sub(Regs8086 *r, uint16_t a, uint16_t b, uint16_t res) {
    regs_set_flag(r, FLAG_CF, flag_carry_sub(a, b));
    regs_set_flag(r, FLAG_OF, flag_overflow_sub(a, b, res));
    regs_set_flag(r, FLAG_ZF, flag_zero(res));
    regs_set_flag(r, FLAG_SF, flag_sign(res));
    regs_set_flag(r, FLAG_PF, flag_parity(lo_byte(res)));
    regs_set_flag(r, FLAG_AF, flag_adjust_sub(lo_byte(a), lo_byte(b)));
}

static inline void regs_update_logic(Regs8086 *r, uint16_t res) {
    regs_set_flag(r, FLAG_CF, false);
    regs_set_flag(r, FLAG_OF, false);
    regs_set_flag(r, FLAG_ZF, flag_zero(res));
    regs_set_flag(r, FLAG_SF, flag_sign(res));
    regs_set_flag(r, FLAG_PF, flag_parity(lo_byte(res)));
    regs_set_flag(r, FLAG_AF, false);
}

typedef enum {
    NEURO_OK = 0,
    NEURO_ERR_INVALID_ADDR,
    NEURO_ERR_INVALID_OPCODE,
    NEURO_ERR_DIV_ZERO,
    NEURO_ERR_STACK_OVERFLOW,
    NEURO_ERR_STACK_UNDERFLOW,
    NEURO_ERR_HALTED,
    NEURO_ERR_KERNEL_PROTECTED,
    NEURO_ERR_SANDBOX,
} NeuroErr;

extern void     cpu_init(Cpu8086 *cpu);
extern void     cpu_init_segs(Cpu8086 *cpu, uint16_t cs, uint16_t ds, uint16_t es, uint16_t ss, uint16_t sp, uint16_t ip);
extern NeuroErr cpu_step(Cpu8086 *cpu);
extern int      cpu_dump_regs(const Cpu8086 *cpu, char *buf, int sz);
extern int      cpu_dump_flags(const Cpu8086 *cpu, char *buf, int sz);
extern void     cpu_dump_mem(const Cpu8086 *cpu, uint16_t addr, int len, char *buf, int bufsz);
extern void     cpu_write_mem(Cpu8086 *cpu, uint16_t addr, const uint8_t *data, int n);
extern void     cpu_hex_dump(const uint8_t *data, int len, uint16_t base, int cols, char *buf, int bufsz);

extern void     phys_init(PhysMem *m);
extern NeuroErr phys_read8(const PhysMem *m, uint16_t seg, uint16_t off, uint8_t *out);
extern NeuroErr phys_write8(PhysMem *m, uint16_t seg, uint16_t off, uint8_t val);
extern NeuroErr phys_read16(const PhysMem *m, uint16_t seg, uint16_t off, uint16_t *out);
extern NeuroErr phys_write16(PhysMem *m, uint16_t seg, uint16_t off, uint16_t val);

extern void     ports_init(Ports *p);
extern NeuroErr ports_inb(const Ports *p, uint16_t port, uint8_t *out);
extern NeuroErr ports_outb(Ports *p, uint16_t port, uint8_t val);

extern void     idt_init(Idt *idt);
extern void     idt_defaults(Idt *idt);
extern void     idt_set(Idt *idt, uint8_t vec, uint16_t seg, uint16_t off);
extern void     idt_get(const Idt *idt, uint8_t vec, uint16_t *seg, uint16_t *off);
extern NeuroErr idt_load_from_mem(Idt *idt, const PhysMem *m);
extern NeuroErr idt_write_to_mem(const Idt *idt, PhysMem *m);

extern void     xorshift64_init_time(Xorshift64 *r);
extern float    xorshift64_f32(Xorshift64 *r);

extern void     neural_init(NeuralCore *nc, Xorshift64 *rng);
extern void     neural_forward(NeuralCore *nc, const float *inputs, float *outputs);
extern void     neural_fragment_pulse(NeuralCore *nc, float intensity);
extern bool     neural_is_awakened(const NeuralCore *nc);

extern void     neuro_hex_dump(const uint8_t *data, int len, uint16_t base, int cols, char *buf, int bufsz);
extern uint64_t neuro_now_ms(void);
extern void     neuro_sleep_ms(int ms);
extern void     neuro_beep(int times, int ms);

#endif
