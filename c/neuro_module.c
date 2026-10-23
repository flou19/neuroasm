#include <emacs-module.h>
#include "neuro_core.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int plugin_is_GPL_compatible;

static Cpu8086    g_cpu;
static PhysMem    g_mem;
static Ports      g_ports;
static Idt        g_idt;
static NeuralCore g_neural;
static Xorshift64 g_rng;
static bool       g_initialized = false;

static emacs_value QNIL, QT;

static void ensure_init(emacs_env *env) {
    if (g_initialized) return;
    xorshift64_init_time(&g_rng);
    phys_init(&g_mem);
    ports_init(&g_ports);
    idt_init(&g_idt);
    idt_defaults(&g_idt);
    neural_init(&g_neural, &g_rng);
    g_initialized = true;
    (void)env;
}

static emacs_value neuro_cpu_init_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)d;
    uint16_t cs = (uint16_t)env->extract_integer(env, args[0]);
    uint16_t ds = (uint16_t)env->extract_integer(env, args[1]);
    uint16_t es = (uint16_t)env->extract_integer(env, args[2]);
    uint16_t ss = (uint16_t)env->extract_integer(env, args[3]);
    uint16_t sp = (uint16_t)env->extract_integer(env, args[4]);
    uint16_t ip = (uint16_t)env->extract_integer(env, args[5]);
    cpu_init_segs(&g_cpu, cs, ds, es, ss, sp, ip);
    return QT;
}

static emacs_value neuro_cpu_step_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)args; (void)d;
    ensure_init(env);
    NeuroErr e = cpu_step(&g_cpu);
    return env->make_integer(env, (intmax_t)e);
}

static emacs_value neuro_cpu_regs_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)args; (void)d;
    ensure_init(env);
    char buf[256];
    cpu_dump_regs(&g_cpu, buf, sizeof(buf));
    return env->make_string(env, buf, (ptrdiff_t)strlen(buf));
}

static emacs_value neuro_cpu_flags_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)args; (void)d;
    ensure_init(env);
    char buf[128];
    cpu_dump_flags(&g_cpu, buf, sizeof(buf));
    return env->make_string(env, buf, (ptrdiff_t)strlen(buf));
}

static emacs_value neuro_cpu_get_reg_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)d;
    ensure_init(env);
    ptrdiff_t len = 0;
    env->copy_string_contents(env, args[0], NULL, &len);
    char *name = malloc((size_t)len);
    if (!name) return env->make_integer(env, 0);
    env->copy_string_contents(env, args[0], name, &len);
    Regs8086 *R = &g_cpu.regs;
    uint16_t v = 0;
    if      (!strcmp(name,"ax"))    v = R->ax;
    else if (!strcmp(name,"bx"))    v = R->bx;
    else if (!strcmp(name,"cx"))    v = R->cx;
    else if (!strcmp(name,"dx"))    v = R->dx;
    else if (!strcmp(name,"si"))    v = R->si;
    else if (!strcmp(name,"di"))    v = R->di;
    else if (!strcmp(name,"sp"))    v = R->sp;
    else if (!strcmp(name,"bp"))    v = R->bp;
    else if (!strcmp(name,"ip"))    v = R->ip;
    else if (!strcmp(name,"flags")) v = R->flags;
    else if (!strcmp(name,"cs"))    v = R->cs;
    else if (!strcmp(name,"ds"))    v = R->ds;
    else if (!strcmp(name,"es"))    v = R->es;
    else if (!strcmp(name,"ss"))    v = R->ss;
    free(name);
    return env->make_integer(env, v);
}

static emacs_value neuro_cpu_set_reg_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)d;
    ensure_init(env);
    ptrdiff_t len = 0;
    env->copy_string_contents(env, args[0], NULL, &len);
    char *name = malloc((size_t)len);
    if (!name) return QNIL;
    env->copy_string_contents(env, args[0], name, &len);
    uint16_t val = (uint16_t)env->extract_integer(env, args[1]);
    Regs8086 *R = &g_cpu.regs;
    if      (!strcmp(name,"ax")) R->ax = val;
    else if (!strcmp(name,"bx")) R->bx = val;
    else if (!strcmp(name,"cx")) R->cx = val;
    else if (!strcmp(name,"dx")) R->dx = val;
    else if (!strcmp(name,"si")) R->si = val;
    else if (!strcmp(name,"di")) R->di = val;
    else if (!strcmp(name,"sp")) R->sp = val;
    else if (!strcmp(name,"bp")) R->bp = val;
    else if (!strcmp(name,"ip")) R->ip = val;
    else if (!strcmp(name,"flags")) R->flags = val;
    free(name);
    return QT;
}

static emacs_value neuro_cpu_dump_mem_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)d;
    ensure_init(env);
    uint16_t addr = (uint16_t)env->extract_integer(env, args[0]);
    int len = (int)env->extract_integer(env, args[1]);
    if (len > 256) len = 256;
    char buf[4096];
    cpu_dump_mem(&g_cpu, addr, len, buf, sizeof(buf));
    return env->make_string(env, buf, (ptrdiff_t)strlen(buf));
}

static emacs_value neuro_cpu_write_mem_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)d;
    ensure_init(env);
    uint16_t addr = (uint16_t)env->extract_integer(env, args[0]);
    ptrdiff_t vlen = env->vec_size(env, args[1]);
    uint8_t *buf = malloc((size_t)vlen);
    if (!buf) return QNIL;
    for (ptrdiff_t i = 0; i < vlen; i++)
        buf[i] = (uint8_t)env->extract_integer(env, env->vec_get(env, args[1], i));
    cpu_write_mem(&g_cpu, addr, buf, (int)vlen);
    free(buf);
    return QT;
}

static emacs_value neuro_cpu_cycles_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)args; (void)d;
    ensure_init(env);
    return env->make_integer(env, (intmax_t)g_cpu.cycles);
}

static emacs_value neuro_rng_next_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)d;
    ensure_init(env);
    uint32_t max = (uint32_t)env->extract_integer(env, args[0]);
    return env->make_integer(env, (intmax_t)xorshift64_range(&g_rng, max));
}

static emacs_value neuro_rng_bool_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)d;
    ensure_init(env);
    uint32_t pct = (uint32_t)env->extract_integer(env, args[0]);
    return xorshift64_bool(&g_rng, pct) ? QT : QNIL;
}

static emacs_value neuro_rng_u16_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)args; (void)d;
    ensure_init(env);
    return env->make_integer(env, (intmax_t)(uint16_t)xorshift64_next(&g_rng));
}

static emacs_value neuro_now_ms_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)args; (void)d;
    return env->make_integer(env, (intmax_t)neuro_now_ms());
}

static emacs_value neuro_sleep_ms_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)d;
    int ms = (int)env->extract_integer(env, args[0]);
    neuro_sleep_ms(ms);
    return QNIL;
}

static emacs_value neuro_beep_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)d;
    int times = (int)env->extract_integer(env, args[0]);
    int ms    = (int)env->extract_integer(env, args[1]);
    neuro_beep(times, ms);
    return QNIL;
}

static emacs_value neuro_hex_dump_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)d;
    ptrdiff_t vlen = env->vec_size(env, args[0]);
    uint16_t base  = (uint16_t)env->extract_integer(env, args[1]);
    int cols       = (int)env->extract_integer(env, args[2]);
    uint8_t *data = malloc((size_t)vlen);
    if (!data) return QNIL;
    for (ptrdiff_t i = 0; i < vlen; i++)
        data[i] = (uint8_t)env->extract_integer(env, env->vec_get(env, args[0], i));
    size_t bufsz = (size_t)vlen * 6 + 64;
    char *buf = malloc(bufsz);
    if (!buf) { free(data); return QNIL; }
    neuro_hex_dump(data, (int)vlen, base, cols, buf, (int)bufsz);
    emacs_value r = env->make_string(env, buf, (ptrdiff_t)strlen(buf));
    free(data);
    free(buf);
    return r;
}

static emacs_value neural_forward_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)d;
    ensure_init(env);
    float inputs[8] = {0};
    ptrdiff_t ilen = env->vec_size(env, args[0]);
    if (ilen > 8) ilen = 8;
    for (ptrdiff_t i = 0; i < ilen; i++)
        inputs[i] = (float)env->extract_float(env, env->vec_get(env, args[0], i));
    float outputs[4] = {0};
    neural_forward(&g_neural, inputs, outputs);
    emacs_value mv_sym  = env->intern(env, "make-vector");
    emacs_value mv_args[2] = { env->make_integer(env, 4), QNIL };
    emacs_value result = env->funcall(env, mv_sym, 2, mv_args);
    for (int i = 0; i < 4; i++)
        env->vec_set(env, result, i, env->make_float(env, (double)outputs[i]));
    return result;
}

static emacs_value neural_pulse_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)d;
    ensure_init(env);
    float intensity = (float)env->extract_float(env, args[0]);
    neural_fragment_pulse(&g_neural, intensity);
    return neural_is_awakened(&g_neural) ? QT : QNIL;
}

static emacs_value neural_awakened_fn(emacs_env *env, ptrdiff_t n, emacs_value *args, void *d) {
    (void)n; (void)args; (void)d;
    ensure_init(env);
    return neural_is_awakened(&g_neural) ? QT : QNIL;
}

typedef emacs_value (*emacs_fn)(emacs_env*, ptrdiff_t, emacs_value*, void*);

static void defun(emacs_env *env, const char *name,
                  ptrdiff_t min_args, ptrdiff_t max_args,
                  emacs_fn fn, const char *doc) {
    emacs_value sym  = env->intern(env, name);
    emacs_value func = env->make_function(env, min_args, max_args, fn, doc, NULL);
    emacs_value fargs[2] = { sym, func };
    env->funcall(env, env->intern(env, "defalias"), 2, fargs);
}

int emacs_module_init(struct emacs_runtime *rt) {
    emacs_env *env = rt->get_environment(rt);
    QNIL = env->intern(env, "nil");
    QT   = env->intern(env, "t");
    ensure_init(env);

    defun(env, "neuro/cpu-init",      6, 6, neuro_cpu_init_fn,      "Init CPU.");
    defun(env, "neuro/cpu-step",      0, 0, neuro_cpu_step_fn,      "Execute one 8086 instruction.");
    defun(env, "neuro/cpu-regs",      0, 0, neuro_cpu_regs_fn,      "Register dump string.");
    defun(env, "neuro/cpu-flags",     0, 0, neuro_cpu_flags_fn,     "Flags string.");
    defun(env, "neuro/cpu-get-reg",   1, 1, neuro_cpu_get_reg_fn,   "Get register by name.");
    defun(env, "neuro/cpu-set-reg",   2, 2, neuro_cpu_set_reg_fn,   "Set register by name.");
    defun(env, "neuro/cpu-dump-mem",  2, 2, neuro_cpu_dump_mem_fn,  "Hex dump CPU memory.");
    defun(env, "neuro/cpu-write-mem", 2, 2, neuro_cpu_write_mem_fn, "Write bytes to CPU memory.");
    defun(env, "neuro/cpu-cycles",    0, 0, neuro_cpu_cycles_fn,    "CPU cycle count.");
    defun(env, "neuro/rng-next",      1, 1, neuro_rng_next_fn,      "RNG int in [0,max).");
    defun(env, "neuro/rng-bool",      1, 1, neuro_rng_bool_fn,      "RNG bool with pct chance.");
    defun(env, "neuro/rng-u16",       0, 0, neuro_rng_u16_fn,       "RNG random u16.");
    defun(env, "neuro/now-ms",        0, 0, neuro_now_ms_fn,        "Current time ms.");
    defun(env, "neuro/sleep-ms",      1, 1, neuro_sleep_ms_fn,      "Sleep N ms.");
    defun(env, "neuro/beep",          2, 2, neuro_beep_fn,          "Beep N times.");
    defun(env, "neuro/hex-dump",      3, 3, neuro_hex_dump_fn,      "Hex dump byte vector.");
    defun(env, "neural/forward",      1, 1, neural_forward_fn,      "Neural forward pass.");
    defun(env, "neural/pulse",        1, 1, neural_pulse_fn,        "Fragment pulse.");
    defun(env, "neural/awakened-p",   0, 0, neural_awakened_fn,     "Neural awakened?");

    emacs_value feat = env->intern(env, "neuro-core");
    emacs_value pargs[1] = { feat };
    env->funcall(env, env->intern(env, "provide"), 1, pargs);
    return 0;
}
