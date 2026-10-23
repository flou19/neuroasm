#define _POSIX_C_SOURCE 200809L
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdbool.h>
#include "neuro_core.h"

#define NEUROD_VERSION    "4.0.0"
#define MAX_CLIENTS       8
#define RECV_BUF_SIZE     4096
#define SEND_BUF_SIZE     4096
#define MAX_STEPS_BURST   10000000L

#ifdef __ANDROID__
#  define DEFAULT_SOCK_DIR_FALLBACK  "/data/data/com.termux/files/usr/tmp"
#else
#  define DEFAULT_SOCK_DIR_FALLBACK  "/tmp"
#endif
#define DEFAULT_SOCK_NAME           "neurodos.sock"

static Cpu8086    g_cpu;
static PhysMem    g_mem;
static Ports      g_ports;
static Idt        g_idt;
static NeuralCore g_neural;
static Xorshift64 g_rng;
static bool       g_initialized = false;
static volatile sig_atomic_t g_running = 1;
static char        g_sockpath_buf[256];
static const char *g_sockpath = NULL;

static void ensure_init(void) {
    if (g_initialized) return;
    xorshift64_init_time(&g_rng);
    cpu_init(&g_cpu);
    phys_init(&g_mem);
    ports_init(&g_ports);
    idt_init(&g_idt);
    idt_defaults(&g_idt);
    neural_init(&g_neural, &g_rng);
    g_initialized = true;
}

static void handle_signal(int sig) {
    (void)sig;
    g_running = 0;
}

typedef struct {
    int     fd;
    char    rbuf[RECV_BUF_SIZE];
    int     rlen;
    char    wbuf[SEND_BUF_SIZE];
    int     wlen;
    bool    active;
} Client;

static Client clients[MAX_CLIENTS];

static Client *client_alloc(int fd) {
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (!clients[i].active) {
            memset(&clients[i], 0, sizeof(clients[i]));
            clients[i].fd     = fd;
            clients[i].active = true;
            return &clients[i];
        }
    }
    return NULL;
}

static void client_close(Client *c) {
    close(c->fd);
    c->active = false;
}

static void client_send(Client *c, const char *msg) {
    int len = (int)strlen(msg);
    int remaining = SEND_BUF_SIZE - c->wlen - 1;
    if (len > remaining) len = remaining;
    memcpy(c->wbuf + c->wlen, msg, len);
    c->wlen += len;
}

static void client_sendf(Client *c, const char *fmt, ...) {
    char tmp[512];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(tmp, sizeof(tmp), fmt, ap);
    va_end(ap);
    client_send(c, tmp);
}

static void dispatch(Client *c, const char *line) {
    ensure_init();

    if (!strncmp(line, "VERSION", 7)) {
        client_sendf(c, "OK neurodos %s\n", NEUROD_VERSION);

    } else if (!strncmp(line, "STEP", 4)) {
        if (g_cpu.halted) {
            client_send(c, "ERR CPU_HALTED\n");
            return;
        }
        NeuroErr e = cpu_step(&g_cpu);
        if (e) {
            client_sendf(c, "ERR %d\n", (int)e);
        } else {
            char buf[256];
            cpu_dump_regs(&g_cpu, buf, sizeof(buf));
            client_sendf(c, "OK %s\n", buf);
        }

    } else if (!strncmp(line, "RUN ", 4)) {
        long n = atol(line + 4);
        if (n <= 0 || n > MAX_STEPS_BURST) {
            client_sendf(c, "ERR INVALID_COUNT %ld\n", n);
            return;
        }
        long done = 0;
        for (long i = 0; i < n; i++) {
            if (g_cpu.halted) break;
            NeuroErr e = cpu_step(&g_cpu);
            if (e) break;
            done++;
        }
        client_sendf(c, "OK %ld %llu\n", done, (unsigned long long)g_cpu.cycles);

    } else if (!strncmp(line, "REGS", 4)) {
        char buf[256];
        cpu_dump_regs(&g_cpu, buf, sizeof(buf));
        client_sendf(c, "OK %s\n", buf);

    } else if (!strncmp(line, "FLAGS", 5)) {
        char buf[128];
        cpu_dump_flags(&g_cpu, buf, sizeof(buf));
        client_sendf(c, "OK %s\n", buf);

    } else if (!strncmp(line, "GETREG ", 7)) {
        const char *name = line + 7;
        uint16_t v = 0;
        Regs8086 *R = &g_cpu.regs;
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
        else { client_send(c, "ERR UNKNOWN_REG\n"); return; }
        client_sendf(c, "OK %u\n", (unsigned)v);

    } else if (!strncmp(line, "SETREG ", 7)) {
        char name[16] = {0};
        char valstr[32] = {0};
        if (sscanf(line + 7, "%15s %31s", name, valstr) != 2) {
            client_send(c, "ERR BAD_ARGS\n"); return;
        }
        unsigned long val = strtoul(valstr, NULL, 0);
        Regs8086 *R = &g_cpu.regs;
        uint16_t v = (uint16_t)val;
        if      (!strcmp(name,"ax")) R->ax = v;
        else if (!strcmp(name,"bx")) R->bx = v;
        else if (!strcmp(name,"cx")) R->cx = v;
        else if (!strcmp(name,"dx")) R->dx = v;
        else if (!strcmp(name,"si")) R->si = v;
        else if (!strcmp(name,"di")) R->di = v;
        else if (!strcmp(name,"sp")) R->sp = v;
        else if (!strcmp(name,"bp")) R->bp = v;
        else if (!strcmp(name,"ip")) R->ip = v;
        else { client_send(c, "ERR UNKNOWN_REG\n"); return; }
        client_send(c, "OK\n");

    } else if (!strncmp(line, "DUMP ", 5)) {
        unsigned addr = 0, len = 64;
        sscanf(line + 5, "%x %u", &addr, &len);
        if (len > 256) len = 256;
        char buf[4096];
        cpu_dump_mem(&g_cpu, (uint16_t)addr, (int)len, buf, sizeof(buf));
        client_sendf(c, "OK\n%s", buf);

    } else if (!strncmp(line, "WRITE ", 6)) {
        unsigned addr = 0;
        if (sscanf(line + 6, "%x", &addr) != 1) {
            client_send(c, "ERR BAD_ARGS\n"); return;
        }
        const char *hex = strchr(line + 6, ' ');
        if (!hex) { client_send(c, "ERR NO_DATA\n"); return; }
        hex++;
        uint8_t data[256];
        int n = 0;
        while (*hex && n < 256) {
            unsigned b = 0;
            if (sscanf(hex, "%02x", &b) != 1) break;
            data[n++] = (uint8_t)b;
            hex += 2;
            while (*hex == ' ') hex++;
        }
        cpu_write_mem(&g_cpu, (uint16_t)addr, data, n);
        client_sendf(c, "OK %d bytes\n", n);

    } else if (!strncmp(line, "CYCLES", 6)) {
        client_sendf(c, "OK %llu\n", (unsigned long long)g_cpu.cycles);

    } else if (!strncmp(line, "RESET", 5)) {
        cpu_init(&g_cpu);
        g_initialized = false;
        ensure_init();
        client_send(c, "OK\n");

    } else if (!strncmp(line, "HALTED", 6)) {
        client_sendf(c, "OK %d\n", g_cpu.halted ? 1 : 0);

    } else if (!strncmp(line, "NEURAL_FORWARD ", 15)) {
        float inputs[8] = {0};
        const char *p = line + 15;
        for (int i = 0; i < 8 && *p; i++) {
            inputs[i] = (float)atof(p);
            while (*p && *p != ' ') p++;
            while (*p == ' ') p++;
        }
        float outputs[4] = {0};
        neural_forward(&g_neural, inputs, outputs);
        client_sendf(c, "OK %.6f %.6f %.6f %.6f\n",
                     (double)outputs[0], (double)outputs[1],
                     (double)outputs[2], (double)outputs[3]);

    } else if (!strncmp(line, "NEURAL_PULSE ", 13)) {
        float intensity = (float)atof(line + 13);
        neural_fragment_pulse(&g_neural, intensity);
        client_sendf(c, "OK awakened=%d progress=%.4f\n",
                     neural_is_awakened(&g_neural) ? 1 : 0,
                     (double)g_neural.research_progress);

    } else if (!strncmp(line, "NEURAL_STATUS", 13)) {
        client_sendf(c, "OK awakened=%d cores=%u progress=%.4f\n",
                     neural_is_awakened(&g_neural) ? 1 : 0,
                     g_neural.cores_online,
                     (double)g_neural.research_progress);

    } else if (!strncmp(line, "RNG ", 4)) {
        uint32_t max = (uint32_t)atol(line + 4);
        client_sendf(c, "OK %u\n", xorshift64_range(&g_rng, max ? max : 100));

    } else if (!strncmp(line, "TIME", 4)) {
        client_sendf(c, "OK %llu\n", (unsigned long long)neuro_now_ms());

    } else if (!strncmp(line, "QUIT", 4)) {
        client_send(c, "BYE\n");
        client_close(c);

    } else if (!strncmp(line, "SHUTDOWN", 8)) {
        client_send(c, "BYE\n");
        g_running = 0;

    } else if (!strncmp(line, "PING", 4)) {
        client_send(c, "PONG\n");

    } else {
        client_sendf(c, "ERR UNKNOWN_CMD %.32s\n", line);
    }
}

static void process_client_input(Client *c) {
    char *buf = c->rbuf;
    int   len = c->rlen;
    int   pos = 0;

    while (pos < len) {
        char *nl = memchr(buf + pos, '\n', len - pos);
        if (!nl) break;
        *nl = '\0';
        char *line = buf + pos;
        int   llen = (int)(nl - line);
        while (llen > 0 && (line[llen-1] == '\r' || line[llen-1] == ' '))
            line[--llen] = '\0';
        if (llen > 0)
            dispatch(c, line);
        pos = (int)(nl - buf) + 1;
    }

    if (pos > 0) {
        memmove(buf, buf + pos, len - pos);
        c->rlen -= pos;
    }
}

static int server_loop(int srv) {
    memset(clients, 0, sizeof(clients));

    while (g_running) {
        fd_set rset, wset;
        FD_ZERO(&rset);
        FD_ZERO(&wset);
        FD_SET(srv, &rset);
        int maxfd = srv;

        for (int i = 0; i < MAX_CLIENTS; i++) {
            if (!clients[i].active) continue;
            FD_SET(clients[i].fd, &rset);
            if (clients[i].wlen > 0)
                FD_SET(clients[i].fd, &wset);
            if (clients[i].fd > maxfd)
                maxfd = clients[i].fd;
        }

        struct timeval tv = {1, 0};
        int n = select(maxfd + 1, &rset, &wset, NULL, &tv);
        if (n < 0) {
            if (errno == EINTR) continue;
            perror("select");
            break;
        }

        if (FD_ISSET(srv, &rset)) {
            int cfd = accept(srv, NULL, NULL);
            if (cfd >= 0) {
                Client *c = client_alloc(cfd);
                if (!c) {
                    const char *msg = "ERR SERVER_FULL\n";
                    { ssize_t _w = write(cfd, msg, strlen(msg)); (void)_w; }
                    close(cfd);
                } else {
                    client_send(c, "NEURODOS " NEUROD_VERSION "\n");
                }
            }
        }

        for (int i = 0; i < MAX_CLIENTS; i++) {
            Client *c = &clients[i];
            if (!c->active) continue;

            if (FD_ISSET(c->fd, &wset) && c->wlen > 0) {
                ssize_t sent = write(c->fd, c->wbuf, c->wlen);
                if (sent > 0) {
                    memmove(c->wbuf, c->wbuf + sent, c->wlen - sent);
                    c->wlen -= (int)sent;
                } else {
                    client_close(c);
                    continue;
                }
            }

            if (FD_ISSET(c->fd, &rset)) {
                ssize_t got = read(c->fd,
                                   c->rbuf + c->rlen,
                                   RECV_BUF_SIZE - c->rlen - 1);
                if (got <= 0) {
                    client_close(c);
                    continue;
                }
                c->rlen += (int)got;
                c->rbuf[c->rlen] = '\0';
                process_client_input(c);
            }
        }
    }

    for (int i = 0; i < MAX_CLIENTS; i++)
        if (clients[i].active)
            client_close(&clients[i]);

    return 0;
}

static void usage(const char *prog) {
    fprintf(stderr,
        "neurodos daemon v" NEUROD_VERSION "\n"
        "Usage: %s [options]\n"
        "  -s <path>   Unix socket path (default: $TMPDIR/" DEFAULT_SOCK_NAME ")\n"
        "  -h          Show this help\n"
        "\nProtocol (newline-delimited):\n"
        "  PING           → PONG\n"
        "  VERSION        → OK neurodos <ver>\n"
        "  STEP           → OK <regs>\n"
        "  RUN <n>        → OK <done> <cycles>\n"
        "  REGS           → OK <regs>\n"
        "  FLAGS          → OK <flags>\n"
        "  GETREG <name>  → OK <val>\n"
        "  SETREG <n> <v> → OK\n"
        "  DUMP <addr> <len> → OK\\n<hexdump>\n"
        "  WRITE <addr> <bytes...> → OK\n"
        "  CYCLES         → OK <n>\n"
        "  HALTED         → OK 0|1\n"
        "  RESET          → OK\n"
        "  NEURAL_FORWARD <f0..f7> → OK <o0..o3>\n"
        "  NEURAL_PULSE <f>       → OK awakened= progress=\n"
        "  NEURAL_STATUS          → OK awakened= cores= progress=\n"
        "  RNG <max>      → OK <n>\n"
        "  TIME           → OK <ms>\n"
        "  QUIT           → BYE  (closes connection)\n"
        "  SHUTDOWN       → BYE  (stops daemon)\n",
        prog);
}

int main(int argc, char **argv) {
    int opt;
    while ((opt = getopt(argc, argv, "s:h")) != -1) {
        switch (opt) {
            case 's': g_sockpath = optarg; break;
            case 'h': usage(argv[0]); return 0;
            default:  usage(argv[0]); return 1;
        }
    }

    if (!g_sockpath) {
        const char *tmpdir = getenv("TMPDIR");
        if (!tmpdir || !*tmpdir) tmpdir = DEFAULT_SOCK_DIR_FALLBACK;
        snprintf(g_sockpath_buf, sizeof(g_sockpath_buf),
                 "%s/%s", tmpdir, DEFAULT_SOCK_NAME);
        g_sockpath = g_sockpath_buf;
    }

    signal(SIGINT,  handle_signal);
    signal(SIGTERM, handle_signal);
    signal(SIGPIPE, SIG_IGN);

    unlink(g_sockpath);
    int srv = socket(AF_UNIX, SOCK_STREAM, 0);
    if (srv < 0) { perror("socket"); return 1; }

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, g_sockpath, sizeof(addr.sun_path) - 1);

    if (bind(srv, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        perror("bind"); close(srv); return 1;
    }
    chmod(g_sockpath, 0600);

    if (listen(srv, MAX_CLIENTS) < 0) {
        perror("listen"); close(srv); return 1;
    }

    ensure_init();

    int ret = server_loop(srv);
    close(srv);
    unlink(g_sockpath);
    return ret;
}
