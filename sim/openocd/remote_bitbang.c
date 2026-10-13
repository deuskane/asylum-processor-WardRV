/*-----------------------------------------------------------------------------
 * Title      : OpenOCD remote_bitbang server for the GHDL simulation
 * Project    : Asylum
 *-----------------------------------------------------------------------------
 * File       : remote_bitbang.c
 * Author     : mrosiere
 *-----------------------------------------------------------------------------
 * Description:
 * TCP server of the OpenOCD "remote_bitbang" adapter, called by the testbench
 * (tb_WardRV_openocd) through GHDL VHPIDIRECT (sim/openocd/rbb_pkg.vhd) :
 *
 *   int rbb_tick (int tdo);
 *
 * The testbench calls rbb_tick every TCK_DIV clock cycles with the current
 * TDO pin. The function returns the JTAG pins driven by OpenOCD :
 *   bit 0 tck, bit 1 tms, bit 2 tdi, bit 3 trst_n, bit 4 srst_n,
 *   bit 5 valid (a pin write was applied by this call),
 *   bit 6 quit  (OpenOCD sent Q, closed the connection, or error)
 *
 * Protocol (OpenOCD doc/manual/jtag/drivers/remote_bitbang.txt) :
 *   '0'..'7' write tck (4), tms (2), tdi (1)
 *   'R'      read TDO, answer '0' or '1'
 *   'r'..'u' write trst (2), srst (1) (1 = asserted)
 *   'B' 'b'  blink on / off (ignored)
 *   'Q'      quit
 *   SWD and sleep commands are ignored ('c' answers '0').
 *
 * Timing : one pin write per call at most. The commands that follow a pin
 * write are processed by the next call, so that a 'R' sent after a falling
 * edge of TCK reads the TDO updated by this edge (TCK period = 2 calls).
 *
 * Socket : listen on 127.0.0.1:RBB_PORT (default 9823) at the first call,
 * wait for OpenOCD (RBB_ACCEPT_TIMEOUT seconds, default 300), one client.
 * Receive without blocking : when no command is pending the call returns
 * immediately (the simulation goes on, the core runs). The answers are
 * buffered and sent when all the received commands have been processed.
 *
 * Environment : RBB_PORT, RBB_ACCEPT_TIMEOUT, RBB_VERBOSE (1 = trace).
 *
 * Build : gcc -shared -fPIC -O2 -o librbb.so remote_bitbang.c
 *-----------------------------------------------------------------------------
 * Copyright (c) 2026
 *-----------------------------------------------------------------------------
 * Revisions  :
 * Date        Version  Author   Description
 * 2026-10-09  1.0      mrosiere Created
 *---------------------------------------------------------------------------*/

#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#define RBB_DEFAULT_PORT           9823
#define RBB_DEFAULT_ACCEPT_TIMEOUT 300

#define RBB_TCK    (1 << 0)
#define RBB_TMS    (1 << 1)
#define RBB_TDI    (1 << 2)
#define RBB_TRST_N (1 << 3)
#define RBB_SRST_N (1 << 4)
#define RBB_VALID  (1 << 5)
#define RBB_QUIT   (1 << 6)

#define RBB_RX_SIZE 65536
#define RBB_TX_SIZE 65536

enum rbb_state { RBB_INIT, RBB_CONNECTED, RBB_QUITTED };

static enum rbb_state state = RBB_INIT;
static int            client_fd = -1;
static int            verbose = 0;
/* Pins : TCK low, TMS high, TRST* and SRST* released */
static int            pins = RBB_TMS | RBB_TRST_N | RBB_SRST_N;

static unsigned char  rx_buf[RBB_RX_SIZE];
static size_t         rx_len = 0;
static size_t         rx_pos = 0;
static unsigned char  tx_buf[RBB_TX_SIZE];
static size_t         tx_len = 0;

/* Statistics */
static unsigned long long nb_calls  = 0;
static unsigned long long nb_writes = 0;
static unsigned long long nb_reads  = 0;
static unsigned long long nb_bytes  = 0;

static int env_int(const char *name, int def)
{
  const char *s = getenv(name);
  if (s == NULL || *s == '\0')
    return def;
  return atoi(s);
}

static void rbb_close(const char *why)
{
  if (client_fd >= 0)
    close(client_fd);
  client_fd = -1;
  state = RBB_QUITTED;
  printf("remote_bitbang: %s (calls %llu, pin writes %llu, TDO reads %llu, bytes %llu)\n",
         why, nb_calls, nb_writes, nb_reads, nb_bytes);
  fflush(stdout);
}

/* Listen on 127.0.0.1:RBB_PORT and wait for OpenOCD */
static int rbb_open(void)
{
  int port    = env_int("RBB_PORT", RBB_DEFAULT_PORT);
  int timeout = env_int("RBB_ACCEPT_TIMEOUT", RBB_DEFAULT_ACCEPT_TIMEOUT);
  int one     = 1;
  int listen_fd;
  struct sockaddr_in addr;
  struct pollfd pfd;
  int rc;

  verbose = env_int("RBB_VERBOSE", 0);

  listen_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (listen_fd < 0) {
    perror("remote_bitbang: socket");
    return -1;
  }
  setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));

  memset(&addr, 0, sizeof(addr));
  addr.sin_family      = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  addr.sin_port        = htons((unsigned short)port);
  if (bind(listen_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
    fprintf(stderr, "remote_bitbang: bind 127.0.0.1:%d: %s\n", port, strerror(errno));
    close(listen_fd);
    return -1;
  }
  if (listen(listen_fd, 1) < 0) {
    perror("remote_bitbang: listen");
    close(listen_fd);
    return -1;
  }
  printf("remote_bitbang: listening on 127.0.0.1:%d (waiting %d s for OpenOCD)\n", port, timeout);
  fflush(stdout);

  pfd.fd     = listen_fd;
  pfd.events = POLLIN;
  do {
    rc = poll(&pfd, 1, timeout * 1000);
  } while (rc < 0 && errno == EINTR);
  if (rc <= 0) {
    fprintf(stderr, "remote_bitbang: no connection after %d s\n", timeout);
    close(listen_fd);
    return -1;
  }

  client_fd = accept(listen_fd, NULL, NULL);
  close(listen_fd);
  if (client_fd < 0) {
    perror("remote_bitbang: accept");
    return -1;
  }
  /* Small answers ('0' / '1') : no Nagle delay */
  setsockopt(client_fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));

  printf("remote_bitbang: OpenOCD connected\n");
  fflush(stdout);
  state = RBB_CONNECTED;
  return 0;
}

/* Send the buffered answers (blocking) */
static int rbb_flush(void)
{
  size_t sent = 0;

  while (sent < tx_len) {
    ssize_t n = send(client_fd, tx_buf + sent, tx_len - sent, MSG_NOSIGNAL);
    if (n < 0) {
      if (errno == EINTR)
        continue;
      return -1;
    }
    sent += (size_t)n;
  }
  tx_len = 0;
  return 0;
}

static int rbb_answer(char c)
{
  if (tx_len == RBB_TX_SIZE && rbb_flush() < 0)
    return -1;
  tx_buf[tx_len++] = (unsigned char)c;
  return 0;
}

int rbb_tick(int tdo)
{
  nb_calls++;

  if (state == RBB_QUITTED)
    return pins | RBB_QUIT;

  if (state == RBB_INIT && rbb_open() < 0) {
    rbb_close("no OpenOCD connection");
    return pins | RBB_QUIT;
  }

  for (;;) {
    unsigned char c;

    if (rx_pos == rx_len) {
      ssize_t n;

      /* Every received command is processed : send the answers, then look
       * for new commands without blocking */
      if (tx_len > 0 && rbb_flush() < 0) {
        rbb_close("send error, connection closed");
        return pins | RBB_QUIT;
      }
      n = recv(client_fd, rx_buf, sizeof(rx_buf), MSG_DONTWAIT);
      if (n > 0) {
        rx_len = (size_t)n;
        rx_pos = 0;
        nb_bytes += (unsigned long long)n;
      } else if (n == 0) {
        rbb_close("OpenOCD closed the connection");
        return pins | RBB_QUIT;
      } else if (errno == EAGAIN || errno == EWOULDBLOCK || errno == EINTR) {
        return pins;
      } else {
        rbb_close("receive error, connection closed");
        return pins | RBB_QUIT;
      }
    }

    c = rx_buf[rx_pos++];
    if (verbose)
      printf("remote_bitbang: '%c' (tdo %d)\n", c, tdo);

    switch (c) {
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
      pins = (pins & (RBB_TRST_N | RBB_SRST_N))
           | ((c - '0') & 4 ? RBB_TCK : 0)
           | ((c - '0') & 2 ? RBB_TMS : 0)
           | ((c - '0') & 1 ? RBB_TDI : 0);
      nb_writes++;
      return pins | RBB_VALID;

    case 'r': case 's': case 't': case 'u':
      pins = (pins & (RBB_TCK | RBB_TMS | RBB_TDI))
           | ((c - 'r') & 2 ? 0 : RBB_TRST_N)
           | ((c - 'r') & 1 ? 0 : RBB_SRST_N);
      nb_writes++;
      return pins | RBB_VALID;

    case 'R':
      nb_reads++;
      if (rbb_answer(tdo ? '1' : '0') < 0) {
        rbb_close("send error, connection closed");
        return pins | RBB_QUIT;
      }
      break;

    case 'c':                                  /* SWDIO read : not supported */
      if (rbb_answer('0') < 0) {
        rbb_close("send error, connection closed");
        return pins | RBB_QUIT;
      }
      break;

    case 'Q':
      rbb_flush();
      rbb_close("OpenOCD quit (Q)");
      return pins | RBB_QUIT;

    default:                                   /* B, b, SWD, sleep, blanks */
      break;
    }
  }
}
