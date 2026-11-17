//-----------------------------------------------------------------------------
// Title      : OpenOCD remote_bitbang server of the WardRV co-simulation
// Project    : Asylum
//-----------------------------------------------------------------------------
// File       : remote_bitbang.c
// Author     : mrosiere
//-----------------------------------------------------------------------------
// Description: TCP server of the OpenOCD "remote_bitbang" adapter, called by
//              tb_WardRV_openocd through GHDL VHPIDIRECT (rbb_pkg.vhd) :
//                int rbb_tick (int tdo);
//              returns the pins driven by OpenOCD : bit 0 TCK, bit 1 TMS,
//              bit 2 TDI, bit 3 quit (OpenOCD sent Q or closed the socket).
//              Commands : '0'..'7' write TCK / TMS / TDI, 'R' read TDO,
//              'Q' quit ; the others (reset pins, blink) are ignored : the
//              reset of the core is dmcontrol.ndmreset.
//              One pin write per call : the next 'R' reads the TDO updated by
//              that write. Without pending command the call returns at once
//              (the core keeps running).
//              Environment : RBB_PORT (9823).
//              Build : gcc -shared -fPIC -o librbb.so remote_bitbang.c
//-----------------------------------------------------------------------------
// Copyright (c) 2026
//-----------------------------------------------------------------------------
// Revisions  :
// Date        Version  Author   Description
// 2026-11-17  1.0      mrosiere Created
//-----------------------------------------------------------------------------

#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

//--------------------------------------
// Constant
//--------------------------------------
#define RBB_QUIT  8
#define BUF_SIZE  4096

//--------------------------------------
// State
//--------------------------------------
static int           client = -1;                              // Socket of OpenOCD
static int           quit   = 0;
static int           pins   = 2;                               // TCK low, TMS high
static unsigned char rx[BUF_SIZE];
static int           rx_len = 0;
static int           rx_pos = 0;

// Wait for OpenOCD on 127.0.0.1:RBB_PORT
static int rbb_open (void)
{
  const char        *port = getenv("RBB_PORT");
  struct sockaddr_in addr;
  int                one  = 1;
  int                server;

  memset(&addr,0,sizeof(addr));
  addr.sin_family      = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  addr.sin_port        = htons(port ? atoi(port) : 9823);

  server = socket(AF_INET,SOCK_STREAM,0);
  setsockopt(server,SOL_SOCKET,SO_REUSEADDR,&one,sizeof(one));
  if (bind  (server,(struct sockaddr *)&addr,sizeof(addr)) < 0 ||
      listen(server,1) < 0)
    {
      perror("remote_bitbang");
      return -1;
    }
  printf("remote_bitbang : waiting for OpenOCD on port %d\n",ntohs(addr.sin_port));
  fflush(stdout);
  client = accept(server,NULL,NULL);
  close         (server);
  setsockopt    (client,IPPROTO_TCP,TCP_NODELAY,&one,sizeof(one));
  return client;
}

//--------------------------------------
// Main
//--------------------------------------
int rbb_tick (int tdo)
{
  if (quit)
    return pins | RBB_QUIT;
  if (client < 0 && rbb_open() < 0)
    {
      quit = 1;
      return pins | RBB_QUIT;
    }

  for (;;)
    {
      unsigned char c;

      if (rx_pos == rx_len)
        {
          // Every command done : new commands, without blocking
          rx_len = recv(client,rx,BUF_SIZE,MSG_DONTWAIT);
          rx_pos = 0;
          if (rx_len < 0)
            {
              rx_len = 0;
              return pins;
            }
        }
      c = (rx_len == 0) ? 'Q' : rx[rx_pos++];                  // 0 : socket closed

      if (c >= '0' && c <= '7')
        {
          // TCK (4), TMS (2), TDI (1) -> bits 0, 1, 2
          pins = ((c - '0') & 4 ? 1 : 0) | ((c - '0') & 2) | ((c - '0') & 1 ? 4 : 0);
          return pins;
        }
      if (c == 'R')
        {
          char answer = tdo ? '1' : '0';
          send(client,&answer,1,MSG_NOSIGNAL);
        }
      if (c == 'Q')
        {
          printf("remote_bitbang : OpenOCD quit\n");
          fflush(stdout);
          close(client);
          quit = 1;
          return pins | RBB_QUIT;
        }
    }
}
