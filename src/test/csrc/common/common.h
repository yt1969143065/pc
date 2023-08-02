#ifndef __COMMON_H
#define __COMMON_H

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <cstdint>
#include <cassert>
#include <cerrno>
#include <pthread.h>
#include <unistd.h>

typedef uint64_t paddr_t;

//config
#define DIFFTEST_COMMIT_WIDTH 8
#define FIRST_INST_ADDRESS 0x80000000
#define DIFF_PROXY SpikeProxy
#define DEFAULT_EMU_RAM_SIZE (256 * 1024 * 1024UL) //256M
#define DEFAULT_EMU_FLASH_SIZE (32 * 1024UL) //32K
#define FLASH_ALIGH_MASK 0xfffffff8

#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[36m"
#define ANSI_COLOR_RESET   "\x1b[0m"

#define eprintf(...) fprintf(stdout, ## __VA_ARGS__)

extern int assert_count;
void assert_init();
void assert_finish();

extern unsigned long EMU_RAM_SIZE;

#endif
