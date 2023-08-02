#ifndef __NEMU_PROXY_H
#define __NEMU_PROXY_H

#include <unistd.h>
#include <dlfcn.h>

#include "common.h"

class RefProxy{
public:
  void (*memcpy)(paddr_t nemu_addr, void *dut_buf, size_t n, bool direction) = NULL;
  void (*regcpy)(void *dut, bool direction) = NULL;
  void (*exec)(uint64_t n) = NULL;
  void (*load_flash_bin)(void *flash_bin, size_t size) = NULL;
};

#define SPIKE_ENV_VARIABLE "SPIKE_HOME"
#define SPIKE_SO_FILENAME  "difftest/build/riscv64-spike-so"
class SpikeProxy : public RefProxy{
public:
  SpikeProxy(int coreid, size_t ram_size);
private:
};
#endif
