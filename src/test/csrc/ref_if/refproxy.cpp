#include <unistd.h>
#include <dlfcn.h>
#include "refproxy.h"

#define check_and_assert(func)                                \
  do {                                                        \
    if (!func) {                                              \
      printf("ERROR: %s\n", dlerror());  \
      assert(func);                                           \
    }                                                         \
  } while (0);

const char *difftest_ref_so = NULL;

SpikeProxy::SpikeProxy(int coreid, size_t ram_size = 0) {
  if (difftest_ref_so == NULL) {
    printf("--diff is not given, "
        "try to use $(" SPIKE_ENV_VARIABLE ")/" SPIKE_SO_FILENAME " by default\n");
    const char *spike_home = getenv(SPIKE_ENV_VARIABLE);
    if (spike_home == NULL) {
      printf("FATAL: $(" SPIKE_ENV_VARIABLE ") is not defined!\n");
      exit(1);
    }
    const char *so = "/" SPIKE_SO_FILENAME;
    char *buf = (char *)malloc(strlen(spike_home) + strlen(so) + 1);
    strcpy(buf, spike_home);
    strcat(buf, so);
    difftest_ref_so = buf;
  }
  printf("SpikeProxy using %s\n", difftest_ref_so);

  void *handle = dlmopen(LM_ID_NEWLM, difftest_ref_so, RTLD_LAZY | RTLD_DEEPBIND);
  if (!handle) {printf("%s\n", dlerror()); assert(0);}

  auto spike_init = (void (*)(int))dlsym(handle, "difftest_init");
  check_and_assert(spike_init);

  this->memcpy = (void (*)(paddr_t, void *, size_t, bool))dlsym(handle, "difftest_memcpy");
  check_and_assert(this->memcpy);

  regcpy = (void (*)(void *, bool))dlsym(handle, "difftest_regcpy");
  check_and_assert(regcpy);

  exec = (void (*)(uint64_t))dlsym(handle, "difftest_exec");
  check_and_assert(exec);

  load_flash_bin = (void (*)(void *flash_bin, size_t size))dlsym(handle, "difftest_load_flash");
  check_and_assert(load_flash_bin);

  if(ram_size){
    printf("Spike ram_size api to be added later, ignore ram_size set\n");
  }
 
  spike_init(0);
}
