#include <sys/mman.h>
#include "ram.h"
#include "common.h"

static uint64_t *ram;
static long img_size = 0;
static pthread_mutex_t ram_mutex;

unsigned long EMU_RAM_SIZE = DEFAULT_EMU_RAM_SIZE;

void* get_img_start() { return &ram[0]; }
long get_img_size() { return img_size; }
void* get_ram_start() { return &ram[0]; }
long get_ram_size() { return EMU_RAM_SIZE; }

void init_ram(const char *img){
  assert(img != NULL);
  printf("The image is %s\n", img);

  //allocate mem space
  ram = (uint64_t *)mmap(NULL, EMU_RAM_SIZE, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  if (ram == (uint64_t *)MAP_FAILED) {
    printf("Warning: Insufficient phisical memory\n");
    EMU_RAM_SIZE = 128 * 1024 * 1024UL;
    ram = (uint64_t *)mmap(NULL, EMU_RAM_SIZE, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
    if (ram == (uint64_t *)MAP_FAILED) {
      printf("Error: Cound not mmap 0x%lx bytes\n", EMU_RAM_SIZE);
      assert(0);
    }
  }
  printf("Using simulated %luMB RAM\n", EMU_RAM_SIZE / (1024 * 1024));
 
  //init ram
  int ret;
  FILE *fp = fopen(img, "rb");
  if(fp == NULL) {printf("Can not open '%s'\n", img); assert(0);}
  fseek(fp, 0, SEEK_END); 
  img_size = ftell(fp);
  if (img_size > EMU_RAM_SIZE) {img_size = EMU_RAM_SIZE;}
  fseek(fp, 0, SEEK_SET);
  ret = fread(ram, img_size, 1, fp);
  assert(ret == 1);
  fclose(fp);

  pthread_mutex_init(&ram_mutex, 0);
}

void ram_finish(){
  munmap(ram, EMU_RAM_SIZE);
  pthread_mutex_destroy(&ram_mutex);
}

extern "C" uint64_t ram_read_helper(uint8_t en, uint64_t rIdx) {
  if (!ram)
    return 0;
  if (en && rIdx >= EMU_RAM_SIZE / sizeof(uint64_t)) {
    rIdx %= EMU_RAM_SIZE / sizeof(uint64_t);
  }
  pthread_mutex_lock(&ram_mutex);
  uint64_t rdata = (en) ? ram[rIdx] : 0;
  pthread_mutex_unlock(&ram_mutex);
  return rdata;
}

extern "C" void ram_write_helper(uint64_t wIdx, uint64_t wdata, uint64_t wmask, uint8_t wen) {
  if (wen && ram) {
    if (wIdx >= EMU_RAM_SIZE / sizeof(uint64_t)) {
      printf("ERROR: ram wIdx = 0x%lx out of bound!\n", wIdx);
      assert(wIdx < EMU_RAM_SIZE / sizeof(uint64_t));
    }
    pthread_mutex_lock(&ram_mutex);
    ram[wIdx] = (ram[wIdx] & ~wmask) | (wdata & wmask);
    pthread_mutex_unlock(&ram_mutex);
  }
}
