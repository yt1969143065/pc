#include <locale.h>
#include <common.h>
#include "difftest.h"
#include "ram.h"
#include "flash.h"
#include "refproxy.h"


static bool has_reset = false;

static char bin_file[256] = "ram.bin";
static char *flash_bin_file = NULL;

static bool enable_difftest = true;
static int max_cycles = 0;

extern "C" void set_bin_file(char *s){
  printf("ram_image:%s\n", s);
  strcpy(bin_file, s);
} 

extern "C" void set_flash_bin(char *s){
  printf("flash_image:%s\n", s);
  flash_bin_file = (char *) malloc(256);
  strcpy(flash_bin_file, s);
}

extern "C" void set_diff_ref_so(char *s){
  printf("diff-test ref so:%s\n", s);
  extern const char *difftest_ref_so; //TODO
  char* buf = (char *)malloc(256);
  strcpy(buf, s);
  difftest_ref_so = buf;
}

extern "C" void set_no_diff() {
  printf("disable difftest\n");
  enable_difftest = false;
}

extern "C" void set_max_cycles(long mc){
  printf("max cycles:%d\n", mc);
  max_cycles = mc;
}

extern "C" void simv_init(){
  printf("simv compiled at %s, %s\n", __DATE__, __TIME__);
  setlocale(LC_NUMERIC, "");
  //difftest
  difftest_init();
  assert_init();
  //dut
  init_ram(bin_file);
  init_flash(flash_bin_file);
  //ref
  if(enable_difftest){
    init_refproxy(EMU_RAM_SIZE); 
  }
} 

extern "C" int simv_step(){
  //assert check
  if(assert_count > 0){ return 1; }

  //cycle check
  static int cycles = 0;
  if(max_cycles != 0){
    if(cycles >= max_cycles){
      eprintf(ANSI_COLOR_YELLOW "EXCEED MAX CYCLE: %d\n" ANSI_COLOR_RESET, max_cycles);
      return 1;
    }
    cycles ++;
  }

  //state check
  if(difftest_state() != -1){
    int trapCode = difftest_state();
    switch(trapCode){
      case 0:  eprintf(ANSI_COLOR_GREEN "HIT GOOD TRAP\n" ANSI_COLOR_RESET); break;
      default:  eprintf(ANSI_COLOR_GREEN "HIT GOOD TRAP\n" ANSI_COLOR_RESET);
    }
    return trapCode + 1;
  } 
  
  //diff state check
  if(enable_difftest){
    return difftest_step();
  }else {
    return 0;
  }
}
