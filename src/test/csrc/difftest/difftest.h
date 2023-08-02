#ifndef __DIFFTEST_H_
#define __DIFFTEST_H__

#include "common.h"
#include "refproxy.h"

#define FIRST_INST_ADDRESS 0x10000000
#define PMEM_BASE 0x80000000UL


enum { DIFFTEST_TO_DUT, DIFFTEST_TO_REF };
enum { DUT_TO_DIFFTEST, REF_TO_DIFFTEST };
enum { REF_TO_DUT, DUT_TO_REF };

typedef struct {
  uint8_t  valid    = 0;
  uint8_t  code     = 0;
  uint64_t pc       = 0;
  uint64_t cycleCnt = 0;
  uint64_t instrCnt = 0;
} trap_event_t;

typedef struct {
  uint8_t  valid    = 0;
  uint64_t pc;
  uint32_t inst;
  uint8_t  rfwen;
  uint8_t  fpwen;
  uint32_t wpdest;
  uint8_t  wdest; 
} instr_commit_t;

typedef struct {
  uint64_t gpr[32];
  uint64_t fpr[32];
} arch_reg_state_t;

typedef struct __attribute__((packed)){
  uint64_t this_pc;
} arch_csr_state_t;

typedef struct {
  trap_event_t     trap;
  instr_commit_t   commit[DIFFTEST_COMMIT_WIDTH];
  arch_reg_state_t regs;
  arch_csr_state_t csr;
} difftest_core_state_t;

const int DIFFTEST_NR_REG = (sizeof(arch_reg_state_t) + sizeof(arch_csr_state_t)) / sizeof(uint64_t);
/*
static const char *reg_name[DIFFTEST_NR_REG+1] = {
  "$0",  "ra",  "sp",   "gp",   "tp",  "t0",  "t1",   "t2",
  "s0",  "s1",  "a0",   "a1",   "a2",  "a3",  "a4",   "a5",
  "a6",  "a7",  "s2",   "s3",   "s4",  "s5",  "s6",   "s7",
  "s8",  "s9",  "s10",  "s11",  "t3",  "t4",  "t5",   "t6",
  "ft0", "ft1", "ft2",  "ft3",  "ft4", "ft5", "ft6",  "ft7",
  "fs0", "fs1", "fa0",  "fa1",  "fa2", "fa3", "fa4",  "fa5",
  "fa6", "fa7", "fs2",  "fs3",  "fs4", "fs5", "fs6",  "fs7",
  "fs8", "fs9", "fs10", "fs11", "ft8", "ft9", "ft10", "ft11",
  "this_pc"
}
*/


class Difftest {
public:
  Difftest();

  DIFF_PROXY *proxy = NULL;
  uint32_t num_commit = 0;
  bool has_commit = false;

  inline bool get_trap_valid(){return dut.trap.valid;}
  inline int get_trap_code(){return dut.trap.code;}
  inline trap_event_t *get_trap_event(){return &(dut.trap);}
  inline instr_commit_t *get_instr_commit(uint8_t index){return &(dut.commit[index]);}
  inline arch_reg_state_t *get_arch_reg_state(){return &(dut.regs);}
  inline difftest_core_state_t *get_dut() {return &dut;}
  inline difftest_core_state_t *get_ref() {return &ref;}
  void update_refproxy(size_t);
  virtual int step();

protected:
  const uint64_t firstCommit_limit = 10000;
  const uint64_t stuck_limit = 5000;
  difftest_core_state_t dut;
  difftest_core_state_t ref;
  uint64_t *dut_regs_ptr = (uint64_t*)&dut.regs;
  uint64_t *ref_regs_ptr = (uint64_t*)&ref.regs;
  bool progress = false;
  uint64_t ticks = 0;
  uint64_t instcnt = 0;
  uint64_t last_commit = 0;
  uint64_t nemu_this_pc;

  inline void update_last_commit(){last_commit = ticks;}
  int check_timeout();
  void raise_trap(int trapCode);
  void clear_step();
  void do_first_instr_commit();
  void do_instr_commit();
};

extern Difftest *difftest;
int difftest_init();
int init_refproxy(size_t);
int difftest_state();
int difftest_step();

#endif
