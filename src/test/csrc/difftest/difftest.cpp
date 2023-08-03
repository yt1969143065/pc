#include "difftest.h"
#include "ram.h"
#include "flash.h"

Difftest *difftest = NULL;

int difftest_init(){
  difftest = new Difftest();
  return 0;
}

int init_refproxy(size_t ramsize = 0){
  difftest -> update_refproxy(ramsize);
  return 0;
}

int difftest_state(){
  if(difftest->get_trap_valid()){
    return difftest->get_trap_code();
  }
  return -1;
}

int difftest_step(){
  return difftest->step();
}

Difftest :: Difftest(){
  clear_step();
}

void Difftest::update_refproxy(size_t ram_size=0){
  proxy = new DIFF_PROXY(0, ram_size);
}

int Difftest::step(){
  progress = false;
  ticks ++;

  dut.csr.this_pc = dut.commit[0].pc;

  if(check_timeout()) {return 1;}
  do_first_instr_commit();
  if(!has_commit){return 0;}

  //pc check: dut this_pc us current pc, ref this_pc is next pc: dut this_pc us current pc, ref this_pc is next pc
  if(dut.csr.this_pc != ref.csr.this_pc && dut.commit[0].valid){
    printf("@%d, pc mismatch: ref=0x%16lx, dut=0x%16lx\n", ticks, ref.csr.this_pc, dut.csr.this_pc);
    printf("@@ %d/%d IPC:%1.2f @@\n\n", instcnt, ticks, 1.0*instcnt/ticks); 
  } 

  //reg data check
  num_commit = 0;
  for(int i=0; i<DIFFTEST_COMMIT_WIDTH && dut.commit[i].valid; i++){
    do_instr_commit();
    dut.commit[i].valid = 0;
    num_commit++; 
  }
  instcnt = instcnt + num_commit;
  
  if(!progress) {return 0;}
  for(int i=0; i<num_commit; i++){
    if(dut.commit[i].rfwen && dut_regs_ptr[dut.commit[i].wdest] != ref_regs_ptr[dut.commit[i].wdest]){
      printf("@%d, x%d different at pc=0x%16lx, right=0x%16lx, wrong=0x%16x, [%d]", 
        ticks, dut.commit[i].wdest, dut.csr.this_pc, 
        ref_regs_ptr[dut.commit[i].wdest],
        dut_regs_ptr[dut.commit[i].wdest],
        i
      );
      printf("@@ %d/%d IPC:%1.2f @@\n\n", instcnt, ticks, 1.0*instcnt/ticks); 
    }
  }
}

int Difftest::check_timeout(){
  if(!has_commit && (ticks > last_commit + firstCommit_limit)) {
    eprintf("No instruction commits for %lu cycles. Please check the first instruction.\n", firstCommit_limit);
    printf("@@ %d/%d IPC:%1.2f @@\n\n", instcnt, ticks, 1.0*instcnt/ticks); 
    return 1;
  }
  if(has_commit && (ticks > last_commit + stuck_limit)){
    eprintf("No instruction commits for %lu cycles, maybe get stuck.\n", stuck_limit);
    printf("@@ %d/%d IPC:%1.2f @@\n\n", instcnt, ticks, 1.0*instcnt/ticks); 
    return 1;
  }
  return 0;
}

void Difftest::raise_trap(int trapCode){
  dut.trap.valid = 1;
  dut.trap.code = trapCode;
}

void Difftest::clear_step(){
  dut.trap.valid = 0;
  for (int i = 0; i < DIFFTEST_COMMIT_WIDTH; i++) {
     dut.commit[i].valid = 0;
  }
}

//FIXME: can't be any FIRST_INST_ADDRESS and any firt commit_num
void Difftest::do_first_instr_commit(){
  if(!has_commit && dut.commit[0].valid){
    if(dut.commit[0].pc != FIRST_INST_ADDRESS){return;}
    printf("The first instruction has commited. Difftest enable.\n");
    has_commit = 1;
    proxy->load_flash_bin(get_flash_path(), get_flash_size());
    proxy->memcpy(PMEM_BASE, get_img_start(), get_img_size(), DIFFTEST_TO_REF);
    proxy->regcpy(dut_regs_ptr, DIFFTEST_TO_REF);
    proxy->regcpy(dut_regs_ptr, REF_TO_DUT);
  }
}

void Difftest::do_instr_commit(){
  progress = true;
  update_last_commit();
  proxy->exec(1);
  proxy->regcpy(dut_regs_ptr, REF_TO_DUT);
}
