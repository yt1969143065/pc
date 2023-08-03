#include "difftest.h"

extern "C" void v_difftest_TrapEvent(
  uint8_t  valid,
  uint8_t  code,
  uint64_t pc  
){
  if (difftest == NULL) return;
  auto packet = difftest->get_trap_event();
  packet->valid    = valid;
  packet->code     = code;
  packet->pc       = pc;
}

extern "C" void v_difftest_InstrCommit(
  uint8_t index,
  uint8_t valid,
  uint8_t rfwen,
  uint8_t fpwen,
  uint32_t wpdest,
  uint8_t wdest,
  uint64_t pc,
  uint32_t instr
){
  if (difftest == NULL) return;
  auto packet = difftest->get_instr_commit(index);
  packet->valid    = valid;
  if (packet->valid) {
    packet->pc       = pc;
    packet->inst     = instr;
    packet->rfwen    = rfwen;
    packet->fpwen    = fpwen;
    packet->wpdest   = wpdest;
    packet->wdest    = wdest;
  }
}

extern "C" void v_difftest_ArchIntRegState(
  uint64_t gpr_0,
  uint64_t gpr_1,
  uint64_t gpr_2,
  uint64_t gpr_3,
  uint64_t gpr_4,
  uint64_t gpr_5,
  uint64_t gpr_6,
  uint64_t gpr_7,
  uint64_t gpr_8,
  uint64_t gpr_9,
  uint64_t gpr_10,
  uint64_t gpr_11,
  uint64_t gpr_12,
  uint64_t gpr_13,
  uint64_t gpr_14,
  uint64_t gpr_15,
  uint64_t gpr_16,
  uint64_t gpr_17,
  uint64_t gpr_18,
  uint64_t gpr_19,
  uint64_t gpr_20,
  uint64_t gpr_21,
  uint64_t gpr_22,
  uint64_t gpr_23,
  uint64_t gpr_24,
  uint64_t gpr_25,
  uint64_t gpr_26,
  uint64_t gpr_27,
  uint64_t gpr_28,
  uint64_t gpr_29,
  uint64_t gpr_30,
  uint64_t gpr_31
){
  if (difftest == NULL) return;
  auto packet = difftest->get_arch_reg_state();
  packet->gpr[ 0] = gpr_0;
  packet->gpr[ 1] = gpr_1;
  packet->gpr[ 2] = gpr_2;
  packet->gpr[ 3] = gpr_3;
  packet->gpr[ 4] = gpr_4;
  packet->gpr[ 5] = gpr_5;
  packet->gpr[ 6] = gpr_6;
  packet->gpr[ 7] = gpr_7;
  packet->gpr[ 8] = gpr_8;
  packet->gpr[ 9] = gpr_9;
  packet->gpr[10] = gpr_10;
  packet->gpr[11] = gpr_11;
  packet->gpr[12] = gpr_12;
  packet->gpr[13] = gpr_13;
  packet->gpr[14] = gpr_14;
  packet->gpr[15] = gpr_15;
  packet->gpr[16] = gpr_16;
  packet->gpr[17] = gpr_17;
  packet->gpr[18] = gpr_18;
  packet->gpr[19] = gpr_19;
  packet->gpr[20] = gpr_20;
  packet->gpr[21] = gpr_21;
  packet->gpr[22] = gpr_22;
  packet->gpr[23] = gpr_23;
  packet->gpr[24] = gpr_24;
  packet->gpr[25] = gpr_25;
  packet->gpr[26] = gpr_26;
  packet->gpr[27] = gpr_27;
  packet->gpr[28] = gpr_28;
  packet->gpr[29] = gpr_29;
  packet->gpr[30] = gpr_30;
  packet->gpr[31] = gpr_31;
}

