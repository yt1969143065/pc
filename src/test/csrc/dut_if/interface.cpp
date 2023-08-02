#include "interface.h"

#define RETURN_NO_NULL \
  if (difftest == NULL) return;

INTERFACE_TRAP_EVENT {
  RETURN_NO_NULL
  auto packet = difftest->get_trap_event();
  packet->valid    = valid;
  packet->code     = code;
  packet->pc       = pc;
}

INTERFACE_INSTR_COMMIT {
  RETURN_NO_NULL
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

INTERFACE_INT_REG_STATE {
  RETURN_NO_NULL
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

INTERFACE_FP_REG_STATE {
  RETURN_NO_NULL
  auto packet = difftest->get_arch_reg_state();
  packet->fpr[ 0] = fpr_0;
  packet->fpr[ 1] = fpr_1;
  packet->fpr[ 2] = fpr_2;
  packet->fpr[ 3] = fpr_3;
  packet->fpr[ 4] = fpr_4;
  packet->fpr[ 5] = fpr_5;
  packet->fpr[ 6] = fpr_6;
  packet->fpr[ 7] = fpr_7;
  packet->fpr[ 8] = fpr_8;
  packet->fpr[ 9] = fpr_9;
  packet->fpr[10] = fpr_10;
  packet->fpr[11] = fpr_11;
  packet->fpr[12] = fpr_12;
  packet->fpr[13] = fpr_13;
  packet->fpr[14] = fpr_14;
  packet->fpr[15] = fpr_15;
  packet->fpr[16] = fpr_16;
  packet->fpr[17] = fpr_17;
  packet->fpr[18] = fpr_18;
  packet->fpr[19] = fpr_19;
  packet->fpr[20] = fpr_20;
  packet->fpr[21] = fpr_21;
  packet->fpr[22] = fpr_22;
  packet->fpr[23] = fpr_23;
  packet->fpr[24] = fpr_24;
  packet->fpr[25] = fpr_25;
  packet->fpr[26] = fpr_26;
  packet->fpr[27] = fpr_27;
  packet->fpr[28] = fpr_28;
  packet->fpr[29] = fpr_29;
  packet->fpr[30] = fpr_30;
  packet->fpr[31] = fpr_31;
}

