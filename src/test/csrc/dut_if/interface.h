#ifndef __DT_INTERFACE_H__
#define __DT_INTERFACE_H__

#include "difftest.h"

#define DIFFTEST_DPIC_FUNC_NAME(name) \
  v_difftest_##name

#define DIFFTEST_DPIC_FUNC_DECL(name) \
  extern "C" void DIFFTEST_DPIC_FUNC_NAME(name)

#define DPIC_ARG_BIT  uint8_t
#define DPIC_ARG_BYTE uint8_t
#define DPIC_ARG_INT  uint32_t
#define DPIC_ARG_LONG uint64_t

// v_difftest_InstrCommit
#define INTERFACE_INSTR_COMMIT           \
  DIFFTEST_DPIC_FUNC_DECL(InstrCommit) ( \
    DPIC_ARG_BYTE index,                 \
    DPIC_ARG_BIT  valid,                 \
    DPIC_ARG_BIT  rfwen,                 \
    DPIC_ARG_BIT  fpwen,                 \
    DPIC_ARG_INT  wpdest,                \
    DPIC_ARG_BYTE wdest,                 \
    DPIC_ARG_LONG pc,                    \
    DPIC_ARG_INT  instr                  \
  )

// v_difftest_TrapEvent
#define INTERFACE_TRAP_EVENT             \
  DIFFTEST_DPIC_FUNC_DECL(TrapEvent) (   \
    DPIC_ARG_BIT  valid,                 \
    DPIC_ARG_BYTE code,                  \
    DPIC_ARG_LONG pc                     \
  )

// v_difftest_ArchIntRegState
#define INTERFACE_INT_REG_STATE          \
  DIFFTEST_DPIC_FUNC_DECL(ArchIntRegState) ( \
    DPIC_ARG_LONG gpr_0,                 \
    DPIC_ARG_LONG gpr_1,                 \
    DPIC_ARG_LONG gpr_2,                 \
    DPIC_ARG_LONG gpr_3,                 \
    DPIC_ARG_LONG gpr_4,                 \
    DPIC_ARG_LONG gpr_5,                 \
    DPIC_ARG_LONG gpr_6,                 \
    DPIC_ARG_LONG gpr_7,                 \
    DPIC_ARG_LONG gpr_8,                 \
    DPIC_ARG_LONG gpr_9,                 \
    DPIC_ARG_LONG gpr_10,                \
    DPIC_ARG_LONG gpr_11,                \
    DPIC_ARG_LONG gpr_12,                \
    DPIC_ARG_LONG gpr_13,                \
    DPIC_ARG_LONG gpr_14,                \
    DPIC_ARG_LONG gpr_15,                \
    DPIC_ARG_LONG gpr_16,                \
    DPIC_ARG_LONG gpr_17,                \
    DPIC_ARG_LONG gpr_18,                \
    DPIC_ARG_LONG gpr_19,                \
    DPIC_ARG_LONG gpr_20,                \
    DPIC_ARG_LONG gpr_21,                \
    DPIC_ARG_LONG gpr_22,                \
    DPIC_ARG_LONG gpr_23,                \
    DPIC_ARG_LONG gpr_24,                \
    DPIC_ARG_LONG gpr_25,                \
    DPIC_ARG_LONG gpr_26,                \
    DPIC_ARG_LONG gpr_27,                \
    DPIC_ARG_LONG gpr_28,                \
    DPIC_ARG_LONG gpr_29,                \
    DPIC_ARG_LONG gpr_30,                \
    DPIC_ARG_LONG gpr_31                 \
  )

// v_difftest_ArchFpRegState
#define INTERFACE_FP_REG_STATE           \
  DIFFTEST_DPIC_FUNC_DECL(ArchFpRegState) ( \
    DPIC_ARG_LONG fpr_0,                 \
    DPIC_ARG_LONG fpr_1,                 \
    DPIC_ARG_LONG fpr_2,                 \
    DPIC_ARG_LONG fpr_3,                 \
    DPIC_ARG_LONG fpr_4,                 \
    DPIC_ARG_LONG fpr_5,                 \
    DPIC_ARG_LONG fpr_6,                 \
    DPIC_ARG_LONG fpr_7,                 \
    DPIC_ARG_LONG fpr_8,                 \
    DPIC_ARG_LONG fpr_9,                 \
    DPIC_ARG_LONG fpr_10,                \
    DPIC_ARG_LONG fpr_11,                \
    DPIC_ARG_LONG fpr_12,                \
    DPIC_ARG_LONG fpr_13,                \
    DPIC_ARG_LONG fpr_14,                \
    DPIC_ARG_LONG fpr_15,                \
    DPIC_ARG_LONG fpr_16,                \
    DPIC_ARG_LONG fpr_17,                \
    DPIC_ARG_LONG fpr_18,                \
    DPIC_ARG_LONG fpr_19,                \
    DPIC_ARG_LONG fpr_20,                \
    DPIC_ARG_LONG fpr_21,                \
    DPIC_ARG_LONG fpr_22,                \
    DPIC_ARG_LONG fpr_23,                \
    DPIC_ARG_LONG fpr_24,                \
    DPIC_ARG_LONG fpr_25,                \
    DPIC_ARG_LONG fpr_26,                \
    DPIC_ARG_LONG fpr_27,                \
    DPIC_ARG_LONG fpr_28,                \
    DPIC_ARG_LONG fpr_29,                \
    DPIC_ARG_LONG fpr_30,                \
    DPIC_ARG_LONG fpr_31                 \
  )

INTERFACE_INSTR_COMMIT;
INTERFACE_TRAP_EVENT;
INTERFACE_INT_REG_STATE;
INTERFACE_FP_REG_STATE;

#endif
