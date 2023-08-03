CSRC_DIR = $(abspath ../src/test/csrc)
VSRC_DIR = $(abspath ../build)

CFILES = $(shell find $(CSRC_DIR) -name "*.cpp")
VFILES = $(shell find $(VSRC_DIR) -name "*.v")
VFILE_LIST = $(shell find $(VSRC_DIR) -name "*.v" > flist.f) 

VCS_CFLAGS += -std=c++11 -static -Wall -I$(CSRC_DIR)/VCS -I$(CSRC_DIR)/common -I$(CSRC_DIR)/difftest -I$(CSRC_DIR)/dutif -I$(CSRC_DIR)/refif
#VCS_CFLAGS += -DDIFF_PROXY=SpikeProxy -DFIRST_INST_ADDRESS=0x80000000

VCS_LDFLAGS += -lpthread -lSDL2 -ldl -lz -lsqlite3

VCS_FLAGS += -full64 +v2k -timescale=1ns/1ns -sverilog -debug_access+all +lint=TFIPC-L +define+CONSIDER_FSDB
VCS_FLAGS += -CFLAGS "$(VCS_CFLAGS)" -LDFLAGS "$(VCS_LDFLAGS)" -j8
VCS_FLAGS += -l compile.log  +error+100
VCS_FLAGS += +define+RANDOMIZE_DELAY=1
VCS_FLAGS += +define+RANDOMIZE_GARBAGE_ASSIGN
VCS_FLAGS += +define+RANDOMIZE_INVALID_ASSIGN
VCS_FLAGS += +define+RANDOMIZE_MEM_INIT
VCS_FLAGS += +define+RANDOMIZE_REG_INIT
VCS_FLAGS += +define+DIFFTEST

compile:$(CFILES) $(VFILES)
	vcs $(VCS_FLAGS) $(CFILES) $(VFILES) 

CASE ?= /home/lyy/nexus-am/apps/coremark/build/coremark-riscv64-xs.bin
run:compile
	./simv +dump +workload=$(CASE) +diff=../ready-to-run/riscv64-spike-so +max-cycles=50000 2>&1 | tee run.log

verdi:$(VFILE_LIST)
	verdi -sv -f flist.f -ssf ./simv.fsdb -sswr signal.rc &

clean:
	rm -rf simv* *.log csrc *.h *.f *info* *.key novas* verdi*

.PHONY: compile run verdi clean

