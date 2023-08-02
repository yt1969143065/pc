import "DPI-C" function void set_bin_file(string bin);
import "DPI-C" function void set_flash_bin(string bin);
import "DPI-C" function void set_diff_ref_so(string diff_so);
import "DPI-C" function void set_no_diff();
import "DPI-C" function void set_max_cycles(int mc);
import "DPI-C" function void simv_init();
import "DPI-C" function int simv_step();

module tb();

reg         clock;
reg         reset;
reg [31:0] max_cycles;

string bin_file;
string flash_bin_file;
string diff_ref_so;


initial begin
  clock = 0;
  reset = 1;
  // enable waveform
  if ($test$plusargs("dump")) begin
      $fsdbDumpfile("simv.fsdb");
      $fsdbDumpvars(0,"+mda");
  end
  // workload: bin file
  if ($test$plusargs("workload")) begin
    $value$plusargs("workload=%s", bin_file);
    set_bin_file(bin_file);
  end
  // boot flash image: bin file
  if ($test$plusargs("flash")) begin
    $value$plusargs("flash=%s", flash_bin_file);
    set_flash_bin(flash_bin_file);
  end
  // diff-test golden model
  if ($test$plusargs("diff")) begin
    $value$plusargs("diff=%s", diff_ref_so);
    set_diff_ref_so(diff_ref_so);
  end
  // disable diff-test
  if ($test$plusargs("no-diff")) begin
    set_no_diff();
  end
  // max cycles to execute, no limit for default
  if ($test$plusargs("max-cycles")) begin
    $value$plusargs("max-cycles=%d", max_cycles);
    set_max_cycles(max_cycles);
  end
  else begin
    max_cycles = 0;
  end

  // Note: reset delay #100 should be larger than RANDOMIZE_DELAY
  #100 reset = 0;
end
always #1 clock <= ~clock;

SimTop sim(
  .clock(clock),
  .reset(reset)
);

reg has_init;
always @(posedge clock) begin
  if (reset) begin
    has_init <= 1'b0;
  end
  else if (!has_init) begin
    simv_init();
    has_init <= 1'b1;
  end

  // check errors
  if (!reset && has_init) begin
    if (simv_step()) begin
      $finish();
    end
  end

end

endmodule

