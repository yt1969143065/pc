import "DPI-C" function longint flash_read_helper
(
  input  bit ren,
  input int addr
);

module FlashHelper (
  input clk,
  input [31:0] addr,
  input ren,
  output reg [63:0] data
);

  always @(negedge clk) begin
    data = flash_read_helper(ren, addr);
  end
endmodule

