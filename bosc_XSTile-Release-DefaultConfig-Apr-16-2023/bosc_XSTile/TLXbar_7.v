module TLXbar_7(
  input          clock,
  input          reset,
  output         auto_in_2_a_ready,
  input          auto_in_2_a_valid,
  input  [2:0]   auto_in_2_a_bits_opcode,
  input  [2:0]   auto_in_2_a_bits_param,
  input  [2:0]   auto_in_2_a_bits_size,
  input  [2:0]   auto_in_2_a_bits_source,
  input  [35:0]  auto_in_2_a_bits_address,
  input  [31:0]  auto_in_2_a_bits_mask,
  input  [255:0] auto_in_2_a_bits_data,
  input          auto_in_2_d_ready,
  output         auto_in_2_d_valid,
  output [2:0]   auto_in_2_d_bits_opcode,
  output [2:0]   auto_in_2_d_bits_size,
  output [2:0]   auto_in_2_d_bits_source,
  output         auto_in_2_d_bits_denied,
  output [255:0] auto_in_2_d_bits_data,
  output         auto_in_2_d_bits_corrupt,
  output         auto_in_1_a_ready,
  input          auto_in_1_a_valid,
  input  [2:0]   auto_in_1_a_bits_opcode,
  input  [2:0]   auto_in_1_a_bits_param,
  input  [2:0]   auto_in_1_a_bits_size,
  input  [1:0]   auto_in_1_a_bits_source,
  input  [35:0]  auto_in_1_a_bits_address,
  input          auto_in_1_a_bits_user_preferCache,
  input  [31:0]  auto_in_1_a_bits_mask,
  input  [255:0] auto_in_1_a_bits_data,
  input          auto_in_1_bready,
  output         auto_in_1_bvalid,
  output [1:0]   auto_in_1_bparam,
  output [35:0]  auto_in_1_baddress,
  output [255:0] auto_in_1_bdata,
  output         auto_in_1_c_ready,
  input          auto_in_1_c_valid,
  input  [2:0]   auto_in_1_c_bits_opcode,
  input  [2:0]   auto_in_1_c_bits_param,
  input  [2:0]   auto_in_1_c_bits_size,
  input  [1:0]   auto_in_1_c_bits_source,
  input  [35:0]  auto_in_1_c_bits_address,
  input          auto_in_1_c_bits_echo_blockisdirty,
  input  [255:0] auto_in_1_c_bits_data,
  input          auto_in_1_d_ready,
  output         auto_in_1_d_valid,
  output [2:0]   auto_in_1_d_bits_opcode,
  output [1:0]   auto_in_1_d_bits_param,
  output [1:0]   auto_in_1_d_bits_source,
  output [3:0]   auto_in_1_d_bits_sink,
  output         auto_in_1_d_bits_echo_blockisdirty,
  output [255:0] auto_in_1_d_bits_data,
  output         auto_in_1_d_bits_corrupt,
  output         auto_in_1_e_ready,
  input          auto_in_1_e_valid,
  input  [3:0]   auto_in_1_e_bits_sink,
  output         auto_in_0_a_ready,
  input          auto_in_0_a_valid,
  input  [2:0]   auto_in_0_a_bits_opcode,
  input  [2:0]   auto_in_0_a_bits_param,
  input  [2:0]   auto_in_0_a_bits_size,
  input  [3:0]   auto_in_0_a_bits_source,
  input  [35:0]  auto_in_0_a_bits_address,
  input          auto_in_0_a_bits_user_preferCache,
  input  [31:0]  auto_in_0_a_bits_mask,
  input  [255:0] auto_in_0_a_bits_data,
  input          auto_in_0_bready,
  output         auto_in_0_bvalid,
  output [2:0]   auto_in_0_bopcode,
  output [1:0]   auto_in_0_bparam,
  output [2:0]   auto_in_0_bsize,
  output [3:0]   auto_in_0_bsource,
  output [35:0]  auto_in_0_baddress,
  output [31:0]  auto_in_0_bmask,
  output [255:0] auto_in_0_bdata,
  output         auto_in_0_c_ready,
  input          auto_in_0_c_valid,
  input  [2:0]   auto_in_0_c_bits_opcode,
  input  [2:0]   auto_in_0_c_bits_param,
  input  [2:0]   auto_in_0_c_bits_size,
  input  [3:0]   auto_in_0_c_bits_source,
  input  [35:0]  auto_in_0_c_bits_address,
  input          auto_in_0_c_bits_echo_blockisdirty,
  input  [255:0] auto_in_0_c_bits_data,
  input          auto_in_0_d_ready,
  output         auto_in_0_d_valid,
  output [2:0]   auto_in_0_d_bits_opcode,
  output [1:0]   auto_in_0_d_bits_param,
  output [2:0]   auto_in_0_d_bits_size,
  output [3:0]   auto_in_0_d_bits_source,
  output [3:0]   auto_in_0_d_bits_sink,
  output         auto_in_0_d_bits_denied,
  output         auto_in_0_d_bits_echo_blockisdirty,
  output [255:0] auto_in_0_d_bits_data,
  output         auto_in_0_d_bits_corrupt,
  output         auto_in_0_e_ready,
  input          auto_in_0_e_valid,
  input  [3:0]   auto_in_0_e_bits_sink,
  input          auto_out_a_ready,
  output         auto_out_a_valid,
  output [2:0]   auto_out_a_bits_opcode,
  output [2:0]   auto_out_a_bits_param,
  output [2:0]   auto_out_a_bits_size,
  output [4:0]   auto_out_a_bits_source,
  output [35:0]  auto_out_a_bits_address,
  output         auto_out_a_bits_user_preferCache,
  output [31:0]  auto_out_a_bits_mask,
  output [255:0] auto_out_a_bits_data,
  output         auto_out_bready,
  input          auto_out_bvalid,
  input  [2:0]   auto_out_bopcode,
  input  [1:0]   auto_out_bparam,
  input  [2:0]   auto_out_bsize,
  input  [4:0]   auto_out_bsource,
  input  [35:0]  auto_out_baddress,
  input  [31:0]  auto_out_bmask,
  input  [255:0] auto_out_bdata,
  input          auto_out_c_ready,
  output         auto_out_c_valid,
  output [2:0]   auto_out_c_bits_opcode,
  output [2:0]   auto_out_c_bits_param,
  output [2:0]   auto_out_c_bits_size,
  output [4:0]   auto_out_c_bits_source,
  output [35:0]  auto_out_c_bits_address,
  output         auto_out_c_bits_echo_blockisdirty,
  output [255:0] auto_out_c_bits_data,
  output         auto_out_d_ready,
  input          auto_out_d_valid,
  input  [2:0]   auto_out_d_bits_opcode,
  input  [1:0]   auto_out_d_bits_param,
  input  [2:0]   auto_out_d_bits_size,
  input  [4:0]   auto_out_d_bits_source,
  input  [3:0]   auto_out_d_bits_sink,
  input          auto_out_d_bits_denied,
  input          auto_out_d_bits_echo_blockisdirty,
  input  [255:0] auto_out_d_bits_data,
  input          auto_out_d_bits_corrupt,
  input          auto_out_e_ready,
  output         auto_out_e_valid,
  output [3:0]   auto_out_e_bits_sink
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
  reg [31:0] _RAND_9;
  reg [31:0] _RAND_10;
  reg [31:0] _RAND_11;
  reg [31:0] _RAND_12;
`endif // RANDOMIZE_REG_INIT
  wire [4:0] _GEN_3 = {{3'd0}, auto_in_1_a_bits_source}; // @[Xbar.scala 237:55]
  wire [4:0] in_1_a_bits_source = _GEN_3 | 5'h18; // @[Xbar.scala 237:55]
  wire [4:0] _GEN_4 = {{3'd0}, auto_in_1_c_bits_source}; // @[Xbar.scala 259:55]
  wire [4:0] in_1_c_bits_source = _GEN_4 | 5'h18; // @[Xbar.scala 259:55]
  wire [4:0] _GEN_5 = {{2'd0}, auto_in_2_a_bits_source}; // @[Xbar.scala 237:55]
  wire [4:0] in_2_a_bits_source = _GEN_5 | 5'h10; // @[Xbar.scala 237:55]
  reg  beatsLeft_2; // @[Arbiter.scala 87:30]
  wire  idle_2 = ~beatsLeft_2; // @[Arbiter.scala 88:28]
  wire [1:0] readys_valid_2 = {auto_in_1_e_valid,auto_in_0_e_valid}; // @[Cat.scala 31:58]
  reg [1:0] readys_mask_2; // @[Arbiter.scala 23:23]
  wire [1:0] _readys_filter_T_4 = ~readys_mask_2; // @[Arbiter.scala 24:30]
  wire [1:0] _readys_filter_T_5 = readys_valid_2 & _readys_filter_T_4; // @[Arbiter.scala 24:28]
  wire [3:0] readys_filter_2 = {_readys_filter_T_5,auto_in_1_e_valid,auto_in_0_e_valid}; // @[Cat.scala 31:58]
  wire [3:0] _GEN_6 = {{1'd0}, readys_filter_2[3:1]}; // @[package.scala 253:43]
  wire [3:0] _readys_unready_T_13 = readys_filter_2 | _GEN_6; // @[package.scala 253:43]
  wire [3:0] _readys_unready_T_16 = {readys_mask_2, 2'h0}; // @[Arbiter.scala 25:66]
  wire [3:0] _GEN_7 = {{1'd0}, _readys_unready_T_13[3:1]}; // @[Arbiter.scala 25:58]
  wire [3:0] readys_unready_2 = _GEN_7 | _readys_unready_T_16; // @[Arbiter.scala 25:58]
  wire [1:0] _readys_readys_T_8 = readys_unready_2[3:2] & readys_unready_2[1:0]; // @[Arbiter.scala 26:39]
  wire [1:0] readys_readys_2 = ~_readys_readys_T_8; // @[Arbiter.scala 26:18]
  wire  readys_2_0 = readys_readys_2[0]; // @[Arbiter.scala 95:86]
  wire  earlyWinner_2_0 = readys_2_0 & auto_in_0_e_valid; // @[Arbiter.scala 97:79]
  reg  state_2_0; // @[Arbiter.scala 116:26]
  wire  muxStateEarly_2_0 = idle_2 ? earlyWinner_2_0 : state_2_0; // @[Arbiter.scala 117:30]
  wire [3:0] _T_166 = muxStateEarly_2_0 ? auto_in_0_e_bits_sink : 4'h0; // @[Mux.scala 27:73]
  wire  readys_2_1 = readys_readys_2[1]; // @[Arbiter.scala 95:86]
  wire  earlyWinner_2_1 = readys_2_1 & auto_in_1_e_valid; // @[Arbiter.scala 97:79]
  reg  state_2_1; // @[Arbiter.scala 116:26]
  wire  muxStateEarly_2_1 = idle_2 ? earlyWinner_2_1 : state_2_1; // @[Arbiter.scala 117:30]
  wire [3:0] _T_167 = muxStateEarly_2_1 ? auto_in_1_e_bits_sink : 4'h0; // @[Mux.scala 27:73]
  wire  requestBOI_0_0 = ~auto_out_bsource[4]; // @[Parameters.scala 54:32]
  wire  requestBOI_0_1 = auto_out_bsource[4:2] == 3'h6; // @[Parameters.scala 54:32]
  wire  requestDOI_0_0 = ~auto_out_d_bits_source[4]; // @[Parameters.scala 54:32]
  wire  requestDOI_0_1 = auto_out_d_bits_source[4:2] == 3'h6; // @[Parameters.scala 54:32]
  wire  requestDOI_0_2 = auto_out_d_bits_source[4:3] == 2'h2; // @[Parameters.scala 54:32]
  wire [12:0] _beatsAI_decode_T_1 = 13'h3f << auto_in_0_a_bits_size; // @[package.scala 234:77]
  wire [5:0] _beatsAI_decode_T_3 = ~_beatsAI_decode_T_1[5:0]; // @[package.scala 234:46]
  wire  beatsAI_decode = _beatsAI_decode_T_3[5]; // @[Edges.scala 219:59]
  wire  beatsAI_opdata = ~auto_in_0_a_bits_opcode[2]; // @[Edges.scala 91:28]
  wire  beatsAI_0 = beatsAI_opdata & beatsAI_decode; // @[Edges.scala 220:14]
  wire [12:0] _beatsAI_decode_T_5 = 13'h3f << auto_in_1_a_bits_size; // @[package.scala 234:77]
  wire [5:0] _beatsAI_decode_T_7 = ~_beatsAI_decode_T_5[5:0]; // @[package.scala 234:46]
  wire  beatsAI_decode_1 = _beatsAI_decode_T_7[5]; // @[Edges.scala 219:59]
  wire  beatsAI_opdata_1 = ~auto_in_1_a_bits_opcode[2]; // @[Edges.scala 91:28]
  wire  beatsAI_1 = beatsAI_opdata_1 & beatsAI_decode_1; // @[Edges.scala 220:14]
  wire [12:0] _beatsAI_decode_T_9 = 13'h3f << auto_in_2_a_bits_size; // @[package.scala 234:77]
  wire [5:0] _beatsAI_decode_T_11 = ~_beatsAI_decode_T_9[5:0]; // @[package.scala 234:46]
  wire  beatsAI_decode_2 = _beatsAI_decode_T_11[5]; // @[Edges.scala 219:59]
  wire  beatsAI_opdata_2 = ~auto_in_2_a_bits_opcode[2]; // @[Edges.scala 91:28]
  wire  beatsAI_2 = beatsAI_opdata_2 & beatsAI_decode_2; // @[Edges.scala 220:14]
  wire [12:0] _beatsCI_decode_T_1 = 13'h3f << auto_in_0_c_bits_size; // @[package.scala 234:77]
  wire [5:0] _beatsCI_decode_T_3 = ~_beatsCI_decode_T_1[5:0]; // @[package.scala 234:46]
  wire  beatsCI_decode = _beatsCI_decode_T_3[5]; // @[Edges.scala 219:59]
  wire  beatsCI_opdata = auto_in_0_c_bits_opcode[0]; // @[Edges.scala 101:36]
  wire  beatsCI_0 = beatsCI_opdata & beatsCI_decode; // @[Edges.scala 220:14]
  wire [12:0] _beatsCI_decode_T_5 = 13'h3f << auto_in_1_c_bits_size; // @[package.scala 234:77]
  wire [5:0] _beatsCI_decode_T_7 = ~_beatsCI_decode_T_5[5:0]; // @[package.scala 234:46]
  wire  beatsCI_decode_1 = _beatsCI_decode_T_7[5]; // @[Edges.scala 219:59]
  wire  beatsCI_opdata_1 = auto_in_1_c_bits_opcode[0]; // @[Edges.scala 101:36]
  wire  beatsCI_1 = beatsCI_opdata_1 & beatsCI_decode_1; // @[Edges.scala 220:14]
  reg  beatsLeft; // @[Arbiter.scala 87:30]
  wire  idle = ~beatsLeft; // @[Arbiter.scala 88:28]
  wire  latch = idle & auto_out_a_ready; // @[Arbiter.scala 89:24]
  wire [2:0] readys_valid = {auto_in_2_a_valid,auto_in_1_a_valid,auto_in_0_a_valid}; // @[Cat.scala 31:58]
  reg [2:0] readys_mask; // @[Arbiter.scala 23:23]
  wire [2:0] _readys_filter_T = ~readys_mask; // @[Arbiter.scala 24:30]
  wire [2:0] _readys_filter_T_1 = readys_valid & _readys_filter_T; // @[Arbiter.scala 24:28]
  wire [5:0] readys_filter = {_readys_filter_T_1,auto_in_2_a_valid,auto_in_1_a_valid,auto_in_0_a_valid}; // @[Cat.scala 31:58]
  wire [5:0] _GEN_8 = {{1'd0}, readys_filter[5:1]}; // @[package.scala 253:43]
  wire [5:0] _readys_unready_T_1 = readys_filter | _GEN_8; // @[package.scala 253:43]
  wire [5:0] _GEN_9 = {{2'd0}, _readys_unready_T_1[5:2]}; // @[package.scala 253:43]
  wire [5:0] _readys_unready_T_3 = _readys_unready_T_1 | _GEN_9; // @[package.scala 253:43]
  wire [5:0] _readys_unready_T_6 = {readys_mask, 3'h0}; // @[Arbiter.scala 25:66]
  wire [5:0] _GEN_10 = {{1'd0}, _readys_unready_T_3[5:1]}; // @[Arbiter.scala 25:58]
  wire [5:0] readys_unready = _GEN_10 | _readys_unready_T_6; // @[Arbiter.scala 25:58]
  wire [2:0] _readys_readys_T_2 = readys_unready[5:3] & readys_unready[2:0]; // @[Arbiter.scala 26:39]
  wire [2:0] readys_readys = ~_readys_readys_T_2; // @[Arbiter.scala 26:18]
  wire [2:0] _readys_mask_T = readys_readys & readys_valid; // @[Arbiter.scala 28:29]
  wire [3:0] _readys_mask_T_1 = {_readys_mask_T, 1'h0}; // @[package.scala 244:48]
  wire [2:0] _readys_mask_T_3 = _readys_mask_T | _readys_mask_T_1[2:0]; // @[package.scala 244:43]
  wire [4:0] _readys_mask_T_4 = {_readys_mask_T_3, 2'h0}; // @[package.scala 244:48]
  wire [2:0] _readys_mask_T_6 = _readys_mask_T_3 | _readys_mask_T_4[2:0]; // @[package.scala 244:43]
  wire  readys__0 = readys_readys[0]; // @[Arbiter.scala 95:86]
  wire  readys__1 = readys_readys[1]; // @[Arbiter.scala 95:86]
  wire  readys__2 = readys_readys[2]; // @[Arbiter.scala 95:86]
  wire  earlyWinner__0 = readys__0 & auto_in_0_a_valid; // @[Arbiter.scala 97:79]
  wire  earlyWinner__1 = readys__1 & auto_in_1_a_valid; // @[Arbiter.scala 97:79]
  wire  earlyWinner__2 = readys__2 & auto_in_2_a_valid; // @[Arbiter.scala 97:79]
  wire  _T_16 = auto_in_0_a_valid | auto_in_1_a_valid | auto_in_2_a_valid; // @[Arbiter.scala 107:36]
  wire  maskedBeats_0 = earlyWinner__0 & beatsAI_0; // @[Arbiter.scala 111:73]
  wire  maskedBeats_1 = earlyWinner__1 & beatsAI_1; // @[Arbiter.scala 111:73]
  wire  maskedBeats_2 = earlyWinner__2 & beatsAI_2; // @[Arbiter.scala 111:73]
  wire  initBeats = maskedBeats_0 | maskedBeats_1 | maskedBeats_2; // @[Arbiter.scala 112:44]
  reg  state__0; // @[Arbiter.scala 116:26]
  wire  muxStateEarly__0 = idle ? earlyWinner__0 : state__0; // @[Arbiter.scala 117:30]
  reg  state__1; // @[Arbiter.scala 116:26]
  wire  muxStateEarly__1 = idle ? earlyWinner__1 : state__1; // @[Arbiter.scala 117:30]
  reg  state__2; // @[Arbiter.scala 116:26]
  wire  muxStateEarly__2 = idle ? earlyWinner__2 : state__2; // @[Arbiter.scala 117:30]
  wire  _out_0_a_earlyValid_T_6 = state__0 & auto_in_0_a_valid | state__1 & auto_in_1_a_valid | state__2 &
    auto_in_2_a_valid; // @[Mux.scala 27:73]
  wire  out_3_0_a_earlyValid = idle ? _T_16 : _out_0_a_earlyValid_T_6; // @[Arbiter.scala 125:29]
  wire  _beatsLeft_T_2 = auto_out_a_ready & out_3_0_a_earlyValid; // @[ReadyValidCancel.scala 49:33]
  wire  allowed__0 = idle ? readys__0 : state__0; // @[Arbiter.scala 121:24]
  wire  allowed__1 = idle ? readys__1 : state__1; // @[Arbiter.scala 121:24]
  wire  allowed__2 = idle ? readys__2 : state__2; // @[Arbiter.scala 121:24]
  wire [255:0] _T_38 = muxStateEarly__0 ? auto_in_0_a_bits_data : 256'h0; // @[Mux.scala 27:73]
  wire [255:0] _T_39 = muxStateEarly__1 ? auto_in_1_a_bits_data : 256'h0; // @[Mux.scala 27:73]
  wire [255:0] _T_40 = muxStateEarly__2 ? auto_in_2_a_bits_data : 256'h0; // @[Mux.scala 27:73]
  wire [255:0] _T_41 = _T_38 | _T_39; // @[Mux.scala 27:73]
  wire [31:0] _T_43 = muxStateEarly__0 ? auto_in_0_a_bits_mask : 32'h0; // @[Mux.scala 27:73]
  wire [31:0] _T_44 = muxStateEarly__1 ? auto_in_1_a_bits_mask : 32'h0; // @[Mux.scala 27:73]
  wire [31:0] _T_45 = muxStateEarly__2 ? auto_in_2_a_bits_mask : 32'h0; // @[Mux.scala 27:73]
  wire [31:0] _T_46 = _T_43 | _T_44; // @[Mux.scala 27:73]
  wire [35:0] _T_63 = muxStateEarly__0 ? auto_in_0_a_bits_address : 36'h0; // @[Mux.scala 27:73]
  wire [35:0] _T_64 = muxStateEarly__1 ? auto_in_1_a_bits_address : 36'h0; // @[Mux.scala 27:73]
  wire [35:0] _T_65 = muxStateEarly__2 ? auto_in_2_a_bits_address : 36'h0; // @[Mux.scala 27:73]
  wire [35:0] _T_66 = _T_63 | _T_64; // @[Mux.scala 27:73]
  wire [4:0] in_0_a_bits_source = {{1'd0}, auto_in_0_a_bits_source}; // @[Xbar.scala 231:18 237:29]
  wire [4:0] _T_68 = muxStateEarly__0 ? in_0_a_bits_source : 5'h0; // @[Mux.scala 27:73]
  wire [4:0] _T_69 = muxStateEarly__1 ? in_1_a_bits_source : 5'h0; // @[Mux.scala 27:73]
  wire [4:0] _T_70 = muxStateEarly__2 ? in_2_a_bits_source : 5'h0; // @[Mux.scala 27:73]
  wire [4:0] _T_71 = _T_68 | _T_69; // @[Mux.scala 27:73]
  wire [2:0] _T_73 = muxStateEarly__0 ? auto_in_0_a_bits_size : 3'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_74 = muxStateEarly__1 ? auto_in_1_a_bits_size : 3'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_75 = muxStateEarly__2 ? auto_in_2_a_bits_size : 3'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_76 = _T_73 | _T_74; // @[Mux.scala 27:73]
  wire [2:0] _T_78 = muxStateEarly__0 ? auto_in_0_a_bits_param : 3'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_79 = muxStateEarly__1 ? auto_in_1_a_bits_param : 3'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_80 = muxStateEarly__2 ? auto_in_2_a_bits_param : 3'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_81 = _T_78 | _T_79; // @[Mux.scala 27:73]
  wire [2:0] _T_83 = muxStateEarly__0 ? auto_in_0_a_bits_opcode : 3'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_84 = muxStateEarly__1 ? auto_in_1_a_bits_opcode : 3'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_85 = muxStateEarly__2 ? auto_in_2_a_bits_opcode : 3'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_86 = _T_83 | _T_84; // @[Mux.scala 27:73]
  reg  beatsLeft_1; // @[Arbiter.scala 87:30]
  wire  idle_1 = ~beatsLeft_1; // @[Arbiter.scala 88:28]
  wire  latch_1 = idle_1 & auto_out_c_ready; // @[Arbiter.scala 89:24]
  wire [1:0] readys_valid_1 = {auto_in_1_c_valid,auto_in_0_c_valid}; // @[Cat.scala 31:58]
  reg [1:0] readys_mask_1; // @[Arbiter.scala 23:23]
  wire [1:0] _readys_filter_T_2 = ~readys_mask_1; // @[Arbiter.scala 24:30]
  wire [1:0] _readys_filter_T_3 = readys_valid_1 & _readys_filter_T_2; // @[Arbiter.scala 24:28]
  wire [3:0] readys_filter_1 = {_readys_filter_T_3,auto_in_1_c_valid,auto_in_0_c_valid}; // @[Cat.scala 31:58]
  wire [3:0] _GEN_11 = {{1'd0}, readys_filter_1[3:1]}; // @[package.scala 253:43]
  wire [3:0] _readys_unready_T_8 = readys_filter_1 | _GEN_11; // @[package.scala 253:43]
  wire [3:0] _readys_unready_T_11 = {readys_mask_1, 2'h0}; // @[Arbiter.scala 25:66]
  wire [3:0] _GEN_12 = {{1'd0}, _readys_unready_T_8[3:1]}; // @[Arbiter.scala 25:58]
  wire [3:0] readys_unready_1 = _GEN_12 | _readys_unready_T_11; // @[Arbiter.scala 25:58]
  wire [1:0] _readys_readys_T_5 = readys_unready_1[3:2] & readys_unready_1[1:0]; // @[Arbiter.scala 26:39]
  wire [1:0] readys_readys_1 = ~_readys_readys_T_5; // @[Arbiter.scala 26:18]
  wire [1:0] _readys_mask_T_8 = readys_readys_1 & readys_valid_1; // @[Arbiter.scala 28:29]
  wire [2:0] _readys_mask_T_9 = {_readys_mask_T_8, 1'h0}; // @[package.scala 244:48]
  wire [1:0] _readys_mask_T_11 = _readys_mask_T_8 | _readys_mask_T_9[1:0]; // @[package.scala 244:43]
  wire  readys_1_0 = readys_readys_1[0]; // @[Arbiter.scala 95:86]
  wire  readys_1_1 = readys_readys_1[1]; // @[Arbiter.scala 95:86]
  wire  earlyWinner_1_0 = readys_1_0 & auto_in_0_c_valid; // @[Arbiter.scala 97:79]
  wire  earlyWinner_1_1 = readys_1_1 & auto_in_1_c_valid; // @[Arbiter.scala 97:79]
  wire  _T_98 = auto_in_0_c_valid | auto_in_1_c_valid; // @[Arbiter.scala 107:36]
  wire  maskedBeats_0_1 = earlyWinner_1_0 & beatsCI_0; // @[Arbiter.scala 111:73]
  wire  maskedBeats_1_1 = earlyWinner_1_1 & beatsCI_1; // @[Arbiter.scala 111:73]
  wire  initBeats_1 = maskedBeats_0_1 | maskedBeats_1_1; // @[Arbiter.scala 112:44]
  reg  state_1_0; // @[Arbiter.scala 116:26]
  wire  muxStateEarly_1_0 = idle_1 ? earlyWinner_1_0 : state_1_0; // @[Arbiter.scala 117:30]
  reg  state_1_1; // @[Arbiter.scala 116:26]
  wire  muxStateEarly_1_1 = idle_1 ? earlyWinner_1_1 : state_1_1; // @[Arbiter.scala 117:30]
  wire  _sink_ACancel_earlyValid_T_3 = state_1_0 & auto_in_0_c_valid | state_1_1 & auto_in_1_c_valid; // @[Mux.scala 27:73]
  wire  sink_ACancel_earlyValid = idle_1 ? _T_98 : _sink_ACancel_earlyValid_T_3; // @[Arbiter.scala 125:29]
  wire  _beatsLeft_T_8 = auto_out_c_ready & sink_ACancel_earlyValid; // @[ReadyValidCancel.scala 49:33]
  wire  allowed_1_0 = idle_1 ? readys_1_0 : state_1_0; // @[Arbiter.scala 121:24]
  wire  allowed_1_1 = idle_1 ? readys_1_1 : state_1_1; // @[Arbiter.scala 121:24]
  wire [255:0] _T_115 = muxStateEarly_1_0 ? auto_in_0_c_bits_data : 256'h0; // @[Mux.scala 27:73]
  wire [255:0] _T_116 = muxStateEarly_1_1 ? auto_in_1_c_bits_data : 256'h0; // @[Mux.scala 27:73]
  wire [35:0] _T_127 = muxStateEarly_1_0 ? auto_in_0_c_bits_address : 36'h0; // @[Mux.scala 27:73]
  wire [35:0] _T_128 = muxStateEarly_1_1 ? auto_in_1_c_bits_address : 36'h0; // @[Mux.scala 27:73]
  wire [4:0] in_0_c_bits_source = {{1'd0}, auto_in_0_c_bits_source}; // @[Xbar.scala 231:18 259:29]
  wire [4:0] _T_130 = muxStateEarly_1_0 ? in_0_c_bits_source : 5'h0; // @[Mux.scala 27:73]
  wire [4:0] _T_131 = muxStateEarly_1_1 ? in_1_c_bits_source : 5'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_133 = muxStateEarly_1_0 ? auto_in_0_c_bits_size : 3'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_134 = muxStateEarly_1_1 ? auto_in_1_c_bits_size : 3'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_136 = muxStateEarly_1_0 ? auto_in_0_c_bits_param : 3'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_137 = muxStateEarly_1_1 ? auto_in_1_c_bits_param : 3'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_139 = muxStateEarly_1_0 ? auto_in_0_c_bits_opcode : 3'h0; // @[Mux.scala 27:73]
  wire [2:0] _T_140 = muxStateEarly_1_1 ? auto_in_1_c_bits_opcode : 3'h0; // @[Mux.scala 27:73]
  wire  latch_2 = idle_2 & auto_out_e_ready; // @[Arbiter.scala 89:24]
  wire [1:0] _readys_mask_T_13 = readys_readys_2 & readys_valid_2; // @[Arbiter.scala 28:29]
  wire [2:0] _readys_mask_T_14 = {_readys_mask_T_13, 1'h0}; // @[package.scala 244:48]
  wire [1:0] _readys_mask_T_16 = _readys_mask_T_13 | _readys_mask_T_14[1:0]; // @[package.scala 244:43]
  wire  _T_152 = auto_in_0_e_valid | auto_in_1_e_valid; // @[Arbiter.scala 107:36]
  wire  _sink_ACancel_earlyValid_T_8 = state_2_0 & auto_in_0_e_valid | state_2_1 & auto_in_1_e_valid; // @[Mux.scala 27:73]
  wire  sink_ACancel_1_earlyValid = idle_2 ? _T_152 : _sink_ACancel_earlyValid_T_8; // @[Arbiter.scala 125:29]
  wire  _beatsLeft_T_14 = auto_out_e_ready & sink_ACancel_1_earlyValid; // @[ReadyValidCancel.scala 49:33]
  wire  allowed_2_0 = idle_2 ? readys_2_0 : state_2_0; // @[Arbiter.scala 121:24]
  wire  allowed_2_1 = idle_2 ? readys_2_1 : state_2_1; // @[Arbiter.scala 121:24]
  assign auto_in_2_a_ready = auto_out_a_ready & allowed__2; // @[Arbiter.scala 123:31]
  assign auto_in_2_d_valid = auto_out_d_valid & requestDOI_0_2; // @[Xbar.scala 179:40]
  assign auto_in_2_d_bits_opcode = auto_out_d_bits_opcode; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_2_d_bits_size = auto_out_d_bits_size; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_2_d_bits_source = auto_out_d_bits_source[2:0]; // @[Xbar.scala 228:69]
  assign auto_in_2_d_bits_denied = auto_out_d_bits_denied; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_2_d_bits_data = auto_out_d_bits_data; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_2_d_bits_corrupt = auto_out_d_bits_corrupt; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_1_a_ready = auto_out_a_ready & allowed__1; // @[Arbiter.scala 123:31]
  assign auto_in_1_bvalid = auto_out_bvalid & requestBOI_0_1; // @[Xbar.scala 179:40]
  assign auto_in_1_bparam = auto_out_bparam; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_1_baddress = auto_out_baddress; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_1_bdata = auto_out_bdata; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_1_c_ready = auto_out_c_ready & allowed_1_1; // @[Arbiter.scala 123:31]
  assign auto_in_1_d_valid = auto_out_d_valid & requestDOI_0_1; // @[Xbar.scala 179:40]
  assign auto_in_1_d_bits_opcode = auto_out_d_bits_opcode; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_1_d_bits_param = auto_out_d_bits_param; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_1_d_bits_source = auto_out_d_bits_source[1:0]; // @[Xbar.scala 228:69]
  assign auto_in_1_d_bits_sink = auto_out_d_bits_sink; // @[Xbar.scala 323:53]
  assign auto_in_1_d_bits_echo_blockisdirty = auto_out_d_bits_echo_blockisdirty; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_1_d_bits_data = auto_out_d_bits_data; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_1_d_bits_corrupt = auto_out_d_bits_corrupt; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_1_e_ready = auto_out_e_ready & allowed_2_1; // @[Arbiter.scala 123:31]
  assign auto_in_0_a_ready = auto_out_a_ready & allowed__0; // @[Arbiter.scala 123:31]
  assign auto_in_0_bvalid = auto_out_bvalid & requestBOI_0_0; // @[Xbar.scala 179:40]
  assign auto_in_0_bopcode = auto_out_bopcode; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_0_bparam = auto_out_bparam; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_0_bsize = auto_out_bsize; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_0_bsource = auto_out_bsource[3:0]; // @[Xbar.scala 228:69]
  assign auto_in_0_baddress = auto_out_baddress; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_0_bmask = auto_out_bmask; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_0_bdata = auto_out_bdata; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_0_c_ready = auto_out_c_ready & allowed_1_0; // @[Arbiter.scala 123:31]
  assign auto_in_0_d_valid = auto_out_d_valid & requestDOI_0_0; // @[Xbar.scala 179:40]
  assign auto_in_0_d_bits_opcode = auto_out_d_bits_opcode; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_0_d_bits_param = auto_out_d_bits_param; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_0_d_bits_size = auto_out_d_bits_size; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_0_d_bits_source = auto_out_d_bits_source[3:0]; // @[Xbar.scala 228:69]
  assign auto_in_0_d_bits_sink = auto_out_d_bits_sink; // @[Xbar.scala 323:53]
  assign auto_in_0_d_bits_denied = auto_out_d_bits_denied; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_0_d_bits_echo_blockisdirty = auto_out_d_bits_echo_blockisdirty; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_0_d_bits_data = auto_out_d_bits_data; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_0_d_bits_corrupt = auto_out_d_bits_corrupt; // @[Nodes.scala 1207:84 LazyModule.scala 311:12]
  assign auto_in_0_e_ready = auto_out_e_ready & allowed_2_0; // @[Arbiter.scala 123:31]
  assign auto_out_a_valid = idle ? _T_16 : _out_0_a_earlyValid_T_6; // @[Arbiter.scala 125:29]
  assign auto_out_a_bits_opcode = _T_86 | _T_85; // @[Mux.scala 27:73]
  assign auto_out_a_bits_param = _T_81 | _T_80; // @[Mux.scala 27:73]
  assign auto_out_a_bits_size = _T_76 | _T_75; // @[Mux.scala 27:73]
  assign auto_out_a_bits_source = _T_71 | _T_70; // @[Mux.scala 27:73]
  assign auto_out_a_bits_address = _T_66 | _T_65; // @[Mux.scala 27:73]
  assign auto_out_a_bits_user_preferCache = muxStateEarly__0 & auto_in_0_a_bits_user_preferCache | muxStateEarly__1 &
    auto_in_1_a_bits_user_preferCache; // @[Mux.scala 27:73]
  assign auto_out_a_bits_mask = _T_46 | _T_45; // @[Mux.scala 27:73]
  assign auto_out_a_bits_data = _T_41 | _T_40; // @[Mux.scala 27:73]
  assign auto_out_bready = requestBOI_0_0 & auto_in_0_bready | requestBOI_0_1 & auto_in_1_bready; // @[Mux.scala 27:73]
  assign auto_out_c_valid = idle_1 ? _T_98 : _sink_ACancel_earlyValid_T_3; // @[Arbiter.scala 125:29]
  assign auto_out_c_bits_opcode = _T_139 | _T_140; // @[Mux.scala 27:73]
  assign auto_out_c_bits_param = _T_136 | _T_137; // @[Mux.scala 27:73]
  assign auto_out_c_bits_size = _T_133 | _T_134; // @[Mux.scala 27:73]
  assign auto_out_c_bits_source = _T_130 | _T_131; // @[Mux.scala 27:73]
  assign auto_out_c_bits_address = _T_127 | _T_128; // @[Mux.scala 27:73]
  assign auto_out_c_bits_echo_blockisdirty = muxStateEarly_1_0 & auto_in_0_c_bits_echo_blockisdirty | muxStateEarly_1_1
     & auto_in_1_c_bits_echo_blockisdirty; // @[Mux.scala 27:73]
  assign auto_out_c_bits_data = _T_115 | _T_116; // @[Mux.scala 27:73]
  assign auto_out_d_ready = requestDOI_0_0 & auto_in_0_d_ready | requestDOI_0_1 & auto_in_1_d_ready | requestDOI_0_2 &
    auto_in_2_d_ready; // @[Mux.scala 27:73]
  assign auto_out_e_valid = idle_2 ? _T_152 : _sink_ACancel_earlyValid_T_8; // @[Arbiter.scala 125:29]
  assign auto_out_e_bits_sink = _T_166 | _T_167; // @[Mux.scala 27:73]
  always @(posedge clock or posedge reset) begin
    if (reset) begin // @[Arbiter.scala 113:23]
      beatsLeft_2 <= 1'h0;
    end else if (latch_2) begin
      beatsLeft_2 <= 1'h0;
    end else begin
      beatsLeft_2 <= beatsLeft_2 - _beatsLeft_T_14;
    end
  end
  always @(posedge clock or posedge reset) begin
    if (reset) begin // @[Arbiter.scala 27:32]
      readys_mask_2 <= 2'h3; // @[Arbiter.scala 28:12]
    end else if (latch_2 & |readys_valid_2) begin // @[Arbiter.scala 23:23]
      readys_mask_2 <= _readys_mask_T_16;
    end
  end
  always @(posedge clock or posedge reset) begin
    if (reset) begin // @[Arbiter.scala 118:30]
      state_2_0 <= 1'h0;
    end else if (idle_2) begin
      state_2_0 <= earlyWinner_2_0;
    end
  end
  always @(posedge clock or posedge reset) begin
    if (reset) begin // @[Arbiter.scala 118:30]
      state_2_1 <= 1'h0;
    end else if (idle_2) begin
      state_2_1 <= earlyWinner_2_1;
    end
  end
  always @(posedge clock or posedge reset) begin
    if (reset) begin // @[Arbiter.scala 113:23]
      beatsLeft <= 1'h0;
    end else if (latch) begin
      beatsLeft <= initBeats;
    end else begin
      beatsLeft <= beatsLeft - _beatsLeft_T_2;
    end
  end
  always @(posedge clock or posedge reset) begin
    if (reset) begin // @[Arbiter.scala 27:32]
      readys_mask <= 3'h7; // @[Arbiter.scala 28:12]
    end else if (latch & |readys_valid) begin // @[Arbiter.scala 23:23]
      readys_mask <= _readys_mask_T_6;
    end
  end
  always @(posedge clock or posedge reset) begin
    if (reset) begin // @[Arbiter.scala 118:30]
      state__0 <= 1'h0;
    end else if (idle) begin
      state__0 <= earlyWinner__0;
    end
  end
  always @(posedge clock or posedge reset) begin
    if (reset) begin // @[Arbiter.scala 118:30]
      state__1 <= 1'h0;
    end else if (idle) begin
      state__1 <= earlyWinner__1;
    end
  end
  always @(posedge clock or posedge reset) begin
    if (reset) begin // @[Arbiter.scala 118:30]
      state__2 <= 1'h0;
    end else if (idle) begin
      state__2 <= earlyWinner__2;
    end
  end
  always @(posedge clock or posedge reset) begin
    if (reset) begin // @[Arbiter.scala 113:23]
      beatsLeft_1 <= 1'h0;
    end else if (latch_1) begin
      beatsLeft_1 <= initBeats_1;
    end else begin
      beatsLeft_1 <= beatsLeft_1 - _beatsLeft_T_8;
    end
  end
  always @(posedge clock or posedge reset) begin
    if (reset) begin // @[Arbiter.scala 27:32]
      readys_mask_1 <= 2'h3; // @[Arbiter.scala 28:12]
    end else if (latch_1 & |readys_valid_1) begin // @[Arbiter.scala 23:23]
      readys_mask_1 <= _readys_mask_T_11;
    end
  end
  always @(posedge clock or posedge reset) begin
    if (reset) begin // @[Arbiter.scala 118:30]
      state_1_0 <= 1'h0;
    end else if (idle_1) begin
      state_1_0 <= earlyWinner_1_0;
    end
  end
  always @(posedge clock or posedge reset) begin
    if (reset) begin // @[Arbiter.scala 118:30]
      state_1_1 <= 1'h0;
    end else if (idle_1) begin
      state_1_1 <= earlyWinner_1_1;
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  beatsLeft_2 = _RAND_0[0:0];
  _RAND_1 = {1{`RANDOM}};
  readys_mask_2 = _RAND_1[1:0];
  _RAND_2 = {1{`RANDOM}};
  state_2_0 = _RAND_2[0:0];
  _RAND_3 = {1{`RANDOM}};
  state_2_1 = _RAND_3[0:0];
  _RAND_4 = {1{`RANDOM}};
  beatsLeft = _RAND_4[0:0];
  _RAND_5 = {1{`RANDOM}};
  readys_mask = _RAND_5[2:0];
  _RAND_6 = {1{`RANDOM}};
  state__0 = _RAND_6[0:0];
  _RAND_7 = {1{`RANDOM}};
  state__1 = _RAND_7[0:0];
  _RAND_8 = {1{`RANDOM}};
  state__2 = _RAND_8[0:0];
  _RAND_9 = {1{`RANDOM}};
  beatsLeft_1 = _RAND_9[0:0];
  _RAND_10 = {1{`RANDOM}};
  readys_mask_1 = _RAND_10[1:0];
  _RAND_11 = {1{`RANDOM}};
  state_1_0 = _RAND_11[0:0];
  _RAND_12 = {1{`RANDOM}};
  state_1_1 = _RAND_12[0:0];
`endif // RANDOMIZE_REG_INIT
  if (reset) begin
    beatsLeft_2 = 1'h0;
  end
  if (reset) begin
    readys_mask_2 = 2'h3;
  end
  if (reset) begin
    state_2_0 = 1'h0;
  end
  if (reset) begin
    state_2_1 = 1'h0;
  end
  if (reset) begin
    beatsLeft = 1'h0;
  end
  if (reset) begin
    readys_mask = 3'h7;
  end
  if (reset) begin
    state__0 = 1'h0;
  end
  if (reset) begin
    state__1 = 1'h0;
  end
  if (reset) begin
    state__2 = 1'h0;
  end
  if (reset) begin
    beatsLeft_1 = 1'h0;
  end
  if (reset) begin
    readys_mask_1 = 2'h3;
  end
  if (reset) begin
    state_1_0 = 1'h0;
  end
  if (reset) begin
    state_1_1 = 1'h0;
  end
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule

