// Copyright 2014 Breckin Loggins
//
// Emulates the Ricoh 6502 variant found in the NES

struct CPU {
  //
  // Hardware Registers
  //
  A:              u8,               // Accumulator Register
  X:              u8,               // X General Purpose Register
  Y:              u8,               // Y General Purpose Register
  SP:             u8,               // Stack Pointer Register
  S:              u8,               // CPU Status Register
  PC:             u16,              // Program Counter Register

}

enum CPUReg {
  A                   = 0b000,      // Accumulator
  X                   = 0b001,      // X Index
  Y                   = 0b010,      // Y Index
  SP                  = 0b011,      // Stack Pointer
  S                   = 0b100,      // Status
  PC                  = 0b101,      // Program Counter
  Invalid             = 0b110,      // Register N/A
}

struct AdrModeInfo {
  name:           ~str,             // Debugging name
  ins_width:      u8,               // Instruction width (bytes)
  zero_page:      bool,             // Whether this is a zero page-based mode
  reg:            CPUReg,           // Which register (if any) applies
  indexed:        bool,             // Whether this is an indexed mode
  relative:       bool,             // Whether this mode is relative
}

// Supported 6502 Addressing Modes 
enum AdrMode {
  Implied(AdrModeInfo),  
  ZeroPage(AdrModeInfo),
  IndexedZeroPage(AdrModeInfo),
  Absolute(AdrModeInfo),
  IndexedAbsolute(AdrModeInfo),
  Relative(AdrModeInfo),
  Accumulator(AdrModeInfo),
  IndirectX(AdrModeInfo),
  IndirectY(AdrModeInfo),
  Immediate(AdrModeInfo), 
}

// Opcode encoding format, as specified by
// http://www.llx.com/~nparker/a2/opcodes.html
enum OpEncoding {
  Direct(u8),         // Directly-encoded (like BRK=0x00)
  ABC(u8, u8, u8),    // aaabbbcc (like ORA, AND, etc)
  XY(u8, u8),         // xxy10000 (conditional branches such as BPL and BEQ)
}

// The "cc" part of the general aaabbbcc opcode format (not all instructions 
// explicitly decode in the format, but they can all be classified according
// to the least significant bottom two bits)
enum OpCC {
  OpCC00               = 0b00,       // BIT, JMP, STY, LDY, CPY, CPX, others...
  OpCC01               = 0b01,       // ORA, AND, EOR, ADC, STA, LDA, CMP, SBC
  OpCC10               = 0b10,       // ASL, ROL, LSR, ROR, STX, LDX, DEC, INC
  OpCC11               = 0b11,       // ILLEGAL INSTRUCTIONS
}

struct OpInfo {
  name:             ~str,           // Opcode name
  code:             OpEncoding,     // The opcode (in the appropriate format)
  mode:             AdrMode,        // OpCode addressing info 
  cycle_count:      u8,             // Number of cycles the opcode takes
  page_cycle:       bool            // Whether the opcode takes an additional
                                    // cycle when memory access crosses a page
                                    // boundary
}

impl CPU {

}