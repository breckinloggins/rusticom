// Copyright 2014 Breckin Loggins
//
// Emulates the Ricoh 6502 variant found in the NES
// http://nesdev.com/6502.txt

use mem::Mem;
mod mem;

pub struct CPU {
  //
  // Hardware Registers
  //
  A:                u8,             // Accumulator Register
  X:                u8,             // X General Purpose Register
  Y:                u8,             // Y General Purpose Register
  SP:               u16,            // Stack Pointer Register
  P:                u8,             // CPU Status Register
  PC:               u16,            // Program Counter Register

  priv cc:          uint,           // Current clock cycle (PPU master clock cycles 3x cpu)
  priv scanline:    int,            // Current scanline
}

// Opcode encoding format is specified by
// http://www.llx.com/~nparker/a2/opcodes.html
// http://www.masswerk.at/6502/6502_instruction_set.html

enum Op {
  ADC,               // Add Memory to Accumulator with Carry
  AND,               // "AND" Memory with Accumulator
  ASL,               // Shift Left One Bit (Memory or Accumulator)
  BCC,               // Branch on Carry Clear
  BCS,               // Branch on Carry Set
  BEQ,               // Branch on Result Zero
  BIT,               // Test Bits in Memory with Accumulator
  BMI,               // Branch on Result Minus
  BNE,               // Branch on Result not Zero
  BPL,               // Branch on Result Plus
  BRK,               // Force Break
  BVC,               // Branch on Overflow Clear
  BVS,               // Branch on Overflow Set
  CLC,               // Clear Carry Flag
  CLD,               // Clear Decimal Mode
  CLI,               // Clear interrupt Disable Bit
  CLV,               // Clear Overflow Flag
  CMP,               // Compare Memory and Accumulator
  CPX,               // Compare Memory and Index X
  CPY,               // Compare Memory and Index Y
  DEC,               // Decrement Memory by One
  DEX,               // Decrement Index X by One
  DEY,               // Decrement Index Y by One
  EOR,               // "Exclusive-Or" Memory with Accumulator
  INC,               // Increment Memory by One
  INX,               // Increment Index X by One
  INY,               // Increment Index Y by One
  JMP,               // Jump to New Location
  JSR,               // Jump to New Location Saving Return Address      
  LDA,               // Load Accumulator with Memory                 
  LDX,               // Load Index X with Memory                     
  LDY,               // Load Index Y with Memory                     
  LSR,               // Shift Right One Bit (Memory or Accumulator)  
  NOP,               // No Operation                                 
  ORA,               // "OR" Memory with Accumulator                 
  PHA,               // Push Accumulator on Stack                    
  PHP,               // Push Processor Status on Stack               
  PLA,               // Pull Accumulator from Stack                  
  PLP,               // Pull Processor Status from Stack             
  ROL,               // Rotate One Bit Left (Memory or Accumulator)  
  ROR,               // Rotate One Bit Right (Memory or Accumulator) 
  RTI,               // Return from Interrupt                        
  RTS,               // Return from Subroutine                       
  SBC,               // Subtract Memory from Accumulator with Borrow 
  SEC,               // Set Carry Flag                               
  SED,               // Set Decimal Mode                             
  SEI,               // Set Interrupt Disable Status                 
  STA,               // Store Accumulator in Memory                  
  STX,               // Store Index X in Memory                      
  STY,               // Store Index Y in Memory                      
  TAX,               // Transfer Accumulator to Index X              
  TAY,               // Transfer Accumulator to Index Y              
  TSX,               // Transfer Stack Pointer to Index X            
  TXA,               // Transfer Index X to Accumulator              
  TXS,               // Transfer Index X to Stack Pointer            
  TYA,               // Transfer Index Y to Accumulator
  KIL,               // Pseudo-op for invalid instructions            
}


enum AdrMode {
  A,
  Abs,
  //AbsX,
  //AbsY,
  Imm,
  Impl,
  //Ind,
  //XInd, // Wraps before deref!
  //IndY, // Wraps before deref!
  Rel,
  Zpg,
  //ZpgX, // wraps!
  //ZpgY  // wraps!
}

enum Reg {
  Ar,
  Xr,
  Yr,
  SPr,
  Pr,
  PCr
}

enum Flag {
  Cf = 0b00000001,            // Bit 1 - Carry
  Zf = 0b00000010,            // Bit 2 - Zero
  If = 0b00000100,            // Bit 3 - Interrupt Disable
  Df = 0b00001000,            // Bit 4 - Decimal - BCD encoding (settable but unsupported)
  Bf = 0b00010000,            // Bit 5 - Break
  Xf = 0b00100000,            // Bit 6 - Ignored (always 0)
  Vf = 0b01000000,            // Bit 7 - Overflow
  Nf = 0b10000000,            // Bit 8 - Negative (sign bit set)
}

// Cycles:
// Every memory access is 1 cycle
// 1 cycle to cross subroutine

// Quirks
// RMW double store
// Lost BRK after interrupt

impl AdrMode {
  fn to_str(&self, c: &CPU, m: &Mem, a1: u8, a2: u8) -> ~str {
    match self {
      &A      => ~"",
      &Abs    => format!("${:02X}{:02X}", a2, a1),
      &Imm    => format!("{}${:02X}", "#", a1),
      &Impl   => ~"",
      &Rel    => format!("${:04X}", c.rel_addr(a1)),
      &Zpg    => format!("${:02X} = {:02X}", a1, m.read(a1 as u16))   
    }
  }

  fn width(&self) -> u8 {
    match self {
      &A      => 1,
      &Abs    => 3,
      &Imm    => 2,
      &Impl   => 1,
      &Rel    => 2,
      &Zpg    => 2,
    }
  }
}

struct OpCode (
  /*code*/    u8,
  /*name*/    ~str,
  /*admode*/  AdrMode,
  /*cycles*/  u8,
  /*handler*/ 'static |c: &mut CPU, m: &mut Mem, a1: u8, a2: u8|
);

impl CPU {
  pub fn new() -> CPU { CPU { A: 0, X: 0, Y: 0, SP: 0, P: 0, PC: 0, cc: 0, scanline: 0 } }

  pub fn reset(&mut self, startAddr: u16) {
    self.PC = startAddr;
    self.SP = 0x10fd;           /* Top of stack (this should really be handled by RESET) */
    self.fset(Xf);              /* Unused flag is set to 1 at all times */
    self.fset(If);              /* Interrupts disabled on startup */
    self.scanline = 241;        /* Attempt to emulate nestest log */
  }

  pub fn tick(&mut self, mem: &mut Mem) {
    let c = mem.read(self.PC);
    let opc = CPU::decode_op(c);
    let op = match opc {
      ADC => OpCode(c, ~"ADC", Imm,  2, |c, _, a1, _ | c.alu_ADC(a1)),
      AND => OpCode(c, ~"AND", Imm,  2, |c, m, a1, _ | c.alu_AND(a1)),
      ASL => OpCode(c, ~"ASL", Impl, 2, |_, _, _ , _ | fail!("ASL not implemented")),
      BCC => OpCode(c, ~"BCC", Rel,  2, |c, _, a1, _ | if !c.fis_set(Cf) { c.b_rel(a1) }),
      BCS => OpCode(c, ~"BCS", Rel,  2, |c, _, a1, _ | if c.fis_set(Cf) { c.b_rel(a1) }),
      BEQ => OpCode(c, ~"BEQ", Rel,  2, |c, _, a1, _ | if c.fis_set(Zf) { c.b_rel(a1) }),
      BIT => OpCode(c, ~"BIT", Zpg,  3, |c, m, a1, _ | c.alu_BIT(m, a1)),
      BMI => OpCode(c, ~"BMI", Rel,  2, |c, m, a1, _ | if c.fis_set(Nf) { c.b_rel(a1) }),
      BNE => OpCode(c, ~"BNE", Rel,  2, |c, _, a1, _ | if !c.fis_set(Zf) { c.b_rel(a1) }),
      BPL => OpCode(c, ~"BPL", Rel,  2, |c, _, a1, _ | if !c.fis_set(Nf) { c.b_rel(a1) }),
      BRK => OpCode(c, ~"BRK", Impl, 7, |c, _, _ , _ | fail!("BRK not implemented")),
      BVC => OpCode(c, ~"BVC", Rel,  2, |c, _, a1, _ | if !c.fis_set(Vf) { c.b_rel(a1) }),
      BVS => OpCode(c, ~"BVS", Rel,  2, |c, _, a1, _ | if c.fis_set(Vf) { c.b_rel(a1) }),
      CLC => OpCode(c, ~"CLC", Impl, 2, |c, _, _ , _ | c.fclr(Cf)),
      CLD => OpCode(c, ~"CLD", Impl, 2, |c, _, _ , _ | c.fclr(Df)),
      CLI => OpCode(c, ~"CLI", Impl, 2, |c, _, _ , _ | c.fclr(If)),
      CLV => OpCode(c, ~"CLV", Impl, 2, |c, _, _ , _ | c.fclr(Vf)),
      CMP => OpCode(c, ~"CMP", Imm,  2, |c, _, a1, _ | c.alu_CMP(Ar, a1)),
      CPX => OpCode(c, ~"CPX", Imm,  2, |c, _, a1, _ | c.alu_CMP(Xr, a1)),
      CPY => OpCode(c, ~"CPY", Imm,  2, |c, _, a1, _ | c.alu_CMP(Yr, a1)),
      DEC => OpCode(c, ~"DEC", Zpg,  5, |_, _, _ , _ | fail!("DEC not implemented")),
      DEX => OpCode(c, ~"DEX", Impl, 2, |c, _, _ , _ | c.alu_DEr(Xr)),
      DEY => OpCode(c, ~"DEY", Impl, 2, |c, _, _ , _ | c.alu_DEr(Yr)),
      EOR => OpCode(c, ~"EOR", Imm,  2, |c, _, a1, _ | c.alu_EOR(a1)),
      INC => OpCode(c, ~"INC", Zpg,  5, |_, _, _ , _ | fail!("INC not implemented")),
      INX => OpCode(c, ~"INX", Impl, 2, |c, _, _ , _ | c.alu_INr(Xr)),
      INY => OpCode(c, ~"INY", Impl, 2, |c, _, _ , _ | c.alu_INr(Yr)),
      JMP => OpCode(c, ~"JMP", Abs,  3, |c, _, a1, a2| c.PC = a2 as u16 << 8 | a1 as u16),
      JSR => OpCode(c, ~"JSR", Abs,  6, |c, m, a1, a2| { c.push16(m, c.PC-1); c.PC = a2 as u16 << 8 | a1 as u16 }),
      LDA => OpCode(c, ~"LDA", Imm,  2, |c, _, a1, _ | c.setreg(Ar, a1)),
      LDX => OpCode(c, ~"LDX", Imm,  2, |c, _, a1, _ | c.setreg(Xr, a1)),
      LDY => OpCode(c, ~"LDY", Imm,  2, |c, _, a1, _ | c.setreg(Yr, a1)),
      LSR => OpCode(c, ~"LSR", Impl, 2, |_, _, _ , _ | fail!("LSR not implemented")),
      NOP => OpCode(c, ~"NOP", Impl, 2, |_, _, _ , _ | {} ),
      ORA => OpCode(c, ~"ORA", Imm,  2, |c, _, a1, _ | c.alu_ORA(a1)),
      PHA => OpCode(c, ~"PHA", Impl, 3, |c, m, _ , _ | c.push8(m, c.A)),
      PHP => OpCode(c, ~"PHP", Impl, 3, |c, m, _ , _ | c.push8(m, c.P)),
      PLA => OpCode(c, ~"PLA", Impl, 4, |c, m, _ , _ | { c.A = c.pull(m); c.fsetv(Nf, c.A); c.fsetv(Zf, c.A) }),
      PLP => OpCode(c, ~"PLP", Impl, 4, |c, m, _ , _ | { let src = c.pull(m); c.setreg(Pr, src) }),
      ROL => OpCode(c, ~"ROL", Impl, 2, |_, _, _ , _ | fail!("ROL not implemented")),
      ROR => OpCode(c, ~"ROR", Impl, 2, |_, _, _ , _ | fail!("ROR not implemented")),
      RTI => OpCode(c, ~"RTI", Impl, 6, |_, _, _ , _ | fail!("RTI not implemented")),
      RTS => OpCode(c, ~"RTS", Impl, 6, |c, m, _ , _ | c.PC = ((c.pull(m) as u16) | ((c.pull(m) as u16) << 8))+1),
      SBC => OpCode(c, ~"SBC", Imm,  2, |c, _, a1, _ | c.alu_SBC(a1)),
      SEC => OpCode(c, ~"SEC", Impl, 2, |c, _, _ , _ | c.fset(Cf)),
      SED => OpCode(c, ~"SED", Impl, 2, |c, _, _ , _ | c.fset(Df)),
      SEI => OpCode(c, ~"SEI", Impl, 2, |c, _, _ , _ | c.fset(If)),
      STA => OpCode(c, ~"STA", Zpg,  3, |c, m, a1, _ | m.write(a1 as u16, c.A)),
      STX => OpCode(c, ~"STX", Zpg,  3, |c, m, a1, _ | m.write(a1 as u16, c.X)),
      STY => OpCode(c, ~"STY", Zpg,  3, |c, m, a1, _ | m.write(a1 as u16, c.Y)),
      TAX => OpCode(c, ~"TAX", Impl, 2, |c, _, _ , _ | c.alu_Trr(Ar, Xr)),
      TAY => OpCode(c, ~"TAY", Impl, 2, |c, _, _ , _ | c.alu_Trr(Ar, Yr)),
      TSX => OpCode(c, ~"TSX", Impl, 2, |c, _, _ , _ | c.alu_Trr(SPr, Xr)),
      TXA => OpCode(c, ~"TXA", Impl, 2, |c, _, _ , _ | c.alu_Trr(Xr, Ar)),
      TXS => OpCode(c, ~"TXS", Impl, 2, |c, _, _ , _ | c.alu_Trr(Xr, SPr)),
      TYA => OpCode(c, ~"TYA", Impl, 2, |c, _, _ , _ | c.alu_Trr(Yr, Ar)),
      _    => fail!("unimplemented or illegal instruction 0x{:X} at 0x{:X}", c, self.PC)
    };

    let OpCode(opcode, name, adrmode, cycles, handler) = op;
    let width = adrmode.width();
    let (a1, a2) = match width {
      1 => (0, 0),
      2 => (mem.read(self.PC+1), 0),
      3 => (mem.read(self.PC+1), mem.read(self.PC+2)),
      x => fail!("illegal opcode instruction width {}", x)
    };

    let prev_pc = self.PC;
    self.PC += width as u16;            // handler may set PC after this

    print!("{:04X}  {:02X} {} {}  {} {:28s}", 
      prev_pc, 
      opcode, 
      if width < 2 { ~"  " } else { format!("{:02X}", a1) }, 
      if width < 3 { ~"  " } else { format!("{:02X}", a2) }, 
      name, 
      adrmode.to_str(self, mem, a1, a2));
    println!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} CYC:{:3u} SL:{:3i}", 
      self.A, self.X, self.Y, self.P, self.SP & 0x00ff, self.cc, self.scanline);

    handler(self, mem, a1, a2);
    self.add_cycles(cycles);
    //self.scanline = (self.cc / 241) as int - 21;
  }

  // 
  // Decode
  //
  /*
  fn decode(c: u8) -> OpCode {
    let op = CPU::decode_op(c);

  }
  */
  fn decode_op(c: u8) -> Op {
    
    // Try directly-encoded opcodes first
    let mut op = CPU::decode_op_direct(c);
    if !CPU::op_is_KIL(op) {
      return op;
    }
    
    // Conditional have a special encoding
    op = CPU::decode_op_branch(c);
    if !CPU::op_is_KIL(op) {
      return op;
    }

    // The rest of the opcodes break down into aaabbbcc 
    // format
    let aaa = (c & 0xE0) >> 5;
    //let bbb = (c & 0x1C) >> 2;
    let cc = c & 0x3;
    op = match cc {
      0b00 => CPU::decode_op_00(aaa),
      0b01 => CPU::decode_op_01(aaa),
      0b10 => CPU::decode_op_10(aaa),
      _    => KIL
    };

    return op;
  }

  fn decode_op_direct(c: u8) -> Op {
    match c {
      0x00 => BRK,
      0x08 => PHP,
      0x18 => CLC,
      0x20 => JSR,
      0x28 => PLP,
      0x38 => SEC,
      0x40 => RTI,
      0x48 => PHA,
      0x58 => CLI,
      0x60 => RTS,
      0x68 => PLA,
      0x78 => SEI,
      0x88 => DEY,
      0x8A => TXA,
      0x98 => TYA,
      0x9A => TXS,
      0xA8 => TAY,
      0xAA => TAX,
      0xB8 => CLV,
      0xBA => TSX,
      0xC8 => INY,
      0xCA => DEX,
      0xD8 => CLD,
      0xE8 => INX,
      0xEA => NOP,
      0xF8 => SED,
      _    => KIL
    }
  }

  fn decode_op_branch(c: u8) -> Op {
    match c {
      0x10 => BPL,
      0x30 => BMI,
      0x50 => BVC,
      0x70 => BVS,
      0x90 => BCC,
      0xB0 => BCS,
      0xD0 => BNE,
      0xF0 => BEQ,
      _    => KIL
    }
  }

  fn decode_op_00(aaa: u8) -> Op {
    match aaa {
      0b001 => BIT,
      0b010 => JMP, 
      0b011 => JMP,
      0b100 => STY,
      0b101 => LDY,
      0b110 => CPY,
      0b111 => CPX,
      _     => KIL
    }
  }

  fn decode_op_01(aaa: u8) -> Op {
    match aaa {
      0b000 => ORA,
      0b001 => AND,
      0b010 => EOR,
      0b011 => ADC,
      0b100 => STA,
      0b101 => LDA,
      0b110 => CMP,
      0b111 => SBC,
      _     => KIL
    }
  }

  fn decode_op_10(aaa: u8) -> Op {
    match aaa {
      0b000 => ASL,
      0b001 => ROL,
      0b010 => LSR,
      0b011 => ROR, 
      0b100 => STX,
      0b101 => LDX,
      0b110 => DEC,
      0b111 => INC,
      _     => KIL
    }
  }

  fn op_is_KIL(op: Op) -> bool {
    match op {
      KIL => true,
      _   => false
    }
  }

  //
  // Clock
  //
  fn add_cycles(&mut self, cpu_cycles: u8) {
    // Convert to master clock (PPU) cycles
    let prev_cc = self.cc;
    self.cc = (self.cc + (cpu_cycles as uint) * 3) % 341;
    if self.cc < prev_cc {
      // New scanline
      self.scanline += 1;
      if self.scanline == 261 { self.scanline = -1 };
    }
  }

  //
  // Regs
  //
  fn setreg(&mut self, r: Reg, val: u8) {
    let set_flags = match r {
      Ar  => { self.A = val; true },
      Xr  => { self.X = val; true },
      Yr  => { self.Y = val; true },
      Pr  => { self.P = val; false },
      SPr => { self.SP = val as u16; false },
      PCr => fail!("cannot set PC with setreg")
    };

    if set_flags {
      self.fsetv(Nf, val);
      self.fsetv(Zf, val);
    }
  }

  fn getreg(&self, r: Reg) -> u8 {
    match r {
      Ar  => self.A, 
      Xr  => self.X,
      Yr  => self.Y,
      Pr  => self.P,
      SPr => self.SP as u8,
      PCr => fail!("PC register is too wide to retrieve with getreg()")
    }
  }

  //
  // Stack
  //
  fn pull(&mut self, mem: &mut Mem) -> u8 {
    self.SP += 1;
    
    if self.SP == 0x1100 {
      fail!("stack underflow");
    }

    let val = mem.read(self.SP);
    return val;
  }

  // TODO: Templatize based on width of type
  fn push8(&mut self, mem: &mut Mem, val: u8) {
    if self.SP == 0x00ff {
      fail!("stack overflow");
    }

    mem.write(self.SP, val);
    self.SP -= 1;
  }

  // TODO: Templatize based on width of type
  fn push16(&mut self, mem: &mut Mem, val: u16) {
    self.push8(mem, ((val & 0xff00) >> 8) as u8);
    self.push8(mem, (val & 0x00ff) as u8);
  }

  //
  // Flags
  //

  fn fset(&mut self, f: Flag) {
    self.P |= f as u8;
  }

  fn fsetv(&mut self, f: Flag, val: u8) {
    match f {
      Nf => if val & 0x80 == 0    { self.fclr(Nf) } else { self.fset(Nf) },
      Zf => if val == 0           { self.fset(Zf) } else { self.fclr(Zf) },
      _  => fail!("Invalid flag {} sent to fsetv", f as u8)
    }
  }

  fn fclr(&mut self, f: Flag) {
    self.P &= !(f as u8);
  }

  fn fis_set(&self, f: Flag) -> bool {
    (self.P & f as u8) != 0
  }

  //
  // Addressing
  //
  fn rel_addr(&self, off: u8) -> u16 { 
    if off & 0x80 != 0 {
      self.PC - ((off & 0x7F) as u16)
    } else {
      self.PC + (off as u16) 
    }
  }

  // TODO: We need CPU-specific memory lookup and read-write ops. They need to
  // return whether we took an extra cycle if crossing a page boundary.

  //
  // Branch
  //
  fn b_rel(&mut self, off: u8) {
    let page = self.PC & 0xff00;
    let addr = self.rel_addr(off);
    self.PC = addr;
    self.add_cycles(if page == self.PC & 0xff00 {
      // Same page, only took one extra cycle
      1
    } else {
      // Crossed page boundary, took two cycles
      2
    });
  }

  //
  // ALU
  //

  fn alu_BIT(&mut self, m: &mut Mem, a1: u8)  {
    let mval = m.read(a1 as u16);
    let res = mval & self.A;

    self.fsetv(Zf, res);
    self.fsetv(Nf, mval);
    if mval & 0x40 != 0 {
      self.fset(Vf);
    } else {
      self.fclr(Vf);
    }
  }

  fn alu_AND(&mut self, a1: u8)   {
    let res = a1 & self.A;
    self.fsetv(Zf, res);
    self.fsetv(Nf, res);

    self.A = res;
  }

  fn alu_CMP(&mut self, r: Reg, a1: u8)   {
    let res: u16 = self.getreg(r) as u16 - a1 as u16;

    self.fsetv(Zf, res as u8);
    self.fsetv(Nf, res as u8);
    if res < 0x100 { self.fset(Cf) }
  }

  fn alu_ORA(&mut self, a1: u8)   {
    let res = a1 | self.A;
    self.fsetv(Zf, res);
    self.fsetv(Nf, res);

    self.A = res;
  }

  fn alu_EOR(&mut self, a1: u8)   {
    let res = a1 ^ self.A;
    self.fsetv(Zf, res);
    self.fsetv(Nf, res);

    self.A = res;
  }

  fn alu_INr(&mut self, r: Reg)   {
    let res = self.getreg(r) + 1;
    self.fsetv(Zf, res);
    self.fsetv(Nf, res);

    self.setreg(r, res);
  }

  fn alu_DEr(&mut self, r: Reg)   {
    let res = self.getreg(r) - 1;
    self.fsetv(Zf, res);
    self.fsetv(Nf, res);

    self.setreg(r, res);
  }

  fn alu_ADC(&mut self, a1: u8)   {
    let res: u16 = a1 as u16 + self.A as u16 + if self.fis_set(Cf) { 1 } else { 0 };
    self.fsetv(Zf, res as u8);
    self.fsetv(Nf, res as u8);
    if !((self.A ^ a1) & 0x80 > 0 && (self.A ^ res as u8) & 0x80 > 0) { self.fset(Vf) };
    if res > 0xff { self.fset(Cf) };
    self.A = res as u8;
  }

  fn alu_SBC(&mut self, a1: u8)   {
    let res: u16 = self.A as u16 - a1 as u16 - if self.fis_set(Cf) { 0 } else { 1 };
    self.fsetv(Zf, res as u8);
    self.fsetv(Nf, res as u8);
    if ((self.A ^ a1) & 0x80 > 0 && (self.A ^ res as u8) & 0x80 > 0) { self.fset(Vf) };
    if res < 0x100 { self.fset(Cf) };
    self.A = res as u8;
  }

  fn alu_Trr(&mut self, r_src: Reg, r_dst: Reg) {
    let res = self.getreg(r_src);
    if match r_dst {
      SPr => false,
      _   => true
    } {
      self.fsetv(Zf, res);
      self.fsetv(Nf, res);
    }

    self.setreg(r_dst, res);
  }

}