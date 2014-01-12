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
  A:              	u8,             // Accumulator Register
  X:              	u8,             // X General Purpose Register
  Y:              	u8,             // Y General Purpose Register
  SP:             	u16,            // Stack Pointer Register
  P:              	u8,             // CPU Status Register
  PC:             	u16,            // Program Counter Register

  priv cc:			uint,			// Current clock cycle (PPU master clock cycles 3x cpu)
}

// Opcode encoding format is specified by
// http://www.llx.com/~nparker/a2/opcodes.html
// http://www.masswerk.at/6502/6502_instruction_set.html

enum AdrMode {
	A,
	Abs,
	//AbsX,
	//AbsY,
	Imm,
	Impl,
	//Ind,
	//XInd,	// Wraps before deref!
	//IndY,	// Wraps before deref!
	Rel,
	Zpg,
	//ZpgX,	// wraps!
	//ZpgY	// wraps!
}

enum Flag {
	Cf = 0b00000001,			// Bit 1 - Carry
	Zf = 0b00000010,			// Bit 2 - Zero
	If = 0b00000100,			// Bit 3 - Interrupt Disable
	Df = 0b00001000,			// Bit 4 - Decimal - BCD encoding (settable but unsupported)
	Bf = 0b00010000,			// Bit 5 - Break
	Xf = 0b00100000,			// Bit 6 - Ignored (always 0)
	Vf = 0b01000000,			// Bit 7 - Overflow
	Nf = 0b10000000,			// Bit 8 - Negative (sign bit set)
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
			&A 		=> ~"",
			&Abs 	=> format!("${:02X}{:02X}", a2, a1),
			&Imm 	=> format!("{}${:02X}", "#", a1),
			&Impl  	=> ~"",
			&Rel 	=> format!("${:04X}", c.rel_addr(a1)),
			&Zpg	=> format!("${:02X} = {:02X}", a1, m.read(a1 as u16))	
		}
	}

	fn width(&self) -> u8 {
		match self {
			&A 		=> 1,
			&Abs 	=> 3,
			&Imm 	=> 2,
			&Impl 	=> 1,
			&Rel    => 2,
			&Zpg 	=> 2,
		}
	}
}

struct OpCode (
	/*code*/ 	u8,
	/*name*/ 	~str,
	/*admode*/ 	AdrMode,
	/*cycles*/ 	u8,
	/*handler*/ 'static |c: &mut CPU, m: &mut Mem, a1: u8, a2: u8|
);

impl CPU {
	pub fn new() -> CPU { CPU { A: 0, X: 0, Y: 0, SP: 0, P: 0, PC: 0, cc: 0 } }

	pub fn run(&mut self, mem: &mut Mem, startAddr: u16) {
		self.PC = startAddr;
		self.SP = 0x10fd;			/* Top of stack (this should really be handled by RESET) */
		self.fset(Xf);				/* Unused flag is set to 1 at all times */
		self.fset(If);				/* Interrupts disabled on startup */

		let mut scanline : int = 0;		/* TODO: Match nestest.log's starting scanline */
		loop {
			let op = match mem.read(self.PC) {
				0x08 => OpCode(0x08, ~"PHP", Impl, 3, |c, m, _ , _ | c.push8(m, c.P)),
				0x18 => OpCode(0x18, ~"CLC", Impl, 2, |c, _, _ , _ | c.fclr(Cf)),
				0x20 => OpCode(0x20, ~"JSR", Abs,  6, |c, m, a1, a2| { c.push16(m, c.PC); c.PC = a2 as u16 << 8 | a1 as u16 }),
				0x28 => OpCode(0x28, ~"PLP", Impl, 4, |c, m, _ , _ | c.P = c.pull(m)),
				0x38 => OpCode(0x38, ~"SEC", Impl, 2, |c, _, _ , _ | c.fset(Cf)),
				0x48 => OpCode(0x48, ~"PHA", Impl, 3, |c, m, _ , _ | c.push8(m, c.A)),
				0x58 => OpCode(0x58, ~"CLI", Impl, 2, |c, _, _ , _ | c.fclr(If)),
				0x4C => OpCode(0x4C, ~"JMP", Abs,  3, |c, _, a1, a2| c.PC = a2 as u16 << 8 | a1 as u16),
				0x68 => OpCode(0x68, ~"PLA", Impl, 4, |c, m, _ , _ | c.A = c.pull(m)),
				0x78 => OpCode(0x78, ~"SEI", Impl, 2, |c, _, _ , _ | c.fset(If)),
				0x84 => OpCode(0x84, ~"STY", Zpg,  3, |c, m, a1, _ | c.Y = m.read(a1 as u16)),
				0x85 => OpCode(0x85, ~"STA", Zpg,  3, |c, m, a1, _ | c.A = m.read(a1 as u16)),
				0x86 => OpCode(0x86, ~"STX", Zpg,  3, |c, m, a1, _ | c.X = m.read(a1 as u16)),
				0x90 => OpCode(0x90, ~"BCC", Rel,  2, |c, _, a1, _ | if !c.fis_set(Cf) { c.b_rel(a1) }),
				0xA0 => OpCode(0xA0, ~"LDY", Imm,  2, |c, _, a1, _ | c.Y = a1),
				0xA2 => OpCode(0xA2, ~"LDX", Imm,  2, |c, _, a1, _ | c.X = a1),
				0xA9 => OpCode(0xA9, ~"LDA", Imm,  2, |c, _, a1, _ | c.A = a1),
				0xB0 => OpCode(0xB0, ~"BCS", Rel,  2, |c, _, a1, _ | if c.fis_set(Cf) { c.b_rel(a1) }),
				0xB8 => OpCode(0xB8, ~"CLV", Impl, 2, |c, _, _ , _ | c.fclr(Vf)),
				0xD0 => OpCode(0xD0, ~"BNE", Rel,  2, |c, _, a1, _ | if !c.fis_set(Zf) { c.b_rel(a1) }),
				0xD8 => OpCode(0xD8, ~"CLD", Impl, 2, |c, _, _ , _ | c.fclr(Df)),
				0xEA => OpCode(0xEA, ~"NOP", Impl, 2, |_, _, _ , _ | {} ),
				0xF0 => OpCode(0xF0, ~"BEQ", Rel,  2, |c, _, a1, _ | if c.fis_set(Zf) { c.b_rel(a1) }),
				0xF8 => OpCode(0xF8, ~"SED", Impl, 2, |c, _, _ , _ | c.fset(Df)),
				x    => fail!("unimplemented or illegal instruction 0x{:X}", x)
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
			self.PC += width as u16;			// handler may set PC after this

			print!("{:04X}  {:02X} {} {}  {} {:28s}", 
				prev_pc, 
				opcode, 
				if width < 2 { ~"  " } else { format!("{:02X}", a1) }, 
				if width < 3 { ~"  " } else { format!("{:02X}", a2) }, 
				name, 
				adrmode.to_str(self, mem, a1, a2));
			println!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} CYC:{:3u} SL:{:3i}", 
				self.A, self.X, self.Y, self.P, self.SP & 0x00ff, self.cc, scanline);

			handler(self, mem, a1, a2);
			self.add_cycles(cycles);
			scanline = (self.cc / 241) as int - 21;
		}
	}

	//
	// Clock
	//
	fn add_cycles(&mut self, cpu_cycles: u8) {
		// Convert to master clock (PPU) cycles
		self.cc = (self.cc + (cpu_cycles as uint) * 3) % 341;
	}

	//
	// Stack
	//

	fn pull(&mut self, mem: &mut Mem) -> u8 {
		if self.SP == 0x1100 {
			fail!("stack underflow");
		}

		let val = mem.read(self.SP);
		self.SP += 1;
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
		self.push8(mem, (val & 0x0f) as u8);
		self.push8(mem, ((val & 0xf0) >> 8) as u8);
	}

	//
	// Flags
	//

	fn fset(&mut self, f: Flag) {
		self.P |= f as u8;
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
		if (off & 0x80 != 0) {
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
				// Crosse page boundary, took two cycles
				2
			});
	}
}