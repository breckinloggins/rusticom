// Copyright 2014 Breckin Loggins
//
// Emulates the Ricoh 6502 variant found in the NES

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

  priv cc:			uint,			// Current clock cycle
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
	//Rel,
	Zpg,
	//ZpgX,	// wraps!
	//ZpgY	// wraps!
}

// Cycles:
// Every memory access is 1 cycle
// 1 cycle to cross subroutine

// Quirks
// RMW double store
// Lost BRK after interrupt

impl AdrMode {
	fn to_str(&self, m: &Mem, a1: u8, a2: u8) -> ~str {
		match self {
			&A 		=> ~"",
			&Abs 	=> format!("${:02X}{:02X}", a2, a1),
			&Imm 	=> format!("{}${:02X}", "#", a1),
			&Impl  	=> ~"",
			&Zpg	=> format!("${:02X} = {:02X}", a1, m.read(a1 as u16))	
		}
	}

	fn width(&self) -> u8 {
		match self {
			&A 		=> 1,
			&Abs 	=> 3,
			&Imm 	=> 2,
			&Impl 	=> 1,
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

		let mut scanline : int = 0;		/* TODO: Match nestest.log's starting scanline */
		loop {
			let op = match mem.read(self.PC) {
				0x08 => OpCode(0x08, ~"PHP", Impl, 3, |c, m, _ , _ | c.push8(m, c.P)),
				0x20 => OpCode(0x20, ~"JSR", Abs,  6, |c, m, a1, a2| { c.push16(m, c.PC); c.PC = a2 as u16 << 8 | a1 as u16 }),
				0x28 => OpCode(0x28, ~"PLP", Impl, 4, |c, m, _ , _ | c.P = c.pull(m)),
				0x48 => OpCode(0x48, ~"PHA", Impl, 3, |c, m, _ , _ | c.push8(m, c.A)),
				0x4C => OpCode(0x4C, ~"JMP", Abs,  3, |c, _, a1, a2| c.PC = a2 as u16 << 8 | a1 as u16),
				0x68 => OpCode(0x68, ~"PLA", Impl, 4, |c, m, _ , _ | c.A = c.pull(m)),
				0x86 => OpCode(0x86, ~"STX", Zpg,  3, |c, m, a1, _ | c.X = m.read(a1 as u16)),
				0xA2 => OpCode(0xA2, ~"LDX", Imm,  2, |c, _, a1, _ | c.X = a1),
				0xEA => OpCode(0xEA, ~"NOP", Impl, 2, |_, _, _ , _ | {} ),
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

			print!("{:04X}  {:02X} {} {}  {} {:28s}", 
				self.PC, 
				opcode, 
				if width < 2 { ~"  " } else { format!("{:02X}", a1) }, 
				if width < 3 { ~"  " } else { format!("{:02X}", a2) }, 
				name, 
				adrmode.to_str(mem, a1, a2));
			println!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} CYC:{:3u} SL:{:3i}", 
				self.A, self.X, self.Y, self.P, self.SP & 0x00ff, self.cc, scanline);

			self.PC += width as u16;			// handler may set PC after this
			handler(self, mem, a1, a2);
			self.cc = (self.cc + (cycles as uint) * 3) % 341;	// Show master clock (PPU) cycles
			scanline = (self.cc / 241) as int - 21;
		}
	}

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

	// TODO: We need CPU-specific memory lookup and read-write ops. They need to
	// return whether we took an extra cycle if crossing a page boundary.


}