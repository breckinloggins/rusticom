// Copyright 2014 Breckin Loggins
//
// Emulates the Ricoh 6502 variant found in the NES

use mem::Mem;
mod mem;

pub struct CPU {
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

// Opcode encoding format is specified by
// http://www.llx.com/~nparker/a2/opcodes.html
// http://www.masswerk.at/6502/6502_instruction_set.html

enum AdrMode {
	A,
	Abs,
	//AbsX,
	//AbsY,
	Imm,
	//Impl,
	//Ind,
	//XInd,
	//IndY,
	//Rel,
	Zpg,
	//ZpgX,
	//ZpgY
}

impl AdrMode {
	fn to_str(&self, m: &Mem, a1: u8, a2: u8) -> ~str {
		match self {
			&A 		=> ~"",
			&Abs 	=> format!("${:02X}{:02X}", a2, a1),
			&Imm 	=> format!("{}${:02X}", "#", a1),
			&Zpg	=> format!("${:02X} = {:02X}", a1, m.read(a1 as u16))	
		}
	}

	fn width(&self) -> u8 {
		match self {
			&A 		=> 1,
			&Abs 	=> 3,
			&Imm 	=> 2,
			&Zpg 	=> 2,
		}
	}
}

struct OpCode (
	/*code*/ 	u8,
	/*name*/ 	~str,
	/*admode*/ 	AdrMode,
	/*handler*/ 'static |c: &mut CPU, m: &mut Mem, a1: u8, a2: u8|
);

impl CPU {
	pub fn new() -> CPU { CPU { A: 0, X: 0, Y: 0, SP: 0, S: 0, PC: 0 } }

	pub fn run(&mut self, mem: &mut Mem, startAddr: u16) {
		self.PC = startAddr;
		self.SP = 0xfd;			/* Top of stack */

		let mut counter = 0;
		loop {
			//let ins = Instruction::decode(mem, self.PC);
			let op = match mem.read(self.PC) {
				0x4C => OpCode(0x4C, ~"JMP", Abs, |c, _, a1, a2| c.PC = a2 as u16 << 8 | a1 as u16),
				0x86 => OpCode(0x86, ~"STX", Zpg, |c, m, a1, _ | c.X = m.read(a1 as u16)),
				0xA2 => OpCode(0xA2, ~"LDX", Imm, |c, _, a1, _ | c.X = a1),
				x    => fail!("unimplemented or illegal instruction 0x{:X}", x)
			};

			let OpCode(opcode, name, adrmode, handler) = op;
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
			println!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}", 
				self.A, self.X, self.Y, self.S, self.SP);

			self.PC += width as u16;	// handler may set PC after this
			handler(self, mem, a1, a2);
			
			if (self.PC as uint) >= mem.capacity - 1 || counter > 20 {
				break;
			}
			counter += 1;
		}
	}
}