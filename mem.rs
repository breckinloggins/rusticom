// Copyright 2014 Breckin Loggins
//
// NES Memory

use std::vec;

pub struct Mem {
	capacity: 		uint,
	priv bytes:		~[u8]
}

impl Mem {
	pub fn new(capacity: uint) -> Mem { 
		let mut m = Mem { 
			capacity: capacity,
			bytes: vec::with_capacity(capacity as uint) 
		};

		unsafe { m.bytes.set_len(capacity) };
		return m;
	}

	pub fn read(self, addr: u16) -> u8 {
		if addr as uint > self.capacity - 1 {
			fail!("memory fault: attempt to read at 0x{:X}", addr);
		}
		return self.bytes[addr];
	}

	pub fn write(mut self, addr: u16, val: u8) {
		if addr as uint > self.capacity - 1 {
			fail!("memory fault: attempt to write at 0x{:X}", addr);
		}

		self.bytes[addr] = val;
	}

	pub fn fill(mut self, addr: u16, src: &[u8]) {
		if (addr as uint) + src.len() > self.capacity {
			fail!("memory fault: attempt to fill {} bytes past memory bounds", src.len())
		}

		let portion = self.bytes.mut_slice_from(addr as uint);
		unsafe { portion.copy_memory(src) };
	}
}