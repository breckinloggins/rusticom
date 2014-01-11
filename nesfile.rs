// Copyright 2014 Breckin Loggins
//
// Reads the iNES file format
use std::str;
use std::path::Path;
use std::io::{File, fs};

// iNES ROM format: http://wiki.nesdev.com/w/index.php/INES
pub struct Header {
  magic:          [u8, ..4],        // Should be "NES" + 0x1A
  prg_size:       u8,               // PRG ROM size in 16KB units
  chr_size:       u8,               // CHR ROM size in 8KB units (or 0)
  flags6:         u8,               // Flags 6 (see documentation)
  flags7:         u8,               // Flags 7 (see documentation)
  prg_ram_size:   u8,               // PRG RAM size in 8KB units (or 0 for 8KB)
  flags9:         u8,               // Flags 9 (see documentation)
  flags10:        u8,               // Flags 10 (see documentation)
  padding:        [u8, ..5]         // Zero padding
}

pub struct ROM {
	header:			Header
}

enum NESFlags6 {
  MirroringMask       = 0b00001001,
  BatteryMask         = 0b00000010,
  TrainerMask         = 0b00000100,
  LowerMapperMask     = 0b11110000
}

enum NESFlags7 {
  VSUnisystemMask     = 0b00000001,
  Playchoice10Mask    = 0b00000010,
  NES20Mask           = 0b00001100,
  UpperMapperMask     = 0b11110000
}

enum NESFlags9 {
  TVSystemMask        = 0b00000001,
  ReservedMask        = 0b11111110
}

impl ROM {
	// Reads a file into memory, creating a ROM object
	pub fn open(filename : &str) -> Result<ROM, ~str> {
		let rom_path = Path::new(filename);
  
		if rom_path.is_file() {
		    println!("ROM found, loading...");
		    println!("Size is {} bytes", fs::stat(&rom_path).size);
		  
		    let mut rom_file = File::open(&rom_path);
		    let rom_bytes = rom_file.read_to_end(); 

		    let header = read_header(rom_bytes);

    		return match validate_header(&header) {
    			Ok(_) => Ok(ROM { header: header }),
    			Err(s) => Err(s)
    		};
  		} else {
    		return Err(format!("{} doesn't look like a ROM file", filename));
    	}
  	}
}

fn read_header(rom : &[u8]) -> Header {
    // TODO: What's the rust way of doing this? I assume doing a copy from
    // an unsafe raw byte buffer?
    return Header {
      magic:        [rom[0], rom[1], rom[2], rom[3]],
      prg_size:     rom[4],
      chr_size:     rom[5],
      flags6:       rom[6],
      flags7:       rom[7],
      prg_ram_size: if rom[8] == 0 { 1 } else { rom[8] }, // Compatibility
      flags9:       rom[9],
      flags10:      rom[10],
      padding:      [rom[11], rom[12], rom[13], rom[14], rom[15]]
    };
}

fn validate_header(header : &Header) -> Result<~str, ~str> {
    let magic_str = str::from_utf8(header.magic);
    if magic_str != "NES\x1a" || header.padding != [0, 0, 0, 0, 0] {
      fail!("File does not appear to be a valid iNES ROM");
    }

    println!("Valid iNES ROM found");
    println!("PRG ROM 16KB Pages: {}", header.prg_size);
    println!("CHR ROM 8KB Pages: {}", header.chr_size);
    println!("PRG RAM 8KB Pages: {}", header.prg_ram_size);

    if header.flags6 & TrainerMask as u8 == TrainerMask as u8 {
      return Err(~"ROMs with Trainer Sections are not supported");
    }

    if header.flags6 & BatteryMask as u8 == BatteryMask as u8 {
      println!("ROM supports a battery");
    }

    let mapper = (header.flags7 & UpperMapperMask as u8) | (header.flags6 & LowerMapperMask as u8);
    println!("Mapper is {}", mapper);
    if mapper > 0 {
      return Err(format!("rusticom does not support mapper {}", mapper));
    }

    let ines_version = (header.flags7 & NES20Mask as u8) >> 2;
    println!("iNES Version: {}", ines_version);
    if ines_version > 0 {
      return Err(~"rusticom does not support this iNES cartridge version");
    }
    
    let mirroring = header.flags6 & MirroringMask as u8;
    if mirroring == 0 {
      println!("ROM is horizontally mirrored");
    } else if mirroring == 1 {
      println!("ROM is vertically mirrored");
    } else {
      return Err(format!("rusticom does not support four-screen mirroring: {}", mirroring));
    }

    return Ok(~"ok");
}

