use std::path::Path;
use std::rt::io::file::stat;
use std::rt::io::io_error;

// iNES ROM format: http://wiki.nesdev.com/w/index.php/INES
struct NESHeader {
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

fn main() {
  println("rusticom v0.1");
  
  let args: ~[~str] = ::std::os::args();
  if args.len() != 2 {
    println("usage: rusticom <path_to_rom_file>");
    return
  }

  /*
   * Load the ROM
   */
  let rom_path = &Path(args[1]);

  do io_error::cond.trap(|_| {
    println("ROM file not found");
  }).inside {
    let info = match stat(rom_path) {
      Some(s) => s,
      None => fail!("unknown error stat-ing ROM path")
    };

    if info.is_file {
      println("ROM found, loading...");
      println(fmt!("Size is %s bytes", info.size.to_str()));
    
      let rom = match std::io::read_whole_file(rom_path)  {
        Ok(r) => r,
        Err(s) => fail!("can't read rom: %s", s)
      };

      // TODO: What's the rust way of doing this? I assume doing a copy from
      // an unsafe raw byte buffer?
      let header = NESHeader {
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

      let magic_str = std::str::from_utf8_slice(header.magic);
      if magic_str != "NES\x1a" || header.padding != [0, 0, 0, 0, 0] {
        fail!("File does not appear to be a valid iNES ROM");
      }

      println("Valid iNES ROM found");
      println(fmt!("PRG ROM 16KB Pages: %s", header.prg_size.to_str()));
      println(fmt!("CHR ROM 8KB Pages: %s", header.chr_size.to_str()));
      println(fmt!("PRG RAM 8KB Pages: %s", header.prg_ram_size.to_str()));

      if header.flags6 & TrainerMask as u8 == TrainerMask as u8 {
        fail!("ROMs with Trainer Sections are not supported");
      }

      if header.flags6 & BatteryMask as u8 == BatteryMask as u8 {
        println("ROM supports a battery");
      }

      let mapper = (header.flags7 & UpperMapperMask as u8) | (header.flags6 & LowerMapperMask as u8);
      println(fmt!("Mapper is %s", mapper.to_str()));
      if mapper > 0 {
        fail!("rusticom does not support mapper %s", mapper.to_str());
      }

      let ines_version = (header.flags7 & NES20Mask as u8) >> 2;
      println(fmt!("iNES Version: %s", ines_version.to_str()));
      if ines_version > 0 {
        fail!("rusticom does not support this iNES cartridge version");
      }
      
      let mirroring = header.flags6 & MirroringMask as u8;
      if mirroring == 0 {
        println("ROM is horizontally mirrored");
      } else if mirroring == 1 {
        println("ROM is vertically mirrored");
      } else {
        fail!("rusticom does not support four-screen mirroring: %s", mirroring.to_str());
      }


    } else {
      println("Path does not look like a ROM file");
    }
  }

}
