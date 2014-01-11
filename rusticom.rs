use std::path::Path;
use std::io::File;

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

struct NES {
  cpu_mem:        [u8, ..0x4000],   // System RAM [TODO: Virtualize]
  //ppu_mem:        [u8, ..0x4000],   // PPU RAM [TODO: Virtualize]
}

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

enum MicroOp {
  Add,
  Mov,
  Jmp,
}

fn read_header(rom : &[u8]) -> ~NESHeader {
    // TODO: What's the rust way of doing this? I assume doing a copy from
    // an unsafe raw byte buffer?
    return ~NESHeader {
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

fn validate_header(header : &NESHeader) {
    let magic_str = std::str::from_utf8(header.magic);
    if magic_str != "NES\x1a" || header.padding != [0, 0, 0, 0, 0] {
      fail!("File does not appear to be a valid iNES ROM");
    }

    println!("Valid iNES ROM found");
    println!("PRG ROM 16KB Pages: {}", header.prg_size);
    println!("CHR ROM 8KB Pages: {}", header.chr_size);
    println!("PRG RAM 8KB Pages: {}", header.prg_ram_size);

    if header.flags6 & TrainerMask as u8 == TrainerMask as u8 {
      fail!("ROMs with Trainer Sections are not supported");
    }

    if header.flags6 & BatteryMask as u8 == BatteryMask as u8 {
      println!("ROM supports a battery");
    }

    let mapper = (header.flags7 & UpperMapperMask as u8) | (header.flags6 & LowerMapperMask as u8);
    println!("Mapper is {}", mapper);
    if mapper > 0 {
      fail!("rusticom does not support mapper {}", mapper);
    }

    let ines_version = (header.flags7 & NES20Mask as u8) >> 2;
    println!("iNES Version: {}", ines_version);
    if ines_version > 0 {
      fail!("rusticom does not support this iNES cartridge version");
    }
    
    let mirroring = header.flags6 & MirroringMask as u8;
    if mirroring == 0 {
      println!("ROM is horizontally mirrored");
    } else if mirroring == 1 {
      println!("ROM is vertically mirrored");
    } else {
      fail!("rusticom does not support four-screen mirroring: {}", mirroring);
    }
}

fn main() {
  println!("rusticom v0.1");
  
  let args: ~[~str] = ::std::os::args();
  if args.len() != 2 {
    println!("usage: rusticom <path_to_rom_file>");
    return
  }

  /*
   * Load the ROM
   */
  let rom_filename = (args[1]).clone();
  let rom_path = Path::new(args[1]);
  
  if rom_path.is_file() {
    println!("ROM found, loading...");
    println!("Size is {} bytes", std::io::fs::stat(&rom_path).size);
  
    let mut rom_file = File::open(&rom_path);
    let rom = rom_file.read_to_end(); 

    let header = read_header(rom);
    validate_header(header);
  } else {
    
    println!("{} doesn't look like a ROM file", rom_filename);
  }

}
