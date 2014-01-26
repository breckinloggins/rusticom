use nesfile::ROM;
use mem::Mem;
use cpu::CPU;

mod nesfile;
mod mem;
mod cpu;

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
  let rom = match ROM::open(rom_filename) {
    Ok(r) => r,
    Err(s) => fail!(s)
  };

  let prg_rom0 = rom.prg_rom(0);
  
  print!("Loading PRG ROM 0 into memory...");
  let mut mem = Mem::new(0x10000);
  mem.fill(0xc000, prg_rom0);

  println!("done.");

  println!("Executing PRG ROM 0 ({} bytes)", prg_rom0.len());
  let mut cpu = CPU::new();
  cpu.reset(0xC000);
  loop { cpu.tick(mem); }
}
