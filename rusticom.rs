use nesfile::ROM;

mod nesfile;

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
  /* let rom = */ match ROM::open(rom_filename) {
    Ok(r) => r,
    Err(s) => fail!(s)
  };

  /*
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
  */



}
