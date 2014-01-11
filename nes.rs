// Copyright 2014 Breckin Loggins
//
// NES machine

struct NES {
  cpu_mem:        [u8, ..0x4000],   // System RAM [TODO: Virtualize]
  //ppu_mem:        [u8, ..0x4000],   // PPU RAM [TODO: Virtualize]
}
