#[allow(dead_code)]

mod registers;
use registers::Registers;

pub trait MemIoAccess {
    fn read_mem(&self, address : u16) -> u8;
    fn write_mem(&mut self, address : u16, value : u8);    
    fn read_port(&mut self, port : u16) -> u8;
    fn write_port(&mut self, port : u16, value : u8);    
}

pub struct Cpu {
    pub registers : Registers,
    pub pc        : u16,
    pub states    : u64,
}

impl Cpu {
    pub fn new() -> Self {
        Self {
            registers : Registers::new(),
            pc        : 0,
            states    : 0,
        }
    }
    pub fn reset(&mut self) {
        self.pc = 0;
        self.states = 0;
    }


    fn add8(&mut self, a : u8 , b : u8, c : bool ) -> u8 {

        let bc = if c {u8::wrapping_add( b, 1 )} else {b};
        
        let acc = u8::wrapping_add(a, bc);

        self.registers.flag_s = acc & 0x80 != 0;
        self.registers.flag_z = acc==0;

        self.registers.flag_h = (a & 0b00001000 != 0) && (bc & 0b00001000 != 0);

        self.registers.flag_p = (a & 0x80 == b & 0x80 ) && ( a & 0x80 != acc & 0x80);
        self.registers.flag_n = false;
        self.registers.flag_c = if c { a >= 0xFF - b } else {a > 0xFF - b};

        acc
      }

    fn stack_pop_byte(&mut self, memio: &dyn MemIoAccess) -> u8 {
        let ret = memio.read_mem( self.registers.sp );
        self.registers.sp = u16::wrapping_add( self.registers.sp, 1 );
        ret
    }

    fn stack_pop_word(&mut self, memio: &mut dyn MemIoAccess) -> u16{
        let vl = self.stack_pop_byte( memio ) as u16;
        let vh = self.stack_pop_byte( memio ) as u16;
        u16::wrapping_add( vl, vh << 8 )
    }


    fn stack_push_byte(&mut self, memio: &mut dyn MemIoAccess, val : u8) {
        self.registers.sp = u16::wrapping_sub( self.registers.sp, 1 );
        memio.write_mem( self.registers.sp, val );
    }

    fn stack_push_word(&mut self, memio: &mut dyn MemIoAccess, val : u16) {
        self.stack_push_byte( memio, (val >> 8  ) as u8 );
        self.stack_push_byte( memio, (val & 0xFF) as u8 );
    }

    fn next_byte(&mut self, memio: &dyn MemIoAccess) -> u8 {
        let ret = memio.read_mem( self.pc );
        self.pc = u16::wrapping_add( self.pc, 1 );
        ret
    }

    fn next_word(&mut self, memio: &dyn MemIoAccess) -> u16 {
        let low = self.next_byte(memio) as u16;
        let hi  = self.next_byte(memio) as u16;
        u16::wrapping_add( low, hi << 8 )
    }


    fn get_r_src(op_byte : u8 ) -> u8 {
        op_byte & 0b00000111
    }

    fn get_r_dst(op_byte : u8 ) -> u8 {
        (op_byte & 0b00111000) >>3
    }

    pub fn next( &mut self, memio: &mut dyn MemIoAccess) {

        // get next byte
        let op_byte = self.next_byte(memio);

        //println!("Execuring opcode: {}", op_byte);
        
        match op_byte {

            //********************************************
            // Instructions 00 to 0F fully implemented
            //********************************************

            // ** NOP **
            0x00 => {self.states = self.states + 4;}

            // ** LD dd, nn **
            0x01 | 0x11 | 0x21 => {
                let r = Cpu::get_r_dst(op_byte);
                let vl = self.next_byte(memio);
                let vh = self.next_byte(memio);

                self.registers.set_r8( r+1, vl);
                self.registers.set_r8( r  , vh);

                self.states = self.states + 10;
            }


            //** LD (BC), A **
            0x02 => {
                memio.write_mem( self.registers.get_bc(), self.registers.a );
                self.states = self.states + 7;
            }


            //** INC BC **
            0x03 => {
                self.registers.set_bc( u16::wrapping_add(self.registers.get_bc(), 1) );
                self.states = self.states + 6;
            }


            //** INC R **
            0x04 | 0x0C | 0x14 | 0x1C | 0x24 | 0x2C => {
                let r = Cpu::get_r_dst(op_byte);
                let rval = self.registers.get_r8( r );

                let carry_save = self.registers.flag_c;
                let res = self.add8( rval, 1, false );
                self.registers.flag_c = carry_save;

                self.registers.set_r8( r, res );
                self.states = self.states + 4;
            }

            //** DEC B **
            0x05 => {
                self.registers.b = u8::wrapping_sub(self.registers.b, 1 );
                self.states = self.states + 4;
            }

            // ** LD r, n **
            0x06 | 0x0e | 0x16 | 0x1e | 0x26 | 0x2e | 0x3e => {
                let r_dst = Cpu::get_r_dst(op_byte);
                let val = self.next_byte(memio);
                self.registers.set_r8( r_dst, val);

                self.states = self.states + 7;
            }

            //** DJNZ, value **
            0x10 => {
                let jr = self.next_byte(memio);
                self.registers.b = u8::wrapping_sub(self.registers.b, 1 );

                if self.registers.b != 0 {
                    if jr < 0x80 {
                        self.pc += jr as u16;
                    } else {
                        self.pc -= (0xFF - jr) as u16 + 1u16;
                    }
                    self.states = self.states + 13;
                } else {
                    self.states = self.states + 8;
                }
            }

            // ** IN A,(n) **
            0xDB => {
                let p = self.next_byte(memio) as u16 + (self.registers.a as u16) << 8;
                self.registers.a = memio.read_port(p);
                self.states = self.states + 11;
            }

            // ** OUT (n),A **
            0xD3 => {
                let p = self.next_byte(memio) as u16 + (self.registers.a as u16) << 8;
                memio.write_port(p, self.registers.a);
                self.states = self.states + 11;
            }

            // ** LD r <- r' **
            0b01000000..=0b01111111 => {
                let r_dst = Cpu::get_r_dst( op_byte );
                let r_src = Cpu::get_r_src( op_byte );
                
                let val = self.registers.get_r8(r_src);
                if r_dst == 0b110 {
                    let addr = self.registers.get_hl();
                    memio.write_mem(addr, val);
                    self.states = self.states + 3;

                } else {
                    self.registers.set_r8( r_dst, val);
                }

                self.states = self.states + 4;
            },

            // ** LD SP, nn
            0x31 => {
                self.registers.sp = self.next_word(memio);
                self.states = self.states + 10;
            },
            
            // ** JP nn **
            0xC3 => 
            {
                self.pc = self.next_word(memio);
                self.states = self.states + 10;
            }

            // ** EXTENDED ED SECTION **
            0xED => {
                let ext_op_byte = self.next_byte(memio);

                // ** OUT (C),r*
                match ext_op_byte {
                    0x41 | 0x49 | 0x51 | 0x59 | 0x61 | 0x69 | 0x79 => {
                        let r = Cpu::get_r_dst(ext_op_byte);
                        let p = self.registers.get_bc();
                        
                        // get content from register and write it to the port
                        let rval = self.registers.get_r8(r);
                        memio.write_port(p, rval);
                        self.states = self.states + 12;
                    }
                    // ** OTIR **
                    0xB3 => {
                        let val = memio.read_mem(self.registers.get_hl());
                        self.registers.inc_hl();

                        self.registers.b = u8::wrapping_sub(self.registers.b, 1 );                        
                        let addr = self.registers.get_bc();
                        memio.write_port(addr, val);

                        if self.registers.b != 0 {
                            self.pc = self.pc - 0x0002;
                            self.states = self.states + 21;
                        } else {
                            self.registers.flag_z = true;
                            self.registers.flag_n = true;
                            self.states = self.states + 16;
                        }
                    }
                    _ => {panic!("Unknown extended instruction 0xED {}", op_byte);}
                }
            },
            // ** CALL **
            0xCD => {
                let next_pc = self.next_word(memio);
                self.stack_push_word(memio, self.pc);
                self.pc = next_pc;
                self.states = self.states + 17;
            },

            // ** RET  **
            0xC9 => {
                self.pc = self.stack_pop_word(memio);
                self.states = self.states + 10;
            },

            // ** PUSH BC **
            0xC5 => {
                self.stack_push_byte(memio, self.registers.b);
                self.stack_push_byte(memio, self.registers.c);
                self.states = self.states + 11;
            },
            // ** PUSH DE **
            0xD5 => {
                self.stack_push_byte(memio, self.registers.d);
                self.stack_push_byte(memio, self.registers.e);
                self.states = self.states + 11;
            },
            // ** PUSH HL **
            0xE5 => {
                self.stack_push_byte(memio, self.registers.h);
                self.stack_push_byte(memio, self.registers.l);
                self.states = self.states + 11;
            },
            // ** PUSH AF **
            0xF5 => {
                self.stack_push_byte(memio, self.registers.a);
                let f = self.registers.get_f();
                self.stack_push_byte(memio, f);
                self.states = self.states + 11;
            },

            _ => {panic!("Unknown instruction {}", op_byte);}
        }





        // // LD r <- (HL)
        // if (op_byte & 0b11000111 ) == 0b00000110 {
        //     let r_dst = Cpu::get_r_dst( op_byte );

        //     let addr = self.registers.get_hl();
        //     let val = memio.read_mem( addr );

        //     self.registers.set_r8( r_dst, val);
            
        //     self.states = self.states + 7;
        //     return;            
        // }

        // // EXTENDED DD group
        // if op_byte == 0xDD  {
        //     let ext_op_byte = self.next_byte(memio);

        //     // LD r <- (IX + d)
        //     if (ext_op_byte & 0b11000111 ) == 0b01000110 {
        //         let r_dst = Cpu::get_r_dst( op_byte );
        //         let d = self.next_byte(memio) as u16;

        //         let addr = u16::wrapping_add(self.registers.ix, d); 
        //         let val = memio.read_mem( addr );

        //         self.registers.set_r8( r_dst, val);
                
        //         self.states = self.states + 19;
        //         return;            
        //     }        

        //     // LD (IX + d) <- r
        //     if (ext_op_byte & 0b11111000 ) == 0b01110000 {
        //         let r_src = Cpu::get_r_src(ext_op_byte);
        //         let d = self.next_byte(memio) as u16;

        //         let addr = u16::wrapping_add(self.registers.ix, d); 
        //         let val = self.registers.get_r8(r_src);
        //         memio.write_mem( addr, val );
                
        //         self.states = self.states + 19;
        //         return;            
        //     }        

        //     // LD (IX+d) <- n
        //     if ext_op_byte == 0x36 {
        //         let d = self.next_byte(memio) as u16;
        //         let val = self.next_byte(memio);

        //         let addr = u16::wrapping_add(self.registers.ix, d); 
        //         memio.write_mem( addr, val );

        //         self.states = self.states + 19;
        //         return;            
        //     }
            
        // }

        // if op_byte == 0xFD  {
        //     let ext_op_byte = self.next_byte(memio);

        //     // LD r <- (IY + d)
        //     if (ext_op_byte & 0b11000111 ) == 0b01000110 {
        //         let r_dst = Cpu::get_r_dst( ext_op_byte );
        //         let d = self.next_byte(memio) as u16;

        //         let addr = u16::wrapping_add(self.registers.iy, d); 
        //         let val = memio.read_mem( addr );

        //         self.registers.set_r8( r_dst, val);
                
        //         self.states = self.states + 19;
        //         return;            
        //     }        

        //     // LD (IY + d) <- r
        //     if (ext_op_byte & 0b11111000 ) == 0b01110000 {
        //         let r_src = Cpu::get_r_src(ext_op_byte);
        //         let d = self.next_byte(memio) as u16;

        //         let addr = u16::wrapping_add(self.registers.iy, d); 
        //         let val = self.registers.get_r8(r_src);
        //         memio.write_mem( addr, val );
                
        //         self.states = self.states + 19;
        //         return;            
        //     }

        //     // LD (IY+d) <- n
        //     if ext_op_byte == 0x36 {
        //         let d = self.next_byte(memio) as u16;
        //         let val = self.next_byte(memio);

        //         let addr = u16::wrapping_add(self.registers.iy, d); 
        //         memio.write_mem( addr, val );

        //         self.states = self.states + 7;
        //         return;            
        //     }
        // }

        // // LD (HL) <- n
        // if op_byte == 0x36 {
        //     let addr = self.registers.get_hl();
        //     let val = self.next_byte(memio);
        //     memio.write_mem( addr, val );
        //     self.states = self.states + 10;
        //     return;            
        // }

        // // LD A <- (BC)
        // if op_byte == 0x0A {
        //     let addr = self.registers.get_bc();
        //     let val = memio.read_mem(addr);
        //     self.registers.a = val;
        //     self.states = self.states + 7;
        //     return;            
        // }

        // // LD A <- (DE)
        // if op_byte == 0x1A {
        //     let addr = self.registers.get_de();
        //     let val = memio.read_mem(addr);
        //     self.registers.a = val;

        //     self.states = self.states + 7;
        //     return;            
        // }

        // // LD A <- (nn)
        // if op_byte == 0x1A {
        //     let al =self.next_byte(memio);
        //     let ah =self.next_byte(memio);
        //     let addr = al as u16 + (ah as u16) << 8;
        //     let val = memio.read_mem(addr);
        //     self.registers.a = val;

        //     self.states = self.states + 13;
        //     return;            
        // } 

        // // LD (BC) <- A
        // if op_byte == 0x02 {
        //     let addr = self.registers.get_bc();
        //     let val = self.registers.a;
        //     memio.write_mem(addr, val);

        //     self.states = self.states + 7;
        //     return;            
        // }

        // // LD (DE) <- A
        // if op_byte == 0x12 {
        //     let addr = self.registers.get_de();
        //     let val = self.registers.a;
        //     memio.write_mem(addr, val);

        //     self.states = self.states + 7;
        //     return;            
        // }
        
        // // LD A (nn) <- A
        // if op_byte == 0x32 {
        //     let al =self.next_byte(memio);
        //     let ah =self.next_byte(memio);
        //     let addr = al as u16 + (ah as u16) << 8;
        //     let val = self.registers.a; 
        //     memio.write_mem(addr, val);

        //     self.states = self.states + 13;
        //     return;            
        // } 

        // // page 50 (PDF)


        // panic!("Unknow opcode {}", op_byte);
        // //memio.write_mem(1, memio.read_mem( 0000 ) + 1);
    }
}


