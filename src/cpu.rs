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

    #[allow(dead_code)]    
    pub fn reset(&mut self) {
        self.pc = 0;
        self.states = 0;
    }

    fn add_states(&mut self, states: u64) {
        self.states += states;
    }


    fn add8(&mut self, a : u8 , b : u8, c : bool ) -> u8 {

        let bc = if c {u8::wrapping_add( b, 1 )} else {b};
        
        let acc = u8::wrapping_add(a, bc);

        self.registers.flag_s = (acc & 0x80) != 0;
        self.registers.flag_z = acc==0;

        self.registers.flag_h = (a & 0b00001000 != 0) && (bc & 0b00001000 != 0);

        self.registers.flag_p = (a & 0x80 == b & 0x80 ) && ( a & 0x80 != acc & 0x80);
        self.registers.flag_n = false;
        self.registers.flag_c = if c { a >= 0xFF - b } else {a > 0xFF - b};

        acc
    }

    fn add16(&mut self, a : u16 , b : u16 ) -> u16 {
        let res = u16::wrapping_add( a , b );
        self.registers.flag_h = ( a & b ) & 0x0800 != 0;  // check carry on bit 11 for half carry flag 
        self.registers.flag_c = a >= (0xFFFF - b);
        self.registers.flag_n = false;
        res
    }

    fn calc_parity(val : u8 ) -> bool {
        // set when bits are even
        let mut mask = 0b00000001; 
        let mut parity = false;
        for _ in 0..=7 {
            if (val & mask) != 0 {
                parity = !parity;
            }
            mask <<= 1;
        }
        parity
    }

    fn and8(&mut self, a : u8 , b : u8) -> u8 {
        let acc = a & b;
        self.registers.flag_s = (acc & 0x80) != 0;
        self.registers.flag_z = acc==0;
        self.registers.flag_h = true;
        self.registers.flag_p = Cpu::calc_parity(acc);
        self.registers.flag_n = false;
        self.registers.flag_c = false;
        acc
    }

    fn or8(&mut self, a : u8 , b : u8 ) -> u8 {
        let acc = a | b;
        self.registers.flag_s = (acc & 0x80) != 0;
        self.registers.flag_z = acc==0;
        self.registers.flag_h = true;
        self.registers.flag_p = Cpu::calc_parity(acc);
        self.registers.flag_n = false;
        self.registers.flag_c = false;
        acc
    }

    fn xor8(&mut self, a : u8 , b : u8 ) -> u8 {
        let acc = a ^ b;
        self.registers.flag_s = (acc & 0x80) != 0;
        self.registers.flag_z = acc==0;
        self.registers.flag_h = true;
        self.registers.flag_p = Cpu::calc_parity(acc);
        self.registers.flag_n = false;
        self.registers.flag_c = false;
        acc
    }

    fn rl8(&mut self,val : u8 ) -> u8 {
        let mut res = val;
        
        let nc = (res & 0b1000000 ) != 0;

        res <<=1;
        if self.registers.flag_c {
            res |= 0b00000001;
        }
        self.registers.flag_c = nc;
        self.registers.flag_z = res == 0;
        self.registers.flag_s = (res & 0x80 ) != 0;
        self.registers.flag_p = Cpu::calc_parity(res);
        self.registers.flag_h = false;
        self.registers.flag_n = false;
        res
    }


    fn rlc8(&mut self,val : u8 ) -> u8 {
        let mut res = val;
        self.registers.flag_c = (res & 0b1000000 ) != 0;
        res <<=1;
        if self.registers.flag_c {
            res |= 0b00000001;
        }
        self.registers.flag_z = res == 0;
        self.registers.flag_s = (res & 0x80 ) != 0;
        self.registers.flag_p = Cpu::calc_parity(res);
        self.registers.flag_h = false;
        self.registers.flag_n = false;
        res
    }

    fn rrc8(&mut self,val : u8 ) -> u8 {
        let mut res = val;
        self.registers.flag_c = (res & 0b0000001 ) != 0;
        res >>=1;
        if self.registers.flag_c {
            res |= 0b10000000;
        }
        self.registers.flag_z = res == 0;
        self.registers.flag_s = (res & 0x80 ) != 0;
        self.registers.flag_p = Cpu::calc_parity(res);
        self.registers.flag_h = false;
        self.registers.flag_n = false;
        res
    }

    fn rr8(&mut self,val : u8 ) -> u8 {
        let mut res = val;
        let nc = (res & 0b0000001 ) != 0;
        res >>=1;
        if self.registers.flag_c {
            res |= 0b10000000;
        }
        self.registers.flag_c = nc;
        self.registers.flag_z = res == 0;
        self.registers.flag_s = (res & 0x80 ) != 0;
        self.registers.flag_p = Cpu::calc_parity(res);
        self.registers.flag_h = false;
        self.registers.flag_n = false;
        res
    }

    fn sla8(&mut self,val : u8 ) -> u8 {
        let mut res = val;
        self.registers.flag_c = (res & 0b1000000 ) != 0;
        res <<=1;

        self.registers.flag_z = res == 0;
        self.registers.flag_s = (res & 0x80 ) != 0;
        self.registers.flag_p = Cpu::calc_parity(res);
        self.registers.flag_h = false;
        self.registers.flag_n = false;
        res
    }

    fn sra8(&mut self,val : u8 ) -> u8 {
        let mut res = val;
        let nc = (res & 0b0000001 ) != 0;
        res = (val >> 1) | (val & 0b10000000);
        self.registers.flag_c = nc;
        self.registers.flag_z = res == 0;
        self.registers.flag_s = (res & 0x80 ) != 0;
        self.registers.flag_p = Cpu::calc_parity(res);
        self.registers.flag_h = false;
        self.registers.flag_n = false;
        res
    }

    fn srl8(&mut self,val : u8 ) -> u8 {
        let mut res = val;
        let nc = (res & 0b0000001 ) != 0;
        res = val >> 1;
        self.registers.flag_c = nc;
        self.registers.flag_z = res == 0;
        self.registers.flag_s = (res & 0x80 ) != 0;
        self.registers.flag_p = Cpu::calc_parity(res);
        self.registers.flag_h = false;
        self.registers.flag_n = false;
        res
    }
    
    fn inc8(&mut self, a : u8  ) -> u8 {
        let acc = u8::wrapping_add(a, 1);
        self.registers.flag_s = (acc & 0x80) != 0;
        self.registers.flag_z = acc==0;
        self.registers.flag_h = false;
        self.registers.flag_p = a == 0xFE;
        self.registers.flag_n = false;
        acc
    }

    fn dec8(&mut self, a : u8  ) -> u8 {
        let acc = u8::wrapping_sub(a, 1);
        self.registers.flag_s = (acc & 0x80) != 0;
        self.registers.flag_z = acc==0;
        self.registers.flag_h = false;// AF BORROW BIT?
        self.registers.flag_p = a == 0x80;
        self.registers.flag_n = false;
        acc
    }

    fn sub8( &mut self, a : u8, b : u8, c : bool) -> u8 {
        let res = self.add8(a, !b, !c);
        self.registers.flag_n = true;
        self.registers.flag_c = !self.registers.flag_c;
        res
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
        let lo = self.next_byte(memio) as u16;
        let hi  = self.next_byte(memio) as u16;
        u16::wrapping_add( lo, hi << 8 )
    }

    fn get_mem_word( memio: &dyn MemIoAccess, addr : u16) -> u16 {
        let lo = memio.read_mem(addr) as u16;
        let hi = memio.read_mem( u16::wrapping_add(addr, 1) ) as u16;
        u16::wrapping_add( lo, hi << 8 )
    }

    fn get_r_src(op_byte : u8 ) -> u8 {
        op_byte & 0b00000111
    }

    fn get_r_dst(op_byte : u8 ) -> u8 {
        (op_byte & 0b00111000) >>3
    }

    fn check_cc(&self, op_byte: u8) -> bool {
        let cond = (op_byte & 0b00111000) >> 3;
        match cond {
            0b000 => !self.registers.flag_z,
            0b001 => self.registers.flag_z,
            0b010 => !self.registers.flag_c,
            0b011 => self.registers.flag_c,
            0b100 => !self.registers.flag_p, // check
            0b101 => self.registers.flag_p,
            0b110 => !self.registers.flag_s, // check
            0b111 => self.registers.flag_s,
            _ => false,
        }
    }

    fn jump_relative(&mut self, jr : u8) {
        if jr < 0x80 {
            self.pc = u16::wrapping_add( self.pc, jr.into());
        } else {
            self.pc = u16::wrapping_sub( self.pc, (0xFF - jr) as u16 + 1u16 );
        }
    }

    pub fn next_cb( &mut self, memio: &mut dyn MemIoAccess) {
        // get next byte
        let op_byte = self.next_byte(memio);
        match op_byte {

            // ** RLC r
            0x00 | 0x01 | 0x02 | 0x03 | 0x04 | 0x05 | 0x07 =>  {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);

                let res = self.rlc8(val);
                self.registers.set_r8(r,res);
                self.add_states(8);
            },
            // RLC (HL)
            0x06 =>  {
                let hl = self.registers.get_hl();
                let val = memio.read_mem(hl);
                let res = self.rlc8(val);
                memio.write_mem(hl, res);
            },

            // ** RRC r
            0x08 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D | 0x0F =>  {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);

                let res = self.rrc8(val);
                self.registers.set_r8(r,res);
                self.add_states(8);
            },
            // RRC (HL)
            0x0E =>  {
                let hl = self.registers.get_hl();
                let val = memio.read_mem(hl);
                let res = self.rrc8(val);
                memio.write_mem(hl, res);
            },

            // ** RL r
            0x10 | 0x11 | 0x12 | 0x13 | 0x14 | 0x15 | 0x17 =>  {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);

                let res = self.rl8(val);
                self.registers.set_r8(r,res);
                self.add_states(8);
            },
            // RL (HL)
            0x16 =>  {
                let hl = self.registers.get_hl();
                let val = memio.read_mem(hl);
                let res = self.rl8(val);
                memio.write_mem(hl, res);
            },

            // ** RR r
            0x18 | 0x19 | 0x1A | 0x1B | 0x1C | 0x1D | 0x1F =>  {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);

                let res = self.rr8(val);
                self.registers.set_r8(r,res);
                self.add_states(8);
            },
            // RR (HL)
            0x1E =>  {
                let hl = self.registers.get_hl();
                let val = memio.read_mem(hl);
                let res = self.rr8(val);
                memio.write_mem(hl, res);
            },

            // ** SLA r
            0x20 | 0x21 | 0x22 | 0x23 | 0x24 | 0x25 | 0x27 =>  {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);

                let res = self.sla8(val);
                self.registers.set_r8(r,res);
                self.add_states(8);
            },
            // SLA (HL)
            0x26 =>  {
                let hl = self.registers.get_hl();
                let val = memio.read_mem(hl);
                let res = self.sla8(val);
                memio.write_mem(hl, res);
            },

            // ** SRA r
            0x28 | 0x29 | 0x2A | 0x2B | 0x2C | 0x2D | 0x2F =>  {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);

                let res = self.sra8(val);
                self.registers.set_r8(r,res);
                self.add_states(8);
            }
            // SRA (HL)
            0x2E =>  {
                let hl = self.registers.get_hl();
                let val = memio.read_mem(hl);
                let res = self.sra8(val);
                memio.write_mem(hl, res);
            },

            // ** SRL r
            0x38 | 0x39 | 0x3A | 0x3B | 0x3C | 0x3D | 0x3F =>  {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);

                let res = self.srl8(val);
                self.registers.set_r8(r,res);
                self.add_states(8);
            }
            // SRL (HL)
            0x3E =>  {
                let hl = self.registers.get_hl();
                let val = memio.read_mem(hl);
                let res = self.srl8(val);
                memio.write_mem(hl, res);
            }

            _ => {panic!("Unknown extended instruction 0xCB 0x{:02X}", op_byte);}
        }
    }


    pub fn next( &mut self, memio: &mut dyn MemIoAccess) -> bool {

        // get next byte
        let op_byte = self.next_byte(memio);

        //println!("Execuring opcode: {}", op_byte);
        
        match op_byte {

            //********************************************
            // Instructions 00 to 0F fully implemented
            //********************************************

            // ** NOP **
            0x00 => {self.add_states(4);}

            0x76 => {
               // panic!("HALT SYSTEM");
               return false;
            },

            // ** LD dd, nn **
            0x01 | 0x11 | 0x21 => {
                let r = Cpu::get_r_dst(op_byte);
                let vl = self.next_byte(memio);
                let vh = self.next_byte(memio);

                self.registers.set_r8( r+1, vl);
                self.registers.set_r8( r  , vh);

                self.add_states(10);
            }

            //** LD (BC), A **
            0x02 => {
                memio.write_mem( self.registers.get_bc(), self.registers.a );
                self.add_states(7);
            }

            // ** LD (HL) <- n
            0x36 =>{
                let addr = self.registers.get_hl();
                let val = self.next_byte(memio);
                memio.write_mem( addr, val );
                self.add_states(10);
            }

            //** INC BC **
            0x03 => {
                self.registers.set_bc( u16::wrapping_add(self.registers.get_bc(), 1) );
                self.add_states( 6 );
            }

            //** INC DE **
            0x13 => {
                self.registers.set_de( u16::wrapping_add(self.registers.get_de(), 1) );
                self.add_states( 6 );
            }

            //** INC HL **
            0x23 => {
                self.registers.set_hl( u16::wrapping_add(self.registers.get_hl(), 1) );
                self.add_states( 6 );
            }

            //** DEC BC **
            0x0B => {
                self.registers.set_bc( u16::wrapping_sub(self.registers.get_bc(), 1) );
                self.add_states( 6 );
            }

            //** DEC DE **
            0x1B => {
                self.registers.set_de( u16::wrapping_sub(self.registers.get_de(), 1) );
                self.add_states( 6 );
            }

            //** DEC HL **
            0x2B => {
                self.registers.set_hl( u16::wrapping_sub(self.registers.get_hl(), 1) );
                self.add_states( 6 );
            }


            //** INC R **
            0x04 | 0x0C | 0x14 | 0x1C | 0x24 | 0x2C => {
                let r = Cpu::get_r_dst(op_byte);
                let rval = self.registers.get_r8( r );
                let res = self.inc8( rval );
                self.registers.set_r8( r, res );
                self.add_states(4);
            }

            //** INC (HL) **
            0x34 => {
                let val = memio.read_mem(self.registers.get_hl());
                let res = self.inc8( val );
                memio.write_mem(self.registers.get_hl(), res);
                self.add_states(11);
            }

            //** DEC r **
            0x05 | 0x0D | 0x15 | 0x1D | 0x25 | 0x2D | 0x3D  => {
                let r = Cpu::get_r_dst(op_byte);
                let rval = self.registers.get_r8( r );
                let res = self.dec8( rval );
                self.registers.set_r8( r, res );
                self.add_states(4);
            }

            // ** LD r, n **
            0x06 | 0x0E | 0x16 | 0x1E | 0x26 | 0x2E | 0x3E => {
                let r_dst = Cpu::get_r_dst(op_byte);
                let val = self.next_byte(memio);
                self.registers.set_r8( r_dst, val);

                self.add_states(7);
            },

            // ** LD HL, (nn)
            0x2A => {
                let val = self.next_word(memio);
                self.registers.set_hl(val);
                self.add_states(16);
            },            

            // ** RLCA
            0x07 => {
                let flag_s = self.registers.flag_s;
                let flag_z = self.registers.flag_z;
                let flag_p = self.registers.flag_p;

                let res = self.rlc8(self.registers.a);
                self.registers.a = res;

                self.registers.flag_s = flag_s;
                self.registers.flag_z = flag_z;
                self.registers.flag_p = flag_p;

                self.add_states(4);
            },

            // RLA
            0x17 => {
                let flag_s = self.registers.flag_s;
                let flag_z = self.registers.flag_z;
                let flag_p = self.registers.flag_p;

                let res = self.rl8(self.registers.a);
                self.registers.a = res;

                self.registers.flag_s = flag_s;
                self.registers.flag_z = flag_z;
                self.registers.flag_p = flag_p;

                self.add_states(4);
            },

            // RRCA
            0x0F => {
                let flag_s = self.registers.flag_s;
                let flag_z = self.registers.flag_z;
                let flag_p = self.registers.flag_p;

                let res = self.rrc8(self.registers.a);
                self.registers.a = res;

                self.registers.flag_s = flag_s;
                self.registers.flag_z = flag_z;
                self.registers.flag_p = flag_p;

                self.add_states(4);
            },

            // RRA
            0x1F => {
                let flag_s = self.registers.flag_s;
                let flag_z = self.registers.flag_z;
                let flag_p = self.registers.flag_p;

                let res = self.rr8(self.registers.a);
                self.registers.a = res;

                self.registers.flag_s = flag_s;
                self.registers.flag_z = flag_z;
                self.registers.flag_p = flag_p;

                self.add_states(4);
            },


            //** DJNZ, value **
            0x10 => {
                let jr = self.next_byte(memio);
                self.registers.b = u8::wrapping_sub(self.registers.b, 1 );

                if self.registers.b != 0 {
                    self.jump_relative(jr);
                    self.add_states( 13 );
                } else {
                    self.add_states(8);
                }
            }

            // ** IN A,(n) **
            0xDB => {
                let p = u16::wrapping_add(self.next_byte(memio) as u16, (self.registers.a as u16) << 8);
                self.registers.a = memio.read_port(p);
                self.add_states(11);
            }


            // ** OUT (n),A **
            0xD3 => {
                let p = u16::wrapping_add(self.next_byte(memio) as u16, (self.registers.a as u16) << 8);
                memio.write_port(p, self.registers.a);
                self.add_states(11);
            }

            // ** LD r <- r' **
            0b01000000..=0b01111111 => {
                let r_dst = Cpu::get_r_dst( op_byte );
                let r_src = Cpu::get_r_src( op_byte );

                let val = if r_src == 0b110 {
                    self.add_states( 3 ); //AF CHECK CONSTRUCTION
                    let addr = self.registers.get_hl();
                    memio.read_mem(addr)
                } else {
                    self.registers.get_r8(r_src)
                };

                if r_dst == 0b110 {
                    let addr = self.registers.get_hl();
                    memio.write_mem(addr, val);
                    self.add_states( 3 );

                } else {
                    self.registers.set_r8( r_dst, val);
                }

                self.add_states(4);
            },

            // ** LD SP, nn
            0x31 => {
                self.registers.sp = self.next_word(memio);
                self.add_states(10);
            },
            
            // LD A <- (BC)
            0x0A => {
                let addr = self.registers.get_bc();
                self.registers.a = memio.read_mem(addr);;
                self.add_states(7);
            },

            // LD A <- (DE)
            0x1A => {
                let addr = self.registers.get_de();
                self.registers.a = memio.read_mem(addr);

                self.add_states(7);
            },

            // LD A <- (nn)
            0x3A  => {
                let al =self.next_byte(memio);
                let ah =self.next_byte(memio);
                let addr = u16::wrapping_add(al as u16, (ah as u16) << 8 );
                self.registers.a = memio.read_mem(addr);

                self.add_states( 13 );
            }, 

            // ** LD (DE) <- A
            0x12 => {
                let addr = self.registers.get_de();
                memio.write_mem(addr, self.registers.a);

                self.add_states(7);
            },
            
            // ** LD A (nn) <- A
            0x32 => {
                let al =self.next_byte(memio);
                let ah =self.next_byte(memio);
                let addr = u16::wrapping_add(al as u16, (ah as u16) << 8 );
                memio.write_mem(addr, self.registers.a);
                self.add_states( 13 );
            },

            // ** ADD A, r
            0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 | 0x87 => {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);
                self.registers.a = self.add8(self.registers.a, val , false);
                self.add_states(4);
            },

            // ** ADC A, r
            0x88 | 0x89 | 0x8A | 0x8B | 0x8C | 0x8D | 0x8F => {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);
                self.registers.a = self.add8(self.registers.a, val , true);
                self.add_states(4);
            },

            // ** ADD A,n  ADC A,n
            0xC6 | 0xCE => {
                let cin = op_byte == 0x8E;
                let val = self.next_byte(memio);
                self.registers.a = self.add8(self.registers.a, val , cin);
                self.add_states(7);
            },

            // ** ADD A, (HL)  ADC A, (HL)
            0x86 | 0x8E => {
                let cin = op_byte == 0x8E;
                let val = memio.read_mem(self.registers.get_hl());
                self.registers.a = self.add8(self.registers.a, val , cin);
                self.add_states(7);
            },
           

            // ** SUB A, r
            0x90 | 0x91 | 0x92 | 0x93 | 0x94 | 0x95 | 0x97 => {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);
                self.registers.a = self.sub8(self.registers.a, val , false);
                self.add_states(4);
            },

            // ** SBC A, r
            0x98 | 0x99 | 0x9A | 0x9B | 0x9C | 0x9D | 0x9F => {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);
                self.registers.a = self.sub8(self.registers.a, val , true);
                self.add_states(4);
            },

            // ** SUB A,n  SUB A,n
            0xD6 | 0xDE => {
                let cin = op_byte == 0xDE;
                let val = self.next_byte(memio);
                self.registers.a = self.sub8(self.registers.a, val , cin);
                self.add_states(7);
            },

            // ** SUB A, (HL)  SBC A, (HL)
            0x96 | 0x9E => {
                let cin = op_byte == 0x9E;
                let val = memio.read_mem(self.registers.get_hl());
                self.registers.a = self.sub8(self.registers.a, val , cin);
                self.add_states(7);
            },

            0x09 => {
                let res = self.add16(self.registers.get_hl(), self.registers.get_bc());
                self.registers.set_hl(res);
                self.add_states(4);
            },
            0x19 => {
                let res = self.add16(self.registers.get_hl(), self.registers.get_de());
                self.registers.set_hl(res);
                self.add_states(4);
            },
            0x29 => {
                let res = self.add16(self.registers.get_hl(), self.registers.get_hl());
                self.registers.set_hl(res);
                self.add_states(4);
            },
            0x39 => {
                let res = self.add16(self.registers.get_hl(), self.registers.sp);
                self.registers.set_hl(res);
                self.add_states(4);
            },

            // ** AND A, r
            0xA0 | 0xA1 | 0xA2 | 0xA3 | 0xA4 | 0xA5 | 0xA7 => {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);
                self.registers.a = self.and8(self.registers.a, val);
                self.add_states(4);
            },

            // ** AND A,n
            0xE6 => {
                let val = self.next_byte(memio);
                self.registers.a = self.and8(self.registers.a, val);
                self.add_states(7);
            },

            // ** AND A, (HL)
            0xA6 => {
                let val = memio.read_mem(self.registers.get_hl());
                self.registers.a = self.and8(self.registers.a, val);
                self.add_states(7);
            },

            // ** OR A, r
            0xB0 | 0xB1 | 0xB2 | 0xB3 | 0xB4 | 0xB5 | 0xB7 => {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);
                self.registers.a = self.or8(self.registers.a, val);
                self.add_states(4);
            },

            // ** OR A,n
            0xF6 => {
                let val = self.next_byte(memio);
                self.registers.a = self.or8(self.registers.a, val);
                self.add_states(7);
            },

            // ** OR A, (HL)
            0xB6 => {
                let val = memio.read_mem(self.registers.get_hl());
                self.registers.a = self.or8(self.registers.a, val);
                self.add_states(7);
            },

            // ** XOR A, r
            0xA8 | 0xA9 | 0xAA | 0xAB | 0xAC | 0xAD | 0xAF => {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);
                self.registers.a = self.xor8(self.registers.a, val);
                self.add_states(4);
            },

            // ** XOR A,n
            0xEE => {
                let val = self.next_byte(memio);
                self.registers.a = self.xor8(self.registers.a, val);
                self.add_states(7);
            },

            // ** XOR A, (HL)
            0xAE => {
                let val = memio.read_mem(self.registers.get_hl());
                self.registers.a = self.xor8(self.registers.a, val);
                self.add_states(7);
            },


            // ** CP A, r
            0xB8 | 0xB9 | 0xBA | 0xBB | 0xBC | 0xBD | 0xBF => {
                let r = Cpu::get_r_src(op_byte);
                let val = self.registers.get_r8(r);
                _ = self.sub8(self.registers.a, val , false);
                self.add_states(4);
            },

            // ** CP A,n
            0xFE => {
                let cin = op_byte == 0xDE;
                let val = self.next_byte(memio);
                _ = self.sub8(self.registers.a, val , cin);
                self.add_states(7);
            },
            
            // ** CP A, (HL)
            0xBE => {
                let val = memio.read_mem(self.registers.get_hl());
                _ = self.sub8(self.registers.a, val , false);
                self.add_states(7);
            },

            
            // ** JP nn **
            0xC3 => 
            {
                self.pc = self.next_word(memio);
                self.add_states(10);
            }

            // ** JP cc, nn
            0xC2 | 0xCA | 0xD2 | 0xDA | 0xE2 | 0xEA | 0xF2 | 0xFA => {
                let addr = self.next_word(memio);
                if self.check_cc(op_byte) {
                    self.pc = addr;
                }
                self.add_states(10);
            }

            // ** JR e
            0x18 => {
                let jr = self.next_byte(memio);
                self.jump_relative(jr);
                self.add_states(12);
            }
            // ** JR C,e  JR NC,e  JR Z,e  JR NZ, e
            0x38 | 0x30 | 0x28 | 0x20 => {
                let jr =self.next_byte(memio);
                if  (op_byte == 0x38 && self.registers.flag_c)  || 
                    (op_byte == 0x30 && !self.registers.flag_c) ||
                    (op_byte == 0x28 && self.registers.flag_z) ||
                    (op_byte == 0x20 && !self.registers.flag_z) {
                    self.jump_relative(jr);
                    self.add_states(12);
                } else {
                    self.add_states(7);
                }
            }

            // ** JP (HL)
            0xE9 => 
            {
                self.pc = Cpu::get_mem_word(memio, self.registers.get_hl());
                self.add_states(4);
            }


            // ** CALL **
            0xCD => {
                let next_pc = self.next_word(memio);
                self.stack_push_word(memio, self.pc);
                self.pc = next_pc;
                self.add_states(17);
            },

            // ** CALL cc,nn
            0xC4 | 0xCC | 0xD4 | 0xDC | 0xE4 | 0xEC | 0xF4 | 0xFC => {
                let next_pc = self.next_word(memio);
                if self.check_cc(op_byte) {
                    self.stack_push_word(memio, self.pc);
                    self.pc = next_pc;
                    self.add_states(17);
                } else {
                    self.add_states(10);
                }
            }

            // ** RET  **
            0xC9 => {
                self.pc = self.stack_pop_word(memio);
                self.add_states(10);
            },

            // ** RET cc
            0xC0 | 0xC8 | 0xD0 | 0xD8 | 0xE0 | 0xE8 | 0xF0 | 0xF8 => {
                if self.check_cc(op_byte) {
                    self.pc = self.stack_pop_word(memio);
                    self.add_states(11);
                } else {
                   self.add_states(5);
                }
            }

            // ** PUSH BC **
            0xC5 => {
                self.stack_push_byte(memio, self.registers.b);
                self.stack_push_byte(memio, self.registers.c);
                self.add_states(11);
            },
            // ** PUSH DE **
            0xD5 => {
                self.stack_push_byte(memio, self.registers.d);
                self.stack_push_byte(memio, self.registers.e);
                self.add_states(11);
            },
            // ** PUSH HL **
            0xE5 => {
                self.stack_push_byte(memio, self.registers.h);
                self.stack_push_byte(memio, self.registers.l);
                self.add_states(11);
            },
            // ** PUSH AF **
            0xF5 => {
                self.stack_push_byte(memio, self.registers.a);
                let f = self.registers.get_f();
                self.stack_push_byte(memio, f);
                self.add_states(11);
            },

            // ** POP BC **
            0xC1 => {
                let c = self.stack_pop_byte(memio); 
                let b = self.stack_pop_byte(memio); 
                self.registers.c = c;
                self.registers.b = b;
                self.add_states(10);
            },

            // ** POP DE **
            0xD1 => {
                let e = self.stack_pop_byte(memio); 
                let d = self.stack_pop_byte(memio); 
                self.registers.e = e;
                self.registers.d = d;
                self.add_states(10);
            },

            // ** POP HL **
            0xE1 => {
                let l = self.stack_pop_byte(memio); 
                let h = self.stack_pop_byte(memio); 
                self.registers.l = l;
                self.registers.h = h;
                self.add_states(10);
            },

            // ** POP AF **
            0xF1 => {
                let f = self.stack_pop_byte(memio); 
                let a = self.stack_pop_byte(memio); 
                self.registers.set_f(f);
                self.registers.a = a;
                self.add_states(10);
            }

            // ** EXTENDED ED SECTIONS **
            0xDD => { 
                let ext_op_byte = self.next_byte(memio);
                match ext_op_byte {
                    //** LD IX, nn **
                    0x21 => {
                        self.registers.ix = self.next_word(memio);
                        self.add_states(14);
                    },

                    // ** LD IX, (nn)
                    0x2A => {
                        let val = self.next_word(memio);
                        self.registers.ix = val;
                        self.add_states(20);
                    },

                    // ** LD SP, IX
                    0xF9 => {
                        self.registers.sp = self.registers.ix;
                        self.add_states(10);
                    },

                    //** INC IX **
                    0x23 => {
                        self.registers.ix = u16::wrapping_add(self.registers.ix, 1 );
                        self.add_states(10);
                    },
                    //** DEC IX **
                    0x2B => {
                        self.registers.ix = u16::wrapping_sub(self.registers.ix, 1 );
                        self.add_states(10);
                    },

                    // ** LD (IX+d), n
                    0x36 => {
                        let d = self.next_byte(memio);
                        let n = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.ix, d.into() );
                        memio.write_mem(addr, n);
                        self.add_states(19);
                    }

                    // ** LD (IX+d), r
                    0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75 | 0x77 => {
                        let r = Cpu::get_r_src(ext_op_byte);
                        let val = self.registers.get_r8(r);
                        let d = self.next_byte(memio);
                        
                        let addr = u16::wrapping_add(self.registers.ix, d.into() );
                        memio.write_mem(addr, val);
                        self.add_states(19);
                    }

                    // ** ADD A, (IX +d), ADC A, (IX +d)
                    0x86 | 0x8E => {
                        let cin = ext_op_byte == 0x8E;
                        let d = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.ix, d.into());
                        let val = memio.read_mem(addr);
                        self.registers.a = self.add8(self.registers.a, val , cin);
                        self.add_states(19);
                    },

                    // ** SUB A, (IX +d), SBC A, (IX +d)
                    0x96 | 0x9E => {
                        let cin = ext_op_byte == 0x9E;
                        let d = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.ix, d.into());
                        let val = memio.read_mem(addr);
                        self.registers.a = self.sub8(self.registers.a, val , cin);
                        self.add_states(19);
                    },

                    // ** AND A, (IX +d)
                    0xA6 => {
                        let d = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.ix, d.into());
                        let val = memio.read_mem(addr);
                        self.registers.a = self.and8(self.registers.a, val );
                        self.add_states(19);
                    },

                    // ** OR A, (IX +d)
                    0xB6 => {
                        let d = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.ix, d.into());
                        let val = memio.read_mem(addr);
                        self.registers.a = self.or8(self.registers.a, val );
                        self.add_states(19);
                    },

                    // ** XOR A, (IX +d)
                    0xAE => {
                        let d = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.ix, d.into());
                        let val = memio.read_mem(addr);
                        self.registers.a = self.xor8(self.registers.a, val );
                        self.add_states(19);
                    },

                    // ** CP A, (IX +d)
                    0xBE => {
                        let d = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.ix, d.into());
                        let val = memio.read_mem(addr);
                        _ = self.sub8(self.registers.a, val,false);
                        self.add_states(19);
                    },

                    // ** INC (IX +d)
                    0x34 => {
                        let d = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.ix, d.into());
                        let val = memio.read_mem(addr);
                        let res = self.inc8(val);
                        memio.write_mem(addr, res);
                        self.add_states(23);
                    },

                    // ** JP (IX)
                    0xE9 => 
                    {
                        self.pc = Cpu::get_mem_word(memio, self.registers.ix);
                        self.add_states(8);
                    },

                    // ** POP IX **
                    0xE1 => {
                        self.registers.ix = self.stack_pop_word(memio); 
                        self.add_states(14);
                    },

                    // ** PUSH IX **
                    0xE5 => {
                        self.stack_push_word(memio, self.registers.ix);
                        self.add_states(15);
                    },

                    // ** CB extended
                    0xCB => {
                        let d = self.next_byte(memio);
                        let ext_ext_op_byte = self.next_byte(memio);

                        match ext_ext_op_byte {
                            // RLC (IX+d)
                            0x06 =>  {
                                let addr = u16::wrapping_add(self.registers.ix, d.into());
                                let val = memio.read_mem(addr);
                                let res = self.rlc8(val);
                                memio.write_mem(addr, res);
                            },
                            // RRC (IX+d)
                            0x0E =>  {
                                let addr = u16::wrapping_add(self.registers.ix, d.into());
                                let val = memio.read_mem(addr);
                                let res = self.rrc8(val);
                                memio.write_mem(addr, res);
                            },
                            // RL (IX+d)
                            0x16 =>  {
                                let addr = u16::wrapping_add(self.registers.ix, d.into());
                                let val = memio.read_mem(addr);
                                let res = self.rl8(val);
                                memio.write_mem(addr, res);
                            },
                            // RR (IX+d)
                            0x1E =>  {
                                let addr = u16::wrapping_add(self.registers.ix, d.into());
                                let val = memio.read_mem(addr);
                                let res = self.rr8(val);
                                memio.write_mem(addr, res);
                            },
                            // SLA (IX+d)
                            0x26 =>  {
                                let addr = u16::wrapping_add(self.registers.ix, d.into());
                                let val = memio.read_mem(addr);
                                let res = self.sla8(val);
                                memio.write_mem(addr, res);
                            },
                            // SRA (IX+d)
                            0x2E =>  {
                                let addr = u16::wrapping_add(self.registers.ix, d.into());
                                let val = memio.read_mem(addr);
                                let res = self.sra8(val);
                                memio.write_mem(addr, res);
                            },
                            // SRL (IX+d)
                            0x3E =>  {
                                let addr = u16::wrapping_add(self.registers.ix, d.into());
                                let val = memio.read_mem(addr);
                                let res = self.srl8(val);
                                memio.write_mem(addr, res);
                            },
                            _ => {panic!("Unknown extended instruction 0xDD 0xCB 0x{:02X}", ext_ext_op_byte);}

                        }
                    }
                                // RLC (HL)
            0x06 =>  {
                let hl = self.registers.get_hl();
                let val = memio.read_mem(hl);
                let res = self.rlc8(val);
                memio.write_mem(hl, res);
            }

                    _ => {panic!("Unknown extended instruction 0xDD 0x{:02X}", ext_op_byte);}
                }
            }

            0xCB => {
                self.next_cb(memio);
            }

            0xED => {
                let ext_op_byte = self.next_byte(memio);

                match ext_op_byte {

                    // ** RLD **
                    0x6F => {
                        let a = self.registers.a;
                        let b = memio.read_mem(self.registers.get_hl());

                        memio.write_mem(self.registers.get_hl(), (b << 4) | (a & 0x0F));
                        self.registers.a = (a & 0xF0) | (b >> 4);

                        self.registers.flag_h = false;                     //Set the flags
                        self.registers.flag_n = false;
                        self.registers.flag_p = Cpu::calc_parity(self.registers.a);
                        self.registers.flag_z = self.registers.a == 0;
                        self.registers.flag_s = (self.registers.a & 0x80) != 0;
                    },
                    // **LD BC, (nn)
                    0x4B => { 
                        let addr = self.next_word(memio);
                        self.registers.c = memio.read_mem(addr);
                        self.registers.b = memio.read_mem(addr+1);
                        self.add_states(20);
                    },
                    // **LD DE, (nn)
                    0x5B => { 
                        let addr = self.next_word(memio);
                        self.registers.e = memio.read_mem(addr);
                        self.registers.d = memio.read_mem(addr+1);
                        self.add_states(20);
                    },
                    
                    // **LD HL, (nn)
                    0x6B => { 
                        let addr = self.next_word(memio);
                        self.registers.l = memio.read_mem(addr);
                        self.registers.h = memio.read_mem(addr+1);
                        self.add_states(20);
                    },

                    // **LD SP, (nn)
                    0x7B => { 
                        let addr = self.next_word(memio);

                        let l = memio.read_mem(addr) as u16;
                        let h = memio.read_mem(addr+1) as u16;
                        self.registers.sp = u16::wrapping_add(l, h << 8);
                        self.add_states(20);
                    },

                    // // ** IN A,(n) **  AF REMOVE?
                    // 0xDB => {
                    //     let p = u16::wrapping_add(self.next_byte(memio) as u16, (self.registers.a as u16) << 8);
                    //     self.registers.a = memio.read_port(p);
                    //     self.add_states(11);
                    // }
                    // ** IN r, (C)*
                    0x40 | 0x48 | 0x50 | 0x58 | 0x60 | 0x68 | 0x78 => {
                        let r = Cpu::get_r_dst(ext_op_byte);
                        let p = self.registers.get_bc();
                        let val = memio.read_port(p);                       
                        self.registers.set_r8(r, val);
                        

                        self.registers.flag_s = val > 0x80;
                        self.registers.flag_z = val == 0;
                        self.registers.flag_h = false;
                        self.registers.flag_p = Cpu::calc_parity(val);
                        self.registers.flag_n = false;

                        self.add_states(12);
                    },

                    // ** OUT (C),r*
                    0x41 | 0x49 | 0x51 | 0x59 | 0x61 | 0x69 | 0x79 => {
                        let r = Cpu::get_r_dst(ext_op_byte);
                        let p = self.registers.get_bc();
                        
                        // get content from register and write it to the port
                        let rval = self.registers.get_r8(r);
                        memio.write_port(p, rval);
                        self.add_states(12);
                    },

                    // ** LDIR **
                    0xB0 => {
                        let val = memio.read_mem(self.registers.get_hl());
                        memio.write_mem( self.registers.get_de(), val);
                        self.registers.inc_de();
                        self.registers.inc_hl();
                        self.registers.dec_bc();

                        let bc = self.registers.get_bc();
                        
                        if bc != 0 {
                            self.pc = self.pc - 0x0002;
                            self.add_states(21);
                        } else {

                            self.registers.flag_h = false;
                            self.registers.flag_p = false;
                            self.registers.flag_n = false;
                            self.add_states(16);
                        }
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
                            self.add_states(21);
                        } else {
                            self.registers.flag_z = true;
                            self.registers.flag_n = true;
                            self.add_states(16);
                        }
                    }
                    _ => {panic!("Unknown extended instruction 0xED 0x{:02X}", ext_op_byte);}
                }
            },

            0xFD => { 
                let ext_op_byte = self.next_byte(memio);
                match ext_op_byte {

                    //** LD IY, nn **
                    0x21 => {
                        self.registers.iy = self.next_word(memio);
                        self.add_states(14);
                    },
                    // ** LD IY, (nn)
                    0x2A => {
                        let val = self.next_word(memio);
                        self.registers.iy = val;
                        self.add_states(20);
                    },            

                    // ** LD SP, IX
                    0xF9 => {
                        self.registers.sp = self.registers.iy;
                        self.add_states(10);
                    },


                    //** INC IY **
                    0x23 => {
                        self.registers.iy = u16::wrapping_add(self.registers.iy, 1 );
                        self.add_states(10);
                    },
                    
                    //** DEC IY **
                    0x2B => {
                        self.registers.iy = u16::wrapping_sub(self.registers.iy, 1 );
                        self.add_states(10);
                    },

                    // ** LD (IY+d), n
                    0x36 => {
                        let d = self.next_byte(memio);
                        let n = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.iy, d.into() );
                        memio.write_mem(addr, n);
                        self.add_states(19);
                    }

                    // ** LD (IY+d), r
                    0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75 | 0x77 => {
                        let r = Cpu::get_r_src(ext_op_byte);
                        let val = self.registers.get_r8(r);
                        let d = self.next_byte(memio);
                        
                        let addr = u16::wrapping_add(self.registers.iy, d.into() );
                        memio.write_mem(addr, val);
                        self.add_states(19);
                    }
                    
                    // ** ADD A, (IY +d), ADC A, (IY +d)
                    0x86 | 0x8E => {
                        let cin = ext_op_byte == 0x8E;
                        let d = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.iy, d.into());
                        let val = memio.read_mem(addr);
                        self.registers.a = self.add8(self.registers.a, val , cin);
                        self.add_states(19);
                    },

                    // ** SUB A, (IY +d), SBC A, (IY +d)
                    0x96 | 0x9E => {
                        let cin = ext_op_byte == 0x9E;
                        let d = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.iy, d.into());
                        let val = memio.read_mem(addr);
                        self.registers.a = self.sub8(self.registers.a, val , cin);
                        self.add_states(19);
                    },

                    // ** AND A, (IY +d)
                    0xA6 => {
                        let d = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.iy, d.into());
                        let val = memio.read_mem(addr);
                        self.registers.a = self.and8(self.registers.a, val );
                        self.add_states(19);
                    },

                    // ** OR A, (IY +d)
                    0xB6 => {
                        let d = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.iy, d.into());
                        let val = memio.read_mem(addr);
                        self.registers.a = self.or8(self.registers.a, val );
                        self.add_states(19);
                    },

                    // ** XOR A, (IY +d)
                    0xAE => {
                        let d = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.iy, d.into());
                        let val = memio.read_mem(addr);
                        self.registers.a = self.xor8(self.registers.a, val );
                        self.add_states(19);
                    },

                    // ** CP A, (IY +d)
                    0xBE => {
                        let d = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.iy, d.into());
                        let val = memio.read_mem(addr);
                        _ = self.sub8(self.registers.a, val,false);
                        self.add_states(19);
                    },

                    // ** INC (IY +d)
                    0x34 => {
                        let d = self.next_byte(memio);
                        let addr = u16::wrapping_add(self.registers.iy, d.into());
                        let val = memio.read_mem(addr);
                        let res = self.inc8(val);
                        memio.write_mem(addr, res);
                        self.add_states(23);
                    },

                    // ** JP (IY)
                    0xE9 => 
                    {
                        self.pc = Cpu::get_mem_word(memio, self.registers.iy);
                        self.add_states(8);
                    },

                    // ** POP IY **
                    0xE1 => {
                        self.registers.iy = self.stack_pop_word(memio); 
                        self.add_states(14);
                    },

                    // ** PUSH IY **
                    0xE5 => {
                        self.stack_push_word(memio, self.registers.iy);
                        self.add_states(15);
                    },

                    // ** CB extended
                    0xCB => {
                        let d = self.next_byte(memio);
                        let ext_ext_op_byte = self.next_byte(memio);

                        match ext_ext_op_byte {
                            // RLC (IY+d)
                            0x06 =>  {
                                let addr = u16::wrapping_add(self.registers.iy, d.into());
                                let val = memio.read_mem(addr);
                                let res = self.rlc8(val);
                                memio.write_mem(addr, res);
                            },
                            // RRC (IY+d)
                            0x0E =>  {
                                let addr = u16::wrapping_add(self.registers.iy, d.into());
                                let val = memio.read_mem(addr);
                                let res = self.rrc8(val);
                                memio.write_mem(addr, res);
                            },
                            // RL (IY+d)
                            0x16 =>  {
                                let addr = u16::wrapping_add(self.registers.iy, d.into());
                                let val = memio.read_mem(addr);
                                let res = self.rl8(val);
                                memio.write_mem(addr, res);
                            },
                            // RR (IY+d)
                            0x1E =>  {
                                let addr = u16::wrapping_add(self.registers.iy, d.into());
                                let val = memio.read_mem(addr);
                                let res = self.rr8(val);
                                memio.write_mem(addr, res);
                            },
                            // SRA (IY+d)
                            0x26 =>  {
                                let addr = u16::wrapping_add(self.registers.iy, d.into());
                                let val = memio.read_mem(addr);
                                let res = self.sla8(val);
                                memio.write_mem(addr, res);
                            },
                            // SRA (IY+d)
                            0x2E =>  {
                                let addr = u16::wrapping_add(self.registers.iy, d.into());
                                let val = memio.read_mem(addr);
                                let res = self.sra8(val);
                                memio.write_mem(addr, res);
                            },
                            // SRL (IY+d)
                            0x3E =>  {
                                let addr = u16::wrapping_add(self.registers.iy, d.into());
                                let val = memio.read_mem(addr);
                                let res = self.srl8(val);
                                memio.write_mem(addr, res);
                            },
                            _ => {panic!("Unknown extended instruction 0xDD 0xCB 0x{:02X}", ext_ext_op_byte);}

                        }
                    },
                    _ => {panic!("Unknown extended instruction 0xFD 0x{:02X}", ext_op_byte);}
                }
            },

            _ => {panic!("Unknown instruction 0x{:02X}", op_byte);}
        }

        true
    }
}


        // // LD r <- (HL)
        // if (op_byte & 0b11000111 ) == 0b00000110 {
        //     let r_dst = Cpu::get_r_dst( op_byte );

        //     let addr = self.registers.get_hl();
        //     let val = memio.read_mem( addr );

        //     self.registers.set_r8( r_dst, val);
            
        //     self.add_states(7);
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
                
        //         self.add_states(19);
        //         return;            
        //     }        

        //     // LD (IX + d) <- r
        //     if (ext_op_byte & 0b11111000 ) == 0b01110000 {
        //         let r_src = Cpu::get_r_src(ext_op_byte);
        //         let d = self.next_byte(memio) as u16;

        //         let addr = u16::wrapping_add(self.registers.ix, d); 
        //         let val = self.registers.get_r8(r_src);
        //         memio.write_mem( addr, val );
                
        //         self.add_states(19);
        //         return;            
        //     }        

        //     // LD (IX+d) <- n
        //     if ext_op_byte == 0x36 {
        //         let d = self.next_byte(memio) as u16;
        //         let val = self.next_byte(memio);

        //         let addr = u16::wrapping_add(self.registers.ix, d); 
        //         memio.write_mem( addr, val );

        //         self.add_states(19);
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
                
        //         self.add_states(19);
        //         return;            
        //     }        

        //     // LD (IY + d) <- r
        //     if (ext_op_byte & 0b11111000 ) == 0b01110000 {
        //         let r_src = Cpu::get_r_src(ext_op_byte);
        //         let d = self.next_byte(memio) as u16;

        //         let addr = u16::wrapping_add(self.registers.iy, d); 
        //         let val = self.registers.get_r8(r_src);
        //         memio.write_mem( addr, val );
                
        //         self.add_states(19);
        //         return;            
        //     }

        //     // LD (IY+d) <- n
        //     if ext_op_byte == 0x36 {
        //         let d = self.next_byte(memio) as u16;
        //         let val = self.next_byte(memio);

        //         let addr = u16::wrapping_add(self.registers.iy, d); 
        //         memio.write_mem( addr, val );

        //         self.add_states(7);
        //         return;            
        //     }
        // }


        // // page 50 (PDF)


        // panic!("Unknow opcode {}", op_byte);
        // //memio.write_mem(1, memio.read_mem( 0000 ) + 1);