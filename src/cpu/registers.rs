pub struct Registers {
    // 16 bit pairs AF BC DE HL
    pub a   : u8,
    pub b   : u8,
    pub c   : u8,
    pub d   : u8,
    pub e   : u8,
    pub h   : u8,
    pub l   : u8,

    // register f
    pub flag_s : bool,
    pub flag_z : bool,
    pub flag_x : bool,
    pub flag_h : bool,
    pub flag_y : bool,
    pub flag_p : bool,
    pub flag_n : bool,
    pub flag_c : bool,

    pub ix  : u16,
    pub iy  : u16,
    pub sp  : u16,

    // Shadow set
    pub a2   : u8,
    pub b2   : u8,
    pub c2   : u8,
    pub d2   : u8,
    pub e2   : u8,
    pub h2   : u8,
    pub l2   : u8,    

    // register f
    pub flag_s2 : bool,
    pub flag_z2 : bool,
    pub flag_x2 : bool,
    pub flag_h2 : bool,
    pub flag_y2 : bool,
    pub flag_p2 : bool,
    pub flag_n2 : bool,
    pub flag_c2 : bool,

    pub ix2 : u16,
    pub iy2 : u16,

    // sp2?
}

impl Registers {
    pub fn new() -> Self {
        Self {
            // 16 0t pairs AF BC DE HL
            a   : 0,
            b   : 0,
            c   : 0,
            d   : 0,
            e   : 0,
            h   : 0,
            l   : 0,

            flag_s : false,
            flag_z : false,
            flag_x : false,
            flag_h : false,
            flag_y : false,
            flag_p : false,
            flag_n : false,
            flag_c : false,

            ix  : 0,
            iy  : 0,
            sp  : 0,

            // Shadow set
            a2   : 0,
            b2   : 0,
            c2   : 0,
            d2   : 0,
            e2   : 0,
            h2   : 0,
            l2   : 0,    

            flag_s2 : false,
            flag_z2 : false,
            flag_x2 : false,
            flag_h2 : false,
            flag_y2 : false,
            flag_p2 : false,
            flag_n2 : false,
            flag_c2 : false,

            ix2 : 0,
            iy2 : 0,
            // sp2?
        }
    }

    pub fn get_r8( &mut self, reg : u8) -> u8 {
        match reg {
            0b000 => self.b,
            0b001 => self.c,
            0b010 => self.d,
            0b011 => self.e,
            0b100 => self.h,
            0b101 => self.l,
            //0b110 => self.f,   // AF NOT LISTED DOUBLE CHECK is (M)
            0b111 => self.a,
            _ => panic!("get_r8 invalid register {}", reg),
        }
    }

    pub fn set_r8( &mut self, reg : u8, val : u8) {
        match reg {
            0b000 => self.b = val,
            0b001 => self.c = val,
            0b010 => self.d = val,
            0b011 => self.e = val,
            0b100 => self.h = val,
            0b101 => self.l = val,
            //0b110 => self.f = val,   // AF NOT LISTED DOUBLE CHECK is (M)
            0b111 => self.a = val,
            _ => panic!("set_r8 invalid register {}", reg),
        }
    }

    pub fn get_bc(&self) -> u16 {
        (self.c as u16 ) + ((self.b as u16) << 8)
    }

    pub fn set_bc(&mut self, val : u16) {
        self.b = (val >> 8) as u8;
        self.c = (val & 0xFF) as u8;
    }

    pub fn get_de(&self) -> u16 {
        (self.d as u16 ) + ((self.e as u16) << 8)
    }

    pub fn set_dec(&mut self, val : u16) {
        self.d = (val >> 8) as u8;
        self.e = (val & 0xFF) as u8;
    }

    pub fn get_hl(&self) -> u16 {
        (self.l as u16 ) + ((self.h as u16) << 8)
    }

    pub fn set_hl(&mut self, val : u16) {
        self.h = (val >> 8) as u8;
        self.l = (val & 0xFF) as u8;
    }

    pub fn inc_hl(&mut self) {
        self.l = u8::wrapping_add(self.l,1);
        if self.l == 0 {
            self.h = u8::wrapping_add(self.h,1);
        }
    }

    pub fn get_f(&mut self) -> u8 {
        let mut ret = 0;
        if self.flag_s { ret += 0b10000000;}
        if self.flag_z { ret += 0b01000000;}
        if self.flag_x { ret += 0b00100000;}
        if self.flag_h { ret += 0b00010000;}
        if self.flag_y { ret += 0b00001000;}
        if self.flag_p { ret += 0b00000100;}
        if self.flag_n { ret += 0b00000010;}
        if self.flag_c { ret += 0b00000001;}
        ret
    }

    pub fn set_f(&mut self, val : u8) {
        self.flag_s = val & 0b10000000 != 0;
        self.flag_z = val & 0b01000000 != 0;
        self.flag_x = val & 0b00100000 != 0;
        self.flag_h = val & 0b00010000 != 0;
        self.flag_y = val & 0b00001000 != 0;
        self.flag_p = val & 0b00000100 != 0;
        self.flag_n = val & 0b00000010 != 0;
        self.flag_c = val & 0b00000001 != 0;
    }
}
