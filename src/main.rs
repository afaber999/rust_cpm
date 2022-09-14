use minifb::{Key, ScaleMode, Window, WindowOptions};

const WIDTH: usize = 640 / 2;
const HEIGHT: usize = 360 / 2;

mod cpu;
use std::borrow::BorrowMut;
use std::collections::VecDeque;
use std::fs::File;
use std::io::Read;

use cpu::Cpu;
use cpu::MemIoAccess;

use std::{thread, time};

fn sleep(millis: u64) {
    let duration = time::Duration::from_millis(millis);
    thread::sleep(duration);
}

pub struct Peripherals {
    last_serial_a : u8,
    last_serial_b : u8,
    mem       : [u8;65536],
    in_keys   : VecDeque<u8>,
    out_chars : VecDeque<u8>,
}

impl Peripherals {
    pub fn new() -> Self {
        Self {
            last_serial_a : 0,
            last_serial_b : 0,
            mem       : [0;65536],
            in_keys   : VecDeque::with_capacity(128),
            out_chars : VecDeque::with_capacity(128),
        }
    }

    pub fn add_in_key(&mut self, ch : u8) {
        self.in_keys.push_back( ch );        
    }

    pub fn get_out_char(&mut self) -> Option<u8> {
        self.out_chars.pop_front()        
    }
}

impl MemIoAccess for Peripherals {
    fn read_mem(&self, address : u16) -> u8 {
        return self.mem[address as usize];
    }

    fn write_mem(&mut self, address : u16, value : u8) {
        self.mem[address as usize] = value;
    }
    fn read_port(&mut self, port : u16) -> u8 {
        let mp = port & 0xFF;
        match mp {
            0x0030 => { 
                if let Some(c) = self.in_keys.pop_front() {
                    self.last_serial_a = c;
                }
                self.last_serial_a                
            },
            0x0031 => { 
                self.last_serial_b
            },
            0x0032 => {
                if self.in_keys.len() > 0 {
                    0b00000101
                } else {
                    0b00000100
                }
            },
            0x0033 => { 
                0b00000100
            },
            _ => {panic!("invalid read port 0x{:02X}  0x{:04X}", mp, port)},
        }
    }

    fn write_port(&mut self, port : u16, value : u8) {

        println!("Write port 0x{:04X} val 0x{:02X} ", port, value);

        match port & 0xFF {
            0x0030 => { 
                self.out_chars.push_back(value);
            },
            0x0031 => { 
            },
            0x0032 => { 
            },
            0x0033 => { 
            },
            _ => {panic!("invalid write port 0x{:04X}", port)},
        }
    }    
}


pub struct Machine {
    cpu : Cpu,
    peripherals : Peripherals,
}

impl Machine {
    pub fn new() -> Self {
        Self {
            cpu : Cpu::new(),
            peripherals : Peripherals::new(),
        }
    }

    pub fn reset(&mut self) {
    }

    pub fn next(&mut self) {
        self.cpu.next( &mut self.peripherals );
    }
}

fn read_binary( target_addr: u16, machine : &mut Machine, filename:&str ) {

    let mut fin = File::open(&filename).expect("no file found");
    let mut lpc : u16 = target_addr;
    loop {
        let mut fb= [0u8; 0x4000];
        let bytes_read = fin.read(&mut fb).expect("read error");
        if bytes_read == 0 {
            break;
        }
        for i in 0..bytes_read {
            machine.peripherals.write_mem( lpc, fb[i]);
            if lpc == 0xFFFF {
                panic!("Read file error, ouf of memory at {} ", i);
            }
            lpc = lpc + 1;
            
        }
    }
    println!("Read file from 0x{:04X} to 0x{:04X} -> {} bytes  ", target_addr, lpc, lpc- target_addr);
}


fn main() {

    println!("Starting Z80_Take1 v0.1a!");
    let mut machine = Machine::new();

    let filename = ".\\asm\\asm.bin";
    read_binary( 0, machine.borrow_mut(), filename );
    
    
    let mut window = Window::new(
        "Z80 CP/M system",
        WIDTH,
        HEIGHT,
        WindowOptions {
            resize: true,
            scale_mode: ScaleMode::UpperLeft,
            ..WindowOptions::default()
        },
    )
    .expect("Unable to create window");

    // Limit to max ~60 fps update rate
    window.limit_update_rate(Some(std::time::Duration::from_micros(16600)));

    let mut buffer: Vec<u32> = Vec::with_capacity(WIDTH * HEIGHT);

    let mut size = (0, 0);

    while window.is_open() && !window.is_key_down(Key::Escape) {
        let new_size = (window.get_size().0, window.get_size().1);
        if new_size != size {
            size = new_size;
            buffer.resize(size.0 * size.1, 0);
        }

        window.get_keys().iter().for_each(|key| match key {
            Key::W => println!("holding w!"),
            Key::T => println!("holding t!"),
            _ => (),
        });

        window.get_keys_released().iter().for_each(|key| match key {
            Key::W => println!("released w!"),
            Key::T => println!("released t!"),
            _ => (),
        });

        window
            .update_with_buffer(&buffer, new_size.0, new_size.1)
            .unwrap();

        while let Some(ch) = machine.peripherals.get_out_char() {
            print!( "{}", char::from( ch ) );
        }

        println!(" PC:0x{:04X} a:0x{:02X} bc:0x{:04X} de:0x{:04X} hl:0x{:04X} sp:0x{:04X}", 
            machine.cpu.pc, 
            machine.cpu.registers.a,
            machine.cpu.registers.get_bc(),
            machine.cpu.registers.get_de(),
            machine.cpu.registers.get_hl(),
            machine.cpu.registers.sp);
        machine.next();
        sleep(10);
    }
}
