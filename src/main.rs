use minifb::{Key, ScaleMode, Window, WindowOptions};
use anyhow::{Result};

use std::cell::RefCell;
use std::rc::Rc;

const WIDTH: usize = 640 / 2;
const HEIGHT: usize = 360 / 2;

mod cpu;
use std::borrow::BorrowMut;
use std::collections::VecDeque;
use std::fs::File;
use std::io::Read;
use std::io::stdout;
use std::io::Write;
use cpu::Cpu;
use cpu::MemIoAccess;

use std::{thread, time};


fn sleep(millis: u64) {
    let duration = time::Duration::from_millis(millis);
    thread::sleep(duration);
}

pub struct Peripherals {
    verbose : u32,
    last_serial_a : u8,
    last_serial_b : u8,
    mem       : [u8;65536],
    in_keys   : VecDeque<u8>,
    out_chars : VecDeque<u8>,
}

impl Peripherals {
    pub fn new(verbose : u32) -> Self {
        Self {
            verbose,
            last_serial_a : 0,
            last_serial_b : 0,
            mem       : [0x76;65536],
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
        // AF MEMORY PROTECTION CHECK
        // if address >= 0x3000 && address < 0x38DA {
        //     panic!("invalid mem write to {:04X} {:02X}",address, value );
        // }

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
                    //println!("GOT LEN {} ", self.in_keys.len()); 
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

        if self.verbose > 90 {
            println!("Write port 0x{:04X} val 0x{:02X} ", port, value);
        }

        match port & 0xFF {
            0x0030 => { 
                self.out_chars.push_back(value);
                //panic!("IO BREAK");
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
    pub fn new(verbose : u32) -> Self {
        Self {
            cpu : Cpu::new(),
            peripherals : Peripherals::new(verbose),
        }
    }

    pub fn reset(&mut self) {
    }

    pub fn next(&mut self) -> bool {
        self.cpu.next( &mut self.peripherals )
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
            machine.peripherals.mem[ lpc as usize] = fb[i];
            if lpc == 0xFFFF {
                panic!("Read file error, ouf of memory at {} ", i);
            }
            lpc = lpc + 1;
            
        }
    }
    println!("Read file from 0x{:04X} to 0x{:04X} -> {} bytes  ", target_addr, lpc, lpc- target_addr);
}


type KeyVec = Rc<RefCell<Vec<u32>>>;

struct Input {
    keys: KeyVec,
}

impl Input {
    fn new(data: &KeyVec) -> Input {
        Input { keys: data.clone() }
    }
}

impl minifb::InputCallback for Input {
    fn add_char(&mut self, uni_char: u32) {
        self.keys.as_ref().borrow_mut().push(uni_char);
        //println!("added key {} len {} ", uni_char, self.keys.as_ref().borrow().len());
    }
}

fn main() -> Result<()> {
    let mut trace_file = File::create("trace.txt")?;
    writeln!( trace_file, "Starting tracer" ).expect("tracerr");
    let verbose = 100;

    println!("Starting Z80_Take1 v0.1a!");
    let mut machine = Machine::new(verbose);

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

    let keys_data = KeyVec::new(RefCell::new(Vec::new()));
    let input = Box::new(Input::new(&keys_data));
            
    // Limit to max ~60 fps update rate
    window.limit_update_rate(Some(std::time::Duration::from_micros(16600)));
    window.set_input_callback(input);

    let mut buffer: Vec<u32> = Vec::with_capacity(WIDTH * HEIGHT);

    let mut size = (0, 0);
    let mut cycles = 0;

    while window.is_open() && !window.is_key_down(Key::Escape) {
        let new_size = (window.get_size().0, window.get_size().1);
        if new_size != size {
            size = new_size;
            buffer.resize(size.0 * size.1, 0);
        }

        // window.get_keys().iter().for_each(|key| match key {
        //     Key::W => println!("holding w!"),
        //     Key::T => println!("holding t!"),
        //     _ => (),
        // });

        // window.get_keys_released().iter().for_each(|key|  {
        //     println!("Release {:?}", key);
        // });

        window
            .update_with_buffer(&buffer, new_size.0, new_size.1)
            .unwrap();




        // while let cc = Some(keys_data.as_ref().borrow_mut().pop() ) {
        //         println!("POPPING {:?}", cc);
        // }
        for t in keys_data.as_ref().borrow_mut().iter() {
            machine.peripherals.add_in_key(*t as u8);
            // println!("Code point: {},   Character: {:?}", *t, char::from_u32(*t));
        }
        keys_data.as_ref().borrow_mut().clear();



        for _ in 0..1000 {

            writeln!( trace_file, " PC:0x{:04X} f:{:08b} a:0x{:02X} bc:0x{:04X} de:0x{:04X} hl:0x{:04X} sp:0x{:04X} (HL) 0x{:02X} (PC) 0x{:02X}", 
                            machine.cpu.pc, 
                            machine.cpu.registers.get_f(),
                            machine.cpu.registers.a,
                            machine.cpu.registers.get_bc(),
                            machine.cpu.registers.get_de(),
                            machine.cpu.registers.get_hl(),
                            machine.cpu.registers.sp,
                            machine.peripherals.mem[machine.cpu.registers.get_hl() as usize],
                            machine.peripherals.mem[machine.cpu.pc as usize],
                        ).expect("TRACEERR");

            if verbose > 10 {
                println!(" PC:0x{:04X} f:{:08b} :0x{:02X} bc:0x{:04X} de:0x{:04X} hl:0x{:04X} sp:0x{:04X} (SP) 0x{:02X} (HL) 0x{:02X} (PC) 0x{:02X}", 
                machine.cpu.pc, 
                machine.cpu.registers.get_f(),
                machine.cpu.registers.a,
                machine.cpu.registers.get_bc(),
                machine.cpu.registers.get_de(),
                machine.cpu.registers.get_hl(),
                machine.cpu.registers.sp,
                machine.peripherals.mem[machine.cpu.registers.sp as usize],
                machine.peripherals.mem[machine.cpu.registers.get_hl() as usize],
                machine.peripherals.mem[machine.cpu.pc as usize]);
            }
            if !machine.next() {
                // sleep(100);
                while let Some(ch) = machine.peripherals.get_out_char() {
                    print!( "{}", char::from( ch ) );
                    drop( stdout().flush());
                }
                println!("***** HALTED ************");
                // sleep(100000);
                panic!("HALTED at {:04X}", machine.cpu.pc);

            } else {
                // sleep(100);
                while let Some(ch) = machine.peripherals.get_out_char() {
                    print!( "{}", char::from( ch ) );
                    drop( stdout().flush());
                    writeln!(trace_file, "----------- IN {} ------------ ", char::from( ch )).expect("TRACEERRR");
                }
            }    
        }

        while let Some(ch) = machine.peripherals.get_out_char() {
            print!( "{}", char::from( ch ) );
            drop( stdout().flush());
        }
        if cycles %10  == 0 {
            drop( stdout().flush());
        }        

        cycles+=1;
    }
    sleep(1);
    drop( trace_file);
    Ok(())
}
