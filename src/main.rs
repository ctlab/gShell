#![feature(globs)]
#![allow(dead_code)]

extern crate libc;
extern crate collections;
use libc::funcs::posix88::unistd::*;
use std::os;
use std::mem;
use posix::*;
use std::c_str::CString;

mod ptrace;
mod posix;
mod syscall;

fn main () {
    let env = "/usr/bin/env";
    let mut c = vec!["env".to_string()];
    if os::args().tail().len() != 0 {
        c.push_all(os::args().tail());
    } else {
        c.push_all(["echo".to_string(), "No argumets".to_string()]);
    }
    unsafe {
        let result = fork_wrapper();
        match result {
            ForkChild =>  {
                let mut vec = Vec::with_capacity(c.len());
                let mut ptr_vec = Vec::with_capacity(c.len());

                for v in c.iter()
                {
                    let c_str = v.as_slice().to_c_str();
                    ptr_vec.push(c_str.as_ptr());
                    vec.push(c_str);
                }

                let t = ptr_vec.as_mut_ptr();
                ptrace::trace_me();
                execv(env.to_c_str().as_ptr(), t); 
            } 
            ForkFailure(_) => {
                println!("An error occurred: {}", result.get_error_as_string());
            }
            ForkParent(child_pid) => {
                let result = ptrace::init_trace(child_pid);
            
                if result.is_error() {
                    println!("An error in init_ptrace occurred: {}", result.get_error_as_string());
                }
                loop {
                    let result = posix::waitpid_wrapper(child_pid, 0);
                    match result {
                        WaitPidFailure(_) => {
                            println!("An error in wait occurred: {}", result.get_error_as_string());
                            break;
                        },
                        WaitPidSuccess(pid, status) => {
                            if ((status >> 8) & (0x80 | posix::SIGTRAP)) != 0 {
                                match ptrace::get_registers(pid) {
                                    Ok(ptrace::UserRegs { orig_rax: syscall_no, rdi: rdi, .. }) => {
                                            //println!("Syscall {} with {}", syscall::get_syscall_name_as_str(syscall_no as uint), handle_syscall_arguments(pid, rdi));
                                        if syscall_no == 2 {
                                            println!("{}", handle_syscall_arguments(pid, rdi));
                                        }
                                        ptrace::syscall(pid); //need this to get only at end
                                        posix::waitpid_wrapper(pid, 0); // and this too
                                    },
                                    Err(errno) => {
                                        println!("Error in ptrace {}", get_strerror(errno)); break;
                                    },
                                }
                            }
                            let result = ptrace::resume_trace(child_pid);
                            if result.is_error() {
                                println!("An error in ptrace_resume occurred: {}", result.get_error_as_string());
                                break;
                            }
                        },
                    }
                }

            }
        }
    }
}


fn pstrdup(word: u64) -> String {
    unsafe {
        let mut bytes    = Vec::new();
        //let mut mut_addr = addr as libc::uint64_t;
    
        let mut i = 0;

        // XXX I'm not using a for loop because of a bug in Rust
        while i < mem::size_of::<libc::uint64_t>() {
            // XXX byte order
            let lsb = (word >> (i * 8)) & 0xFF;
            if lsb == 0 {
                break;
            }
            bytes.push(lsb as u8);
            i += 1;
        }
        //mut_addr += mem::size_of::<libc::uint64_t>() as libc::uint64_t;
        let result = std::str::raw::from_utf8(bytes.as_slice()).to_string();
        result
            //TODO notes touch shows no open, > shows open
        //println!("Syscall {} with {}", syscall::get_syscall_name_as_str(syscall_no as uint), CString::new(rdi as *const i8, false).as_str()); //TODO
    }
}
 
fn get_program_args(pid: int, addr: *mut libc::c_void) -> String {
    let mut args     = Vec::new();
    let mut mut_addr = addr as libc::uint64_t;
 
    loop {
        match ptrace::peektext(pid, mut_addr as *mut libc::c_void) {
            Err(_) | Ok(0) => break,
            Ok(word)       => {
                args.push(pstrdup(word));
            }
        }
 
        mut_addr += mem::size_of::<libc::uint64_t>() as libc::uint64_t;
    }
    args.concat()
}
 
fn handle_syscall_arguments(pid: int, argv_ptr: libc::uint64_t) -> String {
    let argv = get_program_args(pid, argv_ptr as *mut libc::c_void);
    argv
}
