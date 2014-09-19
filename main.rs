#![feature(globs)]
#![allow(dead_code)]

extern crate libc;
use libc::funcs::posix88::unistd::*;
use std::os;
use posix::*;

mod ptrace;
mod posix;

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
                //ptrace::setoptions(child_pid, ptrace::TRACESYSGOOD);
                //let result = posix::waitpid_wrapper(child_pid, 0);
                let result = ptrace::init_trace(child_pid);
            
                if result.is_error() {
                    println!("An error in init_ptrace occurred: {}", result.get_error_as_string());
                }
                loop {
                    let result = posix::waitpid_wrapper(child_pid, 0);
                    println!("RESULT IS {}", result);

                    match result {
                        WaitPidFailure(_) => {
                            println!("An error in wait occurred: {}", result.get_error_as_string());
                            break;
                        },
                        WaitPidSuccess(pid, status) => {
                            //println!("Status is {}", status);
                            if ((status >> 8) & (0x80 | posix::SIGTRAP)) != 0 {
                                match ptrace::get_registers(pid) {
                                    Ok(ptrace::UserRegs { orig_rax: syscall_no, .. }) => {
                                        println!("Syscall_no is {}", syscall_no);
                                        ptrace::syscall(pid); //need this to get only at end
                                        posix::waitpid_wrapper(pid, 0); // and this too
                                    },
                                    Err(errno) => {
                                        println!("Error in ptrace {}", get_strerror(errno)); break;
                                    },
                                }
                            } else {
                                //TODO indeed we don't need this
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
