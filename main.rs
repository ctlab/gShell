#![feature(globs)]
#![allow(dead_code)]

extern crate libc;
use libc::funcs::posix88::unistd::*;
use std::ptr;
use std::os;
use posix::*;

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
                ptrace(TRACEME, 0, ptr::mut_null(), ptr::mut_null());
                let mut vec = Vec::with_capacity(c.len());
                let mut ptr_vec = Vec::with_capacity(c.len());

                for v in c.iter()
                {
                    let c_str = v.as_slice().to_c_str();
                    ptr_vec.push(c_str.as_ptr());
                    vec.push(c_str);
                }

                let t = ptr_vec.as_mut_ptr();
                execv(env.to_c_str().as_ptr(), t); 
            } 
            ForkFailure(_) => {
                println!("An error occurred: {}", result.get_error_as_string());
            }
            ForkParent(child_pid) => {
                println!("I'm the father of {}", child_pid);
            }
            
        }
    }
}
