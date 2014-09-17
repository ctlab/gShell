#![allow(dead_code)]
extern crate libc;
extern crate collections;
use self::collections::*;
use std::os;

extern {
    pub fn ptrace(request: libc::c_int, pid: libc::pid_t, addr: *mut libc::c_void, data: *mut libc::c_void) -> libc::c_long;
    pub fn fork() -> libc::pid_t;
    pub fn exit(status: libc::c_int) -> !;
    pub fn getpid() -> libc::pid_t;
    pub fn waitpid(pid: libc::pid_t, status: *mut libc::c_int, flags: libc::c_int) -> libc::c_int;
    pub fn kill(pid: libc::pid_t, signal: libc::c_int) -> libc::c_int;
    pub fn strerror(errno: libc::c_int) -> *mut libc::c_char;
}

pub trait CouldBeAnError {
    fn is_error(&self) -> bool;
    fn get_error_as_string(&self) -> String;
    fn get_errno(&self) -> int;
}
 
pub enum PosixResult {
    PosixOk,
    PosixError(int),
}

pub fn get_strerror(errno: int) -> String {
    unsafe {
        collections::string::raw::from_buf(strerror(errno as libc::c_int) as *const u8)
    }
}

impl CouldBeAnError for PosixResult {
    fn is_error(&self) -> bool {
        match *self {
            PosixOk       => false,
            PosixError(_) => true,
        }
    }
 
    fn get_error_as_string(&self) -> String {
        match *self {
            PosixOk           => "no error".to_string(), //work on &str
            PosixError(errno) => get_strerror(errno),
        }
    }
 
    fn get_errno(&self) -> int {
        match *self {
            PosixOk           => fail!("You can't get an errno from a success value!"),
            PosixError(errno) => errno,
        }
    }
}

pub enum ForkResult {
    ForkFailure(int),
    ForkChild,
    ForkParent(int),
}
 
impl CouldBeAnError for ForkResult {
    fn is_error(&self) -> bool {
        match *self {
            ForkFailure(_) => true,
            _              => false,
        }
    }
 
    fn get_error_as_string(&self) -> String {
        match *self {
            ForkFailure(errno) => get_strerror(errno),
            _                  => "no error".to_string(),
        }
    }
 
    fn get_errno(&self) -> int {
        match *self {
            ForkFailure(errno) => errno,
            _                  => fail!("You can't get an errno from a success value!"),
        }
    }
}
 
pub enum WaitPidResult {
    WaitPidFailure(int),
    WaitPidSuccess(int, int),
}
 
impl CouldBeAnError for WaitPidResult {
    fn is_error(&self) -> bool {
        match *self {
            WaitPidFailure(_) => true,
            _                 => false,
        }
    }
 
    fn get_error_as_string(&self) -> String {
        match *self {
            WaitPidFailure(errno) => get_strerror(errno),
            _                     => "no error".to_string(),
        }
    }
 
    fn get_errno(&self) -> int {
        match *self {
            WaitPidFailure(errno) => errno,
            _                     => fail!("You can't get an errno from a success value!"),
        }
    }
}
 
pub fn fork_wrapper() -> ForkResult {
    unsafe {
        let pid = fork();
 
        match pid {
            -1  => ForkFailure(os::errno()),
            0   => ForkChild,
            pid => ForkParent(pid as int),
        }
    }
}
 
pub fn getpid_wrapper() -> int {
    unsafe {
        getpid() as int
    }
}
 
pub fn waitpid_wrapper(pid: int, flags: int) -> WaitPidResult {
    unsafe {
        let status : libc::c_int = 0;
 
        let pid = waitpid(pid as libc::pid_t, status as *mut libc::c_int, flags as libc::c_int);
 
        if pid == -1 {
            WaitPidFailure(os::errno())
        } else {
            WaitPidSuccess(pid as int, status as int)
        }
    }
}

pub fn exit_wrapper(status: int) -> ! {
    unsafe {
        exit(status as libc::c_int)
    }
}
 
pub fn kill_wrapper(pid: int, signum: int) -> PosixResult {
    unsafe {
        match kill(pid as libc::pid_t, signum as libc::c_int) {
            -1 => PosixError(os::errno()),
            _  => PosixOk,
        }
    }
}
 
pub static SIGTRAP    : int = 5;
pub static SIGKILL    : int = 9;
pub static ECHILD     : int = 10;
pub static TRACEME    : libc::c_int = 0;
pub static PEEKTEXT   : libc::c_int = 1;
pub static GETREGS    : libc::c_int = 12;
pub static SYSCALL    : libc::c_int = 24;
pub static SETOPTIONS : libc::c_int = 0x4200;
