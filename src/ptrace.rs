extern crate libc;
use std::os;
use std::mem;
use posix::*;
use std::str::SendStr;
use std::ptr::null_mut;

extern {
    pub fn ptrace(request: libc::c_int, pid: libc::pid_t, addr: *mut libc::c_void, data: *mut libc::c_void) -> libc::c_long;
}

#[deriving(Show)]
pub enum PtraceResult {
    PtraceOk,
    PtraceError(int),
}
 
impl CouldBeAnError for PtraceResult {
    fn is_error(&self) -> bool {
        match *self {
            PtraceError(_) => true,
            _              => false,
        }
    }
 
    fn get_error_as_string(&self) -> SendStr {
        match *self {
            PtraceError(errno) => get_strerror(errno),
            _                  => "no error".into_maybe_owned(),
        }
    }
 
    fn get_errno(&self) -> int {
        match *self {
            PtraceError(errno) => errno,
            _                  => fail!("You can't get an errno from a success value!"),
        }
    }
}

#[deriving(Show)]
pub struct UserRegs {
  pub r15       : libc::uint64_t,
  pub r14       : libc::uint64_t,
  pub r13       : libc::uint64_t,
  pub r12       : libc::uint64_t,
  pub rbp       : libc::uint64_t,
  pub rbx       : libc::uint64_t,
  pub r11       : libc::uint64_t,
  pub r10       : libc::uint64_t,
  pub r9        : libc::uint64_t,
  pub r8        : libc::uint64_t,
  pub rax       : libc::uint64_t,
  pub rcx       : libc::uint64_t,
  pub rdx       : libc::uint64_t,
  pub rsi       : libc::uint64_t,
  pub rdi       : libc::uint64_t,
  pub orig_rax  : libc::uint64_t,
  pub rip       : libc::uint64_t,
  pub cs        : libc::uint64_t,
  pub eflags    : libc::uint64_t,
  pub rsp       : libc::uint64_t,
  pub ss        : libc::uint64_t,
  pub fs_base   : libc::uint64_t,
  pub gs_base   : libc::uint64_t,
  pub ds        : libc::uint64_t,
  pub es        : libc::uint64_t,
  pub fs        : libc::uint64_t,
  pub gs        : libc::uint64_t,
}

fn to_ptrace_result(return_value: libc::c_long) -> PtraceResult {
    match return_value {
        -1 => PtraceError(os::errno()),
        _  => PtraceOk,
    }
}
 
pub fn trace_me() -> PtraceResult {
    unsafe {
        to_ptrace_result(ptrace(TRACEME, 0, null_mut(), null_mut()))
    }
}
 
pub fn setoptions(pid: int, options: int) -> PtraceResult {
    unsafe {
        to_ptrace_result(ptrace(SETOPTIONS, pid as libc::pid_t, null_mut(), options as *mut libc::c_void))
    }
}
 
pub fn syscall(pid: int) -> PtraceResult {
    unsafe {
        to_ptrace_result(ptrace(SYSCALL, pid as libc::pid_t, null_mut(), null_mut()))
    }
}
 
pub fn get_registers(pid: int) -> Result<UserRegs, int> {
    unsafe {
        let registers = UserRegs {
          r15       : 0,
          r14       : 0,
          r13       : 0,
          r12       : 0,
          rbp       : 0,
          rbx       : 0,
          r11       : 0,
          r10       : 0,
          r9        : 0,
          r8        : 0,
          rax       : 0,
          rcx       : 0,
          rdx       : 0,
          rsi       : 0,
          rdi       : 0,
          orig_rax  : 0,
          rip       : 0,
          cs        : 0,
          eflags    : 0,
          rsp       : 0,
          ss        : 0,
          fs_base   : 0,
          gs_base   : 0,
          ds        : 0,
          es        : 0,
          fs        : 0,
          gs        : 0,
        };
 
        let result = ptrace(GETREGS, pid as libc::pid_t, null_mut(), mem::transmute(&registers));
 
        if result == -1 {
            Err(os::errno())
        } else {
            Ok(registers)
        }
    }
}
 
pub fn peektext(pid: int, addr: *mut libc::c_void) -> Result<u64, int> {
    unsafe {
        let result = ptrace(PEEKTEXT, pid as libc::pid_t, addr, null_mut());
 
        if result == -1 {
            let errno = os::errno();
 
            if errno != 0 {
                Err(errno)
            } else {
                Ok(result as u64)
            }
        } else {
            Ok(result as u64)
        }
    }
}
 
pub static TRACESYSGOOD : int = 0x00000001;
pub static TRACEFORK    : int = 0x00000002;
pub static TRACEEXEC    : int = 0x00000010;
 
pub mod syscall {
    pub static EXECV : u64 = 59;
}


#[deriving(Show)]
pub enum TraceResult {
    TraceOk,
    TraceError(int),
}
 
impl CouldBeAnError for TraceResult {
    fn is_error(&self) -> bool {
        match *self {
            TraceError(_) => true,
            _             => false,
        }
    }
 
    fn get_error_as_string(&self) -> SendStr {
        match *self {
            TraceError(errno) => get_strerror(errno),
            _                 => "".into_maybe_owned(),
        }
    }
 
    fn get_errno(&self) -> int {
        match *self {
            TraceError(errno) => errno,
            _                 => fail!("You can't get an errno from a success value!"),
        }
    }
}
 
pub fn wrap_result<T: CouldBeAnError>(result: T) -> TraceResult {
    if result.is_error() {
        TraceError(result.get_errno())
    } else {
        TraceOk
    }
}
 
pub fn init_trace(child_pid: int) -> TraceResult {
    match waitpid_wrapper(child_pid, 0) {
        WaitPidFailure(errno)       => TraceError(errno),
        WaitPidSuccess(pid, status) => {
            if status & SIGTRAP != 0 {
                let result = setoptions(pid, TRACEFORK | TRACESYSGOOD | TRACEEXEC);
                if result.is_error() {
                    return wrap_result(result);
                }
                wrap_result(syscall(pid))
            } else {
                TraceError(0) // shit...
            }
        },
    }
}
 
pub fn resume_trace(child_pid: int) -> TraceResult {
    wrap_result(syscall(child_pid))
}
