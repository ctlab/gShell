use posix::CouldBeAnError; // needed for impl below
 
mod ptrace;
 
//enum TraceEvent {
    //SystemCall {
        //syscall_no : word,
        //arguments  : (word, word, word, word, word, word),
    //},
    //Other,
//}
 
enum TraceResult {
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
 
    fn get_error_as_string(&self) -> String {
        match *self {
            TraceError(errno) => posix::strerror(errno),
            _                 => "".to_string(),
        }
    }
 
    fn get_errno(&self) -> int {
        match *self {
            TraceError(errno) => errno,
            _                 => fail!("You can't get an errno from a success value!"),
        }
    }
}
 
fn wrap_result<T: CouldBeAnError>(result: T) -> TraceResult {
    if result.is_error() {
        TraceError(result.get_errno())
    } else {
        TraceOk
    }
}
 
//fn init_trace(child_pid: int) -> TraceResult {
    //match posix::waitpid(child_pid, 0) {
        //posix::WaitPidFailure(errno)       => TraceError(errno),
        //posix::WaitPidSuccess(pid, status) => {
            //if status & posix::SIGTRAP != 0 {
                //let result = ptrace::setoptions(pid, ptrace::TRACEFORK | ptrace::TRACESYSGOOD | ptrace::TRACEEXEC);
                //if result.is_error() {
                    //return wrap_result(result);
                //}
                //wrap_result(ptrace::syscall(pid))
            //} else {
                //TraceError(0) // shit...
            //}
        //},
    //}
//}
 
//fn resume_trace(child_pid: int) -> TraceResult {
    //wrap_result(ptrace::syscall(child_pid))
//}
 
//fn next_trace(callback: &fn(int, TraceEvent) -> bool) -> TraceResult {
    //loop {
        //let result = posix::waitpid(-1, 0);
 
        //match result {
            //posix::WaitPidFailure(errno)       => return TraceError(errno),
            //posix::WaitPidSuccess(pid, status) => {
                //if ((status >> 8) & (0x80 | posix::SIGTRAP)) != 0 {
                    //match ptrace::get_registers(pid) {
                        //Ok(ptrace::UserRegs { orig_rax: syscall_no, rdi: rdi, rsi: rsi, rdx: rdx, rcx: rcx, r8: r8, r9: r9, x }) => {
                            //callback(pid, SystemCall {
                                //syscall_no : syscall_no,
                                //arguments  : ( rdi, rsi, rdx, rcx, r8, r9 ),
                            //});
                        //},
                        //Err(errno) => return TraceError(errno),
                    //}
                //} else {
                    //callback(pid, Other);
                //}
                //let result = resume_trace(pid);
                //if result.is_error() {
                    //return wrap_result(result);
                //}
            //},
        //}
    //}
//}
 
//fn pstrdup(pid: int, addr: *mut libc::c_void) -> String {
    //let mut bytes    = [];
    //let mut mut_addr = addr as word;
 
    //loop  {
        //match ptrace::peektext(pid, mut_addr as *mut libc::c_void) {
            //Err(_)   => break,
            //Ok(word) => {
                //let mut i = 0;
 
                //// XXX I'm not using a for loop because of a bug in Rust
                //while i < sys::size_of::<word>() {
                    //// XXX byte order
                    //let lsb = (word >> (i * 8)) & 0xFF;
                    //if lsb == 0 {
                        //break;
                    //}
                    //bytes.push(lsb as u8);
                    //i += 1;
                //}
            //}
        //}
        //mut_addr += sys::size_of::<word>() as word;
    //}
 
    //str::from_bytes(bytes)
//}
 
//fn get_program_args(pid: int, addr: *libc::c_void) -> ~[~str] {
    //let mut args     = ~[];
    //let mut mut_addr = addr as word;
 
    //loop {
        //match ptrace::peektext(pid, mut_addr as *libc::c_void) {
            //Err(_) | Ok(0) => break,
            //Ok(word)       => {
                //args.push(pstrdup(pid, word as *libc::c_void));
            //}
        //}
 
        //mut_addr += sys::size_of::<word>() as word;
    //}
 
    //args
//}
 
//fn handle_syscall_arguments(pid: int, (_, argv_ptr, _, _, _, _): (word, word, word, word, word, word)) {
    //let argv = get_program_args(pid, argv_ptr as *libc::c_void);
    //io::println(fmt!("executable args: '%?'", argv));
//}
 
//fn run_parent(child_pid: int) -> TraceResult {
    //let result = init_trace(child_pid);
 
    //if result.is_error() {
        //return wrap_result(result);
    //}
 
    //let mut awaiting_return        : HashSet<int> = HashSet::new();
    //let mut seen_first_exec_return : HashSet<int> = HashSet::new();
 
    //let result = for next_trace() |pid, event| {
        //match event {
            //SystemCall { syscall_no: ptrace::syscall::EXECVE, arguments: args } => {
                //if awaiting_return.contains(&pid) {
                    //if seen_first_exec_return.contains(&pid) {
                        //awaiting_return.remove(&pid);
                        //seen_first_exec_return.remove(&pid);
                    //} else {
                        //seen_first_exec_return.insert(pid);
                    //}
                //} else {
                    //handle_syscall_arguments(pid, args);
                    //awaiting_return.insert(pid);
                //}
            //}
            //_ => (),
        //}
    //};
 
    //match result {
        //TraceError(posix::ECHILD) => TraceOk,
        //TraceError(_)             => result,
        //_                         => TraceOk,
    //}
//}
 
//fn main() {
    //let result = posix::fork();
 
    //match result {
        //posix::ForkChild => {
            //let args   = os::args();
            //let result = ptrace::trace_me();
 
            //if result.is_error() {
                //posix::exit(255);
            //}
            //posix::exec(args.tail());
            //posix::exit(255);
        //}
        //posix::ForkFailure(_) => {
            //io::println(fmt!("An error occurred: %s", result.get_error_as_string()));
        //}
        //posix::ForkParent(child_pid) => {
            //let result = run_parent(child_pid);
 
            //if result.is_error() {
                //posix::kill(child_pid, posix::SIGKILL);
                //io::println(fmt!("An error occurred: %s", result.get_error_as_string()));
            //}
        //}
    //}
//}
