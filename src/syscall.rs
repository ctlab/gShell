use std::str::SendStr;

#[deriving(Show)]
pub struct Syscall(&'static str);

pub static SYSCALLS : &'static [Syscall] = [
    Syscall("sys_read"),
    Syscall("sys_write"),
    Syscall("sys_open"),
    Syscall("sys_close"),
    Syscall("sys_stat"),
    Syscall("sys_fstat"),
    Syscall("sys_lstat"),
    Syscall("sys_poll"),
    Syscall("sys_lseek"),
    Syscall("sys_mmap"),
    Syscall("sys_mprotect"),
    Syscall("sys_munmap"),
    Syscall("sys_brk"),
    Syscall("sys_rt_sigaction"),
    Syscall("sys_rt_sigprocmask"),
    Syscall("sys_rt_sigreturn"),
    Syscall("sys_ioctl"),
    Syscall("sys_pread64"),
    Syscall("sys_pwrite64"),
    Syscall("sys_readv"),
    Syscall("sys_writev"),
    Syscall("sys_access"),
    Syscall("sys_pipe"),
    Syscall("sys_select"),
    Syscall("sys_sched_yield"),
    Syscall("sys_mremap"),
    Syscall("sys_msync"),
    Syscall("sys_mincore"),
    Syscall("sys_madvise"),
    Syscall("sys_shmget"),
    Syscall("sys_shmat"),
    Syscall("sys_shmctl"),
    Syscall("sys_dup"),
    Syscall("sys_dup2"),
    Syscall("sys_pause"),
    Syscall("sys_nanosleep"),
    Syscall("sys_getitimer"),
    Syscall("sys_alarm"),
    Syscall("sys_setitimer"),
    Syscall("sys_getpid"),
    Syscall("sys_sendfile"),
    Syscall("sys_socket"),
    Syscall("sys_connect"),
    Syscall("sys_accept"),
    Syscall("sys_sendto"),
    Syscall("sys_recvfrom"),
    Syscall("sys_sendmsg"),
    Syscall("sys_recvmsg"),
    Syscall("sys_shutdown"),
    Syscall("sys_bind"),
    Syscall("sys_listen"),
    Syscall("sys_getsockname"),
    Syscall("sys_getpeername"),
    Syscall("sys_socketpair"),
    Syscall("sys_setsockopt"),
    Syscall("sys_getsockopt"),
    Syscall("sys_clone"),
    Syscall("sys_fork"),
    Syscall("sys_vfork"),
    Syscall("sys_execve"),
    Syscall("sys_exit"),
    Syscall("sys_wait4"),
    Syscall("sys_kill"),
    Syscall("sys_uname"),
    Syscall("sys_semget"),
    Syscall("sys_semop"),
    Syscall("sys_semctl"),
    Syscall("sys_shmdt"),
    Syscall("sys_msgget"),
    Syscall("sys_msgsnd"),
    Syscall("sys_msgrcv"),
    Syscall("sys_msgctl"),
    Syscall("sys_fcntl"),
    Syscall("sys_flock"),
    Syscall("sys_fsync"),
    Syscall("sys_fdatasync"),
    Syscall("sys_truncate"),
    Syscall("sys_ftruncate"),
    Syscall("sys_getdents"),
    Syscall("sys_getcwd"),
    Syscall("sys_chdir"),
    Syscall("sys_fchdir"),
    Syscall("sys_rename"),
    Syscall("sys_mkdir"),
    Syscall("sys_rmdir"),
    Syscall("sys_creat"),
    Syscall("sys_link"),
    Syscall("sys_unlink"),
    Syscall("sys_symlink"),
    Syscall("sys_readlink"),
    Syscall("sys_chmod"),
    Syscall("sys_fchmod"),
    Syscall("sys_chown"),
    Syscall("sys_fchown"),
    Syscall("sys_lchown"),
    Syscall("sys_umask"),
    Syscall("sys_gettimeofday"),
    Syscall("sys_getrlimit"),
    Syscall("sys_getrusage"),
    Syscall("sys_sysinfo"),
    Syscall("sys_times"),
    Syscall("sys_ptrace"),
    Syscall("sys_getuid"),
    Syscall("sys_syslog"),
    Syscall("sys_getgid"),
    Syscall("sys_setuid"),
    Syscall("sys_setgid"),
    Syscall("sys_geteuid"),
    Syscall("sys_getegid"),
    Syscall("sys_setpgid"),
    Syscall("sys_getppid"),
    Syscall("sys_getpgrp"),
    Syscall("sys_setsid"),
    Syscall("sys_setreuid"),
    Syscall("sys_setregid"),
    Syscall("sys_getgroups"),
    Syscall("sys_setgroups"),
    Syscall("sys_setresuid"),
    Syscall("sys_getresuid"),
    Syscall("sys_setresgid"),
    Syscall("sys_getresgid"),
    Syscall("sys_getpgid"),
    Syscall("sys_setfsuid"),
    Syscall("sys_setfsgid"),
    Syscall("sys_getsid"),
    Syscall("sys_capget"),
    Syscall("sys_capset"),
    Syscall("sys_rt_sigpending"),
    Syscall("sys_rt_sigtimedwait"),
    Syscall("sys_rt_sigqueueinfo"),
    Syscall("sys_rt_sigsuspend"),
    Syscall("sys_sigaltstack"),
    Syscall("sys_utime"),
    Syscall("sys_mknod"),
    Syscall("sys_uselib"),
    Syscall("sys_personality"),
    Syscall("sys_ustat"),
    Syscall("sys_statfs"),
    Syscall("sys_fstatfs"),
    Syscall("sys_sysfs"),
    Syscall("sys_getpriority"),
    Syscall("sys_setpriority"),
    Syscall("sys_sched_setparam"),
    Syscall("sys_sched_getparam"),
    Syscall("sys_sched_setscheduler"),
    Syscall("sys_sched_getscheduler"),
    Syscall("sys_sched_get_priority_max"),
    Syscall("sys_sched_get_priority_min"),
    Syscall("sys_sched_rr_get_interval"),
    Syscall("sys_mlock"),
    Syscall("sys_munlock"),
    Syscall("sys_mlockall"),
    Syscall("sys_munlockall"),
    Syscall("sys_vhangup"),
    Syscall("sys_modify_ldt"),
    Syscall("sys_pivot_root"),
    Syscall("sys__sysctl"),
    Syscall("sys_prctl"),
    Syscall("sys_arch_prctl"),
    Syscall("sys_adjtimex"),
    Syscall("sys_setrlimit"),
    Syscall("sys_chroot"),
    Syscall("sys_sync"),
    Syscall("sys_acct"),
    Syscall("sys_settimeofday"),
    Syscall("sys_mount"),
    Syscall("sys_umount2"),
    Syscall("sys_swapon"),
    Syscall("sys_swapoff"),
    Syscall("sys_reboot"),
    Syscall("sys_sethostname"),
    Syscall("sys_setdomainname"),
    Syscall("sys_iopl"),
    Syscall("sys_ioperm"),
    Syscall("sys_create_module"),
    Syscall("sys_init_module"),
    Syscall("sys_delete_module"),
    Syscall("sys_get_kernel_syms"),
    Syscall("sys_query_module"),
    Syscall("sys_quotactl"),
    Syscall("sys_nfsservctl"),
    Syscall("sys_getpmsg"),
    Syscall("sys_putpmsg"),
    Syscall("sys_afs_syscall"),
    Syscall("sys_tuxcall"),
    Syscall("sys_security"),
    Syscall("sys_gettid"),
    Syscall("sys_readahead"),
    Syscall("sys_setxattr"),
    Syscall("sys_lsetxattr"),
    Syscall("sys_fsetxattr"),
    Syscall("sys_getxattr"),
    Syscall("sys_lgetxattr"),
    Syscall("sys_fgetxattr"),
    Syscall("sys_listxattr"),
    Syscall("sys_llistxattr"),
    Syscall("sys_flistxattr"),
    Syscall("sys_removexattr"),
    Syscall("sys_lremovexattr"),
    Syscall("sys_fremovexattr"),
    Syscall("sys_tkill"),
    Syscall("sys_time"),
    Syscall("sys_futex"),
    Syscall("sys_sched_setaffinity"),
    Syscall("sys_sched_getaffinity"),
    Syscall("sys_set_thread_area"),
    Syscall("sys_io_setup"),
    Syscall("sys_io_destroy"),
    Syscall("sys_io_getevents"),
    Syscall("sys_io_submit"),
    Syscall("sys_io_cancel"),
    Syscall("sys_get_thread_area"),
    Syscall("sys_lookup_dcookie"),
    Syscall("sys_epoll_create"),
    Syscall("sys_epoll_ctl_old"),
    Syscall("sys_epoll_wait_old"),
    Syscall("sys_remap_file_pages"),
    Syscall("sys_getdents64"),
    Syscall("sys_set_tid_address"),
    Syscall("sys_restart_syscall"),
    Syscall("sys_semtimedop"),
    Syscall("sys_fadvise64"),
    Syscall("sys_timer_create"),
    Syscall("sys_timer_settime"),
    Syscall("sys_timer_gettime"),
    Syscall("sys_timer_getoverrun"),
    Syscall("sys_timer_delete"),
    Syscall("sys_clock_settime"),
    Syscall("sys_clock_gettime"),
    Syscall("sys_clock_getres"),
    Syscall("sys_clock_nanosleep"),
    Syscall("sys_exit_group"),
    Syscall("sys_epoll_wait"),
    Syscall("sys_epoll_ctl"),
    Syscall("sys_tgkill"),
    Syscall("sys_utimes"),
    Syscall("sys_vserver"),
    Syscall("sys_mbind"),
    Syscall("sys_set_mempolicy"),
    Syscall("sys_get_mempolicy"),
    Syscall("sys_mq_open"),
    Syscall("sys_mq_unlink"),
    Syscall("sys_mq_timedsend"),
    Syscall("sys_mq_timedreceive"),
    Syscall("sys_mq_notify"),
    Syscall("sys_mq_getsetattr"),
    Syscall("sys_kexec_load"),
    Syscall("sys_waitid"),
    Syscall("sys_add_key"),
    Syscall("sys_request_key"),
    Syscall("sys_keyctl"),
    Syscall("sys_ioprio_set"),
    Syscall("sys_ioprio_get"),
    Syscall("sys_inotify_init"),
    Syscall("sys_inotify_add_watch"),
    Syscall("sys_inotify_rm_watch"),
    Syscall("sys_migrate_pages"),
    Syscall("sys_openat"),
    Syscall("sys_mkdirat"),
    Syscall("sys_mknodat"),
    Syscall("sys_fchownat"),
    Syscall("sys_futimesat"),
    Syscall("sys_newfstatat"),
    Syscall("sys_unlinkat"),
    Syscall("sys_renameat"),
    Syscall("sys_linkat"),
    Syscall("sys_symlinkat"),
    Syscall("sys_readlinkat"),
    Syscall("sys_fchmodat"),
    Syscall("sys_faccessat"),
    Syscall("sys_pselect6"),
    Syscall("sys_ppoll"),
    Syscall("sys_unshare"),
    Syscall("sys_set_robust_list"),
    Syscall("sys_get_robust_list"),
    Syscall("sys_splice"),
    Syscall("sys_tee"),
    Syscall("sys_sync_file_range"),
    Syscall("sys_vmsplice"),
    Syscall("sys_move_pages"),
    Syscall("sys_utimensat"),
    Syscall("sys_epoll_pwait"),
    Syscall("sys_signalfd"),
    Syscall("sys_timerfd_create"),
    Syscall("sys_eventfd"),
    Syscall("sys_fallocate"),
    Syscall("sys_timerfd_settime"),
    Syscall("sys_timerfd_gettime"),
    Syscall("sys_accept4"),
    Syscall("sys_signalfd4"),
    Syscall("sys_eventfd2"),
    Syscall("sys_epoll_create1"),
    Syscall("sys_dup3"),
    Syscall("sys_pipe2"),
    Syscall("sys_inotify_init1"),
    Syscall("sys_preadv"),
    Syscall("sys_pwritev"),
    Syscall("sys_rt_tgsigqueueinfo"),
    Syscall("sys_perf_event_open"),
    Syscall("sys_recvmmsg"),
    Syscall("sys_fanotify_init"),
    Syscall("sys_fanotify_mark"),
    Syscall("sys_prlimit64"),
    Syscall("sys_name_to_handle_at"),
    Syscall("sys_open_by_handle_at"),
    Syscall("sys_clock_adjtime"),
    Syscall("sys_syncfs"),
    Syscall("sys_sendmmsg"),
    Syscall("sys_setns"),
    Syscall("sys_getcpu"),
    Syscall("sys_process_vm_readv"),
    Syscall("sys_process_vm_writev")
];


pub fn get_syscall_name(i : uint) -> Syscall {
    SYSCALLS[i]
}

pub fn get_syscall_name_as_str(i : uint) -> SendStr {
    let Syscall(name) = SYSCALLS[i]; 
    name.into_maybe_owned()
}


