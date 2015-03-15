Directory layout:

rX = commitX/work

```
project-root/  
├── .gShell/
│   └── commits/
│       ├── commit1/
│       │   ├─ work/ # raw files
│       │   ├─ changed files/
│       │   ├─ message
│       │   ├─ what was read
│       │   └─ ...
│       └── commit2/
├── work-id1 # unionfs mount r2/work:r1/work:r0/work
├── work-id2 # per open shell
└── ...
```

## Workflows

### Initial
* gshell init project-root # creates new project-root directory with .gshell

### Existing project
* gshell enter project-root # opens new shell, creates new unique identifier id1, creates RO-work-id1 by unionfs
* command1 args1... 
** on precmd, we create new revision r3, remount work-id1 r3=RW:r2:...
** exec command
** on postcmd, we create commit3 with message fc ..., remount work-id1 RO, cd `pwd`

### Close
* unmount work-id1
