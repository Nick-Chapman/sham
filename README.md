# sham

`sham` is a command shell which runs on _MeNicks_, a simulated unix-style OS.

To build, test and run, type `stack run`.

### Goal is to better understand:

- IO redirection
- file descriptors
- pipes
- multi-process OS
- files & file systems


### Everything above has broadly been achieved. We support:

#### `MeNicks`:
- cooperative multi-processing, with fork/exec/wait
- file-descriptors, with close/dup
- open-file table, with ref-counted entries ('files' - everything's a file!)
- system calls which may block (i.e. read/write to empty/full pipes)
- simple flat file-system with append/truncate write-mode
- pipefs

#### `sham`:
- console/command execution; builtins: exit, source (.)
- predefined commands: sham bins cat echo grep head ls man ps rev xargs
- script files
- redirection to/from files/descriptors: < input >> foo 2>&1
- pipes (|)
- backgrounding (&)
- my pid ($$)
- builtin "echo" (runs in process). critical to making "yes | head" work! (give absence of exit codes)
- command line arguments: $0 $1 $2 etc
- predefined scripts: help, yes, bomb, yes, cp
- comments (#)

### Ideas/Plans
- fork initial "sham" from "init"
- predefs commands for arithmetic: sum, ifeq
- read (syntax or command?)
- scripts: head N, drop N, countdown N, wc-l
- lsof
- rm
- quoting (''), sham-vars (foo=something, $foo)
- grouping and sequencing: (...), foo;bar
- introduce tty, which is connected by 3 pipes
- restructure: Interaction stays at level of Prog (not SysCall)
- avoid special handling of Console in SysCall Target
- grep -v
- ls -l
- aysnc operation via "Pause" in Prog and Interaction. Clocked externally
- predefined: mktmp, kill, sleep
- exit codes
- scripts: withtmp, ignore, uptime, alarm, cron, killall
