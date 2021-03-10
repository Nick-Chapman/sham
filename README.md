# bash-sim

Simulate a bash-style shell, coded on top of an unix-style OS.

### Goal is to better understand:

- IO redirection
- file descriptors
- pipes
- multi-process OS
- files & file systems


### Everything above has broadly been achieved. We support:

#### OS:
- cooperative multi-processing, with fork/exec/wait
- file-descriptors, with close/dup
- open-file table, with ref-counted entries ('files' - everything's a file!)
- system calls which may block (i.e. read/write to empty/full pipes)
- simple flat file-system with append/truncate write-mode
- pipefs

#### Bash:
- console/command execution; builtins: exit, source (.)
- predefined commands: bash bins cat echo grep head ls man ps rev xargs
- script files
- redirection to/from files/descriptors: < input >> foo 2>&1
- pipes (|)
- backgrounding (&)
- my pid ($$)


### Ideas/Plans
- exec
- use exec in "y" script, so it terminates properly on EPIPE
- fork initial "bash" from "init"
- lsof
- rm
- command line arguments: $0 $1 etc
- comments (#), quoting (''), bash-vars (foo=something, $foo)
- grouping and sequencing: (...), foo;bar
- introduce tty, which is connected by 3 pipes
- restructure: Interaction stays at level of Prog (not SysCall)
- avoid special handling of Console in SysCall Target
- grep -v
- ls -l
- read (bash syntax or command?)
- arithmetic: sum, ifeq (commands)
- aysnc operation via "Pause" in Prog and Interaction. Clocked externally
- predefined: mktmp, kill, sleep
- scripts: head, drop, wc-l, count, withtmp, ignore, uptime, alarm, cron, killall
