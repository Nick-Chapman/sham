# sham

`sham` is a command shell which runs on _MeNicks_, a simulated unix-style OS.

To build, test and run, type `stack run`.

No real files will be harmed during the execution of `sham`.

_It's not a shell, it's just a sham._

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
- sham: console / script file interpreter
- predefined commands: cat echo grep ls man ps rev xargs
- builtin: echo, exit, exec, source (.)
- redirection to/from files/descriptors: < input >> foo 2>&1
- pipes (|)
- backgrounding (&)
- my pid ($$)
- command line arguments: $0 $1 $2 etc
- sham, sham -c, sham SCRIPT
- scripts: help yes bomb cp
- comments (#)
- syntax: read VAR
- syntax: if PRED COMMAND   (PRED: WORD = WORD | WORD != WORD)
- predefined command: sum N N ... N
- scripts: countdown N, head N
- predefined: lsof (run 'lsof | cat' to see something)
- ls -a
- fork initial "sham" from "init"

### Ideas/Plans
- general vars (foo=something, $foo); + env for pasing info to commands. use for 'sham' level
- rm, mv
- quoting ('')
- grouping and sequencing: (...), foo;bar
- introduce tty, which is connected by 3 pipes
- scripts: drop N, wc-l
- grep -v, ls -l
- restructure: Interaction stays at level of Prog (not SysCall)
- avoid special handling of Console in SysCall Target
- aysnc operation via "Pause" in Prog and Interaction. Clocked externally
- predefined: mktmp, kill, sleep
- exit codes
- scripts: withtmp, ignore, uptime, alarm, cron, killall
