[![Build Status](https://travis-ci.org/ctlab/gShell.svg)](https://travis-ci.org/ctlab/gShell)
### Build
* Using [nix](https://nixos.org/nix/):
```bash
nix-build
```
* Using [cabal](https://www.haskell.org/cabal/):
```bash
cabal sandbox init
cabal install --only-dependencies
cabal build
```

### preRun
```bash
export GSHELL_EXECUTABLE=/path/to/gShell && source /path/to/gshell.zsh
```
We use `export` because we haven't installed `gShell` to a target machine, so it's
unavailable in `$PATH`.

### Run
[![asciicast](https://asciinema.org/a/0bnn9ke4k22dyo4gz7z5ug1h6.png)](https://asciinema.org/a/0bnn9ke4k22dyo4gz7z5ug1h6)

* Make directory for your future work.
```
mkdir my-testing
```
* Init gshell in it.
```
gshell init my-testing
```
* Enter this directory using gshell.
Please note, that new `zsh` session will be opened.
```
gshell enter my-testing
```
* Hack!
* Exit project by typing `exit` in your terminal.
  * Working directory will be auto unmounted.
* Clean project if you need by using:
```
gshell clear my-testing
```

### Available commands:
* **init**     — Init gshell
* **enter**    — Enter project with gshell inited
* **clear**    — Unmount workspaces and remove data
* **push**     — Push changes to master
* **pull**     — Get and apply changes from master
* **commit**   — Commit current action with message
* **checkout** — Go to a specific revision
* **rollback** — Undo changes
* **enterRev** — Enter project with gshell inited at a specific revision
* **log**      — Show log for current workspace
* **graph**    — Show graph from current workspace
* **makefile** — Output script to reproduce workspace
* **off**      — Temporary disable gshell
* **on**       — Temporary enable gshell
