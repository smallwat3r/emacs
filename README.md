# smallwat3r's Emacs

Personal Emacs configuration in vanilla elisp. Requires Emacs 30+.

![Emacs screenshot](images/emacs.png)

After years of using [Doom Emacs](https://github.com/doomemacs/doomemacs), many
keybindings and workflows here are influenced by it.

## Installation

Clone or symlink to `~/.emacs.d` and start Emacs. Packages install automatically
on first run.

## Package management

Packages are managed with [straight.el](https://github.com/radian-software/straight.el),
which provides version locking. The lockfile at `straight/versions/default.el` is
version-controlled for reproducible installs.

Run `make` to see available commands:

```
$ make
  help       Show this help
  install    Install packages (first run or from lockfile)
  update     Pull latest versions and freeze
  freeze     Save current package versions to lockfile
  thaw       Restore packages to lockfile versions
  rebuild    Rebuild all packages
  clean      Remove all installed packages
```
