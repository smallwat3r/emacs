# smallwat3r's Emacs

Personal Emacs configuration in vanilla elisp. Requires Emacs 30+.

## Installation

Clone to `~/.emacs.d` and start Emacs. Packages will install automatically on
first run via [straight.el](https://github.com/radian-software/straight.el).

## Package management

Packages are managed with straight.el, which provides version locking. The
lockfile at `straight/versions/default.el` is version-controlled for
reproducible installs.

### From Emacs

```
M-x straight-pull-all        Update all packages
M-x straight-freeze-versions Save versions to lockfile
M-x straight-thaw-versions   Restore from lockfile
M-x straight-rebuild-all     Rebuild all packages
```

### From CLI

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
