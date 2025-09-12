# dotfiles

![](https://github.com/MinoruSekine/dotfiles/actions/workflows/Emacs.yml/badge.svg?branch=master )
![](https://github.com/MinoruSekine/dotfiles/actions/workflows/shell.yml/badge.svg?branch=master )

This repository includes:

- Common Emacs settings for each environment
- Makefile to validate their syntax (lint)
- Files for GitHub Actions

# Summary of contents in this repository

## For Emacs

### `.emacs.d/my-init.el`

- Configurations
- Installing packages automatically
  - Official packages
  - MELPA
  - Emacs Wiki
    - If `curl` exists on the system,
	  use `curl` to download elisp files instead of `url-copy-file`
- Periodic upgrade packages at exiting Emacs

#### Configurations

- Garbage collection threshold
- Backup files
- Case sensitivity for completion
- font-lock
- fonts
  - Adjusting font size for DPI awareness
- Frame position and size by DPI
- Emacs server
- Mode line
- Sharing configuration for both GUI and CUI Emacs
- Support .editorconfig file
- Tab configuration for files without tabs

#### Configurations for modes

- CEDET (semantic)
- dired
- emacs-lisp-mode
- eshell
  - Disable pager for some commands
  - Import bash alias into eshell
- flycheck
- moccur
- plantuml-mode
- rainbow-delimiters-mode
- tempbuf-mode
- treesit
- whitespace-mode

### `supplemental/windows/emacsdaemon.bat`

- BAT file to launch Emacs as daemon on Windows

## Supplemental scripts for login scripts

### `.profile/emacs_daemon.sh`

- Run Emacs as daemon only if not available

### `.profile/keychain_init.sh`

- Initialize `keychain` for ssh
  - For `keychain` command, not for Keychain on macOS

# Use settings for Emacs

## Features

- Install necessary packages with packages.el if not available
- Independent from tools outside of Emacs
  - On Windows, it is not necessary to install cygwin, mingw, or compatible something

## How to use

### Check out with anyway which you like

In this chapter,
check out to `~/dotfiles` for example.

```
$ mkdir -p ~/dotfiles
$ cd ~/dotfiles
$ git clone URL_of_this_repository
```

### How to use settings for Emacs

Add the next line into `~/.emacs.d/init.el`.

```
(load-file "~/dotfiles/.emacs.d/my-init.el")
```

If you don't need any additonal settings except in `~/dotfiles/.emacs.d/my-init.el`
and your environment supports symbolic link,
you can link to it instead of your own `~/.emacs.d/init.el`.

```
$ ln -s ~/dotfiles/.emacs.d/my-init.el ~/.emacs.d/init.el
```

## Hook

### `my-after-ede-setup-hook`
This is called after called `(global-ede-mode t)`.
So environment dependent EDE setup like as `ede-cpp-root-project` can be hooked to here.

```
(load-file "~/dotfiles/.emacs.d/my-init.el")

;; Settings only for this environment.
(add-hook 'my-after-ede-setup-hook
	  (lambda ()
	    (ede-cpp-root-project "libfixedpointnumber"
				  :file "~/work/libfixedpointnumber/Makefile"
				  :include-path '("/include")
				  :system-include-path '("/usr/local/include/gtest/"
							 "/usr/local/include/gmock/")
				  :compile-command "time nice make -k -j check run-test")
	    ))
```

# Supplemental scripts

## For `.profile`

- Scripts in `.profile` support configuration in `~/.profile`

### How to use each script


- Add the following line into your `.profile`
  - As case of `keychain_init.sh`
    and this repository checked out to `~/`...

```
. "$HOME/dotfiles/.profile/keychain_init.sh"
```

### `.profile/emacs_daemon.sh`

- Launch Emacs as daemon mode only if not launched yet

### `.profile/keychain_init.sh`

- Initialize [keychain](https://www.funtoo.org/Keychain) for ssh
