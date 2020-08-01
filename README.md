# dotfiles

![](https://github.com/MinoruSekine/dotfiles/workflows/CI/badge.svg?branch=master)

This repository includes:

- Common Emacs settings for each environment
- Makefile to validate their syntax (lint)
- Files for GitHub Actions

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
