#!/bin/sh
if emacsclient -e t > /dev/null 2>&1 ; then
    echo Emacs already stays as daemon.
else
    emacs --daemon
fi
