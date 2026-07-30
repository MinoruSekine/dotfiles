#!/bin/sh
if emacsclient -e t > /dev/null 2>&1 ; then
    echo "Emacs server is already available."
else
    emacs --daemon
fi
