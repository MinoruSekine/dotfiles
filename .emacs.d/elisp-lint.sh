#!/bin/sh
EMACS_CMD=emacs
elisplint_output=$($EMACS_CMD -Q --batch --eval "(progn(package-initialize)(require 'elisp-lint)(elisp-lint-file \"$1\"))")
printf "%s\n" "$elisplint_output"
if [ -n "$elisplint_output" ]; then
    exit 1
else
    exit 0
fi
