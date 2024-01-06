#!/bin/sh
emacs -nw --batch --eval "(print load-path #'external-debugging-output)"
