#!/bin/sh
emacs -nw --batch --eval "(print (system-name) #'external-debugging-output)"
