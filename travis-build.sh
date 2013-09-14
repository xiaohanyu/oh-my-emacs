#!/bin/bash

travis_echo () {
    echo "==== $1 ===="
}

get_basic_env_info () {
    travis_echo "CI system version:"
    uname -a
    travis_echo "CI environment variables:"
    export
}

get_basic_env_info

ln -s $TRAVIS_BUILD_DIR $HOME/.emacs.d

ls -lha $HOME

ls -ld $HOME/.emacs.d

$EMACS --batch -nw --eval '(require (quote org))' --eval '(setq ome-dir "/home/travis/.emacs.d")' --eval '(message "%s, %s" emacs-version org-version)' --eval '(org-babel-load-file "/home/travis/.emacs.d/ome.org")'
