#!/bin/sh

case $EMACS_CI in
    emacs-25*)
        PACKAGE_DIR=$(cask package-directory)
        GNUPG_DIR=$PACKAGE_DIR/gnupg
        echo "Importing new elpa key into $GNUPG_DIR"
        mkdir -p $GNUPG_DIR
        chmod 700 $GNUPG_DIR
        gpg --keyserver hkp://keys.gnupg.net --homedir $GNUPG_DIR --recv-keys 066DAFCB81E42C40
        ;;
esac
