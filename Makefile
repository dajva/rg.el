EMACS ?= emacs

PKG_NAME = $(shell cask info | head -1 | cut -f2 -d" ")
PKG_VERSION = $(shell cask version)
PKG = $(PKG_NAME)-$(PKG_VERSION).el

test: build-test package-test

build-test:
	cask clean-elc
	cask exec $(EMACS) -batch -Q -L . -eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" *.el

package-test:
	-@rm -r dist 2> /dev/null || true
	cask package
	cask eval "(package-install-file \"dist/$(PKG)\")"

install:
	cask install

.PHONY: build-test package-test test install
