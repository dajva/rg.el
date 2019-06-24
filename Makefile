PKG_NAME = $(shell cask info | head -1 | cut -f2 -d" ")
PKG_VERSION = $(shell cask version)
PKG_FULL_NAME = $(PKG_NAME)-$(PKG_VERSION)
SOURCES = $(shell cask files)
OBJECTS = $(SOURCES:.el=.elc)
STYLE_CHECK= -L test -L . -l test/style-check.el

all: deps test

test: ert-test style-check build-test package-test

build-test: clean build
	cask clean-elc

clean:
	cask clean-elc

build: $(OBJECTS)

%.elc: %.el
	cask emacs -batch -Q -L . -eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" $<

package-test:
	-@rm -r dist 2> /dev/null || true
	-@rm -r /tmp/$(PKG_FULL_NAME)-elpa 2> /dev/null || true
	cask package
	PKG_FULL_NAME=$(PKG_FULL_NAME) emacs -batch -Q -l test/package-bootstrap.el \
		--eval "(progn (package-install-file (expand-file-name \"dist/$(PKG_FULL_NAME).tar\")) (rg \"rg\" \"elisp\" \"/tmp/$(PKG_FULL_NAME)-elpa\"))"

style-check:
	cask emacs -batch -Q $(STYLE_CHECK) -f run-emacs-lisp-flycheck-and-exit $(SOURCES)

package-lint:
	cask emacs -batch -Q $(STYLE_CHECK) -f run-package-lint-and-exit rg.el

unit-test:
	cask exec ert-runner --pattern rg-unit

integration-test:
	cask exec ert-runner --pattern rg-integration

ert-test:
	cask exec ert-runner --quiet

deps:
	cask install

.PHONY: all test build-test clean package-test style-check package-lint unit-test integration-test ert-test deps
