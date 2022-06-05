PKG_NAME = $(shell cask info | head -1 | cut -f2 -d" ")
PKG_VERSION = $(shell cask version)
PKG_FULL_NAME = $(PKG_NAME)-$(PKG_VERSION)
SOURCES = $(shell cask files)
OBJECTS = $(SOURCES:.el=.elc)
STYLE_CHECK= -L test -L . -l test/style-check.el
DISABLE_DEFALIAS_CHECK= --eval "(defun package-lint--check-defalias (prefix def))"

SPHINX-BUILD = sphinx-build
DOC_DIR = docs
ORG_DOCS= $(wildcard $(DOC_DIR)/*.org)
RST_OUT_DIR = $(DOC_DIR)/rst
RST_DOCS = $(addprefix $(RST_OUT_DIR)/,$(patsubst %.org,%.rst,$(notdir $(ORG_DOCS))))

all: deps test

test: ert-test style-check package-lint build-test

build-test: clean build
	cask clean-elc

$(RST_DOCS): | $(RST_OUT_DIR)

$(RST_OUT_DIR):
	mkdir $(RST_OUT_DIR)

$(RST_OUT_DIR)/%.rst: docs/%.org
	RST_OUT_DIR=$(abspath $(RST_OUT_DIR)) cask emacs --batch -Q -L . -l docs/org-bootstrap.el $< --funcall rg-export-to-rst

rst: $(RST_DOCS)

html: rst
	$(SPHINX-BUILD) -b html $(RST_OUT_DIR) $(RST_OUT_DIR)/_build/html

info: rst
	$(SPHINX-BUILD) -b texinfo $(RST_OUT_DIR) $(RST_OUT_DIR)/_build/info
	make -C $(RST_OUT_DIR)/_build/info
	cp $(RST_OUT_DIR)/_build/info/rgel.info .

docs: html

clean-docs:
	rm $(RST_OUT_DIR)/*.rst
	make -C $(RST_OUT_DIR) clean

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
	cask emacs -batch -Q $(STYLE_CHECK) $(DISABLE_DEFALIAS_CHECK) -f run-package-lint-and-exit rg.el

unit-test:
	cask exec ert-runner --pattern rg-unit

integration-test:
	cask exec ert-runner --pattern rg-integration

ert-test:
	cask exec ert-runner --quiet

deps:
	cask install

.PHONY: all test build-test clean clean-docs package-test style-check package-lint unit-test integration-test ert-test deps docs
