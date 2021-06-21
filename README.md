# rg.el

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg?style=flat)](LICENSE)
[![MELPA Stable](https://stable.melpa.org/packages/rg-badge.svg)](https://stable.melpa.org/#/rg)
[![MELPA](http://melpa.org/packages/rg-badge.svg)](http://melpa.org/#/rg)
[![Build Status](https://github.com/dajva/rg.el/actions/workflows/build.yml/badge.svg?branch=master)](https://github.com/dajva/rg.el/actions/workflows/test.yaml)
[![Coverage Status](https://coveralls.io/repos/github/dajva/rg.el/badge.svg)](https://coveralls.io/github/dajva/rg.el)

Use [ripgrep](https://github.com/BurntSushi/ripgrep) in Emacs.

Ripgrep is a replacement for both grep like (search one file) and ag
like (search many files) tools. It's fast and versatile and written in
Rust. For some introduction and benchmarks, see
[ripgrep is faster than {grep, ag, git grep, ucg, pt, sift}](http://blog.burntsushi.net/ripgrep/).

![screenshot](screenshot.png)

## Installation

This package is available on
[MELPA Stable](https://stable.melpa.org/#/rg) and
[MELPA](http://melpa.org/#/rg). Install with `M-x package-install`
<kbd>RET</kbd> `rg` from within Emacs.

It is also available in GNU Guix as [emacs-rg](https://guix.gnu.org/packages/emacs-rg-1.8.1/).
Install with `guix package -i emacs-rg`.

If you want to install manually just put `rg.el` and the rest of the
elisp files somewhere in your load path and add require the package:

``` el
(require 'rg)
```

`rg` and friends are autoloaded symbols which means it's also possible
to defer loading if you have autoloading setup.

### Setup key bindings
This will setup the default key bindings in a non lazy way. If you
care about startup performance see the next example.

``` el
(rg-enable-default-bindings)
```

See
[documentation](https://rgel.readthedocs.io) for how to handle lazy loading.


### Use old defaults
`rg.el` 2.0.0 will use new default settings to improve look and feel,
more consistent key bindings etc. If you want to use the old defaults
add this to your `init.el`:

``` el
(rg-use-old-defaults)
```

### rg-menu

If you prefer to use a [magit](https://github.com/magit/magit) like
interface as a complement to regular key maps, replace
`(rg-enable-default-bindings)` with `(rg-enable-menu)`. The menus are
built with [transient](https://github.com/magit/transient), which
means that the menus can be modified in the same way as in magit.

## Documentation
Info documentation is included in the package.
Online documentation: https://rgel.readthedocs.io

## Contribute

- Install [cask](http://cask.github.io/).
- Install dependencies:

``` Shell
make deps
```
- Run tests:

``` Shell
make test
```


## License

See [LICENSE](LICENSE).
