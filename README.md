# rg.el

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg?style=flat)](LICENSE)
[![MELPA Stable](https://stable.melpa.org/packages/rg-badge.svg)](https://stable.melpa.org/#/rg)
[![MELPA](http://melpa.org/packages/rg-badge.svg)](http://melpa.org/#/rg)
[![Build Status](https://travis-ci.org/dajva/rg.el.svg?branch=master)](https://travis-ci.org/dajva/rg.el)

Use [ripgrep](https://github.com/BurntSushi/ripgrep) in Emacs.

Ripgrep is a replacement for both grep like (search one file) and ag
like (search many files) tools. It's fast and versatile and written in
Rust. For some introduction and benchmarks, see
[ripgrep is faster than {grep, ag, git grep, ucg, pt, sift}](http://blog.burntsushi.net/ripgrep/).

This package let you run `ripgrep` like `grep` from within Emacs.


## Installation

This package is available on
[MELPA Stable](https://stable.melpa.org/#/rg) and
[MELPA](http://melpa.org/#/rg). Install with `M-x package-install`
<kbd>RET</kbd> `rg` from within Emacs.

If you want to install manually just put `rg.el` somewhere in your
load path and add this to `init.el`

``` el
(require 'rg)
(global-set-key (kbd "M-s") 'rg)
```

`rg` is an autoloaded symbol so it's also possible to defer loading if
you have autoloading setup.


## Usage

### Main entry point - `rg`
Invoke by `M-x rg`. This works the same way as `M-x rgrep`,
i.e. you get an interactive prompt to enter search details. Universal
argument can be used as for `rgrep`.

### Project search - `rg-project`
`M-x rg-project` searches in a project defined by
[projectile](https://github.com/bbatsov/projectile),
[find-file-in-project](https://github.com/technomancy/find-file-in-project)
or a `vc-backend`.

### Do what I mean - `rg-dwim`
`M-x rg-dwim` searches for _thing at point_ in a project in all files
with the same type alias as the current buffer file.

### Type aliases

[ripgrep](https://github.com/BurntSushi/ripgrep) has its own built in
mappings from type names to file name patterns that can be selected
from `rg.el` on invocation of `rg`. It's also possible to specify a
custom file name pattern in the files prompt.

There are some aliases with special meanings in `rg.el`.

| Name | Meaning |
|-----|--------|
| _all_ | all defined types including `rg-custom-type-aliases` |
| _everything_ | all files, i.e. running rg without `--type` flag |
| _custom_ | used internally in ``rg.el`` for mapping custom globs. Do not use this in `rg-custom-type-aliases` |

### The \*rg\* buffer
The `rg` results buffer has bindings for modification of the last
search for quick reruns with refined parameters.

| Binding | Description |
|-----|--------|
| d | Change directory |
| f | Change file pattern |
| r | Change search string |
| i | Toggle `--no-ignore` flag |
| c | Toggle case insensitive setting |
| s | Save search result to unique name |
| S | Save search result, prompt for name |

## Customize

### rg-custom-type-aliases
Custom type patterns as for rgrep are supported via customizing of
`rg-custom-type-aliases`.

```el
(setq rg-custom-type-aliases
  '(("foo" .    "*.foo *.bar")
    ("baz" .    "*.baz *.qux")))
```

### rg-command-line-flags
Additional command line flags that will be appended to the ripgrep
command line.

### rg-define-toggle
This is a macro that can be used to define custom `ripgrep` flag
toggling functions in the rg result buffer. The macro takes the flag
(and potential value) as an argument and optionally binds the toggle
function to a key and enable the flag by default.

The function defined by this macro will be named as the flag name
stripped with leading dashes and prefixed with `rg-custom-toggle-flag-`.

```el
(rg-define-toggle "-uu" "I" t)

```
Creates a function named `rg-custom-toggle-flag-uu` that is on by
default and bound to <kbd>I</kbd> in `rg` result buffer.

``` el
(rg-define-toggle "--context 3" (kbd "C-c c"))
```
Creates a function named `rg-custom-toggle-flag-context` that is off by
default and bound to <kbd>C-c c</kbd> in `rg` result buffer.


## Contribute

- Install [cask](http://cask.github.io/).
- Install dependencies:

``` Shell
make install
```
- Run tests:

``` Shell
make test
```


## License

See [LICENSE](LICENSE).
