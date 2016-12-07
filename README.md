# rg.el

Ripgrep is a replacement for both grep like (search one file) and ag
like (search many files) tools. It's fast and versatile and written in
Rust. This package let you run ``ripgrep like`` like ``grep`` from within Emacs.

## Usage

<kbd>M-x rg</kbd> - This works the same way as <kbd>M-x rgrep</kbd>,
i.e. you get an interactive prompt to enter search details. Universal
argument can be used as for ``rgrep``.

## Customize

``ripgrep`` has its own builtin mappings from type names to file name
patterns that can be selected on invocation of ``rg``. It's also
possible to add custom patterns by customizing ``rg-custom-type-flags``.

## License

See [LICENSE](LICENSE).
