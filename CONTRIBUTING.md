# Contributing to g-mode

Thank you for considering contributing to g-mode!

## Getting Started

Clone the repository and ensure you have **Emacs 28.1+**:

```sh
git clone https://github.com/brlcad/g-mode.git
cd g-mode
```

## Running Tests

Run the full test suite locally before submitting changes:

```sh
emacs -Q -batch -L . -l g-mode.el -l test/g-mode-test.el -f ert-run-tests-batch-and-exit
```

Tests also run automatically on Linux, macOS, Windows, and FreeBSD via
GitHub Actions on every push and pull request.

## Coding Conventions

- **Lexical binding** — all files use `lexical-binding: t`.
- **`cl-lib`** — use `cl-lib` functions (e.g., `cl-find`, `cl-remove-if`)
  rather than the deprecated `cl` package.
- **Naming** — public commands use `g-mode-` prefix; internal helpers
  use `g-mode--` (double dash).
- **Binary safety** — all binary buffer operations must use unibyte
  buffers and `buffer-substring-no-properties`.
- **Comments** — preserve existing comments and docstrings; add
  docstrings to all new `defun`, `defvar`, and `defconst` forms.

## Package Lint

Before submitting, verify MELPA compatibility:

```sh
emacs -Q -batch \
  --eval '(require (quote package))' \
  --eval '(package-initialize)' \
  --eval '(unless (package-installed-p (quote package-lint)) (package-install (quote package-lint)))' \
  --eval '(require (quote package-lint))' \
  -f package-lint-batch-and-exit g-mode.el
```

## Submitting Changes

1. Fork the repository and create a feature branch.
2. Write or update tests in `test/g-mode-test.el` for your changes.
3. Ensure all tests pass and package-lint is clean.
4. Submit a pull request with a clear description of what and why.

## Architecture Overview

See the [README](README.md#architecture) for the binary-buffer / UI-buffer
architecture.  Key points for contributors:

- The `.g` file data lives in a hidden unibyte buffer (`g-mode--binary-buffer`).
- The visible UI is a `tabulated-list-mode` buffer that reads from the binary buffer.
- Mutations write directly into the binary buffer; the UI refreshes via
  `g-mode--update-ui`.
- Object metadata is cached in `g-mode--objects` and invalidated by a
  tick-based mechanism in `g-mode--refresh-entries`.

## License

By contributing, you agree that your contributions will be licensed
under the [MIT License](LICENSE).
