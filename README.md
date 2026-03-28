# g-mode

An Emacs major mode for reading, inspecting, and editing BRL-CAD `.g` binary geometry files.

## Status
In early development.

## Features (Planned)
- Native Emacs Lisp parsing of the `.g` binary database format (No external `BRL-CAD` installations or C/C++ libraries required).
- Read-only inspection and object summary.
- Key-Value metadata viewing and editing.
- Corruption recovery mode.

## Requirements
- Emacs 28.1+ (uses modern `bindat` macro API).
