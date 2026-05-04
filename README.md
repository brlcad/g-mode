# g-mode

[![Linux](https://img.shields.io/github/actions/workflow/status/brlcad/g-mode/test.yml?job=test-linux&label=Linux)](https://github.com/brlcad/g-mode/actions/workflows/test.yml)
[![macOS](https://img.shields.io/github/actions/workflow/status/brlcad/g-mode/test.yml?job=test-macos&label=macOS)](https://github.com/brlcad/g-mode/actions/workflows/test.yml)
[![Windows](https://img.shields.io/github/actions/workflow/status/brlcad/g-mode/test.yml?job=test-windows&label=Windows)](https://github.com/brlcad/g-mode/actions/workflows/test.yml)
[![FreeBSD](https://img.shields.io/github/actions/workflow/status/brlcad/g-mode/test.yml?job=test-freebsd&label=FreeBSD)](https://github.com/brlcad/g-mode/actions/workflows/test.yml)

An Emacs major mode for inspecting, browsing, and editing BRL-CAD `.g`
binary database files natively — no BRL-CAD installation required.

## Overview

`g-mode` provides a tabular UI for navigating BRL-CAD `.g` geometry
database files, implementing its own binary parser in pure Emacs Lisp.
Opening a `.g` file presents a searchable, sortable table of database
records.  From there you can inspect object properties, rename
objects, delete objects, reorder objects, compact the file to reclaim
space, and recover from certain corruption conditions like malformed
headers or corrupt object spans, all without leaving Emacs.

## Requirements

- Emacs 28.1+ (uses modern `bindat` API).

## Installation

Clone this repository and add it to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/g-mode")
(require 'g-mode)
```

Then open any `.g` file and `g-mode` activates automatically.

## Architecture

The binary file buffer is kept pristine and hidden.  A separate
`*g: <filename>*` UI buffer (derived from `tabulated-list-mode`)
displays the object listing.  All mutations (delete, rename) write
directly into the binary buffer; the UI refreshes from it.

```
 .g file on disk
       |
       v
 Binary Buffer (unibyte, read-only)
       |
       v
 g-mode--scan-buffer  -->  Object metadata alists
       |
       v
 *g: filename* UI Buffer (tabulated-list-mode)
       |
       v
 Keybindings: View / Delete / Rename / GC
       |
       v
 Mutations write back to Binary Buffer
```

## Keybindings

### Navigation & Inspection

| Key         | Command                         | Description                              |
|-------------|---------------------------------|------------------------------------------|
| `v` / `RET` | `g-mode-view-object`            | Open detailed property inspector         |
| `o`         | `g-mode-view-object-other-window`| Open inspector in another window         |
| `f` / `/`   | `g-mode-filter`                 | Filter visible objects by name/regex     |
| `h`         | `g-mode-toggle-show-deleted`    | Toggle visibility of Free Space objects  |
| `g`         | `g-mode-revert`                 | Re-scan the database from disk           |
| `q`         | `quit-window`                   | Close the UI buffer                      |
| `?`         | `g-mode-help`                   | Show keybinding help                     |

### Modification, Reordering & Maintenance

| Key         | Command                         | Description                              |
|-------------|---------------------------------|------------------------------------------|
| `d`         | `g-mode-delete-object`          | Mark object as Free Space (DLI=2)        |
| `R`         | `g-mode-rename-object`          | Rename object (inline or append)         |
| `C`         | `g-mode-copy-object`            | Create a copy of the selected object     |
| `M-up`      | `g-mode-move-up`                | Shift record up in the binary layout     |
| `M-down`    | `g-mode-move-down`              | Shift record down in the binary layout   |
| `G` / `x`   | `g-mode-garbage-collect`        | Compact file, reclaiming deleted space   |
| `C-_`       | `g-mode-undo`                   | Undo last mutation (syncs binary & UI)   |
| `s`         | `save-buffer`                   | Save the underlying binary database      |

### Marking (Multi-Select)

| Key         | Command                         | Description                              |
|-------------|---------------------------------|------------------------------------------|
| `m`         | `g-mode-mark`                   | Mark object for batch operations         |
| `u`         | `g-mode-unmark`                 | Unmark object                            |
| `U`         | `g-mode-unmark-all-marks`       | Clear all marks                          |
| `% m`       | `g-mode-mark-regexp`            | Mark objects matching a regex            |
| `t`         | `g-mode-toggle-marks`           | Invert all marks                         |

## Features

### Binary Parsing

- 8-byte database header validation via `bindat-unpack`
- Generic object wrapper parsing with variable-width length/name fields
- Lazy scan loop — walks all records without loading inner payloads
- Attribute parsing (key=value pairs from object interior data)

### Object Inspector (`v`)

Displays in a split buffer and now doubles as the repair surface:

- Object name, size, type codes
- HFlags, AFlags, BFlags
- Parsed key=value attributes
- Structured diagnostics for malformed headers/objects
- Contextual repair/edit buttons for header fixes, raw flag/type edits,
  soft-delete/undelete, Magic2 repair, and rewriting corrupt spans as
  Free Space

### Recovery-Oriented Open

- Invalid or non-canonical database headers no longer block the mode
  from opening a file
- The object list surfaces an `<invalid header>` row when needed
- Scanner recovery attempts to resynchronize on structurally valid
  object candidates rather than any lone `Magic1` byte
- Corrupt spans retain diagnostic details so they can be inspected and
  repaired from the object view

### Deletion (`d`)

- Surgically flips the `DLI` flag to `2` in the binary buffer (3 bytes
  changed)
- Does **not** zero interior bytes — preserves data for recovery
- Object vanishes from UI immediately

### UI Toggle (`h`)

- Toggles `g-mode-show-deleted` to show/hide Free Space entries
- Deleted objects display as `<Free Space>` when visible

### Rename (`R`)

Two strategies depending on the new name length:

- **Inline**: If the new name is the same length or shorter, overwrites
  in-place with NUL padding.  No file growth.
- **Append & Free**: If the new name is longer, constructs a complete
  new object record (header, OLen, NLen, name, interior data, padding,
  magic2), appends it to end of file, and marks the original as Free
  Space.

### Garbage Collection (`G`)

Fault-resilient 3-phase compaction:

1. **Phase 1 — Backup**: Copies all deleted objects to end of file.  If
   interrupted here, the file is still valid (just bigger).
2. **Phase 2 — Compact**: Reads all active object data, then replaces
   the original region with the compacted layout.
3. **Phase 3 — Truncate**: Removes the backup tail.

Prompts for confirmation before proceeding.

## Running Tests

```sh
emacs -Q -batch -L . -l test/g-mode-test.el -f ert-run-tests-batch-and-exit
```

## License

MIT — see [LICENSE](LICENSE).
