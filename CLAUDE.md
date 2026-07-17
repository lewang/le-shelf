# le-shelf

Le's personal Emacs package collection, managed by elpaca. Public commands use the `le::` prefix (double colon).

## Adding a customization — the layering flow

New behavior is split between **this package** (the feature) and the init file (`.le-emacs.d/post-init.el`, the
choices). Keep that separation:

1. **Autoload the entry points.** Put a `;;;###autoload` cookie on each command/entry function so it's callable
   without loading its whole file first.
2. **Let elpaca do the plumbing.** Elpaca byte-compiles the package and generates/refreshes the autoloads. Do NOT add
   a manual `require` or `with-eval-after-load` in init to pull the feature in — the autoload cookie is the contract.
3. **Small files, narrow autoloads.** Split features into small `.el` files so autoloading one command doesn't drag in
   large unrelated code. One concern per file.
4. **Init references the autoloaded command via `use-package`.** In `post-init.el`, name the command in the le-shelf
   `use-package` `:bind` (or `:commands`). The binding autoloads it on first use — no explicit load needed.
5. **The package sets itself up.** Each file is self-contained: its own `require`s, `provide`, autoload cookies, and
   any `cl-defmethod`/mode wiring. The feature works when loaded, independent of init.
6. **Init stays thin.** `post-init.el` is only keybinding choices, behavior tweaks, and variable settings — never
   feature implementation. If it's more than a binding or a `setq`, it belongs in a file here.

Example: `le-project.el` defines `le::project-find-file-both` with a `;;;###autoload` cookie; `post-init.el` wires it
with `([remap project-find-file] . le::project-find-file-both)` in the le-shelf `use-package :bind`. No `require`.

## Elisp conventions

- After editing elisp, eval via `claude-code-ide-mcp-eval` (auto-revert refreshes the file, but code isn't live until
  evaluated).
- Use `if-let*` / `when-let*` — the non-star forms are deprecated.
- Use `declare-function` for cross-file references unless a `require` is warranted.
