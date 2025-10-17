# Trans Overlay

Trans Overlay is an Emacs package for on-the-fly translation with persistent overlay display and database storage. It allows you to translate words or paragraphs in any text file, store translations in a database, and display them as overlays that automatically track text position changes.Inspired by[emacs-immersive-translate](https://github.com/Elilif/emacs-immersive-translate) and [dictionary-overlay](https://github.com/ginqi7/dictionary-overlay).

## Features

- **Inline Translation Display**: Translate words/paragraphs and display translations as overlays that automatically follow text edits
- **Persistent Database Storage**: All translations are saved to SQLite database with automatic position synchronization on file save
- **Interactive List Management**: Browse, search, filter, edit, and manage all translations in a tabulated interface
- **Wordlist Export**: Mark translations for export and generate formatted wordlist documents for study
- **Quick Translation**: Instant translation preview in minibuffer or popup without saving to database
- **Manual Translation Entry**: Add or edit translations manually without using translate-shell

## Dependencies

```elisp
(use-package emacsql
  :straight (:host github :repo "magit/emacsql"))

(use-package emacsql-sqlite-builtin
  :straight (:host github :repo "magit/emacsql"
				   :files ("sqlite-builtin/*.el"))
  :after emacsql)

(use-package ov
  :straight (:host github :repo "emacsorphanage/ov"))
```

**External Dependency**: [translate-shell](https://github.com/soimort/translate-shell) (`trans` command)

## Configuration

```elisp
(add-to-list 'load-path "xxx/trans-overlay")
(require 'trans-overlay)
(require 'trans-overlay-list)

;; Optional: Customize settings
(setq trans-overlay-source-lang "en")        ; Source language
(setq trans-overlay-target-lang "zh-CN")     ; Target language
(setq trans-overlay-shell-command "trans")   ; translate-shell command
(setq trans-overlay-db-file                  ; Database file path
	  (expand-file-name "trans-overlay.db"
						(expand-file-name "trans-overlay" user-emacs-directory)))

(defun trans-overlay-maybe-enable ()
  (when (and (buffer-file-name)
			 (file-exists-p trans-overlay-db-file)
			 (> (length (ignore-errors (trans-overlay-get-file (buffer-file-name)))) 0))
	(trans-overlay-mode 1)))

(add-hook 'find-file-hook #'trans-overlay-maybe-enable)

;; Enable trans-overlay-mode in desired modes
(add-hook 'text-mode-hook #'trans-overlay-mode)
(add-hook 'prog-mode-hook #'trans-overlay-mode)
```

## Basic Operations

### Translation Commands

- `trans-overlay-at-point` (C-c t t) - Translate word/paragraph at point and save to database
- `trans-overlay-add-translation` - Manually add or edit translation without translate-shell
- `trans-overlay-quick-translate` (C-c t q) - Quick translate in minibuffer (no save)
- `trans-overlay-quick-translate-popup` (C-c t p) - Quick translate in popup buffer (no save)

### Display and Management

- `trans-overlay-display-file` (C-c t d) - Display all translations from database in current file
- `trans-overlay-clear-at-point` (C-c t c) - Clear translation at point

### List

- `trans-overlay-list` (C-c t l) - Open translation list (all translations)

## List View Keybindings

| Command                          | Key | Function                           |
|----------------------------------|-----|------------------------------------|
| trans-overlay-list-jump          | RET | Jump to translation and close list |
| trans-overlay-list-other-window  | o   | Preview translation in other window|
| trans-overlay-list-edit          | e   | Edit translation                   |
| trans-overlay-list-delete        | d   | Delete current translation         |
| trans-overlay-list-delete-marked | D   | Delete all marked translations     |
| trans-overlay-list-toggle-mark   | m   | Toggle mark for current item       |
| trans-overlay-list-mark-all      | M   | Mark all visible items             |
| trans-overlay-list-unmark-all    | U   | Unmark all items                   |
| trans-overlay-list-filter        | /   | Filter translations                |
| trans-overlay-list-refresh       | g   | Refresh list                       |
| trans-overlay-list-export        | E   | Export marked translations         |
