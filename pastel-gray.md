Pastel gray color theme
=======================

## Definition

name    | dark(RGB)   | dark(#) | light(RGB)  | light(#)
--------|-------------|---------|-------------|---------
black   | 0 0 0       | #000000 | 26 26 26    | #1A1A1A
red     | 191 34 61   | #BF223D | 211 54 81   | #D33651
green   | 67 115 86   | #437356 | 87 135 106  | #62876A
yellow  | 255 210 101 | #FFD265 | 255 230 121 | #FFE679
blue    | 23 97 168   | #1761A8 | 43 117 188  | #2B75BC
magenta | 207 4 148   | #CF0494 | 227 24 168  | #E318A8
cyan    | 109 188 219 | #6DBCDB | 129 208 239 | #81D0EF
white   | 229 229 229 | #E5E5E5 | 255 255 255 | #FFFFFF

name        | color(RGB)  | color(#)
------------|-------------|---------
foregraound | 229 229 229 | #E5E5E5
background  | 26 26 26    | #1A1A1A
selection   | 181 213 255 | #B5D5FF

## Emacs

foreground-color: #E5E5E5
background-color: #1A1A1A
font-lock-warning-face : #D33651
font-lock-function-name-face: #2B75BC
font-lock-variable-name-face: #E5E5E5
font-lock-keyword-face: #FFE679
font-lock-comment-face: #6DBCDB
font-lock-comment-delimiter-face: #6DBCDB
font-lock-type-face: #437356
font-lock-constant-face: #E318A8
font-lock-builtin-face: #E5E5E5
font-lock-string-face: #D33651
font-lock-negation-char-face: #E5E5E5
region: #555555
dired-directory: #2B75BC
dired-symlink: #6DBCDB
eshell-prompt: #62876A
eshell-ls-readonly: #E5E5E5
eshell-ls-directory: #2B75BC
eshell-ls-executable: #62876A
eshell-ls-archive: #D33651
eshell-ls-symlink: #6DBCDB

### How to apply pastel-gray-theme

1. Copy `pastel-gray-theme.el` to `~/.emacs.d/`
2. `M-x` `customize-themes`
3. Select `pastel-gray`
4. `C-x` `C-s` (This will edit `~/.emacs` as below.)

### How to modify theme

1. Get face names by `C-u` `C-x` `=`
2. `M-x` `customize-themes`
3. Type `?` on pastel-gray
4. Click the `customize` button

*****

by skcho, Dec. 2013.
