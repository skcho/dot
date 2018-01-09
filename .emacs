;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


;; Functions

(defalias 'yes-or-no-p 'y-or-n-p)

(defun select-next-window ()
  (interactive)
  (select-window (next-window (selected-window))))

(defun select-previous-window ()
  (interactive)
  (select-window (previous-window (selected-window))))

(defun up-down-case-char ()
  (interactive)
  (set-mark-command ())
  (forward-char 1)
  (setq myStr (buffer-substring (region-beginning) (region-end)))
  (if (string-equal myStr (upcase myStr))
      (downcase-region (region-beginning) (region-end))
    (upcase-region (region-beginning) (region-end)))
  (backward-char 1))

(defun unfill-paragraph ()	  ; by Stefan Monnier (foo at acm.org)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun eshell-clear ()		  ; by Sailor (http://www.khngai.com/)
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun set-font-size (size)
  "Set font size."
  (interactive "nSize: ")
  (set-face-attribute 'default nil :height (* size 10)))


;; Default settings

(set-fontset-font "fontset-default" 'latin "Inconsolata")
(set-fontset-font "fontset-default" 'hangul "Noto Sans CJK KR")
(set-fontset-font "fontset-default" 'japanese-jisx0208 "Noto Sans CJK JP")
(set-face-attribute 'default nil :font "fontset-default")

(setq inhibit-startup-message t)
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)
(setq make-backup-files nil)

(set-language-environment-input-method "Korean")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

(column-number-mode t)
(tool-bar-mode -1)

(setq x-alt-keysym 'meta)
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

(global-font-lock-mode 1)		; Syntax highlight
(show-paren-mode 1)
(setq search-highlight t)
(setq query-replace-highlight t)

(setq TeX-PDF-mode t)

(global-set-key (kbd "<C-tab>") 'select-next-window)
(global-set-key (kbd "<C-S-tab>") 'select-previous-window)
(global-set-key (kbd "<C-iso-lefttab>") 'select-previous-window)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)
(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "C-c :") 'uncomment-region)
(global-set-key (kbd "C-`") 'up-down-case-char)
(global-set-key (kbd "M-Q") 'unfill-paragraph)

(when window-system			; Disable suspend
  (global-unset-key (kbd "C-z")))

(if (eq system-type 'darwin)
    (set-font-size 18)
  (set-font-size 14))


;; Packages

;; quelpa
;; See https://github.com/quelpa/quelpa

(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; quelpa self-upgrade
;; It raises an error currently.

;; (if (require 'quelpa nil t)
;;     (quelpa-self-upgrade)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
;;     (eval-buffer)))

;; multiple-cursors
;; https://marmalade-repo.org/

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; expand-region
;; https://marmalade-repo.org/

(global-set-key (kbd "C-=") 'er/expand-region)

;; OPAM packages: tuareg, ocp-indent & merlin

(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(load (concat opam-share "/emacs/site-lisp/tuareg-site-file"))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
(require 'ocp-indent)
(require 'merlin)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
(setq merlin-use-auto-complete-mode 'easy)
(setq merlin-command 'opam)

;; auto-complete
;; https://marmalade-repo.org/
;; NOTE: the popup package is installed together.

(add-to-list 'load-path "~/.emacs.d/elpa/popup-0.5")
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-1.4")
(require 'auto-complete)
(global-set-key (kbd "C-c <tab>") 'ac-complete-merlin)

;; Color theme: dracula
;; https://github.com/zenorocha/dracula-theme

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;; Proof General
;; https://proofgeneral.github.io/

(load "~/.emacs.d/lisp/PG/generic/proof-site")

;; company-coq
;; https://github.com/cpitclaudel/company-coq
;; Load company-coq when opening Coq files

(add-hook 'coq-mode-hook #'company-coq-mode)
(put 'company-coq-fold 'disabled nil)

;; Reason mode
;; https://github.com/arichiardi/reason-mode

;; Reason mode using quelpa
;; It raises an error currently.

;; (quelpa '(reason-mode :repo "arichiardi/reason-mode" :fetcher github :stable t))

;; Reason mode by manual install

(defun chomp-end (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                            ""
                            str))

(let ((support-base-dir (concat (replace-regexp-in-string "refmt" "" (file-truename (chomp-end (shell-command-to-string "which refmt")))) ".."))
      (merlin-base-dir (concat (replace-regexp-in-string "ocamlmerlin" "" (file-truename (chomp-end (shell-command-to-string "which ocamlmerlin")))) "..")))
  ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
  (add-to-list 'load-path (concat merlin-base-dir "/share/emacs/site-lisp/"))
  (setq merlin-command (concat merlin-base-dir "/bin/ocamlmerlin"))

  ;; Add npm reason-mode to the emacs load path and tell emacs where to find refmt
  (add-to-list 'load-path (concat support-base-dir "/share/emacs/site-lisp"))
  (setq refmt-command (concat support-base-dir "/bin/refmt")))

;; Disable Eshell's scroll feature
;; https://emacs.stackexchange.com/questions/28819/eshell-goes-to-the-bottom-of-the-page-after-executing-a-command

(add-hook 'eshell-mode-hook
          (defun chunyang-eshell-mode-setup ()
            (remove-hook 'eshell-output-filter-functions
                         'eshell-postoutput-scroll-to-bottom)))

;; OCamlFormat
;; https://github.com/ocaml-ppx/ocamlformat

(load (concat opam-share "/emacs/site-lisp/ocamlformat"))
(add-hook 'before-save-hook 'ocamlformat-before-save)
