;; NOTE: Make sure running package-initialize before load this file.

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

(defun real ()
  "Replace the current buffer with the corresponding file of \
working directory, when opening a file of a specific git commit."
  (defun find-git-root ()
    (defun parent-directory (dir)
      (unless (equal "/" dir)
        (file-name-directory (directory-file-name dir))))
    (defun find-git-root-rec (dir)
      (if (file-exists-p (concat dir ".git"))
          dir
        (find-git-root-rec (parent-directory dir))))
    (find-git-root-rec (symbol-value 'default-directory)))
  (defun find-original-path (s)
    (substring s 0 (string-match "\\.~" s)))
  (interactive)
  (let* ((old-point (point))
         (old-buffer (buffer-name))
         (git-root (find-git-root))
         (file-path (find-original-path old-buffer))
         (new-file (concat git-root file-path)))
    (when (file-exists-p new-file)
      (find-file new-file)
      (goto-char old-point)
      (kill-buffer old-buffer))))


;; Default settings

(set-fontset-font "fontset-default" 'latin "D2Coding")
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

(setq ring-bell-function 'ignore)       ; Disable bell

(if (eq system-type 'darwin)
    (set-font-size 18)
  (set-font-size 12))


;; Packages

;; multiple-cursors
;; https://github.com/magnars/multiple-cursors.el

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; expand-region
;; https://github.com/magnars/expand-region.el

(global-set-key (kbd "C-=") 'er/expand-region)

;; OPAM packages: tuareg, ocp-indent & merlin

(when my-init-ocaml-enabled
  (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
  (load (concat opam-share "/emacs/site-lisp/tuareg-site-file"))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
  (require 'ocp-indent)
  (require 'merlin)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)
  (setq merlin-use-auto-complete-mode 'easy)
  (setq merlin-command 'opam))

;; auto-complete
;; NOTE: the popup package is installed together.

(require 'auto-complete)
(global-set-key (kbd "C-c <tab>") 'ac-complete-merlin)

;; Color theme: dracula
;; https://github.com/zenorocha/dracula-theme

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;; company-coq
;; https://github.com/cpitclaudel/company-coq
;; Load company-coq when opening Coq files

(when my-init-coq-enabled
  (add-hook 'coq-mode-hook #'company-coq-mode)
  (put 'company-coq-fold 'disabled nil))

;; OCamlFormat
;; https://github.com/ocaml-ppx/ocamlformat

(when my-init-ocaml-enabled
  (load (concat opam-share "/emacs/site-lisp/ocamlformat"))
  (add-hook 'before-save-hook 'ocamlformat-before-save))

;; TypeScript
(when my-init-typescript-enabled
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

;; JavaScript
(add-hook 'js-mode-hook
	  (lambda () (setq-default indent-tabs-mode nil)))