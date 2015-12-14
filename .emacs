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
  "Set font size"
  (interactive "nInput size: ")
  (let ((fs "fontset-default"))
    (set-fontset-font fs 'latin "DejaVu Sans Mono")
    (set-fontset-font fs 'hangul "나눔고딕")
    (set-fontset-font fs
		      (cons (decode-char 'ucs #x2293) (decode-char 'ucs #x2294))
		      "DejaVu Sans Mono")
    (set-face-attribute 'default nil :font fs :height (* size 10))))

(setq inhibit-startup-message t)	; No startup message
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
(when (eq system-type 'darwin)		; Mac specific settings
  (setq mac-command-modifier 'meta))

(global-font-lock-mode 1)		; Syntax highlight
(show-paren-mode 1)
(setq search-highlight t)
(setq query-replace-highlight t)

(setq TeX-PDF-mode t)

;; Remove a key map of [C-tab] in magit-mode.el for using it as a
;; window selection in magit-mode.

(global-set-key [(ctrl tab)] 'select-next-window)
(global-set-key [(ctrl shift iso-lefttab)] 'select-previous-window)
(global-set-key [(ctrl shift kp-tab)] 'select-previous-window)
(global-set-key [(ctrl f4)] 'delete-window)
(global-set-key "\C-x\C-k" 'kill-this-buffer)
(global-set-key "\C-x\C-n" 'next-buffer)
(global-set-key "\C-x\C-p" 'previous-buffer)
(global-set-key "\C-x\C-b" 'ibuffer)

(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c:" 'uncomment-region)

(global-set-key [(control ?`)] 'up-down-case-char)

(global-set-key "\M-Q" 'unfill-paragraph)

(when window-system			; Disable suspend
  (global-unset-key (kbd "C-z"))
  (global-unset-key [(control x)(control z)]))

(when window-system
  (set-font-size 14))

;; Package: multiple-cursors
;; See https://marmalade-repo.org/

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Package: expand-region
;; See https://marmalade-repo.org/

(global-set-key (kbd "C-=") 'er/expand-region)

;; Package: ocp-indent & merlin
;; They are sharing load-path.

(add-to-list 'load-path "~/.opam/4.02.1/share/emacs/site-lisp")
(require 'ocp-indent)
(require 'merlin)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(setq merlin-use-auto-complete-mode 'easy)

;; Package: auto-complete
;; See https://marmalade-repo.org/
;; See also
;; http://stackoverflow.com/questions/12053598/loading-packages-installed-through-package-el-in-emacs24
;; to be sure why the initialization hooks.

(add-hook 'after-init-hook 'auto-complete-init-hook)
(defun auto-complete-init-hook ()
  (require 'auto-complete-config)
  (ac-config-default))

(load-file "~/tool/ProofGeneral/generic/proof-site.el")

;; CAUTION: Do not edit the following code.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "xpdf")
     (output-html "xdg-open"))))
 '(custom-enabled-themes (quote (pastel-gray)))
 '(custom-safe-themes
   (quote
    ("fdfe4da783018150c940b8738187b4bb9a2bfc5d6adc0ca15a01ec9c3fb2a7bd" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
