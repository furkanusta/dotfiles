;; -*- lexical-binding: t; -*-
(use-package no-littering :demand t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Defaults & Built-ins          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-data-directory (getenv "EMACS_STORAGE_LOCATION"))
(unless my-data-directory
  (setq my-data-directory "~/Nextcloud"))

(defvar my-papers-directory (concat my-data-directory "/Papers"))
(defvar my-notes-directory (concat my-data-directory "/Notes"))
(defvar my-bibliography (concat my-data-directory "/Papers/Library.bib"))

(defvar my-bibliography-directory (concat my-papers-directory "/bibs"))

(use-package emacs :ensure nil
  :hook
  (before-save . delete-trailing-whitespace)
  (minibuffer-setup-hook . cursor-intangible-mode)
  (after-init . toggle-frame-maximized)
  :preface
  (defvar my-dark-theme 'monokai)
  (defvar my-light-theme 'leuven)
  (defvar my-active-theme my-dark-theme)
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (defun my-switch-theme-helper (old-theme new-theme)
    (disable-theme old-theme)
    (load-theme new-theme t)
    (setq my-active-theme new-theme))
  (defun my-switch-theme ()
    (interactive)
    (progn
      (if (eq my-active-theme my-dark-theme)
          (my-switch-theme-helper my-dark-theme my-light-theme)
        (my-switch-theme-helper my-light-theme my-dark-theme))))
  (defun duplicate-line-or-region (arg)
    (interactive "p")
    (let (beg end (origin (point)))
      (if (and mark-active (> (point) (mark)))
          (exchange-point-and-mark))
      (setq beg (line-beginning-position))
      (if mark-active
          (exchange-point-and-mark))
      (setq end (line-end-position))
      (let ((region (buffer-substring-no-properties beg end)))
        (dotimes (i arg)
          (goto-char end)
          (newline)
          (insert region)
          (setq end (point)))
        (goto-char (+ origin (* (length region) arg) arg)))))
  (defun my-align-comments (beginning end)
    (interactive "*r")
    (let (indent-tabs-mode align-to-tab-stop)
      (align-regexp beginning end "\\(\\s-*\\)//")))
  (defun kill-other-buffers ()
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer) (buffer-list))))
  (defun xah-cut-line-or-region ()
    (interactive)
    (if current-prefix-arg
        (progn
          (kill-new (buffer-string))
          (delete-region (point-min) (point-max)))
      (progn (if (use-region-p)
                 (kill-region (region-beginning) (region-end) t)
               (kill-region (line-beginning-position) (line-beginning-position 2))))))
  (defun xah-copy-line-or-region ()
    (interactive)
    (let (-p1 -p2)
      (if (use-region-p)
          (setq -p1 (region-beginning) -p2 (region-end))
        (setq -p1 (line-beginning-position) -p2 (line-end-position)))
      (progn
        (kill-ring-save -p1 -p2)
        (message "Text copied"))))
  (defun endless/fill-or-unfill ()
    "Like `fill-paragraph', but unfill if used twice."
    (interactive)
    (let ((fill-column
           (if (eq last-command 'endless/fill-or-unfill)
               (progn (setq this-command nil) (point-max))
             fill-column)))
      (call-interactively #'fill-paragraph)))
  (defun scroll-down-in-place (n)
    (interactive "p")
    (forward-line (* -1 n))
    (unless (eq (window-start) (point-min))
      (scroll-down n)))
  (defun scroll-up-in-place (n)
    (interactive "p")
    (forward-line n)
    (unless (eq (window-end) (point-max))
      (scroll-up n)))
  :init
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-unset-key (kbd "C-x c"))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq-default user-full-name "Furkan Usta"
                user-mail-address "furkanusta17@gmail.com"
                save-interprogram-paste-before-kill t
                ad-redefinition-action 'accept
                vc-make-backup-files t
                version-control t
                delete-old-versions t
                calendar-week-start-day 1
                delete-by-moving-to-trash t
                confirm-nonexistent-file-or-buffer nil
                tab-width 4
                tab-stop-list (number-sequence 4 200 4)
                indent-tabs-mode nil
                gdb-many-windows t
                use-file-dialog nil
                use-dialog-box nil
                inhibit-startup-screen t
                inhibit-startup-echo-area-message t
                cursor-type 'bar
                ring-bell-function 'ignore
                scroll-step 1
                sentence-end-double-space -1
                fill-column 100
                emacs-lisp-docstring-fill-column 100
                scroll-step 1
                scroll-conservatively 10000
                auto-window-vscroll nil
                comint-prompt-read-only t
                vc-follow-symlinks t
                scroll-preserve-screen-position t
                frame-resize-pixelwise t
                undo-limit 1280000
                large-file-warning-threshold (* 1024 1024 1024) ;; 1GB
                read-extended-command-predicate #'command-completion-default-include-p
                enable-recursive-minibuffers t
                minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
                read-file-name-completion-ignore-case t
                read-buffer-completion-ignore-case t
                font-use-system-font t)
  :custom
  (confirm-kill-processes nil)
  (column-number-mode 1)
  (display-time-default-load-average nil)
  (display-time-load-average-threshold 100.0)
  (display-time-24hr-format t)
  (display-time-mode 1)
  (history-delete-duplicates t)
  (native-comp-async-report-warnings-errors nil)
  :bind
  ("C-c ." . pop-global-mark)
  ("M-u" . upcase-dwim)
  ("M-l" . downcase-dwim)
  ("M-c" . capitalize-dwim)
  ("M-n" . scroll-up-in-place)
  ("M-p" . scroll-down-in-place)
  ("C-M-;" . my-align-comments)
  ("C-c k b" . kill-other-buffers)
  ("C-c d" . duplicate-line-or-region)
  ("C-c e r" . eval-region)
  ("C-S-d" . delete-backward-char)
  ("M-D" . backward-kill-word)
  ("C-w" . xah-cut-line-or-region)
  ("M-w" . xah-copy-line-or-region)
  ("M-k" . kill-whole-line)
  ("C-x C-f" . find-file-at-point)
  ("RET" . newline-and-indent)
  ([remap fill-paragraph] . endless/fill-or-unfill)
  (:map prog-mode-map ("<tab>" . indent-for-tab-command)))

(use-package exec-path-from-shell
  :hook (after-init . exec-path-from-shell-initialize))

(use-package with-editor
  :hook (shell-mode . with-editor-export-editor))

(use-package calc :ensure nil
  :hook (calc-mode . calc-symbolic-mode))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(use-package eww :ensure nil
  :bind ("<f7>" . eww))

;; (use-package desktop :ensure nil
;;   :custom (desktop-save-mode 1)
;;   :init (add-to-list 'desktop-modes-not-to-save 'dired-mode))

(use-package paren :ensure nil
  :custom (show-paren-mode 1))

;; (use-package display-line-numbers
;;   :custom (global-display-line-numbers-mode 1))

(use-package hl-line :ensure nil
  :custom (global-hl-line-mode t))

(use-package delsel :ensure nil
  :custom (delete-selection-mode 1))

(use-package recentf :ensure nil
  :after no-littering
  :defines recentf-exclude
  :custom
  (recentf-mode t)
  (recentf-save-file (concat no-littering-var-directory "recentf"))
  (recentf-exclude (list no-littering-var-directory "/tmp" no-littering-etc-directory)))

(use-package saveplace :ensure nil
  :hook (server-visit . save-place-find-file-hook)
  :custom (save-place-mode 1))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package which-func :ensure nil
  :custom (which-function-mode t))

(use-package isearch :ensure nil
  :bind ("C-c H S" . isearch-forward))

(use-package eww :ensure nil)

(use-package compilation :ensure nil
  :preface
  (make-variable-buffer-local 'my-compilation-start-time)
  (defun my-compilation-start-hook (proc)
    (setq my-compilation-start-time (current-time)))
  (defun my-compilation-finish-function (buf why)
    (let* ((elapsed  (time-subtract nil my-compilation-start-time))
           (msg (format "Elapsed: %s" (format-time-string "%T.%N" elapsed t))))
      (save-excursion (goto-char (point-max)) (insert msg))
      (alert (format "Emacs: %s at %s" why (buffer-name buf)))
      (message "Compilation %s: %s" (string-trim-right why) msg)))
  :hook
  (compilation-start . my-compilation-start-hook)
  :config
  (add-hook 'compilation-finish-functions #'my-compilation-finish-function))

(provide 'usta-builtins)
