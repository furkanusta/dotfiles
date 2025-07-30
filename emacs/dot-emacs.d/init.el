;;; -*- lexical-binding: t -*-
;; Initialization
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'server)
;; (unless (server-running-p) (server-start))

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "elisp/")))

(setq use-package-enable-imenu-support t)
(setq use-package-compute-statistics t)
(setq use-package-always-defer t)
;; (setq-default use-package-always-ensure t)

(require 'use-package)
(use-package dash :demand t)
(use-package f :demand t)

(defun disable-line-numbers ()
  (display-line-numbers-mode -1))

(defvar use-other-window-alist
  '((display-buffer-same-window display-buffer-pop-up-window)
    (inhibit-same-window . t)))

(defvar my-data-directory (or (getenv "EMACS_STORAGE_LOCATION") (expand-file-name "~/Documents")))
(defvar my-notes-directory (concat my-data-directory "/Notes"))
(defvar my-papers-directory (concat my-notes-directory "/PDF"))
(defvar my-bibliography-directory (concat my-notes-directory "/bibs"))
(defvar my-notes (-filter
                  (lambda (file) (not (s-starts-with? "." (f-filename file))))
                  (f-glob "*.org" my-notes-directory)))
(defvar my-bibliographies (-filter
                           (lambda (file) (not (s-starts-with? "." (f-filename file))))
                           (f-glob "*.bib" my-bibliography-directory)))


(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

;; WSL: WSL1 has "-Microsoft", WSL2 has "-microsoft-standard"
(defvar my-running-under-wsl (string-match "-[Mm]icrosoft" operating-system-release))


(defvar my-dark-theme 'tangonov)
(defvar my-light-theme 'ef-kassio)
(defun my-switch-theme ()
  "Switch theme between light and dark theme."
  (interactive)
  (let* ((my-dark-theme 'tangonov)
         (my-light-theme 'ef-kassio)
         (current-theme (car custom-enabled-themes)) ;; Assume single theme
         (new-theme (if (eq current-theme my-dark-theme) my-light-theme my-dark-theme)))
    (disable-theme current-theme)
    (load-theme new-theme t)))

(defun my-previous-window ()
  "Previous window."
  (interactive)
  (other-window -1))

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
      (dotimes (_ arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun my-align-comments (beginning end)
  (interactive "*r")
  (let ((indent-tabs-mode 'align-to-tab-stop))
    (align-regexp beginning end "\\(\\s-*\\)//")))

(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer) (buffer-list))))

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

(use-package no-littering :demand t)

(use-package emacs :ensure nil
  :hook
  ((minibuffer-setup . cursor-intangible-mode)
   (before-save . delete-trailing-whitespace)
   (after-init . toggle-frame-maximized))
  :init
  (setq backup-directory-alist (list (cons "." (concat no-littering-var-directory "backup/"))))
  (global-unset-key (kbd "C-x c"))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (setq-default user-full-name "Furkan Usta"
                delete-by-moving-to-trash t
                tab-width 4
                indent-tabs-mode nil
                use-file-dialog nil
                use-dialog-box nil
                cursor-type 'bar
                ring-bell-function 'ignore
                scroll-step 1
                fill-column 100
                scroll-step 1
                scroll-conservatively 10000
                auto-window-vscroll nil
                comint-prompt-read-only t
                scroll-preserve-screen-position t
                frame-resize-pixelwise t
                undo-limit 1280000
                enable-recursive-minibuffers t
                minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
                read-buffer-completion-ignore-case t
                font-use-system-font t)
  :custom
  (use-short-answers t)
  (initial-major-mode #'fundamental-mode)
  (version-control t)
  (delete-old-versions t)
  (user-mail-address "furkanusta17@gmail.com")
  (save-interprogram-paste-before-kill t)
  (ad-redefinition-action 'accept)
  (vc-make-backup-files t)
  (tab-stop-list (number-sequence 4 200 4))
  (inhibit-startup-screen t)
  (inhibit-startup-echo-area-message t)
  (emacs-lisp-docstring-fill-column 100)
  (sentence-end-double-space -1)
  (vc-follow-symlinks t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (read-file-name-completion-ignore-case t)
  (confirm-kill-processes nil)
  (column-number-mode 1)
  (display-time-default-load-average nil)
  (display-time-load-average-threshold 100.0)
  (display-time-24hr-format t)
  (display-time-mode 1)
  (history-delete-duplicates t)
  (native-comp-async-report-warnings-errors nil)
  (package-native-compile t)
  (large-file-warning-threshold (* 1024 1024 1024)) ;; 1GB
  (confirm-nonexistent-file-or-buffer nil)
  (mark-even-if-inactive nil)
  ;; (global-visual-line-mode t)
  :preface
  (defun my-mark-line (beg end &optional region)
    (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-beginning-position 2))))
  :config
  ;; (advice-add 'kill-ring-save :before #'my-mark-line)
  ;; (advice-add 'kill-region :before #'my-mark-line)
  (load-theme my-dark-theme)
  (add-to-list 'exec-path (f-expand "~/.local/bin"))
  (add-to-list 'exec-path "/home/furkanu/.local/bin")
  (add-to-list 'delete-frame-functions #'(lambda (frame_) (recentf-save-list)))
;;  (add-to-list 'delete-frame-functions #'(lambda (frame_) (bookmark-save)))
  :bind
  ("M-u" . upcase-dwim)
  ("M-l" . downcase-dwim)
  ("M-c" . capitalize-dwim)
  ;; ("M-n" . scroll-up-in-place)
  ;; ("M-p" . scroll-down-in-place)
  ("C-M-;" . my-align-comments)
  ("C-c k b" . kill-other-buffers)
  ("C-c d" . duplicate-line-or-region)
  ("C-c e r" . eval-region)
  ("C-S-d" . delete-backward-char)
  ("M-D" . backward-kill-word)
  ("C-w" . kill-region)
  ("M-k" . kill-whole-line)
  ("C-x C-f" . find-file-at-point)
  ("RET" . newline-and-indent)
  ("C-x O" . my-previous-window)
  ("C-_" . undo-only)
  ("C-+" . undo-redo)
  ("C-x C-." . pop-to-mark-command)
  ("C-g" . prot/keyboard-quit-dwim)
  ([remap fill-paragraph] . endless/fill-or-unfill)
  (:map prog-mode-map ("<tab>" . indent-for-tab-command)))

(use-package files :ensure nil
  :preface
  (defun my-color-log-files ()
    (when (string= (file-name-extension (buffer-file-name)) "log")
      (ansi-color-apply-on-region (point-min) (point-max) t)))
  :hook
  (find-file . my-color-log-files))

(use-package desktop
  :custom
  (desktop-buffers-not-to-save "^$"))

(use-package calendar :ensure nil
  :custom
  (calendar-week-start-day 1)
  (calendar-date-style 'iso))

(use-package find-file :ensure nil
  :bind
  (:map project-prefix-map
        ("a" . ff-find-other-file-other-window)
        ("A" . ff-find-other-file)))

(use-package url :ensure nil
  :custom
  (url-user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(use-package eww :ensure nil
  :preface
  (defun eww-open-link-another-frame ()
    (interactive)
    (when (= 1 (length (visible-frame-list)))
      (split-window-horizontally))
    (let ((buf (current-buffer)))
      (other-window 1)
      (switch-to-buffer buf)
      (eww-open-in-new-buffer)))
  :bind
  ("<f7>" . eww)
  (:map eww-mode-map
        ("k" . kill-this-buffer)
        ("M-RET" . eww-open-link-another-frame)))

(use-package paren   :ensure nil :custom (show-paren-mode 1))

;; (use-package hl-line :ensure nil :custom (global-hl-line-mode t))

(use-package delsel  :ensure nil :demand t :init (delete-selection-mode 1))

(use-package recentf :ensure nil
  :after no-littering
  :defines recentf-exclude
  :init (no-littering-theme-backups)
  :custom
  (recentf-mode t)
  (recentf-save-file (concat no-littering-var-directory "recentf"))
  (recentf-exclude (list
                    (expand-file-name no-littering-var-directory)
                    (expand-file-name no-littering-etc-directory)
                    no-littering-var-directory "/tmp" no-littering-etc-directory)))

(use-package saveplace :ensure nil
  :custom (save-place-mode 1))

(use-package uniquify :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package which-func :ensure nil
  :hook (prog-mode . which-function-mode)
  :custom (which-func-non-auto-modes '(org-mode markdown-mode bibtex-mode)))

(use-package isearch :ensure nil
  :bind ("C-c H S" . isearch-forward))

(use-package so-long)

(use-package goto-addr
  :hook (org-mode . goto-address-mode)
  :bind ("C-c o l" . goto-address-at-point))

(use-package bookmark
  ;; :hook (kill-emacs . bookmark-save)
  :custom
  (bookmark-watch-bookmark-file 'silent))

(use-package window :ensure nil
  :preface
  (defun hide-titlebar-when-maximized (frame)
    (if (eq 'maximized (alist-get 'fullscreen (frame-parameters frame)))
        (set-frame-parameter frame 'undecorated t)
      (set-frame-parameter frame 'undecorated nil)))
  :init
  (add-hook 'window-size-change-functions 'hide-titlebar-when-maximized))

(use-package dired :ensure nil
  :commands dired-get-file-for-visit
  :preface
  (defun dired-open-xdg ()
    "Try to run `xdg-open' to open the file under point."
    (interactive)
    (if (executable-find "xdg-open")
        (let ((file (ignore-errors (dired-get-file-for-visit)))
              (process-connection-type nil))
          (start-process "" nil "xdg-open" (file-truename file)))
      nil))
  (defun dired-find-readme-other-window ()
    (interactive)
    (let* ((dir (dired-get-filename))
           (files-readme (f-glob (concat dir "/readme*")))
           (files-Readme (f-glob (concat dir "/Readme*")))
           (files-README (f-glob (concat dir "/README*")))
           (file-paths (append files-README files-readme files-Readme))
           (files (seq-filter (lambda (file) (f-exists? file)) file-paths))
           (file (car files)))
      (if (= 1 (count-windows))
          (split-window-horizontally))
      (other-window 1)
      (dired--find-possibly-alternative-file dir)
      (when file
        (find-file file))))
  (defun dired-open-file-or-find-readme ()
    (interactive)
    (let ((path (dired-get-filename)))
      (if (file-directory-p path)
          (dired-find-readme-other-window)
        (dired-find-readme-other-window))))
  (defun dired-open-current-directory-xdg ()
    "Try to run `xdg-open' to open the current directory."
    (interactive)
    (if (executable-find "xdg-open")
        (let ((file (ignore-errors default-directory))
              (process-connection-type nil))
          (start-process "" nil "xdg-open" (file-truename file)))
      nil))
  (defun my-dired-pdf-title ()
  (interactive)
  (when-let* ((file (dired-file-name-at-point))
         (path (expand-file-name file))
         (text-quoting-style 'straight)
         (command (format "exiftool '%s' | grep Title | awk '{print substr($0, index($0, \":\") + 2)}'" path))
         (output (shell-command-to-string command))
         (title (string-trim output)))
    (kill-new title)
    (message title)))
  (defun dired-current-dir ()
    (interactive)
    (dired default-directory))
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-create-destination-dirs 'ask)
  (dired-recursive-copies 'always)
  (dired-use-ls-dired t)
  (dired-listing-switches "-aBhl --group-directories-first --color=never --dired")
  :config
  (defun my--find-nov-buffer (filename)
    (require 'nov)
    (if-let* ((nov-buffer (seq-filter #'(lambda (buffer) (with-current-buffer buffer (string= filename nov-file-name))) (buffer-list))))
        (switch-to-buffer (car nov-buffer))
      (nov--find-file filename 0 0)))
  (define-advice dired--find-file (:around (oldfunc ff filename))
    (if (string= "epub" (f-ext filename))
        (funcall oldfunc #'my--find-nov-buffer filename)
      (funcall oldfunc ff filename)))
  :bind
  ("C-x d" . dired-current-dir)
  (:map dired-mode-map
        ("o" . dired-open-file-or-find-readme)
        ("O" . dired-find-file-other-window)
        ("E" . dired-open-current-directory-xdg)
        ("e" . dired-open-xdg)))

(use-package wdired
  :hook (wdired-mode . highlight-changes-mode))

(use-package dired-x
  :ensure nil
  :custom
  (dired-omit-mode 1))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("H" . dired-hide-dotfiles-mode)))

(use-package trashed)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom (all-the-icons-dired-monochrome nil))

(use-package dired-rsync
  :bind
  (:map dired-mode-map
        ("C-c C-r" . dired-rsync)))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(use-package dired-async :ensure async
  :hook (dired-mode . dired-async-mode))

(use-package elfeed
  :hook ((elfeed-new-entry . ime-elfeed-podcast-tagger))
  :preface
  (defun ime-elfeed-podcast-tagger (entry)
    (when (elfeed-entry-enclosures entry)
      (elfeed-tag entry 'podcast)))
  (defun +rss/delete-pane ()
    "Delete the *elfeed-entry* split pane."
    (interactive)
    (let* ((buf (get-buffer "*elfeed-entry*"))
           (window (get-buffer-window buf)))
      (delete-window window)
      (when (buffer-live-p buf) (kill-buffer buf))))
  (defun elfeed-copy-link-at-point ()
    (interactive)
    (let* ((search-entries (elfeed-search-selected))
           (search-entry (when search-entries (car search-entries)))
           (elfeed-entry (or elfeed-show-entry search-entry))
           (url (elfeed-entry-link elfeed-entry)))
      ;; For wsl hooks to work
      (with-temp-buffer
        (insert url)
        (kill-ring-save (point-min) (point-max)))
      (elfeed-search-untag-all-unread)))
  :custom
  (elfeed-show-entry-switch #'pop-to-buffer)
  (elfeed-show-entry-delete #'+rss/delete-pane)
  (elfeed-search-title-max-width 100))

(use-package elfeed-org
  :after elfeed
  :init (elfeed-org))

(use-package elfeed-search :ensure elfeed
  :after elfeed
  :defines elfeed-show-entry
  :commands (pocket-reader-add-link elfeed-search-selected elfeed-search-untag-all-unread)
  :preface
  (defun elfeed-get-show-or-search-entry ()
    (let* ((search-entries (elfeed-search-selected))
           (search-entry (when search-entries (car search-entries)))
           (elfeed-entry (or elfeed-show-entry search-entry)))
      elfeed-entry))
  (defun elfeed-open-eww ()
    (interactive)
    (let* ((entry (elfeed-get-show-or-search-entry))
           (url (elfeed-entry-link entry)))
      (eww url)
      (elfeed-search-untag-all-unread)))
  :bind (:map elfeed-search-mode-map
              ("P" . pocket-reader-add-link)
              ("M-w" . elfeed-copy-link-at-point)
              ("e" . elfeed-open-eww)))

(use-package elfeed-show :ensure elfeed
  :after elfeed
  :commands (pocket-reader-add-link)
  :bind (:map elfeed-show-mode-map
              ("q" . +rss/delete-pane)
              ("M-w" . elfeed-copy-link-at-point)
              ("P" . pocket-reader-add-link)))

(use-package feed-discovery)

(use-package buffer-move
  :bind
  ("C-c S-<up>"    . buf-move-up)
  ("C-c S-<down>"  . buf-move-down)
  ("C-c S-<left>"  . buf-move-left)
  ("C-c S-<right>" . buf-move-right))

(use-package drag-stuff
  :custom (drag-stuff-global-mode t)
  :bind (:map drag-stuff-mode-map
              ("M-N" . drag-stuff-down)
              ("M-P" . drag-stuff-up)
              ("<M-up>" . drag-stuff-up)
              ("<M-down>" . drag-stuff-down)))

(use-package hungry-delete
  :custom
  (global-hungry-delete-mode 1)
  (hungry-delete-join-reluctantly t)
  :config
  (add-to-list 'hungry-delete-except-modes 'multiple-cursors-mode)
  :bind
  ([remap delete-char] . hungry-delete-forward)
  ([remap delete-forward-char] . hungry-delete-forward)
  ([remap delete-backward-char] . hungry-delete-backward))

(use-package vertico
  :demand t
  :defines vertico-mode actual-vertico-format-candidate
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :preface
  (defun my-vertico-insert-and-exit ()
    (interactive)
    (progn
      (vertico-insert)
      (exit-minibuffer)))
  (require 'dired)
  (defun +completion-category-highlight-files (cand)
    "Customize highlighting based on completion-category"
    (let ((len (length cand)))
      (when (and (> len 0)
                 (eq (aref cand (1- len)) ?/))
        (add-face-text-property 0 len 'dired-directory 'append cand)))
    cand)
  (defun auto-minor-mode-enabled-p (minor-mode)
    "Return non-nil if MINOR-MODE is enabled in the current buffer."
    (and (memq minor-mode minor-mode-list)
         (symbol-value minor-mode)))
  (defun +completion-category-highlight-commands (cand)
    (let ((len (length cand)))
      (when (and (> len 0)
                 (with-current-buffer (nth 1 (buffer-list))
                   (or (eq major-mode (intern cand))
                       (ignore-errors (auto-minor-mode-enabled-p (intern cand))))))
        (add-face-text-property 0 len '(:foreground "red") 'append cand)))
    cand)
  (defvar +completion-category-hl-func-overrides
    `((file . ,#'+completion-category-highlight-files)
      (command . ,#'+completion-category-highlight-commands))
    "Alist mapping category to highlight functions.")
  ;; Pre-select previous directory when entering parent directory from within find-file
  (defvar previous-directory nil
    "The directory that was just left. It is set when leaving a directory and
    set back to nil once it is used in the parent directory.")
  :custom
  (vertico-resize 'grow)
  (vertico-count 15)
  (vertico-cycle t)
  ;; (vertico-indexed-mode t)
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (define-advice vertico-directory-up (:before (&optional N))
    "Set the directory that was just exited from within find-file."
    (save-excursion
      (goto-char (1- (point)))
      (when (search-backward "/" (minibuffer-prompt-end) t)
        ;; set parent directory
        (setq previous-directory (buffer-substring (1+ (point)) (point-max)))
        ;; set back to nil if not sorting by directories or what was deleted is not a directory
        (when (not (string-suffix-p "/" previous-directory))
          (setq previous-directory nil))
        t)))
  (define-advice vertico--update-candidates (:after (&rest _) choose-candidate)
    "Pick the previous directory rather than the prompt after updating candidates."
    (cond
     (previous-directory ; select previous directory
      (setq vertico--index (or (seq-position vertico--candidates previous-directory)
                               vertico--index))
      (setq previous-directory nil))))
  (define-advice vertico--arrange-candidates (:around (func))
    (let ((hl-func (or (alist-get (vertico--metadata-get 'category)
                                  +completion-category-hl-func-overrides)
                       #'identity)))
      (cl-letf* (((symbol-function 'actual-vertico-format-candidate)
                  (symbol-function #'vertico--format-candidate))
                 ((symbol-function #'vertico--format-candidate)
                  (lambda (cand &rest args)
                    (apply #'actual-vertico-format-candidate
                           (funcall hl-func cand) args))))
        (funcall func))))
  :bind
  (:map vertico-map
        ("M-q" . vertico-quick-insert)
        ("C-q" . vertico-quick-exit)
        ("TAB" . vertico-insert)
        ("RET" . my-vertico-insert-and-exit)
        ("?" . minibuffer-completion-help)))

(use-package vertico-directory
  :ensure vertico
  :demand t
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package consult
  :demand t
  :defines project-root project-current
  :functions consult-customize
  :hook
  ((completion-list-mode . consult-preview-at-point-mode)
   (minibuffer-setup . consult-initial-narrow))
  :preface
  (defvar consult--fd-command (if (eq 0 (call-process-shell-command "fdfind")) "fdfind" "fd"))
  (defvar consult--initial-narrow-list nil)
  (defun consult-initial-narrow ()
    (dolist (l consult--initial-narrow-list)
      (when (eval (car l))
        (setq unread-command-events (append unread-command-events (cdr l))))))
  (defun consult--fd-builder (input)
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler arg 'extended t)))
      (when re
        (cons (append
               (list consult--fd-command
                     "--color=never" "--full-path"
                     (consult--join-regexps re 'extended))
               opts)
              hl))))
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (defun my-consult-rga-here ()
    (interactive)
    (let ((consult-ripgrep-args (concat "rga" (substring consult-ripgrep-args 2) " --no-ignore")))
      (consult-ripgrep default-directory)))
  (defun my-consult-ripgrep-here ()
    (interactive)
    (let ((consult-ripgrep-args (concat consult-ripgrep-args " --no-ignore")))
      (consult-ripgrep default-directory (thing-at-point 'symbol))))
  (defun my-consult-ripgrep ()
    (interactive)
    (consult-ripgrep (if (project-current)
                         (project-root (project-current))
                       default-directory)
                     (thing-at-point 'symbol)))
  :bind
  ("M-y" . consult-yank-pop)
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("C-x b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-x 5 b" . consult-buffer-other-frame)
  ("C-c b" . consult-bookmark)
  ("C-c M-:" . consult-complex-command)
  ("C-c r s" . consult-register-store)
  ("C-c r r" . consult-register)
  ("C-c SPC" . consult-mark)
  ("C-c C-SPC" . consult-global-mark)
  ("C-c c c" . consult-compile-error)
  ("C-c c f" . consult-flymake)
  ("C-c c o" . consult-outline)
  ("C-c c i" . consult-imenu)
  ("C-c h s" . my-consult-ripgrep-here)
  ("C-c h a" . my-consult-rga-here)
  ("C-c h S" . my-consult-ripgrep)
  ("C-c h f" . consult-fd)
  ("C-c c s" . consult-isearch-history)
  ("C-s" . consult-line-symbol-at-point)
  :custom
  (register-preview-delay 0)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-preview-key "M-.")
  (consult-narrow-key "<")
  (consult-ripgrep-args
   "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number")
  :config
  ;; (add-to-list 'consult-buffer-filter "^\\\*")
  (consult-customize
   consult-line-symbol-at-point my-consult-ripgrep my-consult-ripgrep-here
   consult-ripgrep consult-git-grep consult-grep consult-line consult-flymake
   consult--source-recent-file consult--source-project-recent-file
   consult-bookmark consult-recent-file consult-xref consult--source-bookmark
   :preview-key '(:debounce 0.01 any))
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

(use-package consult-dir
  :preface
  (defun consult-dir--tramp-docker-hosts ()
    "Get a list of hosts from docker."
    (require 'tramp-container)
    (tramp-container--completion-function tramp-docker-method))
  (defvar consult-dir--source-tramp-docker
    `(:name     "Docker"
                :narrow   ?d
                :category file
                :face     consult-file
                :history  file-name-history
                :items    ,#'consult-dir--tramp-docker-hosts)
    "Docker candiadate source for `consult-dir'.")
  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-docker t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  :bind ("C-c c d" . consult-dir))

(use-package orderless
  :demand t
  :preface
  ;; From doomemacs
  (defun vertico-orderless-dispatch (pattern _index _total)
    (cond ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))))
  :custom
  (orderless-style-dispatchers '(vertico-orderless-dispatch))
  (completion-styles '(orderless basic))
  (completion-ignore-case t)
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (orderless partial-completion)))))
  :config (set-face-attribute 'completions-first-difference nil :inherit nil))

(use-package embark
  :demand t
  :after vertico
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :custom (embark-indicators '(embark-verbose-indicator))
  :preface
  (defun with-minibuffer-keymap (keymap)
    (lambda (fn &rest args)
      (minibuffer-with-setup-hook
          (:append (lambda ()
                     (use-local-map
                      (make-composed-keymap keymap (current-local-map)))))
        (apply fn args))))
  (defvar embark-completing-read-prompter-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-<tab>") 'abort-recursive-edit)
      map))
  (defun embark-act-with-completing-read (&optional arg)
    (interactive "P")
    (let* ((embark-prompter 'embark-completing-read-prompter)
           (embark-indicators '(embark-minimal-indicator)))
      (embark-act arg)))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (advice-add 'embark-completing-read-prompter :around
              (with-minibuffer-keymap embark-completing-read-prompter-map))
  :bind
  ("C-." . embark-act)
  ("C-," . embark-dwim)
  ("M-n" . embark-next-symbol)
  ("M-p" . embark-previous-symbol)
  (:map vertico-map
        ("C-<tab>" . embark-act-with-completing-read)
        ("C-." . embark-act)
        ("C-," . embark-dwim)
        ("C-:" . embark-export))
  (:map embark-file-map
        ("L" . vlf)
        ("S" . sudo-edit-find-file)))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :demand t
  :init (marginalia-mode)
  :custom
  (marginalia-align 'center)
  (marginalia-command-categories
   '((imenu . imenu)
     (persp-switch-to-buffer . buffer))))

(use-package all-the-icons-completion
  :init (all-the-icons-completion-mode))

(use-package corfu
  :custom
  (global-corfu-mode t)
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-preselect-first t)
  (corfu-commit-predicate #'corfu-candidate-previewed-p)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-preview-current 'insert)
  :config
  (require 'kind-icon)
  :bind
  ("C-<tab>" . completion-at-point)
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("C-SPC" . corfu-insert-separator)))

(use-package corfu-candidate-overlay
  :after corfu
  :config
  (corfu-candidate-overlay-mode +1)
  :preface
  (defun my-insert-corfu-or-tab ()
    (interactive)
    (if (and corfu-candidate-overlay--overlay
             (overlayp corfu-candidate-overlay--overlay)
             (corfu-candidate-overlay-at-word-boundary-p)
             (not (string= (corfu-candidate-overlay--get-overlay-property  'after-string) "")))
        (corfu-candidate-overlay-complete-at-point)
      (indent-for-tab-command)))
  :bind
  ("C-<iso-lefttab>" . corfu-candidate-overlay-complete-at-point))

(use-package corfu-history
  :ensure corfu
  :hook (corfu-mode . corfu-history-mode))

(use-package corfu-popupinfo
  :ensure corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay 0.5))

(use-package yasnippet-capf
  :vc (:rev :newest :url "https://github.com/elken/yasnippet-capf")
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package savehist
  :custom
  (savehist-mode t)
  :config
  (add-to-list 'delete-frame-functions #'(lambda (frame_) (savehist-autosave))))

(use-package vertico-repeat :ensure vertico
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("M-r" . vertico-repeat))

(use-package consult-yasnippet
  :custom
  (consult-yasnippet-use-thing-at-point t)
  (consult-yasnippet-always-overwrite-thing-at-point t)
  :bind ("C-c c y" . consult-yasnippet)
  :config
  (consult-customize consult-yasnippet :preview-key '(:debounce 0.01 any)))

(use-package expand-region
  :custom
  (expand-region-contract-fast-key "{")
  :bind
  ("C-c } }" . er/expand-region)
  ("C-c } d" . er/mark-defun)
  ("C-c } c" . er/mark-comment))

(use-package goto-last-point
  :config (goto-last-point-mode)
  :bind ("C-x j p" . goto-last-point))

(use-package link-hint
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

(use-package beacon
  :custom
  (beacon-blink-when-point-moves-vertically 20)
  (beacon-blink-when-buffer-changes nil)
  (beacon-blink-when-window-changes nil)
  (beacon-blink-when-window-scrolls nil)
  (beacon-mode 1)
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'nov-mode)
  (add-to-list 'beacon-dont-blink-major-modes 'vterm-mode)
  (add-to-list 'beacon-dont-blink-major-modes 'pdf-view-mode))

(use-package goggles
  :hook (prog-mode . goggles-mode)
  :custom
  (goggles-pulse t))

(use-package ctrlf
  :custom
  (ctrlf-mode 1)
  (ctrlf-auto-recenter nil)
  :bind
  (:map ctrlf-minibuffer-mode-map
        ("C-s" . ctrlf-forward-default)
        ("C-r" . ctrlf-backward-default)))

(use-package zygospore
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package beginend
  :config (beginend-global-mode))

(use-package resize-window
  :defines resize-window-dispatch-alist
  :config
  (add-to-list 'resize-window-dispatch-alist '(?- shrink-window-if-larger-than-buffer "Shrink if larger" nil))
  (add-to-list 'resize-window-dispatch-alist '(?+ balance-windows "Balance windows" nil))
  :bind ("C-c ;" . resize-window)
  :custom (resize-window-allow-backgrounds nil))

(use-package winner
  :hook (after-init . winner-mode))

(use-package imenu-list
  :custom
  (imenu-list-position 'left)
  (imenu-list-size 0.15)
  :bind ("C-c t i" . imenu-list-smart-toggle))

(use-package all-the-icons-ibuffer :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package dashboard
  :if (daemonp)
  :demand t
  :hook (dashboard-mode . page-break-lines-mode)
  :commands dashboard-insert-section dashboard-insert-heading dashboard-subseq
  :preface
  (defun dashboard-project-switch-function (root)
    (dired root)
    (my-open-readme))

  (defun dashboard-insert-scratch (list-size)
    (dashboard-insert-section
     "Shortcuts:"
     '("*scratch*" "*elfeed*" "init.el" "*vterm*" "Data Folder" "Home")
     list-size
     "Scratch"
     "s"
     `(lambda (&rest ignore)
        (cond
         ((string= "*scratch*" ,el) (switch-to-buffer "*scratch*"))
         ((string= "init.el" ,el) (find-file user-init-file))
         ((string= "Data Folder" ,el) (find-file my-data-directory))
         ((string= "*vterm*" ,el) (multi-vterm))
         ((string= "*elfeed*" ,el)
          (progn
            (if (get-buffer "*elfeed-search*")
                (switch-to-buffer "*elfeed-search*")
              (progn
                (elfeed)
                (elfeed-search-fetch nil)))))
         ((string= "Home" ,el) (dired (expand-file-name "~/")))
         (t (message "%s" ,el))))
     (format "%s" el)))
  :init (dashboard-setup-startup-hook)
  :config (add-to-list 'dashboard-item-generators  '(scratch . dashboard-insert-scratch))
  :custom
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-page-separator "\n\f\n")
  (dashboard-projects-backend 'project-el)
  (dashboard-projects-switch-function 'dashboard-project-switch-function)
  (dashboard-agenda-sort-strategy '(priority-down time-up))
  (dashboard-items '((scratch . 2)
                     (recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)))
  (dashboard-banner-logo-title "Emacs"))

(use-package all-the-icons :demand t)

(use-package doom-modeline
  :custom
  (doom-modeline-mode 1)
  (doom-modeline-icon t)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-project-name t)
  :init
  (doom-modeline-def-modeline 'main
  '(bar workspace-name window-number buffer-info remote-host buffer-position)
  '(compilation misc-info project-name persp-name github debug repl lsp major-mode process vcs check time)))

(use-package diminish)

(use-package ef-themes)

(use-package tangonov-theme
  ;; :after (custom org-faces)
  :demand t
  :config
  ;; Orange, Green, Blue, Red (I was used to this in my previous theme and different order messes my
  ;; brain)
  (custom-set-faces
   '(org-level-1 ((t (:inherit bold :foreground "#FFCA41"))))
   '(org-level-2 ((t (:inherit bold :foreground "#ABDC88"))))
   '(org-level-3 ((t (:inherit bold :foreground "#82AAFF"))))
   '(org-level-4 ((t (:inherit bold :foreground "#FF7B85"))))))

(use-package popper
  :commands popper-select-popup-at-bottom
  :bind (("C-\\"   . popper-toggle)
         ("M-\\"   . popper-cycle)
         ("C-M-\\"   . popper-toggle-type))
  :custom
  (popper-reference-buffers '("\\*Messages\\*" "Output\\*$" help-mode compilation-mode "gptel-"))
  (popper-display-function #'popper-select-popup-at-bottom)
  (popper-mode +1))

(use-package helpful
  :commands (get-buffers-matching-mode helpful-first-buffer-p helpful-not-first-buffer-p)
  :preface (defun alist-switch-or-pop (mode buf  &optional _)
             (if (derived-mode-p mode)
                 (switch-to-buffer buf)
               (progn
                 (when (= (length (window-list)) 1)
                   (split-window-horizontally))
                 (other-window 1)
                 (switch-to-buffer buf nil))))
  :config
  (add-to-list 'display-buffer-alist `("\\*helpful" (,(apply-partially #'alist-switch-or-pop 'helpful-mode))))
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h h" . helpful-at-point)
  :custom (helpful-max-buffers 5))

(use-package prog-mode :ensure nil
  :preface
  (defun eos/previous-function ()
    (interactive)
    (beginning-of-defun))
  (defun eos/next-function ()
    (interactive)
    (beginning-of-defun -1))
  :custom
  (tab-always-indent t)
  :bind
  (:map prog-mode-map
        ("C-c C-p" . eos/previous-function)
        ("C-c C-n" . eos/next-function)))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package project :ensure nil
  :preface (defun my-open-readme ()
             (interactive)
             (let* ((project-root (project-root (project-current t)))
                     (project-files (directory-files project-root nil nil t))
                     (readme-files (seq-filter (lambda (file) (string-prefix-p "readme" file t)) project-files)))
                (if readme-files
                    (let ((readme-file (car readme-files)))
                      (find-file (expand-file-name readme-file project-root)))
                  (dired-current-dir))))
  :custom
  (project-switch-commands #'my-open-readme)
  :bind-keymap ("C-c p" . project-prefix-map))

(use-package projection
  :after project
  :bind
  (:map project-prefix-map
        ("F" . projection-find-other-file)))

(use-package perspective
  :config
  ;; (setq switch-to-prev-buffer-skip
  ;;       (lambda (win buff bury-or-kill)
  ;;         (not (persp-is-current-buffer buff))))
  (consult-customize consult--source-buffer :hidden nil :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  :init
  (persp-mode t)
  :custom
  (persp-mode-prefix-key (kbd "C-c w")))

(use-package perspective-project-bridge
  :after perspective project
  :defines perspective-project-bridge-funcs
  :init
  (setq perspective-project-bridge-funcs (list #'my-open-readme #'project-find-file))
  (perspective-project-bridge-mode t))

(use-package magit
  :init (setq magit-define-global-key-bindings nil)
  :custom
  (magit-define-global-key-bindings nil)
  (magit-blame-echo-style 'headings)
  (magit-remote-git-executable "/tool/pandora64/.package/git-2.37.0/bin/git")
  (magit-repository-directories (list (cons (file-truename "~/Projects") 1)))
  (magit-auto-revert-mode nil)
  (magit-save-repository-buffers nil)
  (magit-uniquify-buffer-names nil)
  :config
  (defun my-magit-auto-revert-mode-advice (orig-fun &rest args)
    (unless (and buffer-file-name (file-remote-p buffer-file-name))
      (apply orig-fun args)))
  (advice-add 'magit-turn-on-auto-revert-mode-if-desired
              :around #'my-magit-auto-revert-mode-advice)
  :bind
  ("C-c g s" . magit-status-quick))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

(use-package git-link :custom (git-link-use-commit t))

(use-package git-timemachine)

(use-package git-messenger
  :custom
  (git-messenger:show-detail t)
  (git-messenger:use-magit-popup t)
  :bind ("C-c g m" . git-messenger:popup-message))

(use-package git-modes)

(use-package magit-pretty-graph
  :vc (:rev :newest :url "https://github.com/georgek/magit-pretty-graph")
  :after magit
  :commands magit-pg-repo
  :preface
  (defun magit-pretty-log ()
    (interactive)
    (magit-pg-repo (or (project-root (project-current)) default-directory))
    (with-current-buffer (get-buffer "*magit-prettier-graph*")
      (view-mode +1)))
  :init
  (transient-append-suffix 'magit-log "l" '("p" "Pretty Log" magit-pretty-log)))

(use-package copy-as-format
  :custom (copy-as-format-default "github"))

(use-package hl-todo
  :custom (global-hl-todo-mode 1))

(use-package abridge-diff
  :after magit
  :init (abridge-diff-mode 1))

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package vterm
  :commands (vterm-next-prompt vterm-prev-prompt)
  :defines (vterm-mode-map vterm-copy-mode-map)
  :hook
  ((vterm-mode . set-no-process-query-on-exit)
   (vterm-mode . disable-line-numbers)
   (vterm-mode . my/vterm-source-bashrc))
  :preface
  (defun my/vterm-source-bashrc ()
    (interactive)
    (vterm-send-string "source ~/.bash_profile")
    (vterm-send-return))
  (defun vterm-cd-other-buffer ()
    (interactive)
    (let* ((buffer (current-buffer))
          (other-buffer (window-buffer (next-window)))
          (directory (with-current-buffer other-buffer default-directory))
          (cmd (concat "cd " directory)))
      (vterm-send-string cmd t)
      (vterm-send-return)))
  (defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))
  (defun vterm-next-prompt () (interactive)
         (re-search-forward "\\] \\$ " nil 'move))
  (defun vterm-prev-prompt () (interactive)
         (move-beginning-of-line nil)
         (re-search-backward "\\] .*\\$ " nil 'move)
         (re-search-forward "\\] \\$ " nil 'move))
  (defun my-vterm-persp-toggle (&optional args)
    "Vterm toggle.
Optional argument ARGS ."
    (interactive "P")
    (cond
     ((derived-mode-p 'vterm-mode)
      (delete-window)) ;; TODO
     ((persp-curr)
      (if-let* ((curr-persp (persp-curr))
                (buffers (persp-buffers curr-persp))
                (buffer-names (seq-map (lambda (buf) (buffer-name buf)) buffers))
                (persp-vterm-name (concat "*vterm: " (persp-name curr-persp) " *"))
                (vterm-buffer (car (seq-filter (lambda (buf) (string-equal persp-vterm-name buf)) buffer-names))))
          (pop-to-buffer vterm-buffer)
        (vterm persp-vterm-name)
        (persp-add-buffer persp-vterm-name)))
     ((get-buffer vterm-buffer-name)
      (pop-to-buffer vterm-buffer-name))
     (t
      (vterm vterm-buffer-name))))
  :config
  (add-to-list 'display-buffer-alist (cons "\\*vterm" use-other-window-alist))
  (add-to-list 'vterm-tramp-shells '("ssh" "/bin/bash"))
  (add-to-list 'vterm-tramp-shells '("scpx" "/bin/bash"))
  :custom
  (vterm-copy-exclude-prompt t)
  (vterm-shell (getenv "SHELL"))
  :bind
  ("<f8>" . my-vterm-persp-toggle)
  (:map vterm-mode-map
        ("<f8>" . my-vterm-persp-toggle)
        ("C-y" . vterm-yank)
        ("M-y" . vterm-yank-pop)
        ("M-:" . eval-expression)
        ("C-<return>" . vterm-cd-other-buffer)
        ("C-S-<return>" . vterm-toggle-insert-cd))
  (:map vterm-copy-mode-map
        ("C-<" . vterm-prev-prompt)
        ("C->" . vterm-next-prompt)))

(use-package compile :ensure nil
  :preface
  (make-variable-buffer-local 'my-compilation-start-time)
  (defun my-compilation-start-hook (_)
    (setq my-compilation-start-time (current-time)))
  (defun my-compilation-finish-function (buf why)
    (let* ((elapsed  (time-subtract nil my-compilation-start-time))
           (msg (format "Elapsed: %s" (format-time-string "%T.%N" elapsed t)))
           (alert-msg (format "Emacs[Compilation]: %s at %s" why (buffer-name buf))))
      (when (string= buf "*compilation*")
        (with-current-buffer buf
          ;; (when (string-match "finished" why)
  	      ;;   (bury-buffer buf)
          ;;   (popper-close-latest))
          (save-excursion (goto-char (point-max)) (insert msg))
          (if my-running-under-wsl
              (start-process "wsl-notify-send.exe" nil "wsl-notify-send.exe" alert-msg)
            (alert alert-msg))
          (message alert-msg)))))
  :hook   (compilation-start . my-compilation-start-hook)
  :custom (compilation-scroll-output t)
  :config (add-hook 'compilation-finish-functions #'my-compilation-finish-function)
  :bind ("C-c C-r" . recompile))

(use-package isend-mode
  :bind ("C-M-<return>" . isend-send))

(use-package repl-toggle
  :custom
  (rtog/mode-repl-alist (list (cons 'emacs-lisp-mode #'ielm)
                              (cons 'python-mode #'python-shell-switch-to-shell)
                              (cons 'tcl-mode #'(lambda () (call-interactively #'inferior-tcl)))))
  (rtog/goto-buffer-fun 'pop-to-buffer)
  :bind ("C-c C-z" . rtog/toggle-repl))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package electric-operator
  :hook (cc-mode . electric-operator-mode))

(use-package yasnippet-snippets)

(use-package yasnippet
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . nil)
        ("M-i" . yas-expand)))

(use-package vc
  :custom
  (vc-handled-backends '(Git)))

(use-package tramp
  :commands (tramp-cleanup-all-connections tramp-cleanup-all-buffers)
  :config (defun tramp-done ()
            (interactive)
            (tramp-cleanup-all-connections)
            (tramp-cleanup-all-buffers))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "/home/furkanu/.local/bin")
  (add-to-list 'tramp-remote-path "/tool/pandora64/.package/git-2.37.0/bin")
  (connection-local-set-profile-variables 'remote-direct-async-process '((tramp-direct-async-process . t)))
  :custom
  (remote-file-name-inhibit-locks t)
  (tramp-use-scp-direct-remote-copying t)
  (remote-file-name-inhibit-auto-save-visited t)
  (remote-file-name-inhibit-cache nil)
  (tramp-default-method "scpx")
  (tramp-copy-size-limit (* 1024 1024)) ;; 1MB

  (tramp-histfile-override nil)
  (tramp-backup-directory-alist backup-directory-alist))

(use-package auto-highlight-symbol
  :hook (prog-mode . auto-highlight-symbol-mode))

(use-package which-key)

(use-package gdb-mi
  :custom
  (gdb-many-windows t))

(use-package hl-prog-extra
  :hook (prog-mode . hl-prog-extra-mode))

(use-package subword
  :hook ((yaml-mode conf-mode java-mode js-mode) . subword-mode))

(use-package bug-reference
  :hook ((prog-mode org-mode) . bug-reference-prog-mode))

(use-package devdocs
  :hook (devdocs-mode . shrface-mode)
  :bind
  ("C-h D" . devdocs-lookup))

(use-package fancy-compilation
  :hook (compilation-mode . fancy-compilation-mode)
  :custom
  (fancy-compilation-override-colors nil))

(use-package obvious
  :vc (:rev :newest :url "https://github.com/alphapapa/obvious.el")
  :bind
  ("C-c v o" . obvious-mode))

(use-package eglot
  ;; :hook
  ;; (prog-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs `(python-ts-mode . ,(cdr (assoc 'python-mode eglot-server-programs))))
  (add-to-list 'eglot-server-programs `(ruby-ts-mode "solargraph" "socket" "--port" :autoport))
  (add-to-list 'eglot-server-programs `((c-ts-mode c++-ts-mode) . ,(cdr (assoc '(c++-mode c-mode) eglot-server-programs))))
  (add-to-list 'eglot-server-programs `(tex-mode . ,(eglot-alternatives
                                                     '(("~/.local/prog/digestif/digestif.sh")
                                                       ("texlab")))))
  (add-to-list 'completion-at-point-functions #'eglot-completion-at-point)
  :custom
  (eglot-events-buffer-size 0)
  :bind
  (:map eglot-mode-map)
  ("M-/" . eglot-find-declaration))

(use-package breadcrumb
  :hook
  (lsp-mode . breadcrumb-mode))

(use-package eglot-booster
  :vc (:rev :newest :url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-idle-delay 1.5))

(use-package project-rootfile
  :defer nil
  :after project
  :config
  ;; (add-to-list 'project-find-functions #'project-rootfile-try-detect t)
  (add-to-list 'project-rootfile-list "pyproject.toml")
  (add-to-list 'project-rootfile-list ".project")
  (add-to-list 'project-rootfile-list ".github"))

(use-package makefile-executor
  :hook (makefile-mode . makefile-executor-mode)
  :bind
  (:map project-prefix-map
        ("x" . makefile-executor-execute-last)
        ("X" . project-execute-extended-command)))

(use-package flymake)

(use-package flymake-cursor
  :hook (flymake-mode . flymake-cursor-mode))

(use-package flymake-diagnostic-at-point
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ("C-c TAB" . hs-toggle-hiding)))

(use-package cc-mode :ensure nil
  :mode
  ("\\.h\\'" . c++-mode)
  ("\\.h\\.pp\\'" . c++-mode)
  ("\\.cc\\.pp\\'" . c++-mode)
  :custom
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "stroustrup")))
  (c-basic-offset 4)
  (c-noise-macro-names '("constexpr"))
  :config
  (c-add-style
   "doom" '((c-comment-only-line-offset . 0)
            (c-hanging-braces-alist (brace-list-open)
                                    (brace-entry-open)
                                    (substatement-open after)
                                    (block-close . c-snug-do-while)
                                    (arglist-cont-nonempty))
            (c-cleanup-list brace-else-brace)
            (c-offsets-alist
             (knr-argdecl-intro . 0)
             (substatement-open . 0)
             (substatement-label . 0)
             (statement-cont . +)
             (case-label . +)
             ;; align args with open brace OR don't indent at all (if open
             ;; brace is at eolp and close brace is after arg with no trailing
             ;; comma)
             (brace-list-intro . 0)
             (brace-list-close . -)
             (arglist-intro . +)
             (arglist-close +cc-lineup-arglist-close 0)
             ;; don't over-indent lambda blocks
             (inline-open . 0)
             (inlambda . 0)
             ;; indent access keywords +1 level, and properties beneath them
             ;; another level
             (access-label . -)
             (inclass +cc-c++-lineup-inclass +)
             (label . 0))))
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'access-label -4)
  (c-toggle-cpp-indent-to-body t))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode)

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package meson-mode)

(use-package ansi-color
  :commands ansi-color-apply-on-region
  :hook (compilation-filter . colorize-compilation-buffer)
  :preface (defun colorize-compilation-buffer ()
             (read-only-mode)
             (ansi-color-apply-on-region (point-min) (point-max))
             (read-only-mode -1))
  :config (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  :custom (ansi-color-for-comint-mode t))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package dockerfile-mode     :mode ("Dockerfile\\'" "\\.docker"))

(use-package docker-compose-mode :mode ("docker-compose\\.yml\\'" "-compose.yml\\'"))

(use-package docker)

(use-package tramp-container :ensure nil)

(use-package verilog-mode
  :mode
  ;; Treesitter mode cannot handle Jinja syntax
  ("\\.v\\.pp\\'" . verilog-mode)
  ("\\.sv\\.pp\\'" . verilog-mode)
  ("\\.gv\\'" . verilog-mode)
  ("\\.gsv\\'" . verilog-mode)
  :custom
  ;; (verilog-auto-inst-sort t)
  (verilog-library-directories '("." "../sim" "../rtl"))
  (verilog-auto-declare-nettype "none")
  (verilog-case-fold nil)
  (verilog-auto-newline nil)
  (verilog-tab-always-indent nil)
  (verilog-auto-indent-on-newline t)
  (verilog-case-indent 4)
  (verilog-cexp-indent 4)
  (verilog-indent-begin-after-if nil)
  (verilog-indent-level 4)
  (verilog-indent-lists nil)
  (verilog-indent-level-behavioral 4)
  (verilog-indent-level-directive 4)
  (verilog-indent-level-declaration 4)
  (verilog-indent-level-module 4))

;; (use-package graphviz-dot-mode
;;   :custom (graphviz-dot-indent-width 4))

(use-package elisp-mode :ensure nil)

(use-package lisp :ensure nil
  :hook (lisp-mode . auto-highlight-symbol-mode))

(use-package lisp-extra-font-lock
  :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))

(use-package highlight-function-calls
  :hook (emacs-lisp-mode . highlight-function-calls-mode))

(use-package comment-or-uncomment-sexp
  :bind ("C-M-;" . comment-or-uncomment-sexp))

(use-package elisp-def
  :hook
  (emacs-lisp-mode . elisp-def-mode)
  (ielm-mode . elisp-def-mode))

(use-package refine
  :bind
  (:map helpful-mode-map
        ("e" . refine)))

(use-package eval-sexp-fu
  :bind
  ("C-c e s" . eval-sexp-fu-eval-sexp-inner-list))

(use-package smartparens
  :hook
  ((prog-mode . smartparens-mode)
   (conf-mode . smartparens-mode)
   (minibuffer-mode . smartparens-mode)
   (text-mode . smartparens-mode))
  :config
  (require 'smartparens-config)
  (add-to-list 'sp-ignore-modes-list 'web-mode)
  :bind (:map smartparens-mode-map
              ("C-c l w" . sp-copy-sexp)
              ("C-c l b" . sp-backward-up-sexp)
              ("C-c l u" . sp-unwrap-sexp)
              ("C-c l f" . sp-forward-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-u" . sp-up-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-c [" . sp-wrap-square)
              ("C-c {" . sp-wrap-curly)
              ("C-c (" . sp-wrap-round)
              ("C-c l SPC" . sp-select-next-thing)
              ("C-c l C-SPC" . sp-select-next-thing-exchange)
              ("C-c l k" . sp-kill-sexp)))

(use-package jinja2-mode
  :mode
  ("\\.jinja\\'" . jinja2-mode)
  ("\\.rgr.pp\\'" . jinja2-mode))

(use-package web-mode
  :hook (web-mode . disable-smartparens)
  :config
  (require 'jinja2-mode)
  (defun disable-smartparens ()
    (smartparens-mode -1))
  ;; (smartparens-mode nil)
  (defun jinja2-close-tag ()
  "Close the previously opened template tag. Original one tries to indent, which is broken"
  (interactive)
  (let ((open-tag (save-excursion (jinja2-find-open-tag))))
    (if open-tag
        (insert
         (if (string= (car open-tag) "block")
             (format "{%% end%s%s %%}"
                     (car open-tag)(nth 1 open-tag))
           (format "{%% end%s %%}"
                   (match-string 2))))
      (error "Nothing to close"))))
  (defun my-jinja-close-tag ()
    (interactive)
    (jinja2-close-tag)
    (indent-for-tab-command))
  :bind
  (:map web-mode-map
        ("C-c [" . jinja2-insert-tag)
        ("C-c ]" . my-jinja-close-tag)))

(use-package format-all)

(use-package js
  :custom (js-indent-level 2))

(use-package python
  :mode
  ("\\.pf\\'" . python-mode)
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (cond
   ((executable-find "ipython")
    (setq python-shell-buffer-name "IPython"
          python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt"))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   (t (setq python-shell-interpreter "python"))))

(use-package perl :ensure nil
  :custom
  (cperl-indent-level 4)
  (cperl-indent-parens-as-block t)
  (cperl-continued-statement-offset 0)
  (cperl-close-paren-offset -4)
  :mode ("\\.pl\\'" . cperl-mode))

(use-package tcl
  :custom
  (tcl-application "tclsh"))

(use-package ruby-mode
  :mode ("\\.dj\\'" . ruby-mode))

(use-package org
  :hook
  (org-mode . smartparens-mode)
  (org-babel-after-execute . org-redisplay-inline-images)
  :preface
  (defun my-update-all-bibs ()
    (interactive)
    (let ((buf (current-buffer))
          (files (f-glob "*.org")))
      (dolist (file files)
        (with-current-buffer (find-file file)
          (org-babel-tangle nil nil "bibtex")))
      (switch-to-buffer buf)))
  :custom
  (org-modules (list 'ol-eww 'org-tempo 'ol-info 'ol-docview 'ol-bibtex 'ol-doi 'org-habit))
  (org-default-notes-file (concat my-notes-directory "/Capture.org"))
  (org-adapt-indentation t)
  (org-catch-invisible-edits 'show-and-error)
  (org-cycle-separator-lines 0)
  (org-edit-src-content-indentation 0)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers nil)
  (org-hide-leading-stars t)
  (org-imenu-depth 2)
  (org-indent-indentation-per-level 1)
  (org-log-done t)
  (org-babel-load-languages '((python . t)
                              (shell . t)
                              (emacs-lisp . t)))
  ;; (org-pretty-entities t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation nil)
  (org-src-tab-acts-natively t)
  (org-yank-adjusted-subtrees t)
  (org-export-with-todo-keywords t)
  (org-export-with-tag nil)
  (org-latex-listings 'minted)
  (org-log-done 'nil) ;; clock-out already records timestamps ??
  (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "|" "DONE")
                       (sequence "PAUSED" "SCHEDULED" "WAITING" "NEXT" "|" "CANCELLED")))
  :config
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  :custom-face
  (org-cite-key ((t (:foreground "light blue" :slant italic))))
  :bind
  (:map org-mode-map
        ("C-c C-." . org-time-stamp-inactive)
        ("DEL" . org-delete-backward-char)
        ("M-q" . nil)))

(use-package my-org-bibtex
  :ensure nil
  :no-require t
  :after (org bibtex)
  :preface
  (defun my/set-roam-refs-from-bibtex-in-buffer ()
    "Iterate over top and second-level headings in buffer,
and process each for BibTeX keys."
    (interactive)
    (org-map-entries
     (lambda ()
       (save-excursion
         (org-back-to-heading t)
         (org-id-get-create)
         (let* ((headline (org-element-at-point))
                (section (org-element-property :contents-begin headline))
                (end (org-element-property :contents-end headline))
                (bibtex-keys '()))
           (when (and section end)
             (save-restriction
               (narrow-to-region section end)
               (goto-char section)
               (while (re-search-forward "^ *#\\+begin_src bibtex" nil t)
                 (let* ((element (org-element-at-point))
                        (value (org-element-property :value element))
                        (lines (split-string value "\n" t))
                        (first-line (car lines)))
                   (when (and first-line (string-match "@\\w+{\\([^,]+\\)," first-line))
                     (push (concat "@" (match-string 1 first-line)) bibtex-keys)))))
             (when bibtex-keys
               (let ((refs (string-join (reverse bibtex-keys) " ")))
                 (org-set-property "ROAM_REFS" refs)))))))))
  (defun my/tag-headings-without-pdf ()
    "For top and second-level Org headings with ROAM_REFS property,
check if corresponding PDF exists in `my-papers-directory`.
If not, add the tag NOPDF; if found, remove the NOPDF tag."
    (interactive)
    (org-map-entries
     (lambda ()
       (when-let ((roam-ref (org-entry-get (point) "ROAM_REFS")))
         (let* ((bibtex-key (substring roam-ref 1))
                (pdf-path (expand-file-name (concat bibtex-key ".pdf") my-papers-directory))
                (tags (org-get-tags)))
           (if (file-exists-p pdf-path)
               (org-toggle-tag "NOPDF" 'off)
             (org-toggle-tag "NOPDF" 'on)))))))
  (defun my/select-random-todo-with-roam-refs (arg)
    "Select a TODO with ROAM_REFS.
With C-u, let user pick a file from `org-agenda-files` (show only filenames).
With C-u C-u, list all matching entries and let user select.
Prioritize entries without NOPDF tag."
    (interactive "P")
    (let* ((all-files (org-agenda-files))
           (display-files (mapcar #'file-name-nondirectory all-files))
           (selected-files (cond
                            ((equal arg '(4)) ; C-u
                             (let* ((chosen (completing-read "Select file: " display-files))
                                    (full-path (seq-find (lambda (f) (string-suffix-p chosen f)) all-files)))
                               (list full-path)))
                            (t all-files)))
           (candidates-inprogress ())
           (candidates-haspdf ())
           (candidates-nopdf ()))
      (dolist (file selected-files)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (when (and (org-entry-is-todo-p) (org-entry-get (point) "ROAM_REFS"))
               (if (member "NOPDF" (org-get-tags))
                   (push (point-marker) candidates-nopdf)
                 (if (string= (car (org-entry-is-todo-p)) "IN-PROGRESS")
                     (push (point-marker) candidates-inprogress)
                   (push (point-marker) candidates-haspdf))))))))

      (if-let* ((candidates (or candidates-inprogress candidates-haspdf candidates-nopdf))
                (choices (mapcar (lambda (c)
                                   (with-current-buffer (marker-buffer c)
                                     (goto-char (marker-position c))
                                     (concat (file-name-nondirectory (buffer-file-name))
                                             ": "
                                             (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment))))
                                 candidates))
                (chosen (if (equal arg '(16))
                            (nth (cl-position
                                  (completing-read "Select entry: " choices) choices :test #'string=) candidates)
                          (seq-random-elt candidates))))
          (progn
            (switch-to-buffer (marker-buffer chosen))
            (goto-char (marker-position chosen))
            (delete-other-windows)
            (my-citar-open-current-file)
            (my-open-citar-note))
        (message "No matching TODOs with ROAM_REFS found."))))
  :bind
  ("C-c n r" . my/select-random-todo-with-roam-refs))

  (use-package my-org-biblio
  :ensure nil
  :no-require t
  :after (org biblio)
  :preface
  (defun my-insert-bibtex-entry (buffer bibtex-entry)
    (interactive)
    (let* ((title (if bibtex-entry
                      (save-excursion
                        (with-temp-buffer
                          (insert bibtex-entry)
                          (goto-char (point-min))
                          (let* ((title (substring (cdr (assoc "title" (bibtex-parse-entry))) 1 -1))
                                 (title-single (string-replace "\n" " " title))
                                 (title-squeezed (replace-regexp-in-string " +" " " title-single)))
                            title-squeezed)))
                    "Title")))
      (with-current-buffer buffer
        (org-insert-heading t)
        (insert title)
        (org-id-get-create)
        (forward-line 1)
        (org-cycle)
        (forward-line 3)
        (insert "   #+begin_src bibtex\n" bibtex-entry "\n   #+end_src")
        (org-edit-src-code)
        (org-edit-src-exit)
        (forward-line 2)
        (insert "\n"))))
  (defun my-insert-bibtex-entry-doi (doi)
    (interactive (list (read-string "DOI: ")))
    (require 'biblio)
    (biblio-doi-forward-bibtex
     (biblio-cleanup-doi doi)
     (apply-partially
      (lambda (buffer result)
        (my-insert-bibtex-entry buffer (biblio-format-bibtex result biblio-bibtex-use-autokey)))
      (current-buffer)))))

(use-package corg
  :vc (:rev :newest :url "https://github.com/isamert/corg.el")
  :hook (org-mode . corg-setup))

(use-package ob-async)

(use-package org-alert
  :commands org-alert-enable
  :config (org-alert-enable))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package org-cliplink
  :after org
  :bind (:map org-mode-map ("C-c i l" . org-cliplink)))

(use-package org-capture :ensure org
  :preface
  (defvar org-capture-file (concat my-notes-directory "/Capture.org"))
  :custom
  (org-capture-templates '(("t" "TODO" entry (file+headline org-capture-file "Tasks")
						    "* TODO %?\n	%a\n  %i\n")
					       ("j" "Journal" entry (file+headline org-capture-file "Journal")
						    "* %U\n	 %a\n	 %i")
                           ("w" "Web site" entry
                            (file "")
                            "* %a :website:\n\n%U %?\n\n%:initial" :immediate-finish t)
					       ("p" "Protocol" entry (file+headline org-capture-file "Inbox")
	    				    "* %?\n	 [[%:link][%:description]]\n	%U\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n")
	    			       ("L" "Protocol Link" entry (file+headline org-capture-file "Inbox")
						    "* %?\n	 [[%:link][%:description]]\n	%U")))
  :bind ("C-c C-c" . org-capture))

(use-package org-table :ensure org
  :preface
  (defun my-org-copy-table-cell ()
    (interactive)
    (when (org-at-table-p)
      (kill-new
       (string-trim
        (substring-no-properties(org-table-get-field))))))
  :bind
  (:map org-mode-map
        ("M-W" . my-org-copy-table-cell)))

(use-package org-protocol :ensure org)

(use-package org-agenda :ensure org
  :custom
  (org-agenda-show-future-repeats nil)
  (org-agenda-files (list my-notes-directory))
  (org-agenda-include-diary t)
  (org-agenda-span 10)
  (org-agenda-start-day "-2d")
  :bind ("C-c c a" . org-agenda))

(use-package org-refile :ensure org
  :preface
  (defun my/org-refile-contents-only ()
    "Refile only the content of the current heading (and remove the leftover heading)"
    (interactive)
    (save-excursion
      (call-interactively 'org-refile)
      (let* ((refile-bm-name (plist-get org-bookmark-names-plist :last-refile))
             (refile-file (bookmark-prop-get refile-bm-name 'filename))
             (refile-pos (bookmark-prop-get refile-bm-name 'position)))
        (with-current-buffer (find-file-noselect refile-file)
          (goto-char refile-pos)
          (org-promote-subtree)
          (beginning-of-line)
          (kill-whole-line)))))
  :custom
  ;; (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps t)
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 2)))
  (org-refile-allow-creating-parent-nodes 'confirm)
  :bind
  ("C-c C-S-w" . my/org-refile-contents-only))

(use-package org-clock :ensure org
  :preface
  (defun my-org-clock-out-habit (current-state)
    (if (string= (org-entry-get nil "STYLE") "habit")
        "DONE"
      current-state))
  :config
  (org-clock-persistence-insinuate)
  :custom
  (org-clock-persist t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-report-include-clocking-task t)
  (org-clock-out-when-done t)
  (org-clock-in-switch-to-state "IN-PROGRESS")
  (org-clock-out-switch-to-state #'my-org-clock-out-habit)
  :bind
  (:map org-mode-map
        ("C-c C-x i" . org-clock-in)))

(use-package org-clock-reminder
  :custom
  (org-clock-reminder-inactive-notifications-p t)
  (org-clock-reminder-interval 45)
  (org-clock-reminder-mode t))

;; (use-package org-habit-stats
;;   :vc (:rev :newest :url "https://github.com/ml729/org-habit-stats")
;;   )


(use-package consult-org-clock
  :vc (:rev :newest :url "https://github.com/overideal/consult-org-clock")
  :after org-mode
  :bind
  ("C-c C-x i" . consult-org-clock)
  ("C-c C-x s" . consult-org-clock-goto))

(use-package org-mru-clock
  ;; :hook (minibuffer-setup . org-mru-clock-embark-minibuffer-hook)
  :bind
  ("C-c C-x i" . org-mru-clock-in)
  ("C-c C-x s" . org-mru-select-recent-task))

(use-package org-appear
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t))

(use-package oc :ensure org
  :config
  (require 'oc-csl)
  (require 'oc-biblatex)
  (require 'oc-natbib)
  :custom
  (org-cite-export-processors '((latex biblatex) (t csl)))
  (org-support-shift-select t))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

;; (use-package org-pretty-table
;;   :vc (:rev :newest :url "https://github.com/Fuco1/org-pretty-table")
;;   :hook (org-mode . org-pretty-table-mode))

(use-package bibtex-completion
  :custom
  (bibtex-completion-bibliography my-bibliographies)
  (bibtex-completion-notes-path my-notes-directory)
  (bibtex-completion-additional-search-fields '(keywords))
  (bibtex-completion-library-path (list my-papers-directory)))

(use-package org-special-block-extras)

(use-package org-journal
  :bind ("C-c i j" . org-journal-new-entry)
  :custom
  (org-journal-dir (concat my-notes-directory "/Journal"))
  (org-journal-file-format "%Y-%m.org")
  (org-journal-date-format "%A, %d %B")
  (org-journal-file-type 'monthly)
  (org-journal-time-format nil)
  (org-journal-created-property-timestamp-format "%F"))

;; (use-package inherit-org
;;   :vc (:rev :newset :url "https://github.com/chenyanming/inherit-org")
;;   :hook ((info-mode helpful-mode ghelp-page-mode) . inherit-org-mode))

(use-package shrface
  :after org
  :commands (shrface-basic shrface-trial shrface-outline-cycle org-at-heading-p)
  :config
  (shrface-basic)
  (shrface-trial)
  :preface
  (defun org-tab-or-next-heading ()
    (interactive)
    (if (org-at-heading-p)
        (shrface-outline-cycle)
      (progn
        (org-next-visible-heading 1)
        (unless (org-at-heading-p)
          (org-previous-visible-heading 1)))))
  :hook (inherit-org-mode . shrface-mode)
  :custom (shrface-href-versatile t)
  :bind (:map shrface-mode-map
              ("<tab>" . shrface-outline-cycle)
              ("C-<tab>" . org-tab-or-next-heading)
              ("<backtab>" . shrface-outline-cycle-buffer)
              ("C-c C-n" . shrface-next-headline)
              ("C-c C-p" . shrface-previous-headline)
              ("C-c C-l" . shrface-links-consult)
              ("C-c C-h" . shrface-headline-consult)
              ("M-<up>" . org-previous-visible-heading)))

(use-package shr-tag-pre-highlight
  :after shrface
  :functions (shr-tag-pre-highlight-fontify
              shr-tag-pre-highlight--get-lang-mode
              shr-tag-pre-highlight-guess-language-attr)
  :preface
  (defun shrface-shr-tag-pre-highlight (pre)
    "Highlighting code in PRE."
    (let* ((shr-folding-mode 'none)
           (shr-current-font 'default)
           (code (with-temp-buffer
                   (shr-generic pre)
                   ;; (indent-rigidly (point-min) (point-max) 2)
                   (buffer-string)))
           (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                     (let ((sym (language-detection-string code)))
                       (and sym (symbol-name sym)))))
           (mode (and lang (shr-tag-pre-highlight--get-lang-mode lang)))
           (start)
           (end))
      (shr-ensure-newline)
      (shr-ensure-newline)
      (setq start (point))
      (insert
       (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
       (or (and (fboundp mode)
                (with-demoted-errors "Error while fontifying: %S"
                  (shr-tag-pre-highlight-fontify code mode)))
           code)
       (propertize "#+END_SRC" 'face 'org-block-end-line ))
      (shr-ensure-newline)
      (setq end (point))
      (if (eq 'my-light-theme (car custom-enabled-themes))
          (add-face-text-property start end '(:background "#D8DEE9" :extend t))
        (add-face-text-property start end '(:background "#292b2e" :extend t)))
      (shr-ensure-newline)
      (insert "\n")))
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shrface-shr-tag-pre-highlight)))

(use-package engine-mode
  :custom
  ;; (engine-mode t)
  (engine/browser-function 'eww-browse-url)
  :init
  (defengine github "https://github.com/search?ref=simplesearch&q=%s")
  (defengine google "https://google.com/search?q=%s" :keybinding "g")
  (defengine wikipedia "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s" :keybinding "w")
  (defengine wolfram "http://www.wolframalpha.com/input/?i=%s"))

(use-package svg-tag-mode
  :vc (:rev :newest :url "https://github.com/rougier/svg-tag-mode"))

(use-package notebook
  :vc (:rev :newest :url "https://github.com/rougier/notebook-mode"))

(use-package org-transclusion
  :vc (:rev :newest :url "https://github.com/nobiot/org-transclusion")
  :custom (org-transclusion-activate-persistent-message nil))

(use-package org-rich-yank
  :bind (:map org-mode-map ("C-M-y" . org-rich-yank)))

(use-package side-notes
  :custom
  (side-notes-display-alist '((side . right)
                              (window-width . 50)))
  (side-notes-file "Notes.org")
  (side-notes-secondary-file "README.org")
  :bind ("C-c t n" . side-notes-toggle-notes))

;; (use-package ox-pandoc)

(use-package org-pandoc-import
  :vc (:rev :newest :url "https://github.com/tecosaur/org-pandoc-import"))

(use-package org-agenda-property
  :custom (org-agenda-property-list '(LOCATION)))

(use-package org-timeline
  :hook (org-agenda-finalize . org-timeline-insert-timeline))

(use-package org-download
  :hook
  (dired-mode . org-download-enable)
  (org-mode . org-download-enable))

(use-package ox-hugo)

(use-package org-tanglesync
  :bind
  ( "C-c M-i" . org-tanglesync-process-buffer-interactive)
  ( "C-c M-a" . org-tanglesync-process-buffer-automatic))

(use-package orgdiff
  :vc (:rev :newest :url "https://github.com/tecosaur/orgdiff"))

(use-package org-elp
  :custom
  (org-elp-idle-time 0.5)
  (org-elp-split-fraction 0.25))

(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

(use-package code-cells
  :hook (python-mode . code-cells-mode-maybe)
  :config
  (let ((map code-cells-mode-map))
      (define-key map "n" (code-cells-speed-key 'code-cells-forward-cell))
      (define-key map "p" (code-cells-speed-key 'code-cells-backward-cell))
      (define-key map "e" (code-cells-speed-key 'code-cells-eval))
      (define-key map "RET" (code-cells-speed-key 'code-cells-eval))
      (define-key map (kbd "TAB") (code-cells-speed-key 'outline-cycle)))
  :bind
  (:map code-cells-mode-map
        ("C-c C-n" . code-cells-forward-cell)
        ("C-c C-p" . code-cells-backward-cell)
        ("M-RET" . code-cells-eval)))

(use-package comint-mime
  :hook (inferior-python-mode . comint-mime-setup))

(use-package org-auctex
  :vc (:rev :newest :url "https://github.com/karthink/org-auctex"))

(use-package org-protocol-capture-html
  :vc (:rev :newest :url "https://github.com/alphapapa/org-protocol-capture-html"))

(use-package citar
  :preface
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-green :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))

  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))

  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))

  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon "circle-o" :face 'all-the-icon-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))

  :custom
  (citar-bibliography my-bibliographies)
  ;; (citar-open-note-function 'orb-citar-edit-note)
  (citar-library-paths (list my-papers-directory))
  (citar-at-point-function 'embark-act)
  (citar-symbol-separator " ")
  :config
  (setq citar-indicators (list citar-indicator-files-icons
                               citar-indicator-links-icons
                               citar-indicator-notes-icons
                               citar-indicator-cited-icons))
  (add-to-list 'completion-at-point-functions 'citar-capf))

(use-package my-citar-org
  :ensure nil
  :no-require t
  :after org
  :init
  (require 'citar)
  (defun my-citar-open-current-resource (files notes)
    "Open REFs of the node at point."
    (interactive)
    (when-let* ((key (substring (org-entry-get (point) "ROAM_REFS") 1))
                (selected (citar--select-resource key :files files :notes notes)))
      (progn
        (when (= 1 (length (window-list)))
          (split-window-horizontally))
        (other-window 1)
        (citar--open-resource (cdr selected) (car selected))
        (when (eq 'file (car selected))
          (pdf-view-fit-width-to-window)))))
  (defun sci-hub-download-url (doi)
    "Get url to the pdf from SCI-HUB"
    (when-let* ((doi)
                (url (concat "https://sci-hub.se/" doi)))
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (setq *asd* (buffer-string))
        (when (search-forward-regexp "\\(https:\\)?//\\([^.]+.\\)?sci-hub.se/.+download=true" nil t)
          (let* ((url (match-string 0))
                 (https-url (if (string-match "https:" url) url (concat "https:" url))))
            https-url)))))
  (defun arxiv-download-url (entry)
    "Get url to the pdf from ArXiv"
    (when
      (or
       (string= "arxiv" (downcase (or (bibtex-completion-get-value "journal" entry) "")))
       (string= "arxiv" (downcase (or (bibtex-completion-get-value "archiveprefix" entry) ""))))
      (let* ((volume (bibtex-completion-get-value "volume" entry))
             (eprint (bibtex-completion-get-value "eprint" entry))
             (arxiv-id (or volume eprint))
             (id (string-remove-prefix "abs/" arxiv-id))
             (url (concat "https://arxiv.org/pdf/" id ".pdf")))
        url)))
  (defun semantic-scholar-download-url (doi)
    "Get url to the pdf from ArXiv"
    (when-let* ((doi)
                (url (format "https://api.semanticscholar.org/graph/v1/paper/DOI:%s?fields=isOpenAccess,openAccessPdf" doi))
                (data (with-current-buffer (url-retrieve-synchronously url) (json-parse-buffer)))
                (is-openAccess (gethash "isOpenAccess" data))
                (url (gethash "url" (gethash "openAccessPdf" data))))
      url))
  (defun my-download-pdf (&optional key open-file)
    (interactive "P")
    (let* ((key (or key (orb-get-node-citekey nil 'assert)))
                (entry (bibtex-completion-get-entry key))
                (bibtex-doi (bibtex-completion-get-value "doi" entry))
                (doi (when bibtex-doi (replace-regexp-in-string "https?://\\(dx.\\)?.doi.org/" "" bibtex-doi)))
                (pdf-url (or (bibtex-completion-get-value "pdf-url" entry)
                             (arxiv-download-url entry)
                             (semantic-scholar-download-url doi)
                             (sci-hub-download-url doi)))
                (pdf-file (concat my-papers-directory "/" key ".pdf")))
      ;; now get file if needed.
      (unless (file-exists-p pdf-file)
        (url-copy-file pdf-url pdf-file))
      (when open-file
        (org-open-file pdf-file))))
  (defvar ex/citar-library-backup-path
    (concat no-littering-var-directory "backup/citar"))
  (defun ex/citar-update-pdf-metadata (citekey)
    (interactive (list (citar-select-ref)))
    (citar--check-configuration 'citar-library-paths)
    (unless citar-library-paths
      (user-error "Make sure `citar-library-paths' is non-nil"))
    (let* ((files (gethash cite-key (citar-get-files cite-key))))
      (unless files
        (user-error (format "There are no PDF files associated with %s" citekey)))
      (let ((file (or (unless (cdr files) (car files)) (completing-read "Please select a PDF file: " files)))
            (title (citar-get-value 'title citekey))
            (author (cond ((citar-get-value 'author citekey)
                           (citar-get-value 'author citekey))
                          ((citar-get-value 'editor citekey)
                           (concat (citar-get-value 'editor citekey) " (ed.)")))))
        (progn
          (copy-file file ex/bib-library-backup-path 1)
          (call-process-shell-command
           (concat "exiftool -overwrite_original_in_place -Title='" title "' -Author='" author "' " file))))))

  (defun my-citar-open-current-note () (interactive) (my-citar-open-current-resource nil t))
  (defun my-citar-open-current-file () (interactive) (my-citar-open-current-resource t nil))
  :bind
  (:map org-mode-map
        ("C-c o d" . my-download-pdf)
        ("C-c o n" . my-citar-open-current-note)
        ("C-c o p" . my-citar-open-current-file)))

;; TODO Expand
(use-package citar-embark
  :no-require
  :ensure citar
  :after citar embark
  :preface
  (defvar my-citar-embark-become-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "f") 'citar-open-library-files)
      (define-key map (kbd "x") 'biblio-arxiv-lookup)
      (define-key map (kbd "c") 'biblio-crossref-lookup)
      (define-key map (kbd "i") 'biblio-ieee-lookup)
      (define-key map (kbd "h") 'biblio-hal-lookup)
      (define-key map (kbd "s") 'biblio-dissemin-lookup)
      (define-key map (kbd "b") 'biblio-dblp-lookup)
      (define-key map (kbd "o") 'biblio-doi-insert-bibtex)
      map))
  :config
  (citar-embark-mode)
  (add-to-list 'embark-become-keymaps 'my-citar-embark-become-map)
  (add-to-list 'embark-keymap-alist '(bib-reference . citar-map))
  (add-to-list 'embark-keymap-alist '(citar-reference . citar-map))
  (add-to-list 'embark-keymap-alist '(citation-key . citar-buffer-map))
  :bind (:map citar-map
              ("M-RET" . my-citar-embark-open-pdf)))

(use-package citar-org
  :ensure citar
  :after (citar oc)
  :custom
  (org-cite-global-bibliography citar-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :bind (:map org-mode-map ("C-c i c" . org-cite-insert)))

(use-package tex :ensure auctex
  :commands TeX-revert-document-buffer
  :config (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :custom
  ;; (TeX-master nil)
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t)
  (TeX-source-correlate-method 'synctex)
  (TeX-electric-math (cons "\\(" "\\)")))

(use-package latex :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :hook ((LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . TeX-PDF-mode))
  :custom (LaTeX-electric-left-right-brace t))

(use-package reftex
  :hook (LaTeX-mode . turn-on-reftex)
  :custom (reftex-plug-into-AUCTeX t))

(use-package cdlatex
  :hook (LaTeX-mode . cdlatex-mode)
  :custom
  (cdlatex-use-dollar-to-ensure-math nil)
  (cdlatex-auto-help-delay 0.5))

(use-package auctex-latexmk
  :after latex
  :functions auctex-latexmk-setup
  :preface
  (defun my-auctex-latexmk-advice (req feature &rest args)
    "Call REQ with FEATURE and ARGS, unless FEATURE is `tex-buf'."
    (unless (eq feature 'tex-buf)
      (apply req feature args)))
  :init
  (unwind-protect
      (progn (advice-add 'require :around #'my-auctex-latexmk-advice)
             (auctex-latexmk-setup))
    (advice-remove 'require #'my-auctex-latexmk-advice))
  :custom (auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package bibtex
  :custom
  (bibtex-align-at-equal-sign t)
  (bibtex-dialect 'biblatex))

(use-package biblio)

(use-package latexdiff
  :preface
  (defun latexdiff-wip ()
    "Compile the pdf difference between the choosen commit and the current version of the current file."
    (interactive)
    (let* ((commits (latexdiff--get-commit-hash-alist))
           (commit-hash (cdr (car commits))))
      (latexdiff-vc--compile-diff-with-current commit-hash))))

(use-package alert
  :custom (alert-default-style 'libnotify))

(use-package ialign)

(use-package immortal-scratch
  :custom (immortal-scratch-mode t))

;; (use-package persistent-scratch
;;   :config (persistent-scratch-setup-default))

(use-package visual-regexp-steroids
  :demand t
  :bind ("C-r" . vr/replace))

(use-package literate-calc-mode)

(use-package nov
  :hook (nov-mode . nov-imenu-setup)
  :mode
  ("\\.epub\\'" . nov-mode)
  ("\\.EPUB\\'" . nov-mode)
  :custom
  (nov-text-width nil))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :preface
  (defun jinx-select-first ()
    "Correct nearest misspelled word."
    (interactive "*")
    (save-excursion
      (jinx--correct-guard
       (when-let* ((start (car (jinx--bounds-of-word)))
                   (end (cdr (jinx--bounds-of-word)))
                   (word (buffer-substring-no-properties start end))
                   (corrections (jinx--correct-suggestions word))
                   (correction (car corrections)))
         (unless (string= word correction)
           (goto-char end)
           (insert-before-markers correction)
           (delete-region start end))))))
  (defun my-jinx-select-first (&optional arg)
    (interactive "P")
    (if arg
        (jinx-correct-nearest)
      (jinx-select-first)))
  :bind
  (:map jinx-mode-map
        ("C-M-i" . my-jinx-select-first)
        ([remap ispell-word] . jinx-correct)))

(use-package wgrep
  :custom (wgrep-auto-save-buffer t))

(use-package deadgrep
  :bind
  ("C-c H s" . deadgrep)
  (:map deadgrep-mode-map ("E" . deadgrep-edit-mode))
  (:map deadgrep-edit-mode-map ("E" . deadgrep-mode)))

(use-package xref)

(use-package avy
  :bind
  ("C-c j j" . avy-goto-char-2)
  ("C-c j J" . avy-goto-char))

(use-package ace-link
  :bind
  ("C-c j l" . ace-link))

(use-package image-mode
  :ensure nil
  :hook (image-mode . disable-line-numbers))

(use-package image+
  :after image-mode
  :bind (:map image-mode-map
              ("C-+" . imagex-sticky-zoom-in)
              ("C--" . imagex-sticky-zoom-out)
              ("C-0" . imagex-sticky-restore-original)))

(use-package vlf
  :hook (vlf-view-mode . disable-line-numbers))

(use-package vlf-setup :ensure vlf
  :config
  (add-to-list 'vlf-forbidden-modes-list 'pdf-view-mode)
  (add-to-list 'vlf-forbidden-modes-list 'nov-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook ((pdf-view-mode . disable-line-numbers)
         (pdf-view-mode . pdf-sync-minor-mode)
         (pdf-view-mode . pdf-links-minor-mode)
         (pdf-view-mode . pdf-history-minor-mode)
         (pdf-view-mode . pdf-annot-minor-mode)
         (pdf-view-mode . pdf-view-themed-minor-mode))
  :preface
  (defun my-open-citar-note ()
    (interactive)
    ;; (require 'citar)
    (let* ((key (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
           (note (car (gethash key (citar-get-notes key)))))
    (delete-other-windows)
    (split-window-right)
    (citar-open-note note)
    (org-fold-show-subtree)
    (let ((start (point)))
      (my-create-or-goto-notes)
      (narrow-to-region start (point)))))
  (defun my-create-or-goto-notes ()
  "Return all subheadings under the current heading."
  (interactive)
  (let* ((heading (-non-nil (org-map-entries (lambda ()
                                             (when (string= (org-get-heading t t t t) "Notes")
                                               (point)))
                                           nil 'tree))))
    (if heading
        (progn
          (goto-char (car heading))
          (org-end-of-subtree t t)
          (previous-line))
      (progn
        (org-insert-heading-respect-content)
        (org-demote)
        (insert "Notes")
        (org-return)
        (call-interactively (global-key-binding (kbd "TAB")))))))
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations nil)
  (pdf-view-resize-factor 1.1)
  :bind
  (:map pdf-view-mode-map
        ("M-w" . pdf-view-kill-ring-save)
        ("o" . pdf-outline)
        ("O" . pdf-occur)
        ("M-g M-g" . pdf-view-goto-page)
        ("i" . my-open-citar-note)
        ("S-SPC" . pdf-view-scroll-down-or-previous-page)))

(use-package my-pdf-org
  :ensure pdf-tools
  :no-require t
  :after (pdf-tools org)
  :preface
  (defun pdf-move-down-other-frame ()
    (interactive)
    (other-window 1)
    (if (eq major-mode 'pdf-view-mode)
        (pdf-view-scroll-up-or-next-page)
      (scroll-up))
    (other-window 1))
  (defun pdf-move-up-other-frame ()
    (interactive)
    (other-window 1)
    (if (eq major-mode 'pdf-view-mode)
        (pdf-view-scroll-down-or-previous-page)
      (scroll-down))
    (other-window 1))
  :bind
  (:map org-mode-map
        ("C-M-n" . pdf-move-down-other-frame)
        ("C-M-p" . pdf-move-up-other-frame)))

(use-package pdf-view-restore
  :after pdf-tools
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :custom (pdf-view-restore-filename (concat no-littering-var-directory "pdf-view-restore")))

(use-package vundo
  :demand t
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t)
  :bind
  ("C-]" . undo-redo)
  ("C-x u" . vundo))

(use-package undo-fu
  :custom
  (undo-limit 67108864)
  (undo-strong-limit 100663296)
  (undo-outer-limit 1006632960)
  :bind
  ([remap undo] . undo-fu-only-undo)
  ([remap redo] . undo-fu-only-redo))

(use-package undo-fu-session
  :custom
  (undo-fu-session-global-mode t)
  (undo-fu-session-compression 'zst))

(use-package sudo-edit)

(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :custom
  (org-sticky-header-always-show-header t)
  (org-sticky-header-show-keyword nil)
  (org-sticky-header-show-priority nil)
  (org-sticky-header-full-path 'full))

(use-package outline
  :hook (LaTeX-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map
              ("C-c TAB" . outline-toggle-children)
              ("C-c o n" . outline-next-heading)
              ("C-c o p" . outline-previous-heading)))

(use-package apheleia
  :custom
  (apheleia-remote-algorithm 'local))

;; detached.el

(use-package consult-tramp
 :vc (:rev :newest :url "https://github.com/Ladicle/consult-tramp"))

(use-package gnutls
  :if (memq window-system '(pc w32))
  :config
  (add-to-list 'gnutls-trustfiles "C:/PROGRAMS/CRT/*.crt"))

(use-package treesit-auto
  :commands make-treesit-auto-recipe
  :custom
  (treesit-auto-install t)
  (global-treesit-auto-mode t)
  :config
   (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
     :lang 'verilog
     :ts-mode 'verilog-ts-mode
     :remap '(verilog-mode)
     :url "https://github.com/tree-sitter/tree-sitter-verilog")))

(use-package emacs-wsl :ensure nil
  :if my-running-under-wsl
  :demand t
  :no-require t
  :preface
  (defun wsl--send-kill-ring (&rest args)
    (let* ((process-connection-type nil)
           (data (substring-no-properties (car kill-ring)))
           (proc (start-process "clip.exe" nil "clip.exe")))
      (unless proc
        (signal 'file-error (list "Not found")))
      (process-send-string proc data)
      (process-send-eof proc)))
  (defun wsl--get ()
    (with-output-to-string
      (let ((coding-system-for-read 'dos)) ;Convert CR->LF.
        (call-process "powershell.exe" nil t nil "-command" "Get-Clipboard"))))
  (defun wsl-yank ()
    (interactive)
    (with-temp-buffer
      (wsl--get)
      (kill-ring-save (point-min) (point-max)))
    (yank))
  :custom
  (select-enable-clipboard nil)
  (split-height-threshold nil)
  :config
  (advice-add #'kill-line :after #'wsl--send-kill-ring)
  (advice-add #'kill-ring-save :after #'wsl--send-kill-ring)
  :bind
  ("C-k" . kill-line)
  ("C-M-y" . wsl-yank)
  (:map org-mode-map
  ("C-M-y" . wsl-yank)))

(use-package verilog-ext
  :hook (verilog-ts-mode . verilog-ext-mode)
  :config
  (verilog-ext-mode-setup)
  :custom
  (verilog-ext-feature-list '(font-lock xref capf hierarchy eglot
                                        flycheck beautify navigation template formatter compilation
                                        imenu which-func hideshow typedefs time-stamp ports))
  :bind
  (:map verilog-ext-mode-map
        ("C-<tab>" . completion-at-point)))

(use-package verilog-ts-mode
  :custom
  (verilog-ts-beautify-instance-extra t))

(use-package citre
  ;; :hook (verilog-ext-mode . citre-mode)
  :config
  (require 'citre-config)
  :bind
  (:map citre-mode-map
        ("C-c C-l j" . citre-jump)))

(use-package plantuml-mode
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path "/usr/share/java/plantuml.jar")
  (org-plantuml-jar-path plantuml-jar-path))

(use-package org-mem
  :custom
  (org-mem-watch-dirs (list my-notes-directory))
  (org-mem-do-warn-title-collisions nil))

(use-package org-node
  :after org
  :hook (org-mode . org-node-backlink-mode)
  :config
  (setq org-mem-do-sync-with-org-id t)
  (setq org-node-backlink-protect-org-super-links nil)
  (org-mem-updater-mode)
  (org-node-cache-mode)
  :custom
  (org-node-alter-candidates t)
  :bind
  (:map org-mode-map
        ("C-c n f" . org-node-find)
        ("C-c n i" . org-node-insert-link)))

(use-package citar-org-node
  :hook (org-mode . citar-org-node-mode))

(use-package whole-line-or-region
  :hook (org-mode . whole-line-or-region-local-mode) ;; For some reason org-mode overrides global
  :custom
  (whole-line-or-region-global-mode t))

(use-package gptel
  :custom
  (gptel-model 'gemini-2.0-flash-exp)
  (gptel-default-mode 'org-mode)
  :preface
  (defun my-gptel-file-ext () (if (eq gptel-default-mode 'markdown-mode) ".md"".org"))
  (defun my-create-or-switch-to-gptel ()
    "Switch to the first buffer starting with 'gptel-' and ending with '.txt'.
If no such buffer exists, call the `gptel` function."
    (interactive)
    (let ((target-buffer
           (seq-find (lambda (buf)
                       (let ((name (buffer-name buf)))
                         (and (string-prefix-p "gptel-" name)
                              (string-suffix-p (my-gptel-file-ext) name))))
                     (buffer-list))))
      (if target-buffer
          (if (eq target-buffer (current-buffer))
              (popper-toggle)
            (pop-to-buffer target-buffer))
        (pop-to-buffer (my/gptel-new-chat)))))
  (defun my/gptel-new-chat ()
    "Save buffer to disk when starting gptel"
    (let* ((ts (format-time-string "%Y%m%dT%H%M" (current-time)))
           (chat-dir (concat no-littering-var-directory "gptel"))
           (file (expand-file-name (concat "gptel-" ts (my-gptel-file-ext)) chat-dir))
           (buffer (gptel file nil nil t)))
      (with-current-buffer buffer
        (unless (file-directory-p chat-dir)
          (make-directory chat-dir :parents))
        (write-file file)
        (set-visited-file-name file)
        (set-buffer-modified-p nil))
      buffer))
  :config
  (setq gptel-backend (gptel-make-gemini
                     "Gemini"
                   :key (auth-info-password (car (auth-source-search :host "gemini")))
                   :stream t))
  :bind
  ("C-c j g" . my-create-or-switch-to-gptel)
  ("C-|" . my-create-or-switch-to-gptel)
  (:map gptel-mode-map
        ("C-\\" . popper-toggle)
        ("C-c ?" . gptel-menu)
        ("C-c RET" . gptel-send)
        ("C-c C-RET" . gptel-send)
        ("C-?" . gptel-menu)))

(use-package init-work :demand t  :load-path "elisp/")

(provide 'init)
;;; init.el ends here
;; //ssh:furkanu@xirengips01:~/
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'list-timers 'disabled nil)
