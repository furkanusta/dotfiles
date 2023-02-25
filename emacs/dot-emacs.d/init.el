;;; -*- lexical-binding: t -*-
;; Initialization
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'server)
(unless (server-running-p) (server-start))
(unless (and (fboundp 'package-installed-p)
             (package-installed-p 'use-package))
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "elisp/")))

(require 'use-package)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package use-package-hydra)

(setq custom-file null-device)
;; Init Done

;; ;; Debug
;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

(use-package dash)
(use-package f)

(setq use-package-always-defer t)
;; (setq-default use-package-always-ensure t)

(defun disable-line-numbers ()
  (display-line-numbers-mode -1))

(defvar use-other-window-alist
  '((display-buffer-same-window display-buffer-pop-up-window)
    (inhibit-same-window . t)))

(defvar my-data-directory (or (getenv "EMACS_STORAGE_LOCATION") (expand-file-name "~/Documents")))
(defvar my-notes-directory (concat my-data-directory "/Notes"))
(defvar my-papers-directory (concat my-notes-directory "/PDF"))
(defvar my-paper-directories (list my-papers-directory (concat my-data-directory "/paperless-ngx/media/documents/archive/academic/")))
(defvar my-bibliography-directory (concat my-notes-directory "/bibs"))
(defvar my-bibliographies (-filter
                           (lambda (file) (not (s-starts-with? "." (f-filename file))))
                           (f-glob "*.bib" my-bibliography-directory)))

(defun my-switch-theme ()
  "Switch theme between light and dark theme."
  (interactive)
  (let* ((my-dark-theme 'tangonov)
         (my-light-theme 'leuven)
         (current-theme (car custom-enabled-themes)) ;; Assume sinle theme
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
  (fset 'yes-or-no-p 'y-or-n-p)
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
  (initial-major-mode #'fundamental-mode)
  (version-control t)
  (delete-old-versions t)
  (calendar-week-start-day 1)
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
  (truncate-lines t)
  :config
  (defadvice kill-ring-save (before slick-copy activate compile)
    "When called interactively with no active region, copy a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))
  (defadvice kill-region (before slick-cut activate compile)
    "When called interactively with no active region, kill a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))
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
  ("C-w" . kill-region)
  ;; ("M-w" . xah-copy-line-or-region)
  ("M-k" . kill-whole-line)
  ("C-x C-f" . find-file-at-point)
  ("RET" . newline-and-indent)
  ("C-x O" . my-previous-window)
  ("C-_" . undo-only)
  ("C-+" . undo-redo)
  ([remap fill-paragraph] . endless/fill-or-unfill)
  (:map prog-mode-map ("<tab>" . indent-for-tab-command)))

(use-package find-file :ensure nil
  :demand t
  :bind
  (:map project-prefix-map
        ("a" . ff-find-other-file-other-window)
        ("A" . ff-find-other-file)))

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

(use-package hl-line :ensure nil :custom (global-hl-line-mode t))

(use-package delsel  :ensure nil :demand t :init (delete-selection-mode 1))

(use-package recentf :ensure nil
  :after no-littering
  :defines recentf-exclude
  :custom
  (recentf-mode t)
  (recentf-save-file (concat no-littering-var-directory "recentf"))
  (recentf-exclude (list
                    (expand-file-name no-littering-var-directory)
                    (expand-file-name no-littering-etc-directory)
                    no-littering-var-directory "/tmp" no-littering-etc-directory)))

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
  :hook (prog-mode . which-function-mode)
  :custom (which-func-non-auto-modes '(org-mode markdown-mode bibtex-mode)))

(use-package isearch :ensure nil
  :bind ("C-c H S" . isearch-forward))

(use-package so-long)

(use-package goto-addr
  :custom (global-goto-address-mode t)
  :bind
  (:map goto-address-highlight-keymap
        ("C-c o l" . goto-address-at-point)))

(use-package bookmark
  :hook (kill-emacs . bookmark-save)
  :custom
  (bookmark-watch-bookmark-file 'silent))

(use-package window
  :ensure nil
  :preface
  (defvar my-hydra-switch-buffer-source nil)
  :custom
  (split-height-threshold nil)
  :hydra
  (hydra-switch-buffer
   (ctl-x-map nil
              :pre (setq my-hydra-switch-buffer-source (or my-hydra-switch-buffer-source (current-buffer)))
              :post (setq my-hydra-switch-buffer-source nil))
   "Switch buffer"
   ("g" (switch-to-buffer my-hydra-switch-buffer-source) "Quit" :exit t)
   ("<left>" previous-buffer "previous-buffer")
   ("<right>" next-buffer "next-buffer")))

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
  (defun dired-current-dir ()
    (interactive)
    (dired default-directory))
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-create-destination-dirs 'ask)
  (dired-recursive-copies 'always)
  (dired-use-ls-dired nil)
  (dired-listing-switches "-aBhlv --group-directories-first --color=never")
  :config
  (defun my--find-nov-buffer (filename)
    (if-let (nov-buffer (seq-filter #'(lambda (buffer) (with-current-buffer buffer (string= filename nov-file-name))) (buffer-list)))
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

(use-package dired)

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

(use-package dired-sidebar
  :custom
  (dired-sidebar-close-sidebar-on-file-open t)
  :bind ("C-c t d" . dired-sidebar-toggle-sidebar))

(use-package dired-quick-sort
  :config
  (dired-quick-sort-setup))

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
  (defun elfeed-youtube-dl ()
    "Youtube-DL link"
    (interactive)
    (let* ((entry (elfeed-get-show-or-search-entry))
           (url (when entry (elfeed-entry-link entry)))
           (default-directory (expand-file-name "~/Downloads")))
      (when url (async-shell-command (format "yt-dlp %s" url)))
      (elfeed-search-untag-all-unread)))
  (defun elfeed-mpv ()
    (interactive)
    (let* ((entry (elfeed-get-show-or-search-entry))
           (url (when entry (elfeed-entry-link entry))))
      (when url (start-process "elfeed-mpv" nil "mpv" url))
      (elfeed-search-untag-all-unread)))
  (defun elfeed-open-eww ()
    (interactive)
    (let* ((entry (elfeed-get-show-or-search-entry))
           (url (elfeed-entry-link entry)))
      (eww url)
      (elfeed-search-untag-all-unread)))
  :bind (:map elfeed-search-mode-map
              ("P" . pocket-reader-add-link)
              ("d" . elfeed-youtube-dl)
              ("e" . elfeed-open-eww)
              ("m" . elfeed-mpv)))

(use-package elfeed-show :ensure elfeed
  :after elfeed
  :commands (pocket-reader-add-link)
  :bind (:map elfeed-show-mode-map
              ("q" . +rss/delete-pane)
              ("P" . pocket-reader-add-link)))

(use-package reddigg
  :defines elfeed-search-mode-map
  :commands promise-finally
  :after elfeed-search
  :preface (defun elfeed-open-reddit ()
             (interactive)
             (require 'promise-finally)
             (let* ((entry (elfeed-get-show-or-search-entry))
                    (url (elfeed-entry-link entry))
                    (existing-buffer (get-buffer "*reddigg-comments*")))
               (progn
                 (when existing-buffer
                   (kill-buffer existing-buffer))
                 (elfeed-search-untag-all-unread)
                 (promise-finally (reddigg-view-comments url)
                                  (lambda ()
                                    (pop-to-buffer "*reddigg-comments*")
                                    (use-local-map (copy-keymap org-mode-map))
                                    (local-set-key "q" #'+rss/delete-pane)
                                    (read-only-mode +1))))))
  :bind (:map elfeed-search-mode-map ("R" . elfeed-open-reddit)))

(use-package feed-discovery)

(use-package pocket-reader)

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
  :bind
  ([remap delete-char] . hungry-delete-forward)
  ([remap delete-forward-char] . hungry-delete-forward)
  ([remap delete-backward-char] . hungry-delete-backward))

(use-package writeroom-mode
  :defines display-line-numbers-mode
  :functions display-line-numbers-mode
  :preface (defun toggle-line-numbers () (display-line-numbers-mode (or (not display-line-numbers-mode) 0)))
  :custom
  (writeroom-width 120)
  (writeroom-mode-line nil)
  (writeroom-fullscreen-effect 'maximized)
  :bind
  (:map writeroom-mode-map
        ("C-c o w" . writeroom-mode)))

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

  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
           (default-directory (cdr prompt-dir)))
      (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))

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
    (consult-ripgrep (and (project-current)
                          (project-root (project-current))
                          default-directory)
                     (thing-at-point 'symbol)))
  :bind
  ("C-x B" . consult-project-buffer)
  ("M-g b" . consult-bookmark)
  ("C-x M-:" . consult-complex-command)
  ("C-x b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-x 5 b" . consult-buffer-other-frame)
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store)
  ("C-M-#" . consult-register)
  ("M-y" . consult-yank-pop)
  ("<help> a" . consult-apropos)
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("C-c SPC" . consult-mark)
  ("C-c C-SPC" . consult-global-mark)
  ("C-c c c" . consult-compile-error)
  ("C-c c f" . consult-flymake)
  ("C-c c o" . consult-outline)
  ("C-c c i" . consult-imenu)
  ("C-c h s" . my-consult-ripgrep)
  ("C-c h a" . my-consult-rga-here)
  ("C-c h S" . my-consult-ripgrep-here)
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
   "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number .")
  :config
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
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  :custom
  (orderless-style-dispatchers '(vertico-orderless-dispatch))
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (orderless partial-completion)))))
  :config
  (set-face-attribute 'completions-first-difference nil :inherit nil))

(use-package embark
  :demand t
  :after vertico
  :init   (setq prefix-help-command #'embark-prefix-help-command)
  :custom (embark-indicators '(embark-verbose-indicator))
  :preface
  (defun sudo-find-file (file)
    "Open FILE as root."
    (interactive "FOpen file as root: ")
    (when (file-writable-p file)
      (user-error "File is user writeable, aborting sudo"))
    (find-file (if (file-remote-p file)
                   (concat "/" (file-remote-p file 'method) ":"
                           (file-remote-p file 'user) "@" (file-remote-p file 'host)
                           "|sudo:root@"
                           (file-remote-p file 'host) ":" (file-remote-p file 'localname))
                 (concat "/sudo:root@localhost:" file))))
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
  (:map vertico-map
        ("C-<tab>" . embark-act-with-completing-read)
        ("C-." . embark-act)
        ("C-," . embark-dwim)
        ("C-:" . embark-export))
  (:map embark-file-map
        ("L" . vlf)
        ("S" . sudo-find-file)))

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
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

(use-package corfu
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (comint-mode . corfu-mode)
         (cmake-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (corfu-popinfo-mode t)
  (corfu-history-mode t)
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

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions
               (cape-super-capf #'cape-symbol #'cape-keyword #'cape-dabbrev)))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package consult-flyspell
  :bind
  (:map flyspell-mode-map
        ("C-c c s" . consult-flyspell)))

(use-package savehist
  :hook (after-init . savehist-mode))

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

(use-package focus
  :bind ("C-c C-f" . focus-mode) ;; Might be unnecessary
  (:map focus-mode-map
        ("C-c C-n" . focus-next-thing)
        ("C-c C-p" . focus-prev-thing)))

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

(use-package goto-chg
  :bind
  ("C-x j c" . goto-last-change)
  ("C-x j C" . goto-last-change-reverse))

(use-package link-hint
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

(use-package beacon
  :custom
  (beacon-blink-when-point-moves-vertically 20)
  (beacon-blink-when-buffer-changes nil)
  (beacon-blink-when-window-changes nil)
  (beacon-mode 1))

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

(use-package fill-column-indicator
  :hook (prog-mode . fci-mode))

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

(use-package bufler :bind ("C-x C-b" . bufler))

(use-package all-the-icons-ibuffer :init (all-the-icons-ibuffer-mode 1))

(use-package dogears
  :custom (dogears-mode t)
  :bind ("M-g M-l" . dogears-go))

(use-package dashboard
  :if (processp server-process)
  :hook (dashboard-mode . page-break-lines-mode)
  :commands dashboard-insert-section dashboard-insert-heading dashboard-subseq
  :preface (defun dashboard-insert-scratch (list-size)
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
  (dashboard-projects-switch-function 'project-persp-switch-project)
  (dashboard-items '((scratch . 2)
                     (recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)))
  (dashboard-banner-logo-title "Emacs"))

(use-package all-the-icons)

(use-package doom-modeline
  :custom
  (doom-modeline-mode 1)
  (doom-modeline-icon t)
  (doom-modeline-buffer-encoding 'nondefault)
  :init
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number buffer-info remote-host buffer-position word-count selection-info)
    '(misc-info persp-name grip irc github debug repl lsp indent-info buffer-encoding process vcs checker time)))

(use-package diminish)

(use-package tangonov-theme
  :after custom
  :init (load-theme 'tangonov t)
  :custom-face
  ;; Orange, Green, Blue, Red (I was used to this in my previous theme and different order messes my
  ;; brain)
  (org-level-1 ((t (:inherit bold :foreground "#FFCA41"))))
  (org-level-2 ((t (:inherit bold :foreground "#ABDC88"))))
  (org-level-3 ((t (:inherit bold :foreground "#82AAFF"))))
  (org-level-4 ((t (:inherit bold :foreground "#FF7B85")))))

(use-package popper
  :commands popper-select-popup-at-bottom
  :bind (("C-\\"   . popper-toggle-latest)
         ("M-\\"   . popper-cycle)
         ("C-M-\\"   . popper-toggle-type))
  :custom
  (popper-reference-buffers '("\\*Messages\\*" "Output\\*$" help-mode compilation-mode))
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

(use-package window-margin
  :vc (:fetcher github :repo "aculich/window-margin.el")
  :hook (markdown-mode . window-margin-mode))

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

(use-package project
  :ensure nil
  :preface (defun my-open-readme ()
             (interactive)
             (let* ((project-root (project-root (project-current)))
                    (project-files (directory-files project-root nil nil t))
                    (readme-files (seq-filter (lambda (file) (string-prefix-p "readme" file t)) project-files)))
               (if readme-files
                   (let ((readme-file (car readme-files)))
                     (find-file (expand-file-name readme-file project-root)))
                 (find-file (expand-file-name "README.org" project-root)))))
  :custom
  (project-switch-commands #'my-open-readme)
  :bind-keymap ("C-c p" . project-prefix-map))
;; Adapted from persp-projectile

(use-package my-project-persp-bridge
  :ensure nil
  :preface
  (defun my-project-name (project)
    (interactive (list (project-root (project-current))))
    (file-name-nondirectory (directory-file-name project)))
  (defun project-persp-switch-project (project-to-switch)
  "Switch to a project or perspective we have visited before.
If the perspective of corresponding project does not exist, this
function will call `persp-switch' to create one and switch to
that before `project-switch-project'
Otherwise, this function calls `persp-switch' to switch to an
existing perspective of the project unless we're already in that
perspective."
  (interactive (list (completing-read "Switch to project: " (project-known-project-roots))))
  (let* ((name (my-project-name project-to-switch))
         (persp (gethash name (perspectives-hash))))
    (cond
     ;; project-specific perspective already exists
     ((and persp (not (equal persp (persp-curr))))
      (persp-switch name))
     ;; persp exists but not match with projectile-name
     ((and persp (not (equal persp name)))
      (persp-switch name)
      (project-switch-project project-to-switch))
     ;; project-specific perspective doesn't exist
     ((not persp)
      (let ((frame (selected-frame)))
        (persp-switch name)
        (project-switch-project project-to-switch)
        ;; Clean up if we switched to a new frame. `helm' for one allows finding
        ;; files in new frames so this is a real possibility.
        (when (not (equal frame (selected-frame)))
          (with-selected-frame frame
            (persp-kill name))))))))
  :bind ([remap project-switch-project] . project-persp-switch-project))

(use-package perspective
  :hook (after-init . persp-mode)
  :commands persp-current-buffers
  :preface
  (defvar perspective-skip-ignore-list
  '("*dashboard*" "*Messages*" "*Warnings*" "*elfeed-search*" "*Fd*" "*compilation*" "*Bufler*" "*Easy-hugo*"))
  (defvar perspective-skip-prefix-list '("magit-"))
  (defvar perspective-skip-ignore-prefix-list
  '("*vterm" "*scratch" "*shell" "*Customize" "*ielm*" "*helpful" "*org" "*ein" "*Org" "*Embark" "*cardboard" "*eww" "*sly"))
  (defun perspective-my-skip-buffer-p (_ buffer _)
    (let ((name (buffer-name buffer)))
      (or
       (and
        (char-equal ?* (seq-elt name 0))
        (not (seq-contains-p perspective-skip-ignore-list name))
        (cl-every
         (lambda (x) x)
         (mapcar (lambda (pref) (not (string-prefix-p pref name))) perspective-skip-ignore-prefix-list)))
       (cl-some
        (lambda (x) x)
        (mapcar (lambda (pref) (string-prefix-p pref name)) perspective-skip-prefix-list))
       (not (seq-contains-p (persp-current-buffers) buffer)))))
  :custom
  (persp-mode-prefix-key (kbd "C-c w"))
  (persp-modestring-short t)
  ;; (persp-initial-frame-name ".dotfiles")
  (switch-to-prev-buffer-skip #'perspective-my-skip-buffer-p))

(use-package magit
  :bind ("C-c g s" . magit-status)
  :init (setq magit-define-global-key-bindings nil)
  :custom
  (magit-define-global-key-bindings nil)
  (magit-blame-echo-style 'headings)
  (magit-repository-directories (list (cons (file-truename "~/Projects") 1))))

(use-package magit-todos
  :init
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  :config
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("T " "Todos" magit-todos-jump-to-todos)))

(use-package git-link
  :custom (git-link-use-commit t))

(use-package git-timemachine)

(use-package git-messenger
  :custom
  (git-messenger:show-detail t)
  (git-messenger:use-magit-popup t)
  :bind ("C-c g m" . git-messenger:popup-message))

(use-package git-modes)

(use-package magit-pretty-graph
  :vc (:fetcher github :repo "georgek/magit-pretty-graph")
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

(use-package diff-hl
  :hook (find-file . diff-hl-mode))

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
  :hook (vterm-mode . set-no-process-query-on-exit)
  :preface
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
  :config
  (add-to-list 'display-buffer-alist (cons "\\*vterm" use-other-window-alist))
  :custom
  (vterm-copy-exclude-prompt t)
  :bind
  (:map vterm-mode-map
        ("C-y" . vterm-yank)
        ("M-y" . vterm-yank-pop)
        ("C-<return>" . vterm-cd-other-buffer)
        ("C-S-<return>" . vterm-toggle-insert-cd))
  (:map vterm-copy-mode-map
        ("C-<" . vterm-prev-prompt)
        ("C->" . vterm-next-prompt)))

(use-package vterm-toggle
  :custom (vterm-toggle-cd-auto-create-buffer nil)
  :bind
  ("<f8>" . vterm-toggle)
  (:map vterm-mode-map
        ("<f8>" . vterm-toggle)
        ("C-c C-n"  . vterm-toggle-forward)
        ("C-c C-p"  . vterm-toggle-backward)
        ("C-<return>" . vterm-toggle-insert-cd)))

(use-package shx
  :hook (shell-mode . shx-mode))

(use-package compile :ensure nil
  :preface
  (make-variable-buffer-local 'my-compilation-start-time)
  (defun my-compilation-start-hook (_)
    (setq my-compilation-start-time (current-time)))
  (defun my-compilation-finish-function (buf why)
    (let* ((elapsed  (time-subtract nil my-compilation-start-time))
           (msg (format "Elapsed: %s" (format-time-string "%T.%N" elapsed t))))
      (if (get-buffer-window "*compilation*")
          (when (string-match "finished" why)
  	        (bury-buffer "*compilation*")
            (popper-close-latest))
        (save-excursion (goto-char (point-max)) (insert msg))
        (alert (format "Emacs: %s at %s" why (buffer-name buf)))
        (message "Compilation %s: %s" (string-trim-right why) msg))))
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

(use-package tramp
  :commands (tramp-cleanup-all-connections tramp-cleanup-all-buffers)
  :config (defun tramp-done ()
            (interactive)
            (tramp-cleanup-all-connections)
            (tramp-cleanup-all-buffers))
  :custom
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
  :vc (:fetcher github :repo "alphapapa/obvious.el"))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs `(python-ts-mode . ,(cdr (assoc 'python-mode eglot-server-programs))))
  (add-to-list 'eglot-server-programs `((c-ts-mode c++-ts-mode) . ,(cdr (assoc '(c++-mode c-mode) eglot-server-programs))))
  (add-to-list 'eglot-server-programs `(tex-mode . ,(eglot-alternatives
                                                     '(("~/.local/prog/digestif/digestif.sh")
                                                       ("texlab"))))))

(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-idle-delay 1.5))

(use-package project-rootfile
  :after project
  :config
  (add-to-list 'project-rootfile-list "pyproject.toml")
  (add-to-list 'project-find-functions #'project-rootfile-try-detect))

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
  :mode ("\\.h\\'" . c++-mode)
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

(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :mode ("\\.sc\\'" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :custom (sbt:program-options '("-Dsbt.supershell=false"))
  :config
  (substitute-key-definition 'minibuffer-complete-word 'self-insert-command minibuffer-local-completion-map))

(use-package ansi-color
  :commands ansi-color-apply-on-region
  :hook (compilation-filter . colorize-compilation-buffer)
  :preface (defun colorize-compilation-buffer ()
             (read-only-mode)
             (ansi-color-apply-on-region (point-min) (point-max))
             (read-only-mode -1))
  :config (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  :custom (ansi-color-for-comint-mode t))

(use-package shell
  :after window
  :config (add-to-list 'display-buffer-alist (cons "\\*shell\\*" use-other-window-alist))
  :bind ("C-<f8>" . shell))

(use-package eshell
  :config (add-to-list 'display-buffer-alist (cons "\\*eshell\\*" use-other-window-alist))
  :bind ("M-<f8>" . eshell))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package dockerfile-mode     :mode ("Dockerfile\\'" "\\.docker"))

(use-package docker-compose-mode :mode ("docker-compose\\.yml\\'" "-compose.yml\\'"))

(use-package docker)

(use-package tramp-container :ensure nil)

(use-package verilog-mode
  :mode
  ("\\.v\\'" . verilog-mode)
  ("\\.sv\\'" . verilog-mode)
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
  (verilog-indent-level-behavioral 4)
  (verilog-indent-level-directive 4)
  (verilog-indent-level-declaration 4)
  (verilog-indent-level-module 4))

(use-package graphviz-dot-mode
  :custom (graphviz-dot-indent-width 4))

(use-package elisp-mode :ensure nil)

(use-package lisp :ensure nil
  :hook (lisp-mode . auto-highlight-symbol-mode))

(use-package sly
  :config (sly-setup '(sly-fancy))
  :functions sly-symbol-completion-mode sly-documentation-lookup
  :hook
  ((sly-mode . (lambda () (sly-symbol-completion-mode -1)))
   (sly-mode . smartparens-mode))
  :preface
  (defun my-sly-lookup ()
    (interactive)
    (let ((browse-url-browser-function 'eww-browse-url))
      (sly-documentation-lookup)))
  :custom
  (inferior-lisp-program "sbcl")
  (sly-symbol-completion-mode -1)
  (sly-kill-without-query-p t)
  (sly-command-switch-to-existing-lisp 'always)
  :bind
  (:map sly-mode-map ("C-h D" . my-sly-lookup)))

(use-package sly-quicklisp)

(use-package sly-repl-ansi-color
  :after sly
  :init (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(use-package lisp-extra-font-lock
  :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))

(use-package highlight-function-calls
  :hook (emacs-lisp-mode . highlight-function-calls-mode))

(use-package easy-escape
  :hook (emacs-lisp-mode . easy-escape-minor-mode))

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

(use-package web-mode
  :hook (web-mode . disable-smartparens)
  :mode "\\.jinja\\'"
  :init
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

(use-package gitlab-ci-mode :mode "\\.gitlab-ci\\.yml\\'")

(use-package format-all)

(use-package python
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

(use-package pyvenv
  :hook
  (python-mode . pyvenv-try-activate)
  (pyvenv-post-activate . (lambda () (pyvenv-restart-python)))
  :preface
  (defun pyvenv-try-activate ()
    (pyvenv-mode t)
    (pyvenv-activate (concat (project-root (project-current))))))

(use-package py-isort
  :hook (before-save . py-isort-before-save))

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

(use-package ruby-mode)
;; (use-package enh-ruby-mode)
;; (use-package rspec)
;; (use-package minitest)

(use-package yard-mode
  :hook (ruby-mode . yard-mode))

(use-package inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package rubocop
  :hook (ruby-mode . rubocop-mode))

(use-package rake)

(use-package org
  :hook
  ((org-mode . turn-on-flyspell)
   (org-mode . smartparens-mode))
  :preface
  (defun my-update-all-bibs ()
    (interactive)
    (let ((buf (current-buffer))
          (files (f-glob "*.org")))
      (dolist (file files)
        (with-current-buffer (find-file file)
          (message "TANGLE: %s" file)
          (org-babel-tangle nil nil "bibtex")))
      (switch-to-buffer buf)))
  :custom
  (org-modules (list 'ol-eww 'org-tempo 'ol-info 'ol-docview 'ol-bibtex 'ol-doi))
  (org-default-notes-file (concat my-notes-directory "/Capture.org"))
  (org-adapt-indentation t)
  (org-catch-invisible-edits 'show-and-error)
  (org-cycle-separator-lines 0)
  (org-edit-src-content-indentation 0)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers t)
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
  (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "NEXT" "|" "DONE")
                       (sequence "PAUSED" "SCHEDULED" "WAITING" "|" "CANCELLED")))
  :config
  (add-to-list 'org-latex-packages-alist '("" "minted"))
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
  (defun my-sync-bibtex-org-entry ()
    (interactive)
    (save-excursion
      (call-interactively #'org-next-block)
      (forward-line 1)
      (let* ((entry (bibtex-parse-entry))
             (key (cdr (assoc "=key=" entry)))
             (title (substring (cdr (assoc "title" entry)) 1 -1))
             (fixed-title (replace-regexp-in-string " +" " " (string-replace "\n" " " title))))
        (org-set-property "ROAM_REFS" (format "[cite:@%s]" key))
        (org-back-to-heading)
        (forward-char 3)
        (org-kill-line)
        (insert fixed-title))
      (forward-line 1)
      (org-cycle))))

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
        (if (= 1 (org-current-level))
            (org-insert-subheading t)
          (org-insert-heading t))
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
                           ;; Ideas??
                           ;; Habit-Journal (:type table-line)
                           ;; Pocket
                           ;; org-web-tools-insert-web-page-as-entry
                           ;; Snippet
                           ;; Elfeed
  					       ("p" "Protocol" entry (file+headline org-capture-file "Inbox")
  	    				    "* %?\n	 [[%:link][%:description]]\n	%U\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n")
  	    			       ("L" "Protocol Link" entry (file+headline org-capture-file "Inbox")
  						    "* %?\n	 [[%:link][%:description]]\n	%U")))
  :bind ("C-c C-c" . org-capture))

;; (use-package org-capture-ref
;;   :after org
;;   :vc (:repo "yantar92/org-capture-ref" :fetcher github)
;;   :config
;;   ;; (add-to-list 'org-capture-templates
;;   ;;          (doct '(:group "Browser link"
;;   ;;                         :type entry
;;   ;;                         :headline "REF"
;;   ;;   		              :file org-capture-file
;;   ;;   		              :fetch-bibtex (lambda () (org-capture-ref-process-capture)) ; this must run first
;;   ;;                         :link-type (lambda () (org-capture-ref-get-bibtex-field :type))
;;   ;;                         :extra (lambda ()
;;   ;;                                  (if (org-capture-ref-get-bibtex-field :journal)
;;   ;;   				                   (s-join "\n"
;;   ;;                                              '("- [ ] download and attach pdf"
;;   ;;   					                         "- [ ] [[elisp:org-attach-open][read paper capturing interesting references]]"
;;   ;;   					                         "- [ ] [[elisp:(browse-url (url-encode-url (format \"https://www.semanticscholar.org/search?q=%s\" (org-entry-get nil \"TITLE\"))))][check citing articles]]"
;;   ;;   					                         "- [ ] [[elisp:(browse-url (url-encode-url (format \"https://www.connectedpapers.com/search?q=%s\" (org-entry-get nil \"TITLE\"))))][check related articles]]"
;;   ;;                                                "- [ ] check if bibtex entry has missing fields"))
;;   ;;                                    ""))
;;   ;;                         :org-entry (lambda () (org-capture-ref-get-org-entry))
;;   ;;   		              :template
;;   ;;                         ("%{fetch-bibtex}* TODO %?%{space}%{org-entry}"
;;   ;;                          "%{extra}"
;;   ;;                          "- Keywords: #%{link-type}")
;;   ;;   		              :children (("Interactive link" :keys "b" :clock-in t :space " " :clock-resume t)
;;   ;;   			                     ("Silent link" :keys "B" :space "" :immediate-finish t))))))
;;   )

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
  :custom
  (org-refile-use-outline-path t)
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9)))
  (org-refile-allow-creating-parent-nodes 'confirm))

(use-package org-clock :ensure org
  :custom
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-report-include-clocking-task t)
  (org-clock-out-when-done t))

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

(use-package org-pretty-table
  :vc (:fetcher github :repo "Fuco1/org-pretty-table")
  :hook (org-mode . org-pretty-table-mode))

(use-package bibtex-completion
  :custom
  (bibtex-completion-bibliography my-bibliographies)
  (bibtex-completion-notes-path my-notes-directory)
  (bibtex-completion-additional-search-fields '(keywords))
  (bibtex-completion-library-path my-paper-directories))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :preface
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))
  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package org-noter
  :vc (:fetcher github :repo "furkanusta/org-noter")
  :custom
  (org-noter-notes-search-path (list my-notes-directory))
  (org-noter-always-create-frame nil)
  (org-noter-doc-split-fraction (cons 0.7  0.3))
  (org-noter-kill-frame-at-session-end nil)
  (org-noter-default-notes-file-names (list "Notes.org"))
  (org-noter-auto-save-last-location t)
  (org-noter-insert-note-no-questions t))

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

(use-package binder
  :custom (binder-default-file-extension "org"))

(use-package inherit-org
  :vc (:repo "chenyanming/inherit-org" :fetcher github)
  :hook ((eww-mode nov-mode info-mode helpful-mode ghelp-page-mode) . inherit-org-mode))

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
  (engine-mode t)
  (engine/browser-function 'eww-browse-url)
  :init
  (defengine github "https://github.com/search?ref=simplesearch&q=%s")
  (defengine google "https://google.com/search?q=%s" :keybinding "g")
  (defengine wikipedia "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s" :keybinding "w")
  (defengine wolfram "http://www.wolframalpha.com/input/?i=%s"))

(use-package org-transclusion
  :vc (:fetcher github :repo "nobiot/org-transclusion")
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

(use-package org-roam
  :preface
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))
  :custom
  (org-roam-directory my-notes-directory)
  (org-roam-auto-replace-fuzzy-links nil)
  (org-roam-capture-templates
   '(("d" "Daily" plain "%?" :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")  :unnarrowed t)
     ("c" "Catch all" entry  :target (file+head "CatchAll.org") "* %^{Note title}\nContext: %a\n%?" :empty-lines-before 1 )
     ("p" "Paper Note" plain "* %^{citekey}" :target (file "Papers.org" "#+title: ${title}"))))
  :bind
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n g" . org-roam-graph)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n I" . org-roam-node-insert-immediate)
  ("C-c n c" . org-roam-capture)
  ;; Dailies
  ("C-c n j" . org-roam-dailies-capture-today)
  :config
  (add-to-list 'completion-at-point-functions 'org-roam-complete-link-at-point)
  (require 'org-roam-protocol))

(use-package org-roam-bibtex
  :after org-roam
  :preface
  (defun my-org-roam-bibtex-fix-property ()
    (interactive)
    (save-excursion
      (while (> (org-current-level) 2)
        (org-up-heading-safe))
      (org-babel-next-src-block)
      (org-set-property "ROAM_REFS"
                        (concat "[cite:@"
                                (buffer-substring (search-forward "{") (1- (search-forward ",")))
                                "]")))))

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package org-roam-timestamps
  :hook (org-roam-mode . org-roam-timestamps-mode))

;; (use-package ox-pandoc)

(use-package org-pandoc-import
  :vc (:fetcher github :repo "tecosaur/org-pandoc-import"))

(use-package org-web-tools)

(use-package org-agenda-property
  :custom (org-agenda-property-list '(LOCATION)))

(use-package org-timeline
  :hook (org-agenda-finalize . org-timeline-insert-timeline))

(use-package org-download
  :hook
  (dired-mode . org-download-enable)
  (org-mode . org-download-enable))

(use-package org-mru-clock
  :bind
  ("C-c C-x c" . org-mru-clock-in)
  ("C-c C-x C-c" . org-mru-clock-select-recent-task))

;; (use-package orgit)
;;
;; (use-package orgit-forge)

(use-package ox-hugo)

(use-package org-tanglesync
  :bind
  ( "C-c M-i" . org-tanglesync-process-buffer-interactive)
  ( "C-c M-a" . org-tanglesync-process-buffer-automatic))

(use-package orgdiff
  :vc (:fetcher github :repo "tecosaur/orgdiff"))

(use-package org-clock-reminder
  :custom
  (org-clock-reminder-interval 1200)
  (org-clock-reminder-mode t))

(use-package org-elp
  :custom
  (org-elp-idle-time 0.5)
  (org-elp-split-fraction 0.25))

(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

;; (use-package org-books
;;   :custom
;;   (org-books-add-to-top nil)
;;   (org-books-file-depth 1)
;;   (org-books-file (concat my-notes-directory "/Books.org")))

(use-package ein)

(use-package code-cells
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

(use-package org-auctex
  :vc (:fetcher github :repo "karthink/org-auctex"))

(use-package org-protocol-capture-html
  :vc (:fetcher github :repo "alphapapa/org-protocol-capture-html"))

(use-package citar
  :custom
  (citar-bibliography my-bibliographies)
  (citar-open-note-function 'orb-citar-edit-note)
  (citar-library-paths my-paper-directories)
  (citar-at-point-function 'embark-act)
  (citar-symbols
   `((file ,(all-the-icons-octicon "file-pdf" :face 'all-the-icons-green :v-adjust -0.1) . " ")
     (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
     (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (citar-symbol-separator " ")
  :bind
  ("C-c o b" . citar-open)
  ("C-c o N" . citar-open-notes)
  ("C-c o P" . citar-open-files))

(use-package my-citar-org
  :ensure nil
  :no-require t
  :after org
  :init
  (require 'citar)
  (defun my-citar-open-current-resource (files notes)
    "Open REFs of the node at point."
    (interactive)
    (if-let ((keys (when-let* ((prop (org-entry-get (point) "ROAM_REFS" t))
                            (refs (when prop (split-string-and-unquote prop)))
                            (oc-cites
                             (seq-map (lambda (ref) (substring ref 7 (- (length ref) 1))) refs)))
                  oc-cites))
             (selected (let* ((actions (bound-and-true-p embark-default-action-overrides))
                              (embark-default-action-overrides `((t . ,#'citar--open-resource) . ,actions)))
                         (citar--select-resource keys :files files :notes notes))))
        (progn
          (when (= 1 (length (window-list)))
            (split-window-horizontally))
          (other-window 1)
          (citar--open-resource (cdr selected) (car selected))
          (when (eq 'file (car selected))
            (pdf-view-fit-width-to-window)))
      (user-error "No ROAM_REFS found")))
  (defun my-citar-open-current-note () (interactive) (my-citar-open-current-resource nil t))
  (defun my-citar-open-current-file () (interactive) (my-citar-open-current-resource t nil))
  :bind
  (:map org-mode-map
        ("C-c o n" . my-citar-open-current-note)
        ("C-c o p" . my-citar-open-current-file)))

;; TODO Expand
(use-package citar-embark
  :no-require t
  :ensure citar
  :after embark
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

(use-package citar-org-roam
  :hook (org-roam-mode . citar-org-roam-mode))

(use-package citar-org
  :ensure citar
  :after (citar oc)
  :custom
  (org-cite-global-bibliography citar-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-file-note-org-include '(org-id org-roam-ref))
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
         (LaTeX-mode . flyspell-mode)
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

(use-package persistent-scratch
  :config (persistent-scratch-setup-default))

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

(use-package flyspell
  :hook ((text-mode org-mode) . flyspell-mode))

(use-package flyspell-correct
  :after flyspell
  :bind
  ("C-c $" . flyspell-correct-wrapper)
  ("C-;" . flyspell-correct-at-point)
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-at-point)
        ("C-c $" . flyspell-correct-wrapper)))

(use-package wgrep
  :custom (wgrep-auto-save-buffer t))

(use-package deadgrep
  :bind
  ("C-c H s" . deadgrep)
  (:map deadgrep-mode-map ("E" . deadgrep-edit-mode))
  (:map deadgrep-edit-mode-map ("E" . deadgrep-mode)))

(use-package xref)

(use-package ace-jump-mode
  :bind ("C-c j j" . ace-jump-mode))

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

(use-package pdf-tools-note
  :no-require t
  :ensure nil
  :after (org-noter pdf-tools)
  :defines org-noter-insert-note
  :preface
  (defun my-org-noter-insert-and-highlight ()
    (interactive)
    (progn
      (when (pdf-view-active-region-p)
        (pdf-annot-add-highlight-markup-annotation (pdf-view-active-region t)))
      (org-noter-insert-note nil)))
  (defun my-org-noter-insert-precise-and-highlight ()
    (interactive)
    (progn
      (when (pdf-view-active-region-p)
        (pdf-annot-add-highlight-markup-annotation (pdf-view-active-region nil)))
      (org-noter-insert-precise-note)
      (when (pdf-view-active-region-p)
        (pdf-view-active-region t))))
  :bind
  (:map pdf-view-mode-map
        ("i" . my-org-noter-insert-and-highlight)
        ("M-i" . my-org-noter-insert-precise-and-highlight))
  (:map org-noter-doc-mode-map
        ("i" . my-org-noter-insert-and-highlight)
        ("M-i" . my-org-noter-insert-precise-and-highlight)))

(use-package pdf-view-restore
  :after pdf-tools
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :custom (pdf-view-restore-filename (concat no-littering-var-directory "pdf-view-restore")))

(use-package vundo
  :demand t
  :defer nil
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  :bind ("C-x u" . vundo))

(use-package sudo-edit)

(use-package ghelp
  :vc (:fetcher github :repo "casouri/ghelp"))

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

;; (use-package lazytab
;;   :vc (:fetcher github :repo "karthink/lazytab"))

(use-package apheleia
  :hook (python-mode . apheleia-mode))

;; Tempel, Yasnippet, Embark
;; detached.el

(use-package consult-tramp
  :vc (:fetcher github :repo "Ladicle/consult-tramp"))

(use-package gnutls
  :if (memq window-system '(pc w32))
  :config
  (add-to-list 'gnutls-trustfiles "C:/PROGRAMS/CRT/*.crt"))

(use-package treesit-auto
  :commands make-treesit-auto-recipe
  :custom
  (treesit-auto-install t)
  :init
   (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
     :lang 'verilog
     :ts-mode 'verilog-ts-mode
     :remap '(verilog-mode)
     :url "https://github.com/tree-sitter/tree-sitter-verilog")))

(use-package emacs-wsl
  :if (executable-find "clip.exe")
  :ensure nil
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
  :init
  (advice-add #'kill-line :after #'wsl--send-kill-ring)
  (advice-add #'kill-ring-save :after #'wsl--send-kill-ring)
  :bind
  ("C-k" . kill-line)
  ("C-M-y" . wsl-yank))

;; (use-package verilog-ts-mode
;;   :after verilog-mode
;;   :vc (:fetcher "github" :repo "gmlarumbe/verilog-ext" :files ("verilog-ts-mode.el")))

;; (add-hook 'python-base-mode-hook 'flymake-mode) (setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-")
;; (add-hook 'eglot-managed-mode-hook
;;           #'(lambda () (cond ((derived-mode-p 'python-base-mode)
;;                               (add-hook 'flymake-diagnostic-functions 'python-flymake nil t)))))

(use-package tempel
  :custom
  (tempel-path (list
                (concat no-littering-etc-directory "tempel/*/*.eld")
                (concat no-littering-etc-directory "tempel/*.eld")))
  :preface
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (append (butlast completion-at-point-functions) (list #'tempel-expand t)))
    )
  :hook
  (prog-mode . tempel-setup-capf)
  (text-mode . tempel-setup-capf)
  :bind
  (:map tempel-map
        ("<tab>" . tempel-next)))

(provide 'init)
;;; init.el ends here
