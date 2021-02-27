;; Initialization
(setq gc-cons-threshold 64000000)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq-default use-package-always-defer t)
;; (setq-default use-package-always-ensure t)

(require 'server)
(unless (server-running-p) (server-start))

(setq custom-file "~/.emacs.d/elisp/custom.el")
(load custom-file)
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "elisp/"))
;; Init Done

;; Debug
;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

(use-package no-littering
  :after recentf
  :commands no-littering-expand-var-file-name
  :init
  (setq-default no-littering-etc-directory (expand-file-name "config/" user-emacs-directory)
                no-littering-var-directory (expand-file-name "data/" user-emacs-directory)
                auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Defaults & Built-ins          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar user-data-directory (getenv "EMACS_STORAGE_LOCATION"))
(unless user-data-directory
  (setq user-data-directory "~/Nextcloud"))

(use-package emacs :ensure nil
  :init
  (setq-default user-full-name "Furkan Usta"
                user-mail-address "furkanusta17@gmail.com"
                user-papers-directory (concat user-data-directory "/Papers")
                user-notes-directory (concat user-data-directory "/Notes")
                user-bibliography (concat user-data-directory "/Papers/Library.bib")
                save-interprogram-paste-before-kill t
                emacs-load-start-time (current-time)
                ad-redefinition-action 'accept
                ;; backup-inhibited t
                vc-make-backup-files t
                version-control t
                delete-old-versions t
                calendar-week-start-day 1
                delete-by-moving-to-trash t
                confirm-nonexistent-file-or-buffer nil
                tab-width 4
                tab-stop-list (number-sequence 4 200 4)
                indent-tabs-mode nil
                backup-directory-alist `(("." . "~/.emacs.d/.gen"))
                gdb-many-windows t
                use-file-dialog nil
                use-dialog-box nil
                inhibit-startup-screen t
                inhibit-startup-echo-area-message t
                inhibit-startup-screen t
                cursor-type 'bar
                ring-bell-function 'ignore
                scroll-step 1
                sentence-end-double-space -1
                fill-column 100
                scroll-step 1
                scroll-conservatively 10000
                initial-major-mode 'org-mode
                auto-window-vscroll nil
                comint-prompt-read-only t
                vc-follow-symlinks t
                scroll-preserve-screen-position t
                frame-resize-pixelwise t
                undo-limit 1280000
                large-file-warning-threshold (* 1024 1024 1024) ;; 1GB
                font-use-system-font t))

(use-package column-number-mode :ensure nil)

(use-package display-time :ensure nil
  :init
  (setq-default display-time-default-load-average nil
                display-time-load-average-threshold 100.0
                display-time-24hr-format t)
  (display-time-mode))

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(toggle-frame-maximized)

(use-package ediff
  :init (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                      ediff-split-window-function 'split-window-horizontally))

;; These are built-in packages and having ensure results in lots of warnings
(use-package desktop
  :ensure nil
  :config
  (desktop-save-mode 1)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode))

(use-package menu-bar :ensure nil :demand t :config (menu-bar-mode -1))
(use-package tool-bar :ensure nil :demand t :config (tool-bar-mode -1))
(use-package scroll-bar :ensure nil :demand t :config (scroll-bar-mode -1))
(use-package frame :ensure nil :demand t :config (blink-cursor-mode 0))

(use-package paren :demand t :config (show-paren-mode 1))
(use-package display-line-numbers :demand t :config (global-display-line-numbers-mode))

(use-package hl-line :config (global-hl-line-mode t))

;;; file opening procedures
(defun dired-open-xdg ()
  "Try to run `xdg-open' to open the file under point."
  (interactive)
  (if (executable-find "xdg-open")
      (let ((file (ignore-errors (dired-get-file-for-visit)))
            (process-connection-type nil))
        (start-process "" nil "xdg-open" (file-truename file)))
    nil))

(use-package dired
  :ensure nil
  :init (setq-default dired-listing-switches "-vaBhl  --group-directories-first"
                      dired-auto-revert-buffer t
                      dired-create-destination-dirs 'ask
                      dired-dwim-target t)
  :bind (:map dired-mode-map
              ("E" . dired-open-xdg)))

(use-package diredfl
  :config (diredfl-global-mode)
  :init (setq-default diredfl-read-priv nil
                      diredfl-write-priv nil
                      diredfl-execute-priv nil))

(use-package delsel :ensure nil :demand t :init (delete-selection-mode 1))

(use-package flyspell)

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-helm
  :after flyspell-correct)

(use-package recentf
  :ensure nil
  :init (recentf-mode t)
  :config (setq-default recent-save-file "~/.emacs.d/recentf"))

(use-package saveplace
  :ensure nil
  :config (save-place-mode 1)
  :hook (server-visit . save-place-find-file-hook))

(use-package uniquify
  :ensure nil
  :init (setq-default uniquify-buffer-name-style 'reverse
                      uniquify-separator " • "
                      uniquify-after-kill-buffer-p t
                      uniquify-ignore-buffers-re "^\\*"))

(use-package which-func :config (which-function-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Functions and keybindings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(global-set-key (kbd "M-n") 'scroll-up-in-place)
(global-set-key (kbd "M-p") 'scroll-down-in-place)
(global-set-key (kbd "<f7>") 'eww)
(global-set-key (kbd "C-M-;") 'my-align-comments)
(global-set-key (kbd "C-c C-k") 'kill-other-buffers)
(global-set-key (kbd "C-c d") 'duplicate-line-or-region)
(global-set-key (kbd "C-c e r") 'eval-region)
(global-set-key (kbd "C-S-d") 'delete-backward-char)
(global-set-key (kbd "M-D") 'backward-kill-word)
(global-set-key (kbd "C-w") 'xah-cut-line-or-region) ; cut
(global-set-key (kbd "M-k") 'kill-whole-line)
(global-set-key (kbd "M-w") 'xah-copy-line-or-region) ; copy
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)
(define-key prog-mode-map (kbd "<tab>") 'indent-for-tab-command)

(defun dashboard-insert-scratch (list-size)
  (dashboard-insert-section
   "Scratch:"
   '("*scratch*")
   list-size
   "s"
   `(lambda (&rest ignore) (switch-to-buffer "*scratch*"))))

(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  (add-to-list 'dashboard-item-generators  '(scratch . dashboard-insert-scratch))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-center-content t
        dashboard-startup-banner 'logo
        dashboard-items '((scratch . 1)
                          (recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5))
        dashboard-banner-logo-title "Emacs"))

(use-package isearch :ensure nil :demand t :bind (("C-c s" . isearch-forward)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Helm          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm packages for other modules will be near their corresponding modules, not in here

(use-package helm
  :diminish helm-mode
  :commands helm-autoresize-mode
  :config
  (require 'helm-config)
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (global-unset-key (kbd "C-x c"))
  :bind
  (("M-x" . helm-M-x)
   ;; ("C-s" . helm-occur)
   ("C-x b" . helm-mini)
   ("C-z" .  helm-select-action)
   ("M-y" . helm-show-kill-ring)
   ("C-c s" . isearch-forward)
   ("C-c C-r" . helm-resume)
   ("<f6>" . helm-imenu)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("<left>" . helm-previous-source)
   ("<right>" . helm-next-source))
  :init
  (setq-default helm-ff-search-library-in-sexp t
                helm-ff-file-name-history-use-recentf t
                helm-ff-allow-non-existing-file-at-point nil
                helm-ff-auto-update-initial-value t
                helm-ff-guess-ffap-filenames t
                helm-ff-guess-ffap-urls nil
                helm-semantic-fuzzy-match t
                helm-M-x-fuzzy-match t
                helm-imenu-fuzzy-match t
                helm-substitute-in-filename-stay-on-remote t
                helm-boring-buffer-regexp-list (list (rx "*magit-") (rx "*helm") (rx "*flycheck"))
                helm-split-window-inside-p t
                helm-move-to-line-cycle-in-source t
                helm-scroll-amount 8))

(use-package helm-files
  :ensure nil
  :bind ("C-x C-f" . helm-find-files)
  :init
  (setq-default helm-ff-skip-boring-files t))

(use-package helm-bibtex
  :init
  (setq-default bibtex-completion-bibliography user-bibliography
                bibtex-completion-library-path (concat user-data-directory "/Papers/")
                bibtex-completion-find-additional-pdfs t
                ;; bibtex-completion-display-formats '((t . "${=has-pdf=:1}     ${author:50}   | ${year:4} |   ${title:150}"))
                bibtex-completion-notes-path (concat user-notes-directory "/Papers.org")))

(use-package helm-tramp)

(use-package helm-fd
  :bind ("C-c h f" . helm-fd))

(use-package helm-swoop
  :bind
  ("C-s" . helm-swoop)
  ("C-c h h" . helm-swoop-back-to-last-point)
  :init (setq-default helm-swoop-split-with-multiple-windows nil
                      helm-swoop-move-to-line-cycle t
                      helm-swoop-use-fuzzy-match nil
                      helm-swoop-speed-or-color t))

(use-package helm-rg
  :bind ("C-c C-s" .  helm-rg))

(use-package helm-bookmarks
  :ensure nil
  :bind ("C-c h b" . helm-bookmarks))

(use-package ace-jump-helm-line
  :after helm
  :bind (:map helm-map
              ("C-'" . ace-jump-helm-line)))

(use-package deadgrep
  :bind ("C-c h s" . deadgrep))

(use-package helm-flycheck
  :after flycheck
  :bind (:map flycheck-mode-map ("C-c h f" . helm-flycheck)))

(use-package helm-lsp)

(use-package xref
  :init (setq-default xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package helm-xref)

(use-package avy
  :bind
  ("M-g c" . avy-goto-char-2)
  ("C-c C-j" . avy-resume)
  ("M-g g" . avy-goto-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Visual          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons)

(use-package all-the-icons-dired :after all-the-icons :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-modeline
  :init (doom-modeline-mode)
  :config (setq-default doom-modeline-buffer-encoding nil))

(use-package diminish)

(use-package darkokai-theme :init (load-theme 'darkokai t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Tools & Utils          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun disable-line-numbers ()
  (display-line-numbers-mode -1))

(defun toggle-line-numbers ()
  (display-line-numbers-mode (or (not display-line-numbers-mode) 0)))

(use-package image-mode
  :ensure nil
  :hook (image-mode . disable-line-numbers))

(use-package image+
  :after image-mode
  :bind (:map image-mode-map
         ("C-+" . imagex-sticky-zoom-in)
         ("C--" . imagex-sticky-zoom-out)
         ("C-0" . imagex-sticky-restore-original)))


(defun +rss/delete-pane ()
  "Delete the *elfeed-entry* split pane."
  (interactive)
  (let* ((buf (get-buffer "*elfeed-entry*"))
         (window (get-buffer-window buf)))
    (delete-window window)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(use-package elfeed
  :init
  (setq-default elfeed-feeds
                '(("http://research.swtch.com/feeds/posts/default" other)
                  ("http://bitbashing.io/feed.xml" other)
                  ("http://preshing.com/feed" other)
                  ("http://danluu.com/atom.xml" other)
                  ("http://tenderlovemaking.com/atom.xml" other)
                  ("http://feeds.feedburner.com/codinghorror/" other)
                  ("http://www.snarky.ca/feed" other)
                  ("http://blog.regehr.org/feed" cpp)
                  ("https://blog.acolyer.org/feed/" other)
                  ("https://randomascii.wordpress.com/" other)
                  ("http://planet.emacsen.org/atom.xml" emacs)
                  ("http://planet.gnome.org/rss20.xml" gnome)
                  ("http://arne-mertz.de/feed/" cpp)
                  ("http://zipcpu.com/" fpga)
                  ("https://code-cartoons.com/feed" other)
                  ("https://eli.thegreenplace.net/feeds/all.atom.xml" cpp)
                  ("https://www.evanjones.ca/index.rss" other)
                  ("https://jvns.ca/atom.xml" other)
                  ("https://aphyr.com/posts.atom" other)
                  ("https://brooker.co.za/blog/rss.xml" other)
                  ("https://rachelbythebay.com/w/atom.xml" other)
                  ("https://mrale.ph/feed.xml" other)
                  ("https://medium.com/feed/@steve.yegge" other)
                  ("https://research.swtch.com/" other)
                  ("http://aras-p.info/atom.xml" other)
                  ("http://city-journal.org/rss" other)
                  ("https://what-if.xkcd.com/feed.atom" xkcd)
                  ("http://xkcd.com/rss.xml" xkcd)
                  ("https://esoteric.codes/rss" other)
                  ("http://irreal.org/blog/?feed=rss2" other)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-xTvXTm-lrLWYk308-Km3A" youtube)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsvn_Po0SmunchJYOWpOxMg" youtube)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCCpTaib_e5C6Q95qwazq8OA" youtube)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCO-_F5ZEUhy0oKrSa69DLMw" youtube)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eEGT06FrWFU6VBnPOR9lg" youtube))
                elfeed-show-entry-switch #'pop-to-buffer
                elfeed-show-entry-delete #'+rss/delete-pane))

(use-package pocket-reader)

(use-package vlf
  :after dired
  :hook (vlf-view-mode . disable-line-numbers)
  :init (require 'vlf-setup)
  (add-to-list 'vlf-forbidden-modes-list 'pdf-view-mode)
  (add-to-list 'vlf-forbidden-modes-list 'nov-mode))

(defun pdf-view-page-number ()
  (interactive)
  (message " [%s/%s]"
           (number-to-string (pdf-view-current-page))
           (number-to-string (pdf-cache-number-of-pages))))


;; requires pdf-tools-install
(use-package pdf-tools
  :hook ((pdf-view-mode . (lambda () (cua-mode 0)))
         (pdf-view-mode . disable-line-numbers)
         (pdf-view-mode . pdf-sync-minor-mode)
         (pdf-view-mode . pdf-links-minor-mode)
         (pdf-view-mode . pdf-history-minor-mode)
         (pdf-view-mode . pdf-view-midnight-minor-mode))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  (setq-default pdf-view-display-size 'fit-page
                pdf-annot-activate-created-annotations nil
                pdf-view-resize-factor 1.1))

(use-package pdf-view-restore
  :after pdf-tools
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :init (setq-default pdf-view-restore-filename "~/.emacs.d/.gen/.pdf-view-restore"))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1)
  :init (setq-default undo-tree-visualizer-timestamps t
                      undo-tree-visualizer-diff t)
  :bind
  ("C-+" . undo-tree-redo)
  ("C-_" . undo-tree-undo))

(use-package immortal-scratch :config (immortal-scratch-mode t))
(use-package persistent-scratch :config (persistent-scratch-setup-default))
(use-package scratch :bind ("M-s M-s" . scratch))


(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode 1)
  :bind ("M-i" . yas-expand)
  (:map yas-minor-mode-map ("<tab>" . nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Programming Tools          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generic
(use-package company
  :diminish company-mode
  :commands company-complete
  :hook (after-init . global-company-mode)
  :init (setq-default company-idle-delay nil))

(use-package helm-company
  :after helm company
  :bind ("<C-tab>" . (function helm-company)))

(use-package company-statistics
  :after company
  :hook (after-init . company-statistics-mode))

(use-package company-quickhelp :config (company-quickhelp-mode t))

(use-package company-lsp
  :after company lsp
  :init
  (push 'company-lsp company-backends)
  (setq-default company-lsp-enable-recompletion t
                company-lsp-enable-snippet t))

(use-package magit
  :init (magit-wip-mode 1)
  :config (setq-default magit-wip-merge-branch t)
  :bind ("C-c g s" . magit-status))

(use-package magit-todos :config (magit-todos-mode))

(use-package diff-hl :config (global-diff-hl-mode))

(use-package hl-todo :config (global-hl-todo-mode))

(use-package flycheck
  :config (global-flycheck-mode)
  :init (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc verilog-verilator)))

(use-package flycheck-pos-tip
  :after flycheck
  :config (flycheck-pos-tip-mode))

(use-package evil-nerd-commenter :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package visual-regexp-steroids
  :init (require 'visual-regexp-steroids)
  :bind ("C-r" . vr/replace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Navigation          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package buffer-move
  :bind
  ("C-c S-<up>"    . buf-move-up)
  ("C-c S-<down>"  . buf-move-down)
  ("C-c S-<left>"  . buf-move-left)
  ("C-c S-<right>" . buf-move-right))

(use-package drag-stuff
  :diminish drag-stuff-mode
  :init (drag-stuff-global-mode t)
  :bind (:map drag-stuff-mode-map
         ("<M-up>" . drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

(use-package eyebrowse
  :init (eyebrowse-mode t)
  :init (setq-default eyebrowse-wrap-around t)
  :bind
  (:map eyebrowse-mode-map
        ("C-c C-w <left>" . eyebrowse-prev-window-config)
        ("C-c C-w l" . eyebrowse-switch-to-window-config)
        ("C-c C-w <right>" . eyebrowse-next-window-config)))

(use-package hungry-delete
  :commands global-hungry-delete-mode
  :init (global-hungry-delete-mode))

(use-package goto-chg :bind ("C-c g ;" . goto-last-change))

(use-package writeroom-mode
  :config (setq-default writeroom-width 150
                        writeroom-mode-line nil)
  :bind ("C-c w r" . writeroom-mode)
  :hook (writeroom-mode . toggle-line-numbers))

(use-package focus
  :bind ("C-c C-f" . focus-mode) ;; Might be unnecessary
  (:map focus-mode-map
        ("C-c C-n" . focus-next-thing)
        ("C-c C-p" . focus-prev-thing)))

(use-package expand-region
  :bind ("C-}" . er/expand-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Org Mode          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :init
  (setq-default org-adapt-indentation t
                org-catch-invisible-edits 'show-and-error
                org-cycle-separator-lines 0
                org-directory user-notes-directory
                org-edit-src-content-indentation 0
                org-fontify-quote-and-verse-blocks t
                org-fontify-done-headline t
                org-fontify-whole-heading-line t
                org-hide-emphasis-markers t
                org-hide-leading-stars t
                org-imenu-depth 4
                org-indent-indentation-per-level 1
                org-log-done t
                org-pretty-entities t
                org-src-fontify-natively t
                org-src-preserve-indentation nil
                org-src-tab-acts-natively t
                org-yank-adjusted-subtrees t
                org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "|" "DONE")
                                    (sequence "PAUSED" "SCHEDULED" "|"  "CANCELLED")))
  :hook
  (org-mode . turn-on-flyspell)
  (org-mode . auto-fill-mode)
  :bind (:map org-mode-map ("C-c C-." . org-time-stamp-inactive)))

(use-package org-alert
  :commands org-alert-enable
  :config (org-alert-enable))

(use-package org-fragtog :hook (org-mode . org-fragtog-mode))

(use-package org-babel :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     ;; (http . t)
     (shell . t)
     (emacs-lisp . t))))

(use-package org-cliplink :bind (:map org-mode-map ("C-c i l" . org-cliplink)))

(use-package org-capture :ensure nil
  :init
  (setq-default  org-capture-file (concat org-directory "/Capture.org")
                 org-default-notes-file org-capture-file
                 org-capture-templates
                '(("t" "TODO" entry (file+headline org-capture-file "Tasks")
                   "* TODO %?\n  %a\n  %i\n")
                  ("j" "Journal" entry (file+headline org-capture-file "Journal")
                   "* %U\n  %a\n  %i")
                  ("p" "Protocol" entry (file+headline org-capture-file "Inbox")
                   "* %?\n  [[%:link][%:description]]\n  %U\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n")
	              ("L" "Protocol Link" entry (file+headline org-capture-file "Inbox")
                   "* %?\n  [[%:link][%:description]]\n  %U")))
  :bind ("C-c c" . org-capture))

(use-package org-protocol :ensure nil)

(use-package org-agenda :ensure nil
  :init (setq-default org-agenda-files (list org-directory)
                      org-agenda-include-diary t
                      org-agenda-span 10
                      org-agenda-start-day "-2d")
  :bind ("C-c a" . org-agenda))

(use-package org-refile :ensure nil
  :init (setq-default org-refile-use-outline-path t
                      org-refile-targets (quote ((nil :maxlevel . 9)
                                                 (org-agenda-files :maxlevel . 9)))
                      org-refile-allow-creating-parent-nodes (quote confirm)))

(use-package org-clock :ensure nil
  :init (setq-default org-clock-out-remove-zero-time-clocks t
                      org-clock-report-include-clocking-task t
                      org-clock-out-when-done t))

(use-package org-tempo :defer nil :after org)

(use-package org-ref
  :init (setq-default org-ref-bibliography-notes (concat org-directory "/Papers.org")
                      org-ref-default-bibliography (list user-bibliography)
                      org-ref-pdf-directory (concat user-data-directory "/Papers/")
                      org-ref-show-broken-links t))

(use-package bibtex
  :init (setq-default bibtex-align-at-equal-sign t))

(use-package biblio)

(use-package org-noter
  :init
  (setq-default org-noter-notes-search-path (list org-directory)
                org-noter-default-notes-file-names (list "Papers.org")
                org-noter-auto-save-last-location t
                org-noter-insert-note-no-questions t))

(use-package org-books
  :init (setq org-books-file (concat user-notes-directory "/Books.org")
              org-books-file-depth 0))

;; https://alhassy.github.io/org-special-block-extras/
(use-package org-special-block-extras)

(use-package ebib
  :init
  (setq-default ebib-preload-bib-files (list user-bibliography)
                ebib-bibtex-dialect 'biblatex
                ebib-default-directory 'first-bib-dir
                ebib-index-window-size 20
                ebib-bib-search-dirs (list user-papers-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          C++          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :ensure nil
  :mode ("\\.h\\'" . c++-mode)
  :init
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (custom-set-variables '(c-noise-macro-names '("constexpr")))
  (setq-default c-default-style "stroustrup"
                c-basic-offset 4
                c-indent-level 4
                access-label 0
                tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
                tab-width 4
                indent-tabs-mode nil)
  (with-eval-after-load 'flycheck
    (setq-default flycheck-clang-language-standard "c++1z"
                flycheck-gcc-language-standard "c++1z"
                flycheck-cppcheck-standards "c++1z"
                flycheck-clang-standard-library "libc++")
    (flycheck-add-mode 'c/c++-cppcheck 'c/c++-gcc)
    (flycheck-add-mode 'c/c++-cppcheck 'c/c++-clang)))

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode)

(use-package cmake-font-lock :hook (cmake-mode . cmake-font-lock-activate))

(use-package meson-mode :hook (meson-mode . company-mode))

(use-package lsp-mode
  :init
  (setq-default lsp-auto-execute-action nil
                lsp-before-save-edits nil
                lsp-keymap-prefix "C-c C-l"
                ;; lsp-auto-guess-root t
                lsp-enable-snippet t
                lsp-enable-xref t
                lsp-enable-imenu t
                lsp-prefer-flymake nil
                lsp-enable-indentation nil
                lsp-prefer-capf t
                lsp-enable-file-watchers nil
                lsp-enable-text-document-color nil
                lsp-enable-semantic-highlighting nil
                lsp-enable-on-type-formatting nil
                ;; lsp-auto-configure t
                ;; lsp-idle-delay 0.500
                read-process-output-max (* 2 1024 1024)
                lsp-enable-on-type-formatting nil))

;; (use-package lsp-ui
;;   :init
;;   (setq-default lsp-ui-flycheck-enable t
;;                 lsp-ui-imenu-enable t
;;                 ;; lsp-ui-peek-enable t
;;                 lsp-ui-sideline-enable t
;;                 lsp-ui-doc-position 'top))

;; (use-package dap
;;   :config
;;   ;; (tooltip-mode 1)
;;   (dap-mode 1)
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1))

;; (use-package dap-lldb)

(use-package origami
  :hook (prog-mode . origami-mode)
  :bind
  ("C-c ," . origami-toggle-node)
  ("C-c C-." . origami-close-all-nodes)
  ("C-c C->" . origami-open-all-nodes))

;; (use-package lsp-origami :hook origami-mode)

(use-package company-c-headers
  :after company
  :init (add-to-list 'company-backends 'company-c-headers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Perl          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cperl-mode
  :ensure nil
  :init
  (defalias 'perl-mode 'cperl-mode)
  (setq-default cperl-indent-level 4
                cperl-close-paren-offset -4
                cperl-continued-statement-offset 4
                cperl-indent-parens-as-block t
                cperl-tab-always-indent nil)
  (with-eval-after-load 'flycheck (flycheck-add-mode 'perl-perlcritic 'perl)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;           Go            ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package go-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;          Scala          ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :mode ("\\.sc\\'" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :init
  (setq-default sbt:program-options '("-Dsbt.supershell=false"))
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package lsp-metals
  :hook  (scala-mode . lsp)
  :init (setq lsp-metals-treeview-show-when-views-received nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Shell          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode -1))

(use-package ansi-color
  :commands ansi-color-apply-on-region
  :hook (compilation-filter . colorize-compilation-buffer)
  :init
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t))

(use-package shell
  :bind (:map shell-mode-map ("<tab>" . completion-at-point)))

(add-to-list 'auto-mode-alist '("\\.v\\'" . fundamental-mode))

(use-package treemacs
  :commands treemacs-resize-icons treemacs-fringe-indicator-mode
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  :init
  (setq-default treemacs-position 'right
                treemacs-width 50)
  (treemacs-resize-icons 20)
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
  :bind
  (:map global-map
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)))

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit :after treemacs magit)

(use-package lsp-treemacs :config (lsp-treemacs-sync-mode 1))

(use-package treemacs-persp
  :after treemacs persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

(use-package dockerfile-mode :mode ("Dockerfile\\'" "\\.docker"))
(use-package docker-compose-mode :mode ("docker-compose\\.yml\\'" "-compose.yml\\'"))

;; (use-package docker)

(use-package flycheck-clang-analyzer
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(use-package goto-chg
  :bind
  ("C-." . goto-last-change)
  ("C->" . goto-last-change-reverse))

(defun my-open-readme ()
  (let* ((project-name (projectile-project-name))
         (project-root (projectile-project-root))
         (project-files (directory-files project-root nil nil t))
         (readme-files (seq-filter (lambda (file) (string-prefix-p "readme" file t)) project-files)))
    (if readme-files
        (let ((readme-file (car readme-files)))
          (find-file (expand-file-name readme-file project-root)))
      (find-file (expand-file-name "README.org" project-root)))))

(use-package projectile
  :config (projectile-mode 1)
  :init (setq-default projectile-enable-caching t
                        projectile-switch-project-action 'my-open-readme
                        projectile-completion-system 'helm)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map)))

(use-package helm-projectile :after helm projectile :init (helm-projectile-on))

(use-package all-the-icons-ibuffer
  :config (all-the-icons-ibuffer-mode 1))

(use-package link-hint
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

(use-package fountain-mode)

(use-package shrface
  :after nov
  :demand t
  :hook (nov-mode . shrface-mode)
  :init
  (setq shrface-href-versatile t
        nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
  :config
  (shrface-basic)
  (shrface-trial)
  :bind
  (:map nov-mode-map
        ("<tab>" . shrface-outline-cycle)
        ("S-<tab>" . shrface-outline-cycle-buffer)
        ("C-j" . shrface-next-headline)
        ("C-k" . shrface-previous-headline)))

(use-package eww
  :defer t
  :hook (eww-after-render . shrface-mode)
  :config (require 'shrface)
  :bind
  (:map eww-mode-map
        ("<tab>" . shrface-outline-cycle)
        ("S-<tab>" . shrface-outline-cycle-buffer)
        ("C-j" . shrface-next-headline)
        ("C-k" . shrface-previous-headline)))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :init (setq nov-text-width 100))

(use-package verilog-mode
  :mode
  ("\\.v\\'" . verilog-mode)
  ("\\.sv\\'" . verilog-mode)
  :init (setq-default verilog-auto-newline nil
                      verilog-tab-always-indent nil
                      verilog-auto-indent-on-newline nil
                      verilog-case-indent 4
                      verilog-cexp-indent 4
                      verilog-indent-begin-after-if nil
                      verilog-indent-level 4
                      verilog-indent-level-behavioral 4
                      verilog-indent-level-directive 4
                      verilog-indent-level-declaration 4
                      verilog-indent-level-module 4))

(defun bm-save-all ()
  (progn (bm-buffer-save-all)
         (bm-repository-save)))

(use-package bm
  :init
  (setq-default bm-cycle-all-buffers t
                bm-restore-repository-on-load t
                bm-repository-file (concat no-littering-var-directory "bm-repository")
                bm-buffer-persistence t)
  :hook
  ((after-init . bm-repository-load)
   (after-save . bm-buffer-save)
   (vc-before-checkin . bm-buffer-save)
   (find-file . bm-buffer-restore)
   (after-revert . bm-buffer-restore)
   (kill-buffer . bm-buffer-save)
   (kill-emacs . bm-save-all))
  :bind (("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)
         ("C-<f2>" . bm-toggle)
         ("<left-fringe> <mouse-1>" . bm-toggle-mouse)
         ( "<left-fringe> <mouse-4>" . bm-previous-mouse)
         ( "<left-fringe> <mouse-5>" . bm-next-mouse)))

(use-package beacon
  :init (beacon-mode 1)
  :init (setq-default beacon-blink-when-point-moves-vertically 20
                      beacon-blink-when-buffer-changes nil
                      beacon-blink-when-window-changes nil))

(use-package neotree
  :after projectile
  ;; :hook (projectile-after-switch-project . neotree-projectile-action)
  :init (setq-default neo-smart-open t
                      neo-vc-integration nil)
  :bind ("C-x t n" . neotree-toggle))

(use-package vterm)

(use-package multi-vterm :load-path "elisp/" :demand t
  :bind
  ("<f8>" . multi-vterm-dedicated-open)
  ("C-<f8>" . multi-vterm-dedicated-toggle)
  ("M-<f8>" . multi-vterm))

(use-package binder)

(use-package tree-sitter
  :init (global-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package graphviz-dot-mode
  :config (setq-default graphviz-dot-indent-width 4))

(use-package company-graphviz-dot)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;         LISP        ::
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sly)

(use-package helm-sly
  :after helm-company
  :config (sly-mrepl . company-mode)
  :bind
  (:map sly-mrepl-mode-map ("<tab>" . helm-company)))

(use-package smartparens
  :config (require 'smartparens-config))

;;
(use-package calfw)
(use-package calfw-org :after calfw
  :init (setq-default cfw:org-overwrite-default-keybinding t)
  :bind
  ("C-c C-f" . cfw:open-org-calendar)
  (:map cfw:calendar-mode-map ("<return>" . cfw:org-open-agenda-day)))


(defun exec-find (command) (locate-file command exec-path exec-suffixes 1))

;; (use-package reformatter)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Python       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package blacken
  :init (setq-default blacken-line-length 100)
  :hook (python-mode . blacken-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;        LaTeX        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tex
  :mode ("\\.tex\\'" . latex-mode)
  :init (setq-default TeX-master nil
                      TeX-parse-self t
                      TeX-auto-save t
                      ;; TeX-electric-sub-and-superscript t
                      TeX-parse-self t
                      TeX-auto-save t
                      LaTeX-electric-left-right-brace t
                      TeX-view-program-selection '((output-pdf "PDF Tools"))
                      TeX-source-correlate-start-server t
                      TeX-source-correlate-method 'synctex
                      TeX-electric-math (cons "\\(" "\\)"))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :hook ((LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . TeX-PDF-mode)))


(use-package reftex
  :hook (LaTeX-mode . turn-on-reftex)
  :config (setq reftex-plug-into-AUCTeX t))

(use-package cdlatex
  :hook (LaTeX-mode . cdlatex-mode))

(use-package company-auctex
  :commands company-auctex-init
  :init (company-auctex-init))

(use-package auctex-latexmk
  :commands auctex-latexmk-setup
  :init (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; (use-package magic-latex-buffer
;;   :hook (LaTeX-mode . magic-latex-buffer))
