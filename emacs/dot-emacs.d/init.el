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
; (setq-default use-package-always-ensure t)
(setq custom-file "~/.emacs.d/elisp/custom.el")
(load custom-file)

(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "elisp/"))
;; Init Done


;; Debug
;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Defaults & Built-ins          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default user-full-name "Furkan Usta"
              user-mail-address "furkanusta17@gmail.com"
              user-bibliography "~/Documents/Nextcloud/Papers/Library.bib"
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
              auto-save-file-name-transforms `((".*" "~/.emacs.d/.gen/" t))
              ediff-window-setup-function 'ediff-setup-windows-plain
              ediff-split-window-function 'split-window-horizontally
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
              display-time-default-load-average nil
              display-time-24hr-format t
              undo-limit 1280000
              font-use-system-font t)

;; These are not usable with use-package
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode)
(global-display-line-numbers-mode t)
(blink-cursor-mode 0)
(show-paren-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; These are built-in packages and having ensure results in lots of warnings
(use-package desktop
  :ensure nil
  :init (desktop-save-mode 1)
  :config (add-to-list 'desktop-modes-not-to-save 'dired-mode))

(use-package dired
  :ensure nil
  :config (setq-default
           dired-listing-switches "-vaBhl  --group-directories-first"
           dired-dwim-target t))

(use-package diredfl
  :init (diredfl-global-mode)
  :config (setq-default diredfl-read-priv nil
                        diredfl-write-priv nil
                        diredfl-execute-priv nil))

(use-package delsel
  :ensure nil
  :init (delete-selection-mode 1))

(use-package flyspell)

(use-package hideshow
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind
  ("C-c C-," . hs-toggle-hiding)
  ("C-c C-." . hs-hide-all)
  ("C-c C->" . hs-show-all))

(use-package recentf
  :ensure nil
  :init (recentf-mode t)
  :config (setq-default recent-save-file "~/.emacs.d/recentf"))

(use-package saveplace
  :ensure nil
  :init (save-place-mode 1)
  :config (setq-default server-visit-hook (quote (save-place-find-file-hook))))

(use-package uniquify
  :ensure nil
  :config
  (setq-default uniquify-buffer-name-style 'reverse
                uniquify-separator " • "
                uniquify-after-kill-buffer-p t
                uniquify-ignore-buffers-re "^\\*"))

(use-package which-func :init (which-function-mode t))

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
(global-set-key (kbd "<f8>") 'shell)
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

(use-package dashboard
  :ensure t
  :init (dashboard-setup-startup-hook)
  :config (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
                dashboard-center-content t
                dashboard-startup-banner 'logo
                dashboard-items '((recents  . 5)
                                  (bookmarks . 5)
                                  (projects . 5)
                                  (agenda . 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq-default ivy-use-virtual-buffers t
                enable-recursive-minibuffers t
                search-default-mode #'char-fold-to-regexp
                ivy-count-format "(%d/%d) "
                ivy-wrap t
                ivy-height 20
                ivy-extra-directories '()
                counsel-find-file-at-point t
                )
  :bind
  (("C-s" . swiper-thing-at-point)
   ("C-c C-r" . ivy-resume)
   ("C-x b" . ivy-switch-buffer)
   ("C-M-j" . ivy-immediate-done)
;; (setq projectile-completion-system 'ivy)
;; (global-set-key (kbd "C-c n") 'counsel-fzf)
   ;; (global-set-key (kbd "C-c v") 'ivy-push-view)
   ;; (global-set-key (kbd "C-c V") 'ivy-pop-view)
   ("C-c C-f" . counsel-fzf)
   ("C-c C-s" . counsel-rg)))

(use-package lsp-ivy)

(use-package counsel-projectile
  :init (counsel-projectile-mode 1))

(use-package amx :bind (("M-x" . amx)))

(use-package ivy-rich
  :init (ivy-rich-mode 1)
  :config (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package historian
  :init (historian-mode 1))
(use-package ivy-historian
  :after ivy
  :init (ivy-historian-mode 1))

;; (use-package all-the-icons-ivy
;;   :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Visual          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons)

(use-package all-the-icons-dired :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package display-time :init (display-time-mode))

(use-package column-number :init (column-number-mode))

(use-package doom-modeline :init (doom-modeline-mode))

(use-package diminish)

(use-package my-darkokai-theme
  :init (load-theme 'my-darkokai t)
  :config (setq my-darkokai-mode-line-padding 4))

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

(use-package elfeed
  :config
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
                  ("https://www.reddit.com/r/cpp/top/.rss?t=week" cpp)
                  ("https://www.reddit.com/r/programming/top/.rss?t=week" prog)
                  ("https://www.reddit.com/r/emacs/top/.rss?t=week" prog)
                  ("https://www.reddit.com/r/linux/top/.rss?t=week" prog)
                  ("https://www.reddit.com/r/python/top/.rss?t=month" python)
                  ("https://www.reddit.com/r/philosophy/top/.rss?t=month" soc)
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
                  ("https://medium.theuxblog.com/feed" ux)
                  ("https://feeds.feedburner.com/uxmovement" ux)
                  ("http://aras-p.info/atom.xml" other)
                  ("http://city-journal.org/rss" other)
                  ("https://writing.kemitchell.com/feed.xml" other)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eEGT06FrWFU6VBnPOR9lg" youtube)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCO-_F5ZEUhy0oKrSa69DLMw" youtube)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-xTvXTm-lrLWYk308-Km3A" youtube)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7dF9qfBMXrSlaaFFDvV_Yg" youtube)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCK6XWOay4sher8keh2x1jLA" youtube)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCU1Fhn0o5S0_mdcgwCPuLDg" youtube)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsvn_Po0SmunchJYOWpOxMg" youtube)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UClJ7gpJ9MRXDnbA8N_5NSKQ" youtube)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UChti8oyWC3oW91LpfZ2bmSQ" youtube cpp)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAczr0j6ZuiVaiGFZ4qxApw" youtube cpp)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCJpMLydEYA08vusDkq3FmjQ" youtube cpp)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCQ4JGczdlU3ofHWf3NuCX8g" youtube cpp)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCv2_41bSAa5Y_8BacJUZfjQ" youtube cpp)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMlGfpWw-RUdWX_JbLCukXg" youtube cpp)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC5e__RG9K3cHrPotPABnrwg" youtube cpp)
                  ("http://xkcd.com/rss.xml" xkcd))))

(use-package vlf
  :after dired
  :hook (vlf-view-mode . disable-line-numbers)
  :init (require 'vlf-setup)
  (add-to-list 'vlf-forbidden-modes-list 'pdf-view-mode))

(defun pdf-view-page-number ()
  (interactive)
  (message " [%s/%s]"
           (number-to-string (pdf-view-current-page))
           (number-to-string (pdf-cache-number-of-pages))))


;; requires pdf-tools-install
(use-package pdf-tools
  :hook ((pdf-view-mode . (lambda () (cua-mode 0)))
         (pdf-view-mode . disable-line-numbers)
         (pdf-view-mode . pdf-view-midnight-minor-mode))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page
                pdf-annot-activate-created-annotations nil
                pdf-view-resize-factor 1.1)
  :bind (:map pdf-view-mode-map ("t" . pdf-view-page-number)))

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  (setq-default pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (setq-default undo-tree-visualizer-timestamps t
                undo-tree-visualizer-diff t)
  :bind
  ("C-+" . undo-tree-redo)
  ("C-_" . undo-tree-undo))

(use-package immortal-scratch :init (immortal-scratch-mode t))
(use-package persistent-scratch :init (persistent-scratch-setup-default))

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
  :bind ("<C-tab>" . (function company-complete))
  :config (setq-default company-idle-delay nil))

(use-package company-quickhelp :init (company-quickhelp-mode t))

(use-package magit
  :bind ("C-c g s" . magit-status))

(use-package magit-todos :init (magit-todos-mode))

(use-package diff-hl :init (global-diff-hl-mode))

(use-package hl-todo :config (global-hl-todo-mode))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc verilog-verilator)))

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
  :config (setq-default eyebrowse-wrap-around t)
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
  :init (setq-default writeroom-width 150)
  :bind ("C-c w r" . writeroom-mode)
  :hook (writeroom-mode . toggle-line-numbers))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Org Mode          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :config
  (setq-default org-src-fontify-natively t
                org-directory "~/Documents/Nextcloud/Notes"
                org-catch-invisible-edits 'show-and-error
                org-yank-adjusted-subtrees t
                org-hide-emphasis-markers t
                org-src-tab-acts-natively t
                org-edit-src-content-indentation 0
                org-fontify-quote-and-verse-blocks t
                org-cycle-separator-lines 0
                org-src-preserve-indentation nil
                org-imenu-depth 4
                org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "|" "DONE")
                                    (sequence "PAUSED" "SCHEDULED" "|"  "CANCELLED")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     ;; (http . t)
     (shell . t)
     (emacs-lisp . t)))
  :hook
  (org-mode . turn-on-flyspell)
  (org-mode . auto-fill-mode)
  :bind (:map org-mode-map ("C-c C-." . org-time-stamp-inactive)))

(use-package org-cliplink
  :bind
  (:map org-mode-map
        ("C-c i l" . org-cliplink)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          C++          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ccls vs cquery: Overall ccls seems better.
;; ccls vs rtags : It looks like rtags has couple more features, but I had some problems setting it up before

(use-package cc-mode
  :ensure nil
  :mode ("\\.h\\'" . c++-mode)
  :config
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
                ;; flycheck-disabled-checkers '(c/c++-clang))
    (flycheck-add-mode 'c/c++-cppcheck 'c/c++-gcc)
    (flycheck-add-mode 'c/c++-cppcheck 'c/c++-clang)))

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode)

(use-package cmake-font-lock :hook (cmake-mode . cmake-font-lock-activate))

(use-package lsp-mode
  :hook (scala-mode . lsp)
  :init
  (setq-default lsp-auto-execute-action nil
                lsp-before-save-edits nil
                lsp-auto-guess-root t
                lsp-prefer-flymake :none
                lsp-enable-indentation nil
                lsp-enable-snippet nil
                lsp-enable-on-type-formatting nil))

(use-package lsp-origami
  :init
  (add-hook 'origami-mode-hook #'lsp-origami-mode))

(use-package company-lsp
  :after company
  :config (push 'company-lsp company-backends))

(use-package xref
  :config (setq-default xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package company-c-headers
  :after company
  :init (add-to-list 'company-backends 'company-c-headers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Perl          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cperl-mode
  :ensure nil
  :init (defalias 'perl-mode 'cperl-mode)
  :config
  (setq-default cperl-indent-level 4
                cperl-close-paren-offset -4
                cperl-continued-statement-offset 4
                cperl-indent-parens-as-block t
                cperl-tab-always-indent nil)
  (with-eval-after-load 'flycheck (flycheck-add-mode 'perl-perlcritic 'perl)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;          Scala          ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (setq-default sbt:program-options '("-Dsbt.supershell=false" "-mem" "16384"))
  ;; WORKAROUND: allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

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
  :config
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t))

(use-package shell
  :bind (:map shell-mode-map ("<tab>" . completion-at-point)))

(add-to-list 'auto-mode-alist '("\\.v\\'" . fundamental-mode))
