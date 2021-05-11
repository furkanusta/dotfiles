;; Initialization
(setq gc-cons-threshold 64000000)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(require 'server)
(unless (server-running-p) (server-start))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq-default use-package-always-defer t)
;; (setq-default use-package-always-ensure t)

(use-package quelpa
  :custom (quelpa-update-melpa-p nil))

(use-package quelpa-use-package :demand t)

(setq custom-file (concat user-emacs-directory "elisp/custom.el"))
;; Init Done

;; ;; Debug
;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

(defun disable-line-numbers ()
  (display-line-numbers-mode -1))

(defvar use-other-window-alist
  '((display-buffer-use-some-window display-buffer-pop-up-window)
    (inhibit-same-window . t)))

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

(toggle-frame-maximized)

(use-package emacs :ensure nil
  :hook (before-save . delete-trailing-whitespace)
  :preface
  (defvar my-dark-theme 'darkokai)
  (defvar my-light-theme 'leuven)
  (defvar my-active-theme 'darkokai)
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
  :init
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-unset-key (kbd "C-x c"))
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
                scroll-step 1
                scroll-conservatively 10000
                auto-window-vscroll nil
                comint-prompt-read-only t
                vc-follow-symlinks t
                scroll-preserve-screen-position t
                frame-resize-pixelwise t
                undo-limit 1280000
                large-file-warning-threshold (* 1024 1024 1024) ;; 1GB
                font-use-system-font t)
  :custom
  (column-number-mode 1)
  (display-time-default-load-average nil)
  (display-time-load-average-threshold 100.0)
  (display-time-24hr-format t)
  (display-time-mode 1)
  :bind
  ("C-c ." . pop-global-mark)
  ("M-u" . upcase-dwim)
  ("M-l" . downcase-dwim)
  ("M-c" . capitalize-dwim))

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

;; ;; These are built-in packages and having ensure results in lots of warnings
;; (use-package desktop :ensure nil
;;   :custom (desktop-save-mode 1)
;;   :init (add-to-list 'desktop-modes-not-to-save 'dired-mode))

(use-package paren
  :custom (show-paren-mode 1))

;; (use-package display-line-numbers
;;   :custom (global-display-line-numbers-mode 1))

(use-package hl-line
  :custom (global-hl-line-mode t))

;;; file opening procedures

(use-package dired :ensure nil
  :commands dired-get-file-for-visit
  :preface (defun dired-open-xdg ()
          "Try to run `xdg-open' to open the file under point."
          (interactive)
          (if (executable-find "xdg-open")
              (let ((file (ignore-errors (dired-get-file-for-visit)))
                    (process-connection-type nil))
                (start-process "" nil "xdg-open" (file-truename file)))
            nil))
  :custom
  (dired-use-ls-dired nil)
  (dired-listing-switches "-aBhl  --group-directories-first --color=never")
  (dired-auto-revert-buffer t)
  (dired-create-destination-dirs 'ask)
  (dired-dwim-target t)
  :bind (:map dired-mode-map ("E" . dired-open-xdg)))

(use-package delsel :ensure nil
  :custom (delete-selection-mode 1))

(use-package flyspell)

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-helm
  :after flyspell-correct)

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

(use-package isearch :ensure nil
  :bind ("C-c C-s" . isearch-forward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Helm          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :bind
  ((:map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("<left>" . helm-previous-source)
         ("<right>" . helm-next-source)))
  :custom
  (helm-split-window-inside-p t)
  (helm-move-to-line-cycle-in-source t)
  (helm-scroll-amount 8)
  (helm-autoresize-mode 1)
  (helm-mode +1))

(use-package helm-command :ensure helm
  :bind ("M-x" . helm-M-x))

(use-package helm-imenu :ensure helm
  :bind ("<f6>" . helm-imenu))

(use-package helm-ring :ensure helm
  :bind ("M-y" . helm-show-kill-ring))

(use-package helm-buffers :ensure helm
  :custom (helm-boring-buffer-regexp-list (list (rx "*magit-") (rx "*helm") (rx "*flycheck")))
  :bind ("C-x b" . helm-mini))

(use-package helm-files :ensure helm
  :commands (helm-get-selection helm-next-line helm-previous-line helm-ff-move-to-first-real-candidate)
  :preface (defun helm-skip-dots (old-func &rest args)
             (apply old-func args)
             (let ((sel (helm-get-selection)))
               (if (and (stringp sel) (string-match "/\\.$" sel))
                   (helm-next-line 2)))
             (let ((sel (helm-get-selection))) ; if we reached .. move back
               (if (and (stringp sel) (string-match "/\\.\\.$" sel))
                   (helm-previous-line 1))))
  :config (advice-add #'helm-ff-move-to-first-real-candidate :around #'helm-skip-dots)
  :bind ("C-x C-f" . helm-find-files)
  :custom
  (helm-substitute-in-filename-stay-on-remote t)
  (helm-ff-skip-boring-files t)
  (helm-ff-search-library-in-sexp t)
  (helm-ff-file-name-history-use-recentf t)
  (helm-ff-allow-non-existing-file-at-point nil)
  (helm-ff-auto-update-initial-value t)
  (helm-ff-guess-ffap-filenames t)
  (helm-ff-guess-ffap-urls nil))

(use-package helm-bibtex
  :defines my-bibliography
  :custom
  (bibtex-completion-bibliography my-bibliography)
  (bibtex-completion-library-path (concat my-data-directory "/Papers/"))
  (bibtex-completion-find-additional-pdfs t)
  (bibtex-completion-notes-path (concat my-notes-directory "/Papers.org")))

(use-package tramp
  :commands (tramp-cleanup-all-connections tramp-cleanup-all-buffers)
  :config (defun tramp-done ()
            (interactive)
            (tramp-cleanup-all-connections)
            (tramp-cleanup-all-buffers))
  :custom
  (tramp-backup-directory-alist backup-directory-alist))

(use-package helm-tramp)

;; (use-package helm-fd
;;   :bind ("C-c h f" . helm-fd))

(use-package helm-swoop
  :bind
  ("C-s" . helm-swoop)
  ("C-c h h" . helm-swoop-back-to-last-point)
  :custom
  (helm-swoop-split-with-multiple-windows nil)
  (helm-swoop-move-to-line-cycle t)
  (helm-swoop-use-fuzzy-match nil)
  (helm-swoop-speed-or-color t))

(use-package helm-rg
  :bind ("C-c h s" .  helm-rg))

(use-package helm-bookmark
  :ensure nil
  :bind ("C-c h b" . helm-bookmarks))

(use-package ace-jump-helm-line
  :after helm
  :bind (:map helm-map ("C-'" . ace-jump-helm-line)))

(use-package deadgrep
  :bind ("C-c h S" . deadgrep))

(use-package helm-flycheck
  :after flycheck
  :bind (:map flycheck-mode-map ("C-c h f" . helm-flycheck)))

(use-package helm-lsp)

(use-package xref)

(use-package helm-xref
  :custom (xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package avy
  :bind
  ("M-g c" . avy-goto-char-2)
  ("C-c C-j" . avy-resume)
  ("M-g g" . avy-goto-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Visual          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons)

(use-package doom-modeline
  :custom
  (doom-modeline-mode 1)
  (doom-modeline-buffer-encoding nil))

(use-package diminish)

(use-package darkokai-theme
  :init (load-theme 'darkokai t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Tools & Utils          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  :preface
  (defun +rss/delete-pane ()
    "Delete the *elfeed-entry* split pane."
    (interactive)
    (let* ((buf (get-buffer "*elfeed-entry*"))
           (window (get-buffer-window buf)))
      (delete-window window)
      (when (buffer-live-p buf) (kill-buffer buf))))
  :custom
  (elfeed-feeds
   '(("http://research.swtch.com/feeds/posts/default" other)
     ("http://bitbashing.io/feed.xml" other)
     ("http://preshing.com/feed" other)
     ("http://danluu.com/atom.xml" other)
     ("http://tenderlovemaking.com/atom.xml" other)
     ("http://feeds.feedburner.com/codinghorror/" other)
     ("http://www.snarky.ca/feed" other)
     ("http://blog.regehr.org/feed" cpp)
     ("https://blog.acolyer.org/feed/" other)
     ("https://randomascii.wordpress.com/feed/" other)
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
     ("https://research.swtch.com/" other)
     ("http://aras-p.info/atom.xml" other)
     ("https://what-if.xkcd.com/feed.atom" xkcd)
     ("http://xkcd.com/rss.xml" xkcd)
     ("https://esoteric.codes/rss" other)
     ("http://irreal.org/blog/?feed=rss2" emacs)
     ("https://drewdevault.com/feed.xml" other)
     ("https://jacobian.org/index.xml" other)
     ("https://old.reddit.com/r/cpp/top.rss?t=week" cpp)
     ("https://old.reddit.com/r/emacs/top.rss?t=week" emacs)
     ("https://old.reddit.com/r/python/top.rss?t=week" python)
     ("https://old.reddit.com/r/fpga/top.rss?t=week" fpga)
     ("https://old.reddit.com/r/ruby/top.rss?t=month" ruby)
     ("https://old.reddit.com/r/java/top.rss?t=month" java)
     ("https://old.reddit.com/r/linux/top.rss?t=week" linux)
     ("https://old.reddit.com/r/programming/top.rss?t=week" prog)
     ("https://old.reddit.com/r/askhistorians/top.rss?t=month" hist)
     ("https://old.reddit.com/r/badhistory/top.rss?t=month" hist)
     ("https://dave.cheney.net/feed/atom" go)
     ("https://blog.theincredibleholk.org/atom.xml" prog)
     ("https://ferd.ca/feed.rss" prog)
     ("https://benjamincongdon.me/blog/feed.xml" blog)
     ("https://erikbern.com/index.xml" blog)
     ("https://www.benkuhn.net/index.xml" blog)
     ("https://rjlipton.wpcomstaging.com/feed/" blog)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-xTvXTm-lrLWYk308-Km3A" youtube)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsvn_Po0SmunchJYOWpOxMg" youtube)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCCpTaib_e5C6Q95qwazq8OA" youtube)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCO-_F5ZEUhy0oKrSa69DLMw" youtube)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eEGT06FrWFU6VBnPOR9lg" youtube)))
  (elfeed-show-entry-switch #'pop-to-buffer)
  (elfeed-show-entry-delete #'+rss/delete-pane)
  (elfeed-search-title-max-width 100))

(use-package elfeed-search :ensure elfeed
  :after elfeed
  :defines elfeed-show-entry
  :commands (pocket-reader-elfeed-search-add-link elfeed-search-selected elfeed-search-untag-all-unread)
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
      (when url (async-shell-command (format "youtube-dl %s" url)))
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
              ("P" . pocket-reader-elfeed-entry-add-link)
              ("d" . elfeed-youtube-dl)
              ("e" . elfeed-open-eww)
              ("m" . elfeed-mpv)))

(use-package elfeed-show :ensure elfeed
  :after elfeed
  :commands (pocket-reader-elfeed-search-add-link)
  :bind (:map elfeed-show-mode-map
              ("q" . +rss/delete-pane)
              ("P" . pocket-reader-elfeed-search-add-link)))

(use-package reddigg
  :defines elfeed-search-mode-map
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
                                  (lambda () (with-current-buffer (get-buffer "*reddigg-comments*")
                                               (local-set-key "q" #'+rss/delete-pane)
                                               (read-only-mode +1)))))))
  :bind (:map elfeed-search-mode-map ("R" . elfeed-open-reddit)))

(use-package feed-discovery)

(use-package pocket-reader)

(use-package vlf
  :hook (vlf-view-mode . disable-line-numbers))

(use-package vlf-setup :ensure vlf
  :config
  (add-to-list 'vlf-forbidden-modes-list 'pdf-view-mode)
  (add-to-list 'vlf-forbidden-modes-list 'nov-mode))

(use-package pdf-tools
  :quelpa (pdf-tools :fetcher github :repo "vedang/pdf-tools" :files ("lisp/*" "server/*"))
  :hook ((pdf-view-mode . (lambda () (cua-mode 0)))
         (pdf-view-mode . disable-line-numbers)
         (pdf-view-mode . pdf-sync-minor-mode)
         (pdf-view-mode . pdf-links-minor-mode)
         (pdf-view-mode . pdf-history-minor-mode)
         (pdf-view-mode . pdf-view-midnight-minor-mode))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations nil)
  (pdf-view-resize-factor 1.1)
  :bind (:map pdf-view-mode-map
              ("M-w" . pdf-view-kill-ring-save)
              ("S-SPC" . pdf-view-scroll-down-or-previous-page)))

(use-package pdf-view-restore
  :after pdf-tools
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :custom (pdf-view-restore-filename (concat no-littering-var-directory ".pdf-view-restore")))

(use-package undo-tree
  :diminish undo-tree-mode
  :custom
  (global-undo-tree-mode 1)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  :bind
  ("C-+" . undo-tree-redo)
  ("C-_" . undo-tree-undo))

(use-package immortal-scratch
  :custom (immortal-scratch-mode t))

(use-package persistent-scratch
  :config (persistent-scratch-setup-default))

(use-package scratch
  :bind ("M-s M-s" . scratch))

(use-package yasnippet
  :custom (yas-global-mode 1)
  :bind ("M-i" . yas-expand)
  (:map yas-minor-mode-map ("<tab>" . nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Programming Tools          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generic
(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-backends '(company-cmake company-capf company-clang))
  (company-idle-delay nil))

(use-package company-files :ensure company
  :after company
  :config (push 'company-files company-backends))

(use-package helm-company
  :after helm company
  :bind ("C-<tab>" . helm-company))

(use-package company-statistics
  :after company
  :hook (company-mode . company-statistics-mode)
  :custom (company-statistics-file (concat no-littering-var-directory "company-statistics-cache.el")))

(use-package company-quickhelp
  :custom (company-quickhelp-mode t))

(use-package magit
  :bind ("C-c g s" . magit-status)
  :custom (magit-blame-echo-style 'headings))

(use-package magit-todos
  :config
  (let ((inhibit-message t))
    (magit-todos-mode 1))
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

(use-package github-review)

(use-package gitignore-mode)

(use-package copy-as-format
  :custom (copy-as-format-default "github"))

(use-package diff-hl
  :hook (find-file . diff-hl-mode))

(use-package hl-todo
  :custom (global-hl-todo-mode 1))

(use-package flycheck
  :commands flycheck-add-mode
  :custom
  (global-flycheck-mode nil)
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc verilog-verilator))
  (flycheck-clang-language-standard "c++17")
  (flycheck-gcc-language-standard "c++17")
  (flycheck-cppcheck-standards "c++17")
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (flycheck-add-mode 'c/c++-cppcheck 'c++mode))

(use-package flycheck-pos-tip
  :after flycheck
  :custom (flycheck-pos-tip-mode 1))

(use-package evil-nerd-commenter :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package visual-regexp-steroids
  :demand t
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
  :custom (drag-stuff-global-mode t)
  :bind (:map drag-stuff-mode-map
              ("<M-up>" . drag-stuff-up)
              ("<M-down>" . drag-stuff-down)))

(use-package frame-movement
  :load-path "elisp/"
  :bind
  ("C-x 5 n" . frame-movement/select-next-frame)
  ("C-x 5 p" . frame-movement/select-prev-frame))

(use-package hungry-delete
  :custom
  (global-hungry-delete-mode 1)
  (hungry-delete-join-reluctantly t)
  :bind
  ([remap delete-char] . hungry-delete-forward)
  ([remap delete-forward-char] . hungry-delete-forward)
  ([remap delete-backward-char] . hungry-delete-backward))

(use-package writeroom-mode
  :hook (writeroom-mode . toggle-line-numbers)
  :preface (defun toggle-line-numbers () (display-line-numbers-mode (or (not display-line-numbers-mode) 0)))
  :custom
  (writeroom-width 150)
  (writeroom-mode-line nil)
  :bind ("C-c w r" . writeroom-mode))

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
  :config
  (defvar org-capture-file (concat my-notes-directory "/Capture.org"))
  (setq org-default-notes-file org-capture-file)
  :custom
  (org-adapt-indentation t)
  (org-catch-invisible-edits 'show-and-error)
  (org-cycle-separator-lines 0)
  (org-edit-src-content-indentation 0)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-imenu-depth 4)
  (org-indent-indentation-per-level 1)
  (org-log-done t)
  (org-pretty-entities t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation nil)
  (org-src-tab-acts-natively t)
  (org-yank-adjusted-subtrees t)
  (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "|" "DONE")
                       (sequence "PAUSED" "SCHEDULED" "|"  "CANCELLED")))
  :hook
  (org-mode . turn-on-flyspell)
  (org-mode . auto-fill-mode)
  :bind (:map org-mode-map ("C-c C-." . org-time-stamp-inactive))
  :config (org-babel-do-load-languages
           'org-babel-load-languages
           '((python . t)
             ;; (http . t)
             (shell . t)
             (emacs-lisp . t))))

(use-package ob-async)

(use-package org-alert
  :commands org-alert-enable
  :config (org-alert-enable))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package org-cliplink
  :bind (:map org-mode-map ("C-c i l" . org-cliplink)))

(use-package org-capture :ensure nil
  :after org
  :custom
  (org-capture-templates '(("t" "TODO" entry (file+headline org-capture-file "Tasks")
  						    "* TODO %?\n	%a\n  %i\n")
  					       ("j" "Journal" entry (file+headline org-capture-file "Journal")
  						    "* %U\n	 %a\n	 %i")
  					       ("p" "Protocol" entry (file+headline org-capture-file "Inbox")
  	    				    "* %?\n	 [[%:link][%:description]]\n	%U\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n")
  	    			       ("L" "Protocol Link" entry (file+headline org-capture-file "Inbox")
  						    "* %?\n	 [[%:link][%:description]]\n	%U")))
  :bind ("C-c c" . org-capture))

(use-package org-protocol :ensure nil)

(use-package org-agenda :ensure nil
  :custom
  (org-agenda-files (list my-notes-directory))
  (org-agenda-include-diary t)
  (org-agenda-span 10)
  (org-agenda-start-day "-2d")
  :bind ("C-c a" . org-agenda))

(use-package org-refile :ensure nil
  :custom
  (org-refile-use-outline-path t)
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9)))
  (org-refile-allow-creating-parent-nodes 'confirm))

(use-package org-clock :ensure nil
  :custom
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-report-include-clocking-task t)
  (org-clock-out-when-done t))

(use-package org-tempo :ensure nil
  :after org)

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-pretty-table
  :quelpa (org-pretty-table :fetcher github :repo "Fuco1/org-pretty-table")
  :hook (org-mode . org-pretty-table-mode))

;; (use-package org-variable-pitch
;;   :quelpa (org-variable-pitch :fetcher github :repo "cadadr/elisp" :files ("org-variable-pitch.el"))
;;   :hook (org-mode . org-pretty-table-mode))

(use-package org-table-sticky-header
  :hook (org-mode . org-table-sticky-header-mode))

(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :custom (org-sticky-header-always-show-header nil))

;; (add-hook 'org-mode-hook #'valign-mode)

(use-package org-ref
  :custom
  (org-ref-bibliography-notes (concat my-notes-directory "/Papers.org"))
  (org-ref-default-bibliography (list my-bibliography))
  (org-ref-pdf-directory (concat my-data-directory "/Papers/"))
  (org-ref-show-broken-links t))

(use-package bibtex
  :custom (bibtex-align-at-equal-sign t))

(use-package biblio)

(use-package org-noter
  :custom
  (org-noter-notes-search-path (list my-notes-directory))
  (org-noter-default-notes-file-names (list "Papers.org"))
  (org-noter-auto-save-last-location t)
  (org-noter-insert-note-no-questions t))

(use-package org-books
  :custom
  (org-books-file (concat my-notes-directory "/Books.org"))
  (org-books-file-depth 0))

;; https://alhassy.github.io/org-special-block-extras/
(use-package org-special-block-extras)

(use-package org-marginalia
  :quelpa (org-marginalia :fetcher github :repo "nobiot/org-marginalia")
  :hook (org-mode . org-marginalia-mode)
  :commands (org-marginalia-next org-marginalia-prev)
  :preface
  (defun org-marginalia-make-annotation ()
    (interactive)
    (let ((mark-end (region-end)))
      (org-marginalia-mark (region-beginning) (region-end))
      (org-marginalia-save)
      (org-marginalia-open (1- mark-end))
      (goto-char (point-max))))
  (defun org-marginalia-browse-forward ()
    (interactive)
    (let ((buf (current-buffer)))
      (org-marginalia-next) (org-marginalia-open (point))
      (pop-to-buffer buf nil t)))
(defun org-marginalia-browse-backward ()
    (interactive)
    (let ((buf (current-buffer)))
      (org-marginalia-prev) (org-marginalia-open (point))
      (pop-to-buffer buf nil t)))
  :bind
  ("C-c i m" . org-marginalia-make-annotation)
  ("C-c m o" . org-marginalia-open)
  ("C-c m ]" . org-marginalia-browse-forward)
  ("C-c m [" . org-marginalia-browse-backward))

(use-package org-journal
  :bind ("C-c i j" . org-journal-new-entry)
  :custom (org-journal-dir (concat my-notes-directory "/Journal")))

(use-package org-super-links
  :quelpa (org-super-links :fetcher github :repo "toshism/org-super-links"))

(use-package binder
  :custom (binder-default-file-extension "org"))

(use-package literate-calc-mode)

(use-package ebib
  :custom
  (ebib-preload-bib-files (list my-bibliography))
  (ebib-bibtex-dialect 'biblatex)
  (ebib-default-directory 'first-bib-dir)
  (ebib-index-window-size 20)
  (ebib-bib-search-dirs (list my-papers-directory)))

(use-package annotate
  :bind ("C-c i a" . annotate-annotate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          C++          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode :ensure nil
  :config
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  :custom
  (c-default-style "stroustrup")
  (c-basic-offset 4)
  (c-indent-level 4)
  (access-label 0))

(use-package c++-mode :ensure nil
  :mode ("\\.h\\'" . c++-mode)
  :custom (c-noise-macro-names '("constexpr")))

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode)

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package meson-mode
  :hook (meson-mode . company-mode))

(use-package lsp-mode
  :custom
  (lsp-auto-execute-action nil)
  (lsp-before-save-edits nil)
  (lsp-keymap-prefix "C-c C-l")
  (lsp-enable-snippet t)
  (lsp-enable-xref t)
  (lsp-enable-imenu t)
  (lsp-prefer-flymake nil)
  (lsp-enable-indentation nil)
  (lsp-prefer-capf t)
  (lsp-enable-file-watchers nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-semantic-highlighting nil)
  (lsp-enable-on-type-formatting nil)
  (read-process-output-max (* 2 1024 1024))
  (lsp-enable-on-type-formatting nil))

;; (use-package lsp-ui
;;   :custom
;;   (lsp-ui-flycheck-enable t)
;;   (lsp-ui-imenu-enable t)
;;   (lsp-ui-sideline-enable t)
;;   (lsp-ui-doc-position 'top))

;; (use-package dap
;;   :custom
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;          Perl          ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package cperl-mode :ensure nil
;;   :config (defalias 'perl-mode 'cperl-mode)
;;   :custom
;;   (cperl-indent-level 4)
;;   (cperl-close-paren-offset -4)
;;   (cperl-continued-statement-offset 4)
;;   (cperl-indent-parens-as-block t)
;;   (cperl-tab-always-indent nil))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;          Scala          ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :mode ("\\.sc\\'" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :custom (sbt:program-options '("-Dsbt.supershell=false"))
  :config (substitute-key-definition
           'minibuffer-complete-word
           'self-insert-command
           minibuffer-local-completion-map))

(use-package lsp-metals
  :hook  (scala-mode . lsp-deferred)
  :custom (lsp-metals-treeview-show-when-views-received nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Shell          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ansi-color
  :commands ansi-color-apply-on-region
  :hook (compilation-filter . colorize-compilation-buffer)
  :preface (defun colorize-compilation-buffer ()
             (read-only-mode)
             (ansi-color-apply-on-region (point-min) (point-max))
             (read-only-mode -1))
  :config (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  :custom (ansi-color-for-comint-mode 1))

(use-package shell
  :after window
  :hook (shell-mode . company-mode)
  :config (add-to-list 'display-buffer-alist (cons "\\*shell\\*" use-other-window-alist))
  :bind
  ("C-<f8>" . shell)
  (:map shell-mode-map ("<tab>" . helm-company)))

(use-package treemacs
  :commands treemacs-resize-icons treemacs-is-file-git-ignored?
  :hook (treemacs-mode . disable-line-numbers)
  :config
  (treemacs-resize-icons 20)
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
  :bind
  (:map global-map
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-f" . treemacs-find-file))
  :custom
  (treemacs-position 'right)
  (treemacs-width 50))

(use-package treemacs-filewatch-mode :ensure nil
  :hook (treemacs-mode . treemacs-filewatch-mode))

(use-package treemacs-fringe-indicator-mode :ensure nil
  :hook (treemacs-mode . treemacs-fringe-indicator-mode))

(use-package treemacs-follow-mode :ensure nil
  :hook (treemacs-mode . treemacs-follow-mode))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

(use-package lsp-treemacs
  :hook (treemacs-mode . lsp-treemacs-sync-mode))

(use-package persp-mode
  :hook (after-init . persp-mode)
  :custom (persp-state-default-file (concat no-littering-var-directory ".persp")))

(use-package treemacs-persp
  :commands treemacs-set-scope-type
  :config (treemacs-set-scope-type 'Perspectives))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" "\\.docker"))
(use-package docker-compose-mode
  :mode ("docker-compose\\.yml\\'" "-compose.yml\\'"))

(use-package docker)
(use-package docker-tramp)

(use-package flycheck-clang-analyzer
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(use-package goto-chg
  :bind
  ("C-." . goto-last-change)
  ("C->" . goto-last-change-reverse))

(use-package projectile
  :commands projectile-project-name projectile-project-root
  :preface (defun my-open-readme ()
          (let* ((project-name (projectile-project-name))
                 (project-root (projectile-project-root))
                 (project-files (directory-files project-root nil nil t))
                 (readme-files (seq-filter (lambda (file) (string-prefix-p "readme" file t)) project-files)))
            (if readme-files
                (let ((readme-file (car readme-files)))
                  (find-file (expand-file-name readme-file project-root)))
              (find-file (expand-file-name "README.org" project-root)))))
  :custom
  (projectile-mode 1)
  (projectile-enable-caching t)
  (projectile-switch-project-action 'my-open-readme)
  (projectile-completion-system 'helm)
  (projectile-known-projects-file (concat no-littering-var-directory "projectile-bookmarks.eld"))
  (projectile-sort-order 'recentf)
  (projectile-inedxing-method 'hybrid)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map)))

(use-package persp-projectile)

(use-package helm-projectile :after helm projectile
  :config (helm-projectile-on))

(use-package link-hint
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

(use-package fountain-mode)

(use-package inherit-org
  :quelpa (inherit-org :repo "chenyanming/inherit-org" :fetcher github)
  :hook ((eww-mode nov-mode info-mode helpful-mode) . inherit-org-mode))

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
              ("TAB" . #'org-tab-or-next-heading)
              ("<backtab>" . shrface-outline-cycle-buffer)
              ("M-<down>" . org-next-visible-heading)
              ("M-<up>" . org-previous-visible-heading)))

(use-package eww)

(use-package nov
  :mode
  ("\\.epub\\'" . nov-mode)
  ("\\.EPUB\\'" . nov-mode)
  :custom (nov-text-width 100))

(use-package verilog-mode
  :mode
  ("\\.v\\'" . verilog-mode)
  ("\\.sv\\'" . verilog-mode)
  :custom
  ;; (verilog-auto-inst-sort t)
  (verilog-auto-declare-nettype "none")
  (verilog-case-fold nil)
  (verilog-auto-newline nil)
  (verilog-tab-always-indent nil)
  (verilog-auto-indent-on-newline nil)
  (verilog-case-indent 4)
  (verilog-cexp-indent 4)
  (verilog-indent-begin-after-if nil)
  (verilog-indent-level 4)
  (verilog-indent-level-behavioral 4)
  (verilog-indent-level-directive 4)
  (verilog-indent-level-declaration 4)
  (verilog-indent-level-module 4))

(use-package bm
  :commands bm-buffer-save-all bm-repository-save
  :after no-littering
  :preface (defun bm-save-all ()
             (progn (bm-buffer-save-all)
                    (bm-repository-save)))
  :init (setq bm-restore-repository-on-load t)
  :custom
  (bm-cycle-all-buffers t)
  (bm-repository-file (concat no-littering-var-directory "bm-repository"))
  (bm-buffer-persistence t)
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
  :custom
  (beacon-blink-when-point-moves-vertically 20)
  (beacon-blink-when-buffer-changes nil)
  (beacon-blink-when-window-changes nil)
  (beacon-mode 1))

(use-package neotree
  :after projectile
  :custom
  (neo-smart-open t)
  (neo-vc-integration nil)
  (neo-theme 'icons)
  :hook (neotree-mode . disable-line-numbers)
  :bind ("C-x t d" . neotree-toggle))

(use-package vterm
  :commands (vterm-next-prompt vterm-prev-prompt)
  :config (add-to-list 'display-buffer-alist (cons "\\*vterm" use-other-window-alist))
  :preface
  (defun vterm-next-prompt () (interactive) (re-search-forward "msi.*\\$ " nil 'move))
  (defun vterm-prev-prompt () (interactive)
         (move-beginning-of-line nil)
         (re-search-backward "msi.*\\$ " nil 'move)
         (re-search-forward "\\$ " nil 'move))
  :bind
  (:map vterm-copy-mode-map
        ("C-<" . vterm-prev-prompt)
        ("C-," . vterm-next-prompt)))

(use-package vterm-toggle
  :custom (vterm-toggle-cd-auto-create-buffer nil)
  :bind
  ("<f8>" . vterm-toggle)
  (:map vterm-mode-map
        ("C-<return>" . vterm-toggle-insert-cd)))

(use-package tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))
  ;; :custom (global-tree-sitter-mode 1))

(use-package graphviz-dot-mode
  :custom (graphviz-dot-indent-width 4))

(use-package company-graphviz-dot :ensure nil)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;         LISP        ::
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sly
  :config (sly-setup '(sly-mrepl)))

(use-package helm-sly
  :after sly helm-company
  :defines sly-mrepl-mode-map
  :hook (sly-mrepl . company-mode)
  :bind (:map sly-mrepl-mode-map ("<tab>" . helm-company)))

(use-package smartparens)
(use-package smartparens-config :ensure smartparens)

;;
(use-package calfw)

(use-package calfw-org :after calfw
  :custom (cfw:org-overwrite-default-keybinding t)
  :bind
  ("C-c C-f" . cfw:open-org-calendar)
  (:map cfw:calendar-mode-map ("<return>" . cfw:org-open-agenda-day)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Python       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package blacken
  :hook (python-mode . blacken-mode)
  :custom (blacken-line-length 100))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;        LaTeX        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tex :ensure auctex
  :commands TeX-revert-document-buffer
  :config (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :custom
  (TeX-master nil)
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
  :hook (LaTeX-mode . cdlatex-mode))

(use-package company-auctex
  :commands company-auctex-init
  :config (company-auctex-init))

(use-package auctex-latexmk
  :commands auctex-latexmk-setup
  :config (auctex-latexmk-setup)
  :custom (auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package popper
  :commands popper-select-popup-at-bottom
  :bind (("C-\\"   . popper-toggle-latest)
         ("M-\\"   . popper-cycle)
         ("C-M-\\"   . popper-toggle-type))
  :custom
  (popper-reference-buffers '("\\*Messages\\*" "Output\\*$" help-mode compilation-mode))
  (popper-display-function #'popper-select-popup-at-bottom)
  (popper-mode +1))

(use-package flx)

(use-package helm-flx
  :hook (helm-mode . helm-flx-mode)
  :custom
  (helm-flx-for-helm-find-files t)
  (helm-flx-for-helm-locate t))

(use-package company-flx
  :hook (company-mode . company-flx-mode))

(use-package helpful
  :commands (get-buffers-matching-mode helpful-first-buffer-p helpful-not-first-buffer-p)
  :preface (defun alist-switch-or-pop (mode buf  &optional alist)
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

(use-package transient-dwim
  :bind ("M-=" . transient-dwim-dispatch))

(use-package screenshot
  :quelpa (screenshot :fetcher github :repo "tecosaur/screenshot")
  :custom
  (screenshot-line-numbers t)
  (screenshot-min-width 100))

(use-package shx
  :hook (shell-mode . shx-mode))

(use-package compile
  :custom (compilation-scroll-output t)
  :bind ("C-c C-r" . recompile))

(use-package isend-mode)

(use-package alert
  :custom (alert-default-style 'libnotify))

(use-package helm-pass)

;; (use-package togetherly)

(use-package crdt
  :commands (crdt-connect crdt-share-buffer)
  :quelpa (crdt :fetcher git :url "https://code.librehq.com/qhong/crdt.el"))

(use-package emacs-everywhere
  :preface (defun disable-modes ()
             (setq hungry-delete-chars-to-skip " \t\r\f\v")
             (beacon-mode -1)
             (toggle-truncate-lines 1))
  :hook (emacs-everywhere-mode . disable-modes))

(use-package ein)

(use-package ob-ipython
  :hook (org-babel-after-execute . org-display-inline-images)
  :config
  (add-to-list 'org-latex-minted-langs '(ipython "python"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t))))

(use-package org-transclusion
  ;; :quelpa (org-transclusion :fetcher git :url "https://github.com/nobiot/org-transclusion")
  :load-path "elisp/"
  :hook (org-mode . org-transclusion-mode)
  :custom (org-transclusion-activate-persistent-message nil))

(use-package org-roam
      :custom
      (org-roam-directory (concat my-notes-directory "/Roam"))
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n t" . org-roam-buffer-toggle-display)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(use-package org-ql)

(use-package helm-org-ql)

;; (use-package org-z
;;   :quelpa (org-z :fetcher github :repo "landakram/org-z")
;;   :custom
;;   (org-z-mode 1)
;;   (org-z-directories (list my-notes-directory))
;;   (org-z-knowledge-dirs (list my-notes-directory))
;;   (org-z-completion-backend 'helm))

;; (use-package org-z-helm :requires org-z)

(use-package org-linker :quelpa (org-linker :fetcher github :repo "toshism/org-linker"))

(use-package org-super-links
  ;; :quelpa (org-super-links :fetcher github :repo "toshism/org-super-links")
  :bind (:map org-mode-map
              ("C-c s s" . org-super-links-link)
              ("C-c s d" . org-super-links-delete-link)
              ("C-c s l" . org-super-links-store-link)
              ("C-c s i" . org-super-links-insert-link)))

(use-package org-super-links-peek
  :quelpa (org-super-links-peek :fetcher github :repo "toshism/org-super-links-peek")
  :bind (:map org-mode-map ("C-c s p" . org-super-links-peek-link)))

;; (use-package hl-prog-extra
;;   :commands (hl-prog-extra-mode)
;;   :custom (global-hl-prog-extra-mode 1))

(use-package ibuffer-sidebar
  :commands (ibuffer-sidebar-toggle-sidebar)
  :bind ("C-x t s" . ibuffer-sidebar-toggle-sidebar))

(use-package ibuffer-vc
  :preface (defun ibuffer-vc-setup ()
             (ibuffer-vc-set-filter-groups-by-vc-root)
             (unless (eq ibuffer-sorting-mode 'alphabetic)
               (ibuffer-do-sort-by-alphabetic)))
  :hook
  (ibuffer-hook . ibuffer-vc-setup)
  (ibuffer-sidebar-mode-hook . ibuffer-vc-setup))

(use-package multicolumn)

(use-package side-notes
  :custom
  (side-notes-display-alist '((side . right)
                              (window-width . 50)))
  (side-notes-file "Notes.org")
  (side-notes-secondary-file "README.org")
  :bind ("C-x t n" . side-notes-toggle-notes))
