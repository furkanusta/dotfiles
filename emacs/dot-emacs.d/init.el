;; Initialization
(setq gc-cons-threshold 64000000)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(require 'quelpa)

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(defvar my-quelpa-dir (concat user-emacs-directory "quelpa"))
(defvar my-quelpa-build-dir (concat my-quelpa-dir "build"))

(setq-default use-package-always-defer t)
;; (setq-default use-package-always-ensure t)

(require 'server)
(unless (server-running-p) (server-start))

(setq custom-file (concat user-emacs-directory "elisp/custom.el"))
(load custom-file :noerror)
;; Init Done

;; ;; Debug
;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

(defun disable-line-numbers ()
  (display-line-numbers-mode -1))

(defvar use-other-window-alist
  '((display-buffer-use-some-window display-buffer-pop-up-window)
    (inhibit-same-window . t)))

(defun alist-switch-or-pop (mode buf  &optional alist)
    (if (derived-mode-p mode)
        (progn (message "HERE") (switch-to-buffer buf))
      (progn
        (when (= (length (window-list)) 1)
          (split-window-horizontally))
        (other-window 1)
        (message "HERE:: %s" (buffer-name buf))
        (switch-to-buffer buf nil))))
  
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
  :hook (before-save-hook . delete-trailing-whitespace)
  :init
  (fset 'yes-or-no-p 'y-or-n-p)
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
                font-use-system-font t
                initial-scratch-message ""
                initial-major-mode 'org-mode)
  :custom
  (column-number-mode 1)
  (display-time-default-load-average nil)
  (display-time-load-average-threshold 100.0)
  (display-time-24hr-format t)
  (display-time-mode 1)
  :bind
  ("C-c ." . pop-global-mark))

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

(use-package display-line-numbers
  :custom (global-display-line-numbers-mode 1))

(use-package hl-line
  :custom (global-hl-line-mode t))

;;; file opening procedures

(use-package dired :ensure nil
  :commands dired-get-file-for-visit
  :init (defun dired-open-xdg ()
          "Try to run `xdg-open' to open the file under point."
          (interactive)
          (if (executable-find "xdg-open")
              (let ((file (ignore-errors (dired-get-file-for-visit)))
                    (process-connection-type nil))
                (start-process "" nil "xdg-open" (file-truename file)))
            nil))
  :custom
  (dired-listing-switches "-vaBhl  --group-directories-first --color=always")
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
  :init
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude "/tmp")
  (add-to-list 'recentf-exclude no-littering-etc-directory))

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

(use-package dashboard
  :commands dashboard-insert-section dashboard-insert-heading dashboard-subseq
  :init
  (defun dashboard-insert-scratch (list-size)
    (dashboard-insert-section
     "Scratch:"
     '("*scratch*")
     list-size
     "s"
     `(lambda (&rest ignore) (switch-to-buffer "*scratch*"))))
  (dashboard-setup-startup-hook)
  :config (add-to-list 'dashboard-item-generators  '(scratch . dashboard-insert-scratch))
  :custom
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-items '((scratch . 1)
                     (recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)))
  (dashboard-banner-logo-title "Emacs"))

(use-package isearch :ensure nil
  :bind (("C-c s" . isearch-forward)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Helm          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm packages for other modules will be near their corresponding modules, not in here

(use-package helm
  :commands (helm-get-selection helm-next-line helm-previous-line helm-preselect helm-ff-move-to-first-real-candidate helm-skip-dots)
  :config (require 'helm-config)
  :init
  (defun helm-skip-dots (old-func &rest args)
    "Skip . and .. initially in helm-find-files.  First call OLD-FUNC with ARGS."
    (apply old-func args)
    (let ((sel (helm-get-selection)))
      (if (and (stringp sel) (string-match "/\\.$" sel))
          (helm-next-line 2)))
    (let ((sel (helm-get-selection))) ; if we reached .. move back
      (if (and (stringp sel) (string-match "/\\.\\.$" sel))
          (helm-previous-line 1))))
  (advice-add #'helm-preselect :around #'helm-skip-dots)
  (advice-add #'helm-ff-move-to-first-real-candidate :around #'helm-skip-dots)
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
  :custom
  (helm-ff-search-library-in-sexp t)
  (helm-ff-file-name-history-use-recentf t)
  (helm-ff-allow-non-existing-file-at-point nil)
  (helm-ff-auto-update-initial-value t)
  (helm-ff-guess-ffap-filenames t)
  (helm-ff-guess-ffap-urls nil)
  (helm-semantic-fuzzy-match t)
  (helm-M-x-fuzzy-match t)
  (helm-imenu-fuzzy-match t)
  (helm-substitute-in-filename-stay-on-remote t)
  (helm-boring-buffer-regexp-list (list (rx "*magit-") (rx "*helm") (rx "*flycheck")))
  (helm-split-window-inside-p t)
  (helm-move-to-line-cycle-in-source t)
  (helm-scroll-amount 8)
  (helm-autoresize-mode 1)
  (helm-mode 1))

(use-package helm-files
  :ensure nil
  :bind ("C-x C-f" . helm-find-files)
  :custom (helm-ff-skip-boring-files t))

(use-package helm-bibtex
  :defines my-bibliography
  :custom
  (bibtex-completion-bibliography my-bibliography)
  (bibtex-completion-library-path (concat my-data-directory "/Papers/"))
  (bibtex-completion-find-additional-pdfs t)
  (bibtex-completion-notes-path (concat my-notes-directory "/Papers.org")))

(use-package tramp
  :init (defun tramp-done ()
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
  :bind ("C-c C-s" .  helm-rg))

(use-package helm-bookmark
  :ensure nil
  :bind ("C-c h b" . helm-bookmarks))

(use-package ace-jump-helm-line
  :after helm
  :bind (:map helm-map ("C-'" . ace-jump-helm-line)))

(use-package deadgrep
  :bind ("C-c h s" . deadgrep))

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

(use-package all-the-icons
  :commands all-the-icons-octicon)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :custom (all-the-icons-ibuffer-mode 1))

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
  :defines (elfeed-show-mode-map elfeed-search-mode-map elfeed-show-entry)
  :commands (elfeed-search-selected elfeed-get-show-or-search-entry)
  :init
  (defun elfeed-get-show-or-search-entry ()
    (let* ((search-entries (elfeed-search-selected))
           (search-entry (when search-entries (car search-entries)))
           (elfeed-entry (or elfeed-show-entry search-entry)))
      elfeed-entry))
  (defun elfeed-open-eww ()
    (interactive)
    (let* ((entry (elfeed-get-show-or-search-entry))
           (url (elfeed-entry-link entry)))
      (eww url)))
  (defun +rss/delete-pane ()
  "Delete the *elfeed-entry* split pane."
  (interactive)
  (let* ((buf (get-buffer "*elfeed-entry*"))
         (window (get-buffer-window buf)))
    (delete-window window)
    (when (buffer-live-p buf)
      (kill-buffer buf))))
  (defun elfeed-youtube-dl ()
    "Youtube-DL link"
    (interactive)
    (let* ((entry (elfeed-get-show-or-search-entry))
           (url (when entry (elfeed-entry-link entry)))
           (default-directory (expand-file-name "~/Downloads")))
      (when url (async-shell-command (format "youtube-dl %s" url)))))
  (defun elfeed-mpv ()
    (interactive)
    (let* ((entry (elfeed-get-show-or-search-entry))
           (url (when entry (elfeed-entry-link entry))))
      (when url
        ;; (async-shell-command (format "mpv %s" url) nil nil))))
        (start-process "elfeed-mpv" nil "mpv" url))))
  (require 'reddigg)
  (defun elfeed-open-reddit ()
    (interactive)
    (let* ((entry (elfeed-get-show-or-search-entry))
           (url (elfeed-entry-link entry))
           (existing-buffer (get-buffer "*reddigg-comments*")))
      (progn
        (when existing-buffer
          (kill-buffer existing-buffer))
        (elfeed-untag entry 'unread)
        (next-line 1)
        (promise-finally (reddigg-view-comments url)
                         (lambda () (message "%s" (with-current-buffer (get-buffer "*reddigg-comments*")
                                                    (read-only-mode +1))))))))
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
                  ("https://drewdevault.com/feed.xml" other)
                  ("https://jacobian.org/index.xml" other)
                  ("https://old.reddit.com/r/cpp/top.rss?t=week" cpp)
                  ("https://old.reddit.com/r/emacs/top.rss?t=week" emacs)
                  ("https://old.reddit.com/r/python/top.rss?t=week" python)
                  ("https://old.reddit.com/r/ruby/top.rss?t=month" ruby)
                  ("https://old.reddit.com/r/perl/top.rss?t=month" perl)
                  ("https://old.reddit.com/r/java/top.rss?t=month" java)
                  ("https://old.reddit.com/r/linux/top.rss?t=week" linux)
                  ("https://old.reddit.com/r/programming/top.rss?t=week" prog)
                  ("https://old.reddit.com/r/askhistorians/top.rss?t=week" hist)
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
  (elfeed-search-title-max-width 100)
  :bind
  (:map elfeed-show-mode-map
        ("q" . +rss/delete-pane)
        ("P" . pocket-reader-elfeed-entry-add-link)
        ("d" . elfeed-youtube-dl)
        ("R" . elfeed-open-reddit)
        ("e" . elfeed-open-eww)
        ("m" . elfeed-mpv))
  (:map elfeed-search-mode-map
        ("d" . elfeed-youtube-dl)
        ("P" . pocket-reader-elfeed-search-add-link)
        ("R" . elfeed-open-reddit)
        ("e" . elfeed-open-eww)
        ("m" . elfeed-mpv)))

(use-package feed-discovery)

(use-package pocket-reader
  :commands (pocket-reader-elfeed-search-add-link pocket-reader-elfeed-entry-add-link))

(use-package vlf
  :after dired
  :hook (vlf-view-mode . disable-line-numbers)
  :defines vlf-forbidden-modes-list
  :config
  (require 'vlf-setup)
  (add-to-list 'vlf-forbidden-modes-list 'pdf-view-mode)
  (add-to-list 'vlf-forbidden-modes-list 'nov-mode))

;; requires pdf-tools-install
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
  :hook (after-init . global-company-mode)
  :custom
  (company-backends '(company-cmake company-capf company-clang))
  (company-idle-delay nil))

(use-package company-files :ensure company
  :after company
  :init (push 'company-files company-backends))

(use-package helm-company
  :after helm company
  :bind ("C-<tab>" . helm-company))

(use-package company-statistics
  :after company
  :hook (after-init . company-statistics-mode)
  :custom (company-statistics-file (concat no-littering-var-directory "company-statistics-cache.el")))

(use-package company-quickhelp
  :custom (company-quickhelp-mode t))

(use-package company-lsp
  :after company lsp
  :init (push 'company-lsp company-backends)
  :custom
  (company-lsp-enable-recompletion t)
  (company-lsp-enable-snippet t))

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
  :custom (global-diff-hl-mode 1))

(use-package hl-todo
  :custom (global-hl-todo-mode 1))

(use-package flycheck
  :commands flycheck-add-mode
  :custom
  (global-flycheck-mode 1)
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc verilog-verilator))
  (flycheck-clang-language-standard "c++20")
  (flycheck-gcc-language-standard "c++20")
  (flycheck-cppcheck-standards "c++20")
  :config
  (flycheck-add-mode 'perl-perlcritic 'perl)
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

(use-package eyebrowse
  :custom
  (eyebrowse-wrap-around t)
  (eyebrowse-mode 1)
  :bind
  (:map eyebrowse-mode-map
        ("C-c C-w <left>" . eyebrowse-prev-window-config)
        ("C-c C-w l" . eyebrowse-switch-to-window-config)
        ("C-c C-w <right>" . eyebrowse-next-window-config)))

(use-package hungry-delete
  :load-path "elisp/"
  :custom
  (global-hungry-delete-mode 1)
  (hungry-delete-join-reluctantly t)
  :bind
  ([remap delete-char] . hungry-delete-forward)
  ([remap delete-forward-char] . hungry-delete-forward)
  ([remap delete-backward-char] . hungry-delete-backward))

(use-package writeroom-mode
  :hook (writeroom-mode . toggle-line-numbers)
  :init (defun toggle-line-numbers () (display-line-numbers-mode (or (not display-line-numbers-mode) 0)))
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
  :hook (org-mode . org-appear-mode))

(use-package org-pretty-table
  :quelpa (org-pretty-table :fetcher github :repo "Fuco1/org-pretty-table")
  :hook (org-mode . org-pretty-table-mode))

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

(use-package ebib
  :custom
  (ebib-preload-bib-files (list my-bibliography))
  (ebib-bibtex-dialect 'biblatex)
  (ebib-default-directory 'first-bib-dir)
  (ebib-index-window-size 20)
  (ebib-bib-search-dirs (list my-papers-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          C++          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode :ensure nil
  :init
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Perl          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cperl-mode :ensure nil
  :config (defalias 'perl-mode 'cperl-mode)
  :custom
  (cperl-indent-level 4)
  (cperl-close-paren-offset -4)
  (cperl-continued-statement-offset 4)
  (cperl-indent-parens-as-block t)
  (cperl-tab-always-indent nil))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;          Scala          ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :mode ("\\.sc\\'" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :custom (sbt:program-options '("-Dsbt.supershell=false"))
  :init (substitute-key-definition
         'minibuffer-complete-word
         'self-insert-command
         minibuffer-local-completion-map))

(use-package lsp-metals
  :hook  (scala-mode . lsp)
  :custom (lsp-metals-treeview-show-when-views-received nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Shell          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ansi-color
  :commands ansi-color-apply-on-region
  :hook (compilation-filter . colorize-compilation-buffer)
  :init (defun colorize-compilation-buffer ()
          (read-only-mode)
          (ansi-color-apply-on-region (point-min) (point-max))
          (read-only-mode -1))
  :config (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  :custom (ansi-color-for-comint-mode 1))

(use-package shell
  :after window
  :config (add-to-list 'display-buffer-alist
                       (cons "\\*shell\\*" use-other-window-alist))
  :bind ("C-<f8>" . shell)
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

(use-package treemacs-persp
  :commands treemacs-set-scope-type
  :config (treemacs-set-scope-type 'Perspectives))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" "\\.docker"))
(use-package docker-compose-mode
  :mode ("docker-compose\\.yml\\'" "-compose.yml\\'"))

;; (use-package docker)

(use-package flycheck-clang-analyzer
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(use-package goto-chg
  :bind
  ("C-." . goto-last-change)
  ("C->" . goto-last-change-reverse))

(use-package projectile
  :commands projectile-project-name projectile-project-root
  :init (defun my-open-readme ()
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

(use-package helm-projectile :after helm projectile
  :init (helm-projectile-on))

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
  :commands (org-tab-or-next-heading shrface-basic shrface-trial shrface-outline-cycle org-at-heading-p)
  :config
  (shrface-basic)
  (shrface-trial)
  :init
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
  :mode ("\\.epub\\'" . nov-mode)
  :custom (nov-text-width 100))

(use-package verilog-mode
  :mode
  ("\\.v\\'" . verilog-mode)
  ("\\.sv\\'" . verilog-mode)
  :custom
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
  :init
  (defun bm-save-all ()
    (progn (bm-buffer-save-all)
           (bm-repository-save)))
  (setq bm-restore-repository-on-load t)
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
  :bind ("C-x t n" . neotree-toggle))

(use-package vterm
  :commands (vterm-next-prompt vterm-prev-prompt)
  :config (add-to-list 'display-buffer-alist (cons "\\*vterm" use-other-window-alist))
  :init
  (defun vterm-next-prompt () (interactive) (re-search-forward "msi.*\\$ " nil 'move))
  (defun vterm-prev-prompt () (interactive)
         (move-beginning-of-line nil)
         (re-search-backward "msi.*\\$ " nil 'move)
         (re-search-forward "\\$ " nil 'move))
  :bind
  ("<f8>" . vterm)
  (:map vterm-copy-mode-map
        ("C-<" . vterm-prev-prompt)
        ("C-," . vterm-next-prompt)))

(use-package binder)

(use-package tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :custom (global-tree-sitter-mode 1))

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

(use-package smartparens
  :config (require 'smartparens-config))

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
  :init (auctex-latexmk-setup)
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
  :custom (shx-global-mode 1))

(use-package compile
  :custom (compilation-scroll-output t)
  :bind ("C-c C-r" . recompile))

(use-package isend-mode)

(use-package alert
  :custom (alert-default-style 'libnotify))

;; Does not work well with dark mode (Text unreadable)
;; (use-package svg-tag-mode
;;   :quelpa (svg-tag-mode :repo "rougier/svg-tag-mode"
;;                         :fetcher github
;;                         :files ("svg-tag-mode.el")))

(use-package auth-source-pass
  :defines auth-source
  :init (setq auth-source '(password-store)))

(use-package pass)

(use-package helm-pass)

;; (use-package togetherly)

(use-package crdt
  :commands (crdt-connect crdt-share-buffer)
  :quelpa (crdt :fetcher git :url "https://code.librehq.com/qhong/crdt.el"))

(use-package org-marginalia
  :quelpa (org-marginalia :fetcher github :repo "nobiot/org-marginalia"))

(use-package emacs-everywhere
  :init (defun disable-modes ()
          (beacon-mode -1)
          (toggle-truncate-lines -1))
  :hook (emacs-everywhere-mode . disable-beacon))
