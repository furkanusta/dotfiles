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
(setq custom-file "~/.emacs.d/elisp/custom.el")
(load custom-file)

(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
;; Init Done


;; Debug
;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Defaults & Built-ins          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default user-full-name "Furkan Usta"
              user-mail-address "furkanusta17@gmail.com"
              save-interprogram-paste-before-kill t
              emacs-load-start-time (current-time)
              ad-redefinition-action 'accept
              ;; backup-inhibited t
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

;; ;; These are built-in packages and having ensure results in lots of warnings
;; (use-package desktop
;;   :ensure nil
;;   :init (desktop-save-mode 1)
;;   :config (add-to-list 'desktop-modes-not-to-save 'dired-mode))

(use-package dired
  :ensure nil
  :init (setq-default dired-dwim-target t))

(use-package delsel
  :ensure nil
  :init (delete-selection-mode 1))

(use-package flyspell)

(use-package hideshow
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind
  ("C-c <" . hs-toggle-hiding)
  ("C-c >" . hs-hide-all)
  ("C-c ." . hs-show-all))

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

(use-package windmove
  :bind (("C-c <left>" . windmove-left)
         ("C-c <right>" . windmove-right)
         ("C-c <up>" . windmove-up)
         ("C-c <down>" . windmove-down)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Helm          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm packages for other modules will be near their corresponding modules, not in here

(use-package helm
  :diminish helm-mode
  :commands helm-autoresize-mode
  :init
  (require 'helm-config)
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (global-unset-key (kbd "C-x c"))
  :bind
  (("C-x C-f" . helm-find-files)
   ("M-x" . helm-M-x)
   ("C-s" . helm-occur)
   ("C-x b" . helm-mini)
   ("C-z" .  helm-select-action)
   ("M-y" . helm-show-kill-ring)
   ("C-c s" . isearch-forward)
   ("<f6>" . helm-imenu)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("<left>" . helm-previous-source)
   ("<right>" . helm-next-source))
  :config
  (setq helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source t
        helm-scroll-amount 8)
  (setq-default helm-ff-search-library-in-sexp t
                helm-ff-file-name-history-use-recentf t
                helm-ff-allow-non-existing-file-at-point t
                helm-ff-auto-update-initial-value t
                helm-ff-guess-ffap-filenames t
                helm-ff-guess-ffap-urls nil
                helm-semantic-fuzzy-match t
                helm-M-x-fuzzy-match t
                helm-imenu-fuzzy-match t
                helm-substitute-in-filename-stay-on-remote t
                helm-boring-buffer-regexp-list (list (rx "*magit-") (rx "*helm") (rx "*flycheck"))))

;; (use-package helm-swoop
;;   :config
;;   (setq-default helm-swoop-move-to-line-cycle t
;;                 helm-swoop-use-line-number-face t
;;                 helm-swoop-split-direction 'split-window-vertically
;;                 helm-swoop-split-with-multiple-windows t
;;                 helm-swoop-move-to-line-cycle t)
;;   :bind
;;   (("C-s" . helm-swoop-without-pre-input)
;;    ("C-c C-SPC" . helm-swoop-back-to-last-point)
;;    ("C-c s" . isearch-forward)
;;    :map helm-swoop-map
;;    ("C-r" . helm-previous-line)
;;    ("C-s" . helm-next-line)))

(use-package helm-bibtex
  :config
  (setq-default bibtex-completion-bibliography "~/Documents/Nextcloud/org/Bibliography.bib"
                bibtex-completion-library-path "~/Documents/Nextcloud//Papers/"
                bibtex-completion-notes-path "~/Documents/Nextcloud/org/helm-bibtex-notes"))

(use-package helm-tramp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Visual          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons)

(use-package spaceline-config
  :ensure spaceline
  :commands spaceline-helm-mode
  :init (with-eval-after-load 'helm (spaceline-helm-mode t)))

(use-package spaceline-all-the-icons
  :commands spaceline-toggle-all-the-icons-time-off
  :after spaceline
  :init (spaceline-all-the-icons-theme)
  :config (spaceline-toggle-all-the-icons-time-off))

(use-package diminish)

(use-package monokai-theme :demand t)

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
                  ("https://www.reddit.com/r/python/top/.rss?t=month" python)
                  ("https://www.reddit.com/r/java/top/.rss?t=month" java)
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


;; ;; workaround for pdf-tools not reopening to last-viewed page of the pdf:
;; ;; https://github.com/politza/pdf-tools/issues/18#issuecomment-269515117
;; (use-package bookmark+
;;   :load-path "elisp/bookmark-plus/"
;;   :config
;;   (setq-default bookmarks-pdf "~/.emacs.d/bookmarks-pdf"))

;; (defun brds/pdf-set-last-viewed-bookmark ()
;;   (interactive)
;;   (when (eq major-mode 'pdf-view-mode)
;; 	(bmkp-switch-bookmark-file-create bookmarks-pdf t)
;; 	(bookmark-set (brds/pdf-generate-bookmark-name))
;; 	(bmkp-switch-bookmark-file-create bookmark-default-file t)))

;; (defun brds/pdf-jump-last-viewed-bookmark ()
;;   (bmkp-switch-bookmark-file-create bookmarks-pdf t)
;;   (bookmark-set "PDF-LAST-VIEWED: fake") ; this is new
;;   (when (brds/pdf-has-last-viewed-bookmark)
;; 	(bookmark-jump (brds/pdf-generate-bookmark-name)))
;;   (bmkp-switch-bookmark-file-create bookmark-default-file t))

;; (defun brds/pdf-has-last-viewed-bookmark ()
;;   (assoc (brds/pdf-generate-bookmark-name) bmkp-latest-bookmark-alist))

;; (defun brds/pdf-generate-bookmark-name ()
;;   (concat "PDF-LAST-VIEWED: " (buffer-file-name)))

;; (defun brds/pdf-set-all-last-viewed-bookmarks ()
;;   (dolist (buf (buffer-list))
;; 	(with-current-buffer buf
;; 	  (brds/pdf-set-last-viewed-bookmark))))

;; requires pdf-tools-install
(use-package pdf-tools
  :hook ((pdf-view-mode . (lambda () (cua-mode 0)))
         (pdf-view-mode . disable-line-numbers))
         ;; (pdf-view-mode . brds/pdf-jump-last-viewed-bookmark)
         ;; (kill-buffer . brds/pdf-set-last-viewed-bookmark))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  ;; (unless noninteractive (add-hook 'kill-emacs-hook #'brds/pdf-set-all-last-viewed-bookmarks))`
  (setq-default pdf-view-display-size 'fit-page
                pdf-annot-activate-created-annotations t
                pdf-view-resize-factor 1.1)
  :bind (:map pdf-view-mode-map ("t" . pdf-view-page-number)))

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

;; ;; Doesn't work with company-quickhelp but can provide fuzzy matching where company-flx cannot
;; (use-package helm-company
;;   :after company
;;   :bind ("<C-tab>" . (function helm-company)))

(use-package company-quickhelp :init (company-quickhelp-mode t))

(use-package helm-ag
  :init (custom-set-variables '(helm-follow-mode-persistent t))
  :bind
  ("C-c h p" . helm-do-ag-project-root)
  ("C-c h s" .  helm-do-ag)
  ("C-c h S" . helm-ag))

;; Has quite ugly arguments line at the beginning and does not support edit mode
;; (use-package helm-rg
;;   :init (custom-set-variables '(helm-follow-mode-persistent t))
;;   :bind
;;   ("C-c r g" .  helm-rg))

(use-package magit
  :bind ("C-c g s" . magit-status))

(use-package magit-todos :init (magit-todos-mode))

(use-package diff-hl :config (setq-default global-diff-hl-mode t))

(use-package hl-todo :config (global-hl-todo-mode))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc verilog-verilator)))

(use-package helm-flycheck
  :after flycheck
  :bind (:map flycheck-mode-map ("C-c h f" . helm-flycheck)))

(use-package evil-nerd-commenter :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package projectile
  :init (projectile-mode 1)
  :config (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package visual-regexp-steroids
  :init (require 'visual-regexp-steroids)
  :bind ("C-r" . vr/replace))

;; (use-package quickrun
;;   :bind
;;   ("C-c e e" . quickrun-region)
;;   ("C-c e q" . quickrun-replace-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Navigation          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
  :bind ("C-c o" . ace-window))

;; (use-package beacon
;;   :init (beacon-mode 1)
;;   :config (setq-default
;;            beacon-blink-when-window-changes nil
;;            beacon-blink-when-buffer-changes nil))

(use-package bm
  :init (setq-default
         bm-restore-repository-on-load t
         bm-repository-file "~/.emacs.d/bm-repository"
         bm-buffer-persistence t)
  :hook (find-file-hooks . bm-buffer-restore)
  :config (setq-default bm-cycle-all-buffers t)
  :bind
  ("C-c b b" . bm-toggle)
  ("C-c b n" . bm-next)
  ("C-c b p" . bm-previous))

(use-package helm-bm
  :bind
  ("C-c b h" . helm-bm))

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

(use-package smart-hungry-delete
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
		 ("C-d" . smart-hungry-delete-forward-char))
  :config (smart-hungry-delete-add-default-hooks))

(use-package goto-chg :bind ("C-c g ;" . goto-last-change))

(use-package dired-single
  :bind
  (:map dired-mode-map
        ("<return>" . dired-single-buffer)
        ("C-<" . dired-single-up-directory)))

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  (setq-default pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))


(use-package writeroom-mode
  :init (setq-default writeroom-width 150)
  :hook (writeroom-mode . toggle-line-numbers))

;; (use-package treemacs
;;   :hook (treemacs-mode . disable-line-numbers)
;; ;;   :config
;; ;;   (setq-default treemacs-collapse-dirs              3
;; ;;                 treemacs-deferred-git-apply-delay   0.5
;; ;;                 treemacs-display-in-side-window     t
;; ;;                 treemacs-file-event-delay           5000
;; ;;                 treemacs-file-follow-delay          0.2
;; ;;                 treemacs-follow-after-init          t
;; ;;                 treemacs-follow-recenter-distance   0.1
;; ;;                 treemacs-goto-tag-strategy          'refetch-index
;; ;;                 treemacs-indentation                2
;; ;;                 treemacs-indentation-string         " "
;; ;;                 treemacs-is-never-other-window      nil
;; ;;                 treemacs-max-git-entries            5000
;; ;;                 treemacs-no-png-images              nil
;; ;;                 treemacs-project-follow-cleanup     nil
;; ;;                 treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;; ;;                 treemacs-recenter-after-file-follow nil
;; ;;                 treemacs-recenter-after-tag-follow  nil
;; ;;                 treemacs-show-cursor                nil
;; ;;                 treemacs-show-hidden-files          t
;; ;;                 treemacs-silent-filewatch           nil
;; ;;                 treemacs-silent-refresh             nil
;; ;;                 treemacs-sorting                    'alphabetic-desc
;; ;;                 treemacs-space-between-root-nodes   t
;; ;;                 treemacs-tag-follow-cleanup         t
;; ;;                 treemacs-tag-follow-delay           1.5
;; ;;                 treemacs-width                      35)
;; ;;   (treemacs-follow-mode t)
;; ;;   (treemacs-filewatch-mode t)
;; ;;   (treemacs-fringe-indicator-mode t)
;;   :bind
;;   ("M-0"       . treemacs-select-window)
;;   ("C-x t t"   . treemacs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Org Mode          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :config
  (setq-default org-src-fontify-natively t
                org-directory "~/Documents/Nextcloud/org"
                org-catch-invisible-edits 'show-and-error
                org-yank-adjusted-subtrees t
                org-hide-emphasis-markers t
                org-src-tab-acts-natively t
                org-edit-src-content-indentation 0
                org-fontify-quote-and-verse-blocks t
                org-cycle-separator-lines 0
                org-src-preserve-indentation nil
                org-imenu-depth 4)
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

(defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
  (let* ((rlt ad-return-value)
         (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\|example\\|quote\\)")
         (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\|example\\|quote\\)")
         (case-fold-search t)
         b e)
    (when ad-return-value
      (save-excursion
        (setq b (re-search-backward begin-regexp nil t))
        (if b (setq e (re-search-forward end-regexp nil t))))
      (if (and b e (< (point) e)) (setq rlt nil)))
    (setq ad-return-value rlt)))

(use-package org-journal
  :config
  (setq-default org-journal-dir (concat org-directory "/journal/")
                org-journal-carryover-items nil)
  :bind ("C-c i j" . org-journal-new-entry))

(use-package org-agenda
  :bind ("C-c a" . org-agenda)
  :config (setq-default org-agenda-files (list org-directory)))

;; (defun my/org-ref-open-pdf-at-point ()
;;   "Open the pdf for bibtex key under point if it exists."
;;   (interactive)
;;   (let* ((key (thing-at-point 'filename t))
;;          (pdf-file (funcall org-ref-get-pdf-filename-function key)))
;;     (if (file-exists-p pdf-file)
;;         (find-file pdf-file)
;;       (message "No PDF found for %s" key))))

;; (use-package org-ref
;;   :config
;;   (setq-default reftex-default-bibliography (list (concat org-directory "/Bibliography.bib"))
;;                 org-ref-bibliography-notes (concat org-directory "/Readings.org")
;;                 org-ref-default-bibliography (list (concat org-directory "/Bibliography.bib"))
;;                 org-ref-pdf-directory "~/Documents/Nextcloud/Papers/"
;;                 org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point))

;; (use-package ox-latex
;;   :config
;;   (setq-default org-latex-pdf-process
;;                 '("pdflatex -interaction nonstopmode -output-directory %o %f"
;;                   "bibtex %b"
;;                   "pdflatex -interaction nonstopmode -output-directory %o %f"
;;                   "pdflatex -interaction nonstopmode -output-directory %o %f")))

(use-package ox)

(use-package ox-hugo
  :after ox)

(use-package org-cliplink
  :bind
  (:map org-mode-map
        ("C-c i l" . org-cliplink)))

;; (use-package org-doing
;;   :config
;;   (setq-default org-doing-file (concat org-directory "/Doing.org")))

(use-package interleave
  :config
  (setq-default interleave-org-notes-dir-list '("~/Documents/Nextcloud/Papers/")))


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

;; (use-package cmake-ide
;;   :after rtags
;;   :init (cmake-ide-setup))

;; ;; requires rtags-install
;; (use-package rtags
;;   :hook (c++-mode . rtags-start-process-unless-running)
;;   :config
;;   (setq-default rtags-autostart-diagnostics t
;;                 rtags-completions-enabled t
;;                 rtags-use-helm t
;;                 rtags-display-result-backend 'helm))

;; (use-package company-rtags
;;   :after company
;;   :config (add-to-list 'company-backends 'company-rtags))


(use-package ccls
  :hook ((c++-mode) . (lambda () (require 'ccls) (lsp)))
  :config (setq-default ccls-executable "/home/eksi/.local/programs/ccls/build/ccls"))

(use-package lsp-mode
  :init
  (setq-default lsp-auto-execute-action nil
                lsp-before-save-edits nil
                lsp-auto-guess-root t
                lsp-prefer-flymake :none
                lsp-enable-indentation nil
                ;; lsp-enable-snippet nil
                lsp-enable-on-type-formatting nil))

;; (use-package lsp-ui
;;   :init
;;   (setq-default lsp-ui-flycheck-enable t))

(use-package company-lsp
  :after company
  :config (push 'company-lsp company-backends))

(use-package helm-lsp)

(use-package xref
  :config (setq-default xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package company-c-headers
  :after company
  :init (add-to-list 'company-backends 'company-c-headers))

;; (use-package flycheck-clang-analyzer
;;   :after flycheck
;;   :config (flycheck-clang-analyzer-setup))

;; (use-package rmsbolt :config (setq-default rmsbolt-automatic-recompile nil))

;; (use-package semantic
;;   :hook ((python-mode . semantic-mode)
;;          (c++-mode . semantic-mode)))

;; (use-package srefactor
;;   :after semantic
;;   :config (setq-default srefactor-ui-menu-show-help nil)
;;   :bind
;;   (:map c++-mode-map
;;         ("C-c r s" . srefactor-refactor-at-point)))


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
;; (use-package scala-mode)


;; (use-package ensime
;;   :config
;;   (setq-default ensime-eldoc-hints 'all
;;                 ensime-graphical-tooltips t
;;                 ensime-tooltip-hints t
;;                 ensime-startup-notification nil
;;                 ensime-sem-high-enabled-p nil))

;; (use-package sbt-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Python          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Although elpy provide refactoring, goto-defitnition didn't work and it messes with defaults
;;   (even though sane defaults weren't selected).
;; jedi is also fine but I am continouing with anaconda simply because I used it before and
;;   since I am not working on huge projects I didn't encounter any performacne issues

(use-package virtualenvwrapper :init (venv-initialize-interactive-shells))
(use-package auto-virtualenvwrapper :after virtualenvwrapper)

;; (use-package anaconda-mode)

;; (use-package company-anaconda
;;   :after company
;;   :init (add-to-list 'company-backends 'company-anaconda))

(use-package python
  :hook ((python-mode . auto-virtualenvwrapper-activate)))
         ;; (python-mode . anaconda-mode)
         ;; (python-mode . anaconda-eldoc-mode)))

;; (use-package flycheck-pycheckers
;;   :after flycheck
;;   :hook (flycheck-mode . flycheck-pycheckers-setup)
;;   :config (setq-default flycheck-pycheckers-checkers '(flake8 pyflakes)
;;                         flycheck-pycheckers-max-line-length 100))


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

;; Broken
;; (use-package readline-complete)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;          Docker          ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package dockerfile-mode :mode ("Dockerfile\\'" "\\.docker"))

;; (use-package docker-compose-mode :mode ("docker-compose\\.yml\\'" "-compose.yml\\'"))

;; (use-package docker-tramp)
