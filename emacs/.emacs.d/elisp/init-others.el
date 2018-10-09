(defun disable-linum-mode ()
  (linum-mode -1))

(use-package dired
  :init (setq-default dired-dwim-target t))

(use-package vlf
  :after dired
  :hook (vlf-view-mode . disable-linum-mode)
  :init (require 'vlf-setup))


(defun pdf-view-page-number ()
  (interactive)
  (message " [%s/%s]"
           (number-to-string (pdf-view-current-page))
           (number-to-string (pdf-cache-number-of-pages))))

;; requires pdf-tools-install
(use-package pdf-tools
  :hook ((pdf-view-mode . (lambda () (cua-mode 0)))
         (pdf-view-mode . disable-linum-mode))
  :mode ("\\.pdf\\'" . pdf-view-mode)
 :config
 (setq-default pdf-view-display-size 'fit-page
               pdf-annot-activate-created-annotations t
               pdf-view-resize-factor 1.1)
 :bind (:map pdf-view-mode-map ("t" . pdf-view-page-number)))

(use-package git-timemachine :bind ("C-c g t" . git-timemachine))

(use-package magit :bind ("C-c g s" . magit-status))

(use-package magit-todos :init (magit-todos-mode))

(use-package diff-hl :config (setq-default global-diff-hl-mode t))

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

(use-package flycheck
  :init (global-flycheck-mode)
  :commands flycheck-add-mode
  :config
  (setq-default flycheck-clang-language-standard "c++1z"
                flycheck-gcc-language-standard "c++1z"
                flycheck-cppcheck-standards "c++1z"
                flycheck-clang-standard-library "libc++"
                flycheck-disabled-checkers '(emacs-lisp-checkdoc c/c++-clang))
  (flycheck-add-mode 'perl-perlcritic 'perl)
  (flycheck-add-mode 'c/c++-cppcheck 'c/c++-gcc)
  (flycheck-add-mode 'c/c++-cppcheck 'c/c++-clang)
  (flycheck-add-mode 'c/c++-cppcheck 'rtags))

(use-package evil-nerd-commenter :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package visual-regexp-steroids
  :init (require 'visual-regexp-steroids)
  :bind ("C-r" . vr/replace))

;; (use-package ace-jump-mode :bind ("C-c j" . ace-jump-char-mode))

(use-package windmove
  :bind (("C-c <left>" . windmove-left)
         ("C-c <right>" . windmove-right)
         ("C-c <up>" . windmove-up)
         ("C-c <down>" . windmove-down)))

(use-package drag-stuff
  :diminish drag-stuff-mode
  :init (drag-stuff-global-mode t)
  :bind (:map drag-stuff-mode-map
         ("<M-up>" . drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

(use-package hideshow
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind
  ("C-c <" . hs-toggle-hiding)
  ("C-c >" . hs-hide-all)
  ("C-c ." . hs-show-all))

(use-package buffer-move
  :bind
  ("C-c S-<up>"    . buf-move-up)
  ("C-c S-<down>"  . buf-move-down)
  ("C-c S-<left>"  . buf-move-left)
  ("C-c S-<right>" . buf-move-right))

(use-package projectile :init (setq-default projectile-completion-system 'helm))

(use-package flyspell)

;; Shell
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
  ;; :hook (compilation-shell-minor-mode goto-address-mode ansi-color-for-comint-mode-on)
  :bind (:map shell-mode-map ("<tab>" . completion-at-point)))

;; (use-package bash-completion :init (bash-completion-setup))

(use-package dockerfile-mode :mode ("Dockerfile\\'" "\\.docker"))

(use-package docker-compose-mode :mode ("docker-compose\\.yml\\'" "-compose.yml\\'"))

(use-package docker-tramp)

(use-package dockerfile-mode)

(use-package image-mode
  :hook (image-mode . disable-linum-mode))

(use-package image+
  :after image-mode
  :bind (:map image-mode-map
         ("C-+" . imagex-sticky-zoom-in)
         ("C--" . imagex-sticky-zoom-out)
         ("C-0" . imagex-sticky-restore-original)))

;; (use-package mingus
;;   :config (setq-default mingus-mode-always-modeline nil)
;;   :bind
;;   ("<C-XF86AudioPlay>" . mingus-toggle)
;;   ("<C-XF86AudioPrev>" . mingus-prev)
;;   ("<C-XF86AudioNext>" . mingus-next))

;; (use-package fzf :bind ("C-c f z" . fzf))

(use-package elfeed
  :init
  (setq-default elfeed-feeds
                '("http://research.swtch.com/feeds/posts/default"
                  "http://bitbashing.io/feed.xml"
                  "http://lemire.me/blog/feed/"
                  "http://preshing.com/feed"
                  "http://danluu.com/atom.xml"
                  "http://tenderlovemaking.com/atom.xml"
                  "http://feeds.feedburner.com/codinghorror/"
                  "https://www.discoverdev.io/rss.xml"
                  "http://www.snarky.ca/feed"
                  "http://blog.regehr.org/feed"
                  ("http://planet.emacsen.org/atom.xml" emacs)
                  ("http://planet.gnome.org/rss20.xml" gnome)
                  ("http://arne-mertz.de/feed/" cpp)
                  ("http://xkcd.com/rss.xml" xkcd)
                  )))

(use-package bm
  :init (setq bm-restore-repository-on-load t)
  :hook (find-file-hooks . bm-buffer-restore)
  :config (setq-default bm-cycle-all-buffers t)
  :bind
  ("C-c b b" . bm-toggle)
  ("C-c b n" . bm-next)
  ("C-c b p" . bm-previous))

(use-package helm-bm
  :bind
  ("C-c b h" . helm-bm))

(use-package eyebrowse
  :init (eyebrowse-mode t)
  :config (setq-default eyebrowse-wrap-around t)
  :bind
  (:map eyebrowse-mode-map
        ("C-c C-w <left>" . eyebrowse-prev-window-config)
        ("C-c C-w l" . eyebrowse-switch-to-window-config)
        ("C-c C-w <right>" . eyebrowse-next-window-config)))

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
   ("C-c C-o" . helm-occur)
   ("C-x b" . helm-mini)
   ("C-z" .  helm-select-action)
   ("M-y" . helm-show-kill-ring)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action))
  :config
  (setq helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source t
        helm-scroll-amount 8)
  (setq-default helm-ff-search-library-in-sexp t
                helm-ff-file-name-history-use-recentf t
                helm-semantic-fuzzy-match t
                helm-M-x-fuzzy-match t
                helm-imenu-fuzzy-match t
                helm-boring-buffer-regexp-list (list (rx "*magit-") (rx "*helm") (rx "*flycheck"))))

(use-package helm-swoop
  :config
  (setq-default helm-swoop-move-to-line-cycle t
                helm-swoop-use-line-number-face t
                helm-swoop-split-direction 'split-window-vertically
                helm-swoop-split-with-multiple-windows t
                helm-swoop-move-to-line-cycle t)
  :bind
  (("C-s" . helm-swoop-without-pre-input)
   ("C-c C-SPC" . helm-swoop-back-to-last-point)
   ("C-c s" . isearch-forward)
   :map helm-swoop-map
   ("C-r" . helm-previous-line)
   ("C-s" . helm-next-line)))

(use-package helm-ag
  :init (custom-set-variables '(helm-follow-mode-persistent t))
  :bind
  ("C-c a p" . helm-do-ag-project-root)
  ("C-c a g" .  helm-do-ag))

(use-package helm-flycheck
  :after flycheck
  :bind (:map flycheck-mode-map ("C-c h" . helm-flycheck)))

(use-package helm-bibtex
  :config
  (setq-default bibtex-completion-bibliography "~/Dropbox/org/Bibliography.bib"
                bibtex-completion-library-path "~/Dropbox/Papers/"
                bibtex-completion-notes-path "~/Dropbox/org/helm-bibtex-notes"))


(use-package org
  :config
  (setq-default org-src-fontify-natively t
                org-directory "~/Dropbox/org"
                org-yank-adjusted-subtrees t
                org-hide-emphasis-markers t
                org-src-tab-acts-natively t
                org-edit-src-content-indentation 0
                org-fontify-quote-and-verse-blocks t
                org-cycle-separator-lines 0
                org-src-preserve-indentation t
                org-imenu-depth 4)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (http . t)
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
  :bind ("C-c a l" . org-agenda-list)
  :config (setq-default org-agenda-files (list org-directory)))

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((key (thing-at-point 'filename t))
         (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
        (find-file pdf-file)
      (message "No PDF found for %s" key))))

(use-package org-ref
  :config
  (setq-default reftex-default-bibliography (list (concat org-directory "/Bibliography.bib"))
                org-ref-bibliography-notes (concat org-directory "/Readings.org")
                org-ref-default-bibliography (list (concat org-directory "/Bibliography.bib"))
                org-ref-pdf-directory "~/Dropbox/Papers/"
                org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point))

(use-package ox-latex
  :config
  (setq-default org-latex-pdf-process
                '("pdflatex -interaction nonstopmode -output-directory %o %f"
                  "bibtex %b"
                  "pdflatex -interaction nonstopmode -output-directory %o %f"
                  "pdflatex -interaction nonstopmode -output-directory %o %f")))

;; (use-package org-alert
;;   :commands org-alert-check
;;   :config
;;   (org-alert-check))

(use-package org-cliplink
  :bind
  (:map org-mode-map
        ("C-c i l" . org-cliplink)))

;; (use-package writeroom-mode
;;   :hook (writeroom-mode . (lambda () (linum-mode -1)))
;;   :config (setq-default writeroom-width 120)
;;   :bind (:map org-mode-map ("C-c w r" . writeroom-mode)))

;; (use-package org-capture
;;   :config
;;   (setq-default org-default-notes-file (concat org-directory "/Capture.org"))
;;   (setq org-capture-templates
;;         '(
;;           ("n" "Note" entry (file+headline "~/Dropbox/org/Notes.org" "Notes") "* %?\n%T\n%a")
;;           ("N" "Note (clipboard)" entry (file+headline "~/Dropbox/org/Notes.org" "Notes") "* %?\n%T\n%a\n%i")
;;           ;; ("b" "Bookmark" entry (file+headline "~/Dropbox/orgfiles/links.org" "Links")
;;           ;;  "* %? %^L %^g \n%T" :prepend t)
;;           ("i" "Idea" item (file+headline "~/Dropbox/org/Story.org" "Topics") "%?\n")
;;           ("d" "Drawing" item (file+headline "~/Dropbox/org/Drawing.org" "Scene") "%?")
;;           ("t" "TODO" entry (file "~/Dropbox/org/TODO.org") "* %?\n%T")
;;           ("T" "TODO (clipboard)" entry (file "~/Dropbox/org/TODO.org") "* %?\n%T\n%i")
;;           ("p" "Protocol" entry (file+headline ,(concat org-directory "Notes.org") "Browser")
;;            "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
;;           ("L" "Protocol Link" entry (file+headline ,(concat org-directory "Bookmarks.org") "Browser")
;;            "* %? [[%:link][%:description]] \nCaptured On: %U")
;;           ;; ("j" "Journal" entry (file+datetree "~/Dropbox/journal.org")
;;           ;;  "* %?\nEntered on %U\n  %i\n  %a")
;;           ))
;;   :bind ("C-c c" . org-capture))

;; (use-package org-protocol
;;   :config
;;   (setq-default org-capture-templates
;;         `(
;;           ("n" "Note" entry (file+headline "~/Dropbox/org/Notes.org" "Notes") "* %?\n%T\n%a")
;;           ("N" "Note (clipboard)" entry (file+headline "~/Dropbox/org/Notes.org" "Notes") "* %?\n%T\n%a\n%i")
;;           ;; ("b" "Bookmark" entry (file+headline "~/Dropbox/orgfiles/links.org" "Links")
;;           ;;  "* %? %^L %^g \n%T" :prepend t)
;;           ("i" "Idea" item (file+headline "~/Dropbox/org/Story.org" "Topics") "%?\n")
;;           ("d" "Drawing" item (file+headline "~/Dropbox/org/Drawing.org" "Scene") "%?")
;;           ("t" "TODO" entry (file "~/Dropbox/org/TODO.org") "* %?\n%T")
;;           ("T" "TODO (clipboard)" entry (file "~/Dropbox/org/TODO.org") "* %?\n%T\n%i")
;;           ("p" "Protocol" entry (file+headline ,(concat org-directory "/Notes.org") "Notes (Browser)")
;;            "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
;;           ("L" "Protocol Link" entry (file+headline ,(concat org-directory "/Notes.org") "Bookmark (Browser)")
;;            "* %? [[%:link][%:description]] \nCaptured On: %U")
;;           ("j" "Journal" entry (file+datetree "~/Dropbox/journal.org")
;;            "* %?\nEntered on %U\n  %i\n  %a")
;;           )))

(use-package interleave
  :config
  (setq-default interleave-org-notes-dir-list '("~/Dropbox/Papers/")))

;; (use-package ox-pandoc)

(use-package ob-async)


(provide 'init-others)
