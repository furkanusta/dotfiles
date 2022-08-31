;; -*- lexical-binding: t; -*-

;;; -*- lexical-binding: t -*-
(use-package transient-dwim
  :bind ("M-=" . transient-dwim-dispatch))

(use-package alert
  :custom (alert-default-style 'libnotify))

(use-package transpose-mark)

(use-package ialign)

(use-package immortal-scratch
  :custom (immortal-scratch-mode t))

(use-package persistent-scratch
  :config (persistent-scratch-setup-default))

(use-package scratch
  :bind ("M-s M-s" . scratch))

(use-package visual-regexp-steroids
  :demand t
  :bind ("C-r" . vr/replace))

(use-package literate-calc-mode)

(use-package nov
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
  :bind
  ("C-c j j" . ace-jump-mode)
  ("C-c j c" . ace-jump-char-mode))

(use-package ace-link
  :bind
  ("C-c j l" . ace-link))

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

(use-package vlf
  :hook (vlf-view-mode . disable-line-numbers))

(use-package vlf-setup :ensure vlf
  :config
  (add-to-list 'vlf-forbidden-modes-list 'pdf-view-mode)
  (add-to-list 'vlf-forbidden-modes-list 'nov-mode))

(use-package pdf-tools
  :quelpa (pdf-tools :fetcher github :repo "vedang/pdf-tools" :files ("lisp/*" "server/*"))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook ((pdf-view-mode . disable-line-numbers)
         (pdf-view-mode . pdf-sync-minor-mode)
         (pdf-view-mode . pdf-links-minor-mode)
         (pdf-view-mode . pdf-history-minor-mode)
         (pdf-view-mode . pdf-annot-minor-mode)
         (pdf-view-mode . pdf-view-themed-minor-mode))
  :preface
  (defun pdf-move-down-other-frame ()
    (interactive)
    (other-window 1)
    (pdf-view-scroll-up-or-next-page)
    (other-window 1))
  (defun pdf-move-up-other-frame ()
    (interactive)
    (other-window 1)
    (pdf-view-scroll-down-or-previous-page)
    (other-window 1))
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
        ("S-SPC" . pdf-view-scroll-down-or-previous-page))
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

;; (use-package undo-tree
;;   :custom
;;   (global-undo-tree-mode t)
;;   (undo-tree-visualizer-timestamps t)
;;   (undo-tree-visualizer-diff t)
;;   (undo-tree-auto-save-history t)
;;   (undo-tree-enable-undo-in-region t)
;;   (undo-tree-incompatible-major-modes '(elfeed-search-mode elfeed-entry-mode term-mode vterm-mode fundamental-mode))
;;   :bind
;;   (:map undo-tree-map
;;         ("C-+" . undo-tree-redo)
;;         ("C-_" . undo-tree-undo)))

(use-package vundo
  :load-path "elisp/"
  :demand t
  :defer nil
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package sudo-edit)

(provide 'usta-uncategorized)
