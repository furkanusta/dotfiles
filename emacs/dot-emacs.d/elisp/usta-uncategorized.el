;; -*- lexical-binding: t; -*-

(use-package transient-dwim
  :bind ("M-=" . transient-dwim-dispatch))

(use-package screenshot
  :quelpa (screenshot :fetcher github :repo "tecosaur/screenshot")
  :custom
  (screenshot-line-numbers t)
  (screenshot-min-width 100))

(use-package alert
  :custom (alert-default-style 'libnotify))

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

(use-package backup-walker)

(use-package backups-mode
  :quelpa (backups-mode :fetcher github :repo "chadbraunduin/backups-mode")
  :bind (:map backups-minor-mode-keymap ("C-x s B" . list-backups)))

(use-package lively)

(use-package jump-tree)

(use-package transpose-mark)

(use-package tiny
  :config (tiny-setup-default))

(use-package ialign)

(use-package immortal-scratch
  :custom (immortal-scratch-mode t))

(use-package persistent-scratch
  :config (persistent-scratch-setup-default))

(use-package scratch
  :bind ("M-s M-s" . scratch))

(use-package visual-regexp-steroids :demand t
  :bind ("C-r" . vr/replace))


(use-package literate-calc-mode)

(use-package fountain-mode)

(use-package nov
  :mode
  ("\\.epub\\'" . nov-mode)
  ("\\.EPUB\\'" . nov-mode)
  :custom (nov-text-width 100))


(use-package flyspell)

(use-package flyspell-correct
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-c $" . flyspell-correct-at-point)
        ("C-;" . flyspell-correct-wrapper)))

(use-package wgrep)

(use-package deadgrep
  :bind
  ("C-c H s" . deadgrep)
  (:map deadgrep-mode-map ("E" . deadgrep-edit-mode))
  (:map deadgrep-edit-mode-map ("E" . deadgrep-mode)))

(use-package xref)

(use-package avy
  :bind
  ("C-c j j" . avy-goto-char)
  ("C-c j r" . avy-resume)
  ("C-c j b" . avy-pop-mark)
  ("C-c j g" . avy-goto-line))

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
  :after org-noter
  :demand t
  :defines org-noter-insert-note
  :quelpa (pdf-tools :fetcher github :repo "vedang/pdf-tools" :files ("lisp/*" "server/*"))
  :hook ((pdf-view-mode . (lambda () (cua-mode 0)))
         (pdf-view-mode . disable-line-numbers)
         (pdf-view-mode . pdf-sync-minor-mode)
         (pdf-view-mode . pdf-links-minor-mode)
         (pdf-view-mode . pdf-history-minor-mode)
         (pdf-view-mode . pdf-annot-minor-mode)
         (pdf-view-mode . pdf-view-themed-minor-mode))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations nil)
  (pdf-view-resize-factor 1.1)
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
        ("M-w" . pdf-view-kill-ring-save)
        ("o" . pdf-outline)
        ("i" . my-org-noter-insert-and-highlight)
        ("M-i" . my-org-noter-insert-precise-and-highlight)
        ("S-SPC" . pdf-view-scroll-down-or-previous-page))
  (:map org-noter-doc-mode-map
        ("i" . my-org-noter-insert-and-highlight)
        ("M-i" . my-org-noter-insert-precise-and-highlight)))

(use-package pdf-view-restore
  :after pdf-tools
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :custom (pdf-view-restore-filename (concat no-littering-var-directory "pdf-view-restore")))

(use-package undo-tree
  :diminish undo-tree-mode
  :custom
  (global-undo-tree-mode 1)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-tree-auto-save-history t)
  (undo-tree-enable-undo-in-region t)
  :bind
  ("C-+" . undo-tree-redo)
  ("C-_" . undo-tree-undo))

(use-package secret-mode
  :quelpa (secret-mode :fetcher github :repo "bkaestner/secret-mode.el"))

(use-package sudo-edit)

(use-package piper
  :quelpa (piper :fetcher gitlab :repo "howardabrams/emacs-piper"))

(use-package dtache
  :quelpa (dtache :fetcher gitlab :repo "niklaseklund/dtache"))

(provide 'usta-uncategorized)
