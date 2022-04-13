;; -*- lexical-binding: t; -*-

;;; -*- lexical-binding: t -*-
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

;; (use-package emacs-everywhere
;;   :preface (defun disable-modes ()
;;              (setq hungry-delete-chars-to-skip " \t\r\f\v")
;;              (beacon-mode -1)
;;              (toggle-truncate-lines 1))
;;   :hook (emacs-everywhere-mode . disable-modes))

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

(use-package visual-regexp-steroids
  :demand t
  :bind ("C-r" . vr/replace))


(use-package literate-calc-mode)

(use-package fountain-mode)

(use-package nov
  :mode
  ("\\.epub\\'" . nov-mode)
  ("\\.EPUB\\'" . nov-mode)
  :custom (nov-text-width 100))

(use-package flyspell
  :hook (text-mode . flyspell-mode))

(use-package flyspell-correct
  :bind
  ("C-c $" . flyspell-correct-wrapper)
  ("C-;" . flyspell-correct-at-point)
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-at-point)
        ("C-c $" . flyspell-correct-wrapper)))

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
        ("M-g M-g" . pdf-view-goto-page)
        ("S-SPC" . pdf-view-scroll-down-or-previous-page))
  (:map org-mode-map
        ("C-M-n" . pdf-move-down-other-frame)
        ("C-M-p" . pdf-move-up-other-frame)))

(use-package pdf-tools-note
  :no-require t
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

(use-package undo-tree
  ;; :hook
  ;; (prog-mode . undo-tree-mode)
  ;; (text-mode . undo-tree-mode)
  ;; (bibtex-mode . undo-tree-mode)
  :custom
  (global-undo-tree-mode t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-tree-auto-save-history t)
  (undo-tree-enable-undo-in-region t)
  :bind
  ("C-+" . undo-tree-redo)
  ("C-_" . undo-tree-undo))

(use-package redacted)

(use-package sudo-edit)

(use-package piper
  :quelpa (piper :fetcher gitlab :repo "howardabrams/emacs-piper"))

(use-package dtache
  :quelpa (dtache :fetcher gitlab :repo "niklaseklund/dtache")
  :preface
  (defun my/dtache-state-transition-notification (session)
    "Send an `alert' notification when SESSION becomes inactive."
    (let ((status (dtache--session-status session))
          (title
           (pcase (dtache--session-status session)
             ('success "Dtache finished!")
             ('failure "Dtache failed!"))))
      (alert (dtache--session-command session)
             :title title
             :severity (pcase status
                         ('success 'moderate)
                         ('failure 'high))
             :category 'compile
             :id (pcase status
                   ('success 'dtache-success)
                   ('failure 'dtache-failure)))))
  :custom
  (dtache-notification-function #'my/dtache-state-transition-notification))

(use-package dtache-compile
  :ensure dtache
  :hook (after-init . dtache-compile-setup)
  :bind (([remap compile] . dtache-compile)
         ([remap recompile] . dtache-compile-recompile)
         :map dtache-compilation-mode-map
         ("C-c C-q" . dtache-detach-dwim)))

(use-package dtache-consult
  :ensure dtache
  :bind ([remap dtache-open-session] . dtache-consult-session))

(use-package dtache-projectile
  :no-require t
  :after (dtache projectile)
  :preface
  (defun my/dtache-projectile-run-compilation (cmd &optional use-comint-mode)
    "If CMD is a string execute it with `dtache-compile', optionally USE-COMINT-MODE."
    (if (functionp cmd)
        (funcall cmd)
      (let ((dtache-session-origin 'projectile))
        (dtache-compile cmd use-comint-mode))))
  :config
  (advice-add 'projectile-run-compilation :override #'my/dtache-projectile-run-compilation))


(use-package dtache-vterm
  :no-require t
  :after (dtache vterm)
  :preface
  (defun dtache-vterm-send-input (&optional detach)
    "Create a `dtache' session."
    (interactive)
    (vterm-send-C-a)
    (let* ((input (buffer-substring-no-properties (point) (vterm-end-of-line)))
           (dtache-session-origin 'vterm)
           (dtache-session-action
            '(:attach dtache-shell-command-attach-session
                      :view dtache-view-dwim
                      :run dtache-shell-command))
           (dtache-session-mode
            (if detach 'create 'create-and-attach)))
      (vterm-send-C-k)
      (process-send-string vterm--process (dtache-dtach-command input t))
      (vterm-send-C-e)
      (vterm-send-return)))
  (defun dtache-vterm-attach (session)
    "Attach to an active `dtache' session."
    (interactive
     (list
      (let* ((host-name (car (dtache--host)))
             (sessions
              (thread-last (dtache-get-sessions)
                           (seq-filter (lambda (it)
                                         (string= (car (dtache--session-host it)) host-name)))
                           (seq-filter (lambda (it) (eq 'active (dtache--determine-session-state it)))))))
        (dtache-completing-read sessions))))
    (let ((dtache-session-mode 'attach))
      (process-send-string vterm--process (dtache-dtach-command session t))
      (vterm-send-return)))
  (defun dtache-vterm-detach ()
    "Detach from a `dtache' session."
    (interactive)
    (process-send-string vterm--process dtache--dtach-detach-character))
  :bind (:map vterm-mode-map
              ("<S-return>" . #'dtache-vterm-send-input)
              ("<C-return>" . #'dtache-vterm-attach)
              ("C-c C-d" . #'dtache-vterm-detach)))

(use-package emms
  :custom
  (emms-source-file-default-directory "~/Music/")
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (require 'emms-history)
  (emms-history-load)
  :bind
  ("C-c m p". emms-pause))

(use-package kmacro-x
  :custom
  (kmacro-x-atomic-undo-mode 1)
  :bind ("C-c k" . kmacro-x-mc-region))

(provide 'usta-uncategorized)
