
;;; file opening procedures
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
  (defun dired-current-dir ()
    (interactive)
    (dired default-directory))
  :custom
  (dired-use-ls-dired nil)
  (dired-listing-switches "-aBhl  --group-directories-first --color=never")
  (dired-auto-revert-buffer t)
  (dired-create-destination-dirs 'ask)
  (dired-dwim-target t)
  :bind
  ("C-x d" . dired-current-dir)
  (:map dired-mode-map ("E" . dired-open-xdg)))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("H" . dired-hide-dotfiles-mode)))

(use-package fd-dired
  :preface
  (defvar fd-dired-current-args-history nil)
  (defun fd-dired-current ()
    (interactive)
    (fd-dired default-directory (read-string "Run fd (with args and search): " ""
                                             '(fd-dired-current-args-history . 1))))
  :bind
  (:map dired-mode-map
        ("f" . fd-dired-current)
        ("F" . fd-dired)))

(use-package diredfl
  :custom-face
  (diredfl-exec-priv ((t nil)))
  (diredfl-read-priv ((t nil)))
  (diredfl-write-priv ((t nil))))

(use-package trashed)


(use-package neotree
  :after projectile
  :custom
  (neo-smart-open t)
  (neo-vc-integration nil)
  (neo-theme 'icons)
  :hook (neotree-mode . disable-line-numbers)
  :bind ("C-c t d" . neotree-toggle))

(use-package treemacs
  :commands treemacs-resize-icons treemacs-is-file-git-ignored?
  :hook
  (treemacs-mode . disable-line-numbers)
  (treemacs-mode . treemacs-filewatch-mode)
  (treemacs-mode . treemacs-fringe-indicator-mode)
  (treemacs-mode . treemacs-follow-mode)
  :config
  (treemacs-resize-icons 20)
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
  :bind
  (:map global-map
        ("C-c t t"   . treemacs)
        ("C-c t B"   . treemacs-bookmark)
        ("C-c t C-f" . treemacs-find-file))
  :custom
  (treemacs-position 'right)
  (treemacs-width 50))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

(use-package lsp-treemacs
  :hook (treemacs-mode . lsp-treemacs-sync-mode))

(use-package treemacs-perspective
  :commands treemacs-set-scope-type
  :config (treemacs-set-scope-type 'Perspectives))

(use-package fzf
  :bind ("C-c f f" . fzf-find-file))

(provide 'usta-dired)
