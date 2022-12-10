;; -*- lexical-binding: t; -*-
;;; file opening procedures
;;; -*- lexical-binding: t -*-
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
  (defun dired-find-readme-other-window ()
    (interactive)
    (progn
      (let* ((dir (dired-get-filename))
             (files-readme (f-glob (concat dir "/readme*")))
             (files-Readme (f-glob (concat dir "/Readme*")))
             (files-README (f-glob (concat dir "/README*")))
             (file-paths (append files-README files-readme files-Readme))
             ;; (file-paths (seq-map (lambda (file) (concat dir "/" file)) file-names))
             (files (seq-filter (lambda (file) (f-exists? file)) file-paths))
             (file (car files)))
        (if (= 1 (count-windows))
            (split-window-horizontally))
        (other-window 1)
        (dired--find-possibly-alternative-file dir)
        (when file
          (find-file file)))))
  (defun dired-open-current-directory-xdg ()
    "Try to run `xdg-open' to open the current directory."
    (interactive)
    (if (executable-find "xdg-open")
        (let ((file (ignore-errors default-directory))
              (process-connection-type nil))
          (start-process "" nil "xdg-open" (file-truename file)))
      nil))
  (defun dired-current-dir ()
    (interactive)
    (dired default-directory))
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-create-destination-dirs 'ask)
  (dired-recursive-copies 'always)
  (dired-use-ls-dired nil)
  (dired-listing-switches "-aBhlv --group-directories-first --color=never")
  :bind
  ("C-x d" . dired-current-dir)
  (:map dired-mode-map
        ("o" . dired-find-readme-other-window)
        ("O" . dired-find-file-other-window)
        ("E" . dired-open-current-directory-xdg)
        ("e" . dired-open-xdg)))

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
  ("C-c H f" . fd-dired-current)
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
  :after project
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
  :bind (("C-c t t"   . treemacs)
         ("C-c t B"   . treemacs-bookmark)
         ("C-c t C-f" . treemacs-find-file))
  :custom
  (treemacs-position 'right)
  (treemacs-width 50))

(use-package treemacs-all-the-icons :after treemacs
  :config (treemacs-load-theme "all-the-icons"))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom (all-the-icons-dired-monochrome nil))

(use-package treemacs-magit
  :after treemacs magit)

(use-package lsp-treemacs
  :hook (treemacs-mode . lsp-treemacs-sync-mode))

(use-package treemacs-perspective
  :commands treemacs-set-scope-type
  :config (treemacs-set-scope-type 'Perspectives))

(use-package affe
  :preface
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  :custom
  (affe-count 10000)
  (affe-regexp-compiler #'affe-orderless-regexp-compiler)
  :bind
  ("C-c h F" . affe-find))

(use-package dired-rsync
  :bind
  (:map dired-mode-map
        ("C-c C-r" . dired-rsync)))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(use-package dired-async :ensure async
  :hook (dired-mode . dired-async-mode))

(provide 'usta-dired)
