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

(use-package usta-builtins :load-path "elisp/" :demand t)
(use-package usta-helm :load-path "elisp/" :demand t)
(use-package usta-navigation :load-path "elisp/" :demand t)
(use-package usta-visuals :load-path "elisp/" :demand t)
(use-package usta-company :load-path "elisp/" :demand t)
(use-package usta-dired :load-path "elisp/" :demand t)
(use-package usta-elfeed :load-path "elisp/" :demand t)
(use-package usta-latex :load-path "elisp/" :demand t)
(use-package usta-org :load-path "elisp/" :demand t)
(use-package usta-prog :load-path "elisp/" :demand t)
(use-package usta-prog-lang :load-path "elisp/" :demand t)
(use-package usta-uncategorized :load-path "elisp/" :demand t)

(use-package flx)
