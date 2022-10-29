;;; -*- lexical-binding: t -*-
;; Initialization
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'server)
(unless (server-running-p) (server-start))

(unless (and (fboundp 'package-installed-p)
             (package-installed-p 'use-package))
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "elisp/")))

(require 'use-package)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(unless (package-installed-p 'quelpa-use-package)
  (quelpa '(quelpa-use-package :fetcher git :url "https://github.com/quelpa/quelpa-use-package.git")))

(use-package quelpa
  :custom (quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :custom (quelpa-use-package-inhibit-loading-quelpa t))

(use-package use-package-hydra)

(setq use-package-always-defer t)
;; (setq-default use-package-always-ensure t)

(setq custom-file (concat user-emacs-directory "elisp/custom.el"))
;; Init Done

;; Debug
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(defun disable-line-numbers ()
  (display-line-numbers-mode -1))

(defvar use-other-window-alist
  '((display-buffer-same-window display-buffer-pop-up-window)
    (inhibit-same-window . t)))

(require 'usta-builtins)
(require 'usta-dired)
(require 'usta-elfeed)
(require 'usta-latex)
(require 'usta-navigation)
(require 'usta-vertico)
(require 'usta-org)
(require 'usta-prog)
(require 'usta-prog-lang)
(require 'usta-uncategorized)
(require 'usta-citar)
(require 'usta-visuals)

