;;; -*- lexical-binding: t -*-
;; Initialization
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; (require 'server)
;; (unless (server-running-p) (server-start))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(require 'quelpa)
(customize-set-variable 'quelpa-update-melpa-p nil)

(setq-default use-package-always-defer t)
;; (setq-default use-package-always-ensure t)
(unless (package-installed-p 'quelpa-use-package)
  (quelpa '(quelpa-use-package :fetcher git :url "https://github.com/quelpa/quelpa-use-package.git")))
(require 'quelpa-use-package)
(customize-set-variable 'quelpa-use-package-inhibit-loading-quelpa t)

(require 'use-package-hydra)

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

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "elisp/")))

(setenv "DISPLAY" ":1")


(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 100
                    :weight 'medium
                    :width 'normal)

(require 'usta-builtins)
(require 'usta-dired)
(require 'usta-elfeed)
(require 'usta-latex)
(require 'usta-navigation)
(require 'usta-org)
(require 'usta-prog)
(require 'usta-prog-lang)
(require 'usta-uncategorized)
(require 'usta-vertico)
(require 'usta-visuals)
