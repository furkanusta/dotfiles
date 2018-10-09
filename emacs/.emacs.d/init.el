(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

(setq-default user-full-name "Furkan Usta"
              user-mail-address "furkanusta17@gmail.com")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(setq custom-file "~/.emacs.d/elisp/custom.el")
(load custom-file)

(add-to-list 'load-path (concat user-emacs-directory "elisp/"))

(use-package init-defaults :demand t)
(use-package init-others :demand t)
(use-package init-programming-languages :demand t)

(require 'readline-complete)
