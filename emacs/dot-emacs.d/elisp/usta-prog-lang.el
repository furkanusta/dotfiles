;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          C++          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode :ensure nil
  :mode
  ("\\.h\\'" . c++-mode)
  :config
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  :custom
  (c-default-style "stroustrup")
  (c-basic-offset 4)
  (c-indent-level 4)
  (access-label 0)
  (c-noise-macro-names '("constexpr")))

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode)

(use-package flycheck-clang-analyzer
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package meson-mode
  :hook (meson-mode . company-mode))

(use-package lsp-mode
  :custom
  (lsp-auto-execute-action nil)
  (lsp-before-save-edits nil)
  (lsp-keymap-prefix "C-c C-l")
  (lsp-enable-snippet t)
  (lsp-enable-xref t)
  (lsp-enable-imenu t)
  (lsp-prefer-flymake nil)
  (lsp-enable-indentation nil)
  (lsp-prefer-capf t)
  (lsp-enable-file-watchers nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-semantic-highlighting nil)
  (lsp-enable-on-type-formatting nil)
  (read-process-output-max (* 2 1024 1024))
  (lsp-enable-on-type-formatting nil))

(use-package origami
  :hook (prog-mode . origami-mode)
  :bind
  ("C-c ," . origami-toggle-node)
  ("C-c C-." . origami-close-all-nodes)
  ("C-c C->" . origami-open-all-nodes))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;          Scala          ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :mode ("\\.sc\\'" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :custom (sbt:program-options '("-Dsbt.supershell=false"))
  :config (substitute-key-definition
           'minibuffer-complete-word
           'self-insert-command
           minibuffer-local-completion-map))

(use-package lsp-metals
  :hook  (scala-mode . lsp-deferred)
  :custom (lsp-metals-treeview-show-when-views-received nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Shell          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ansi-color
  :commands ansi-color-apply-on-region
  :hook (compilation-filter . colorize-compilation-buffer)
  :preface (defun colorize-compilation-buffer ()
             (read-only-mode)
             (ansi-color-apply-on-region (point-min) (point-max))
             (read-only-mode -1))
  :config (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  :custom (ansi-color-for-comint-mode 1))

(use-package shell
  :after window
  :hook (shell-mode . company-mode)
  :config (add-to-list 'display-buffer-alist (cons "\\*shell\\*" use-other-window-alist))
  :bind ("C-<f8>" . shell))

(use-package eshell
  :config (add-to-list 'display-buffer-alist (cons "\\*eshell\\*" use-other-window-alist))
  :bind ("M-<f8>" . eshell))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Docker         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" "\\.docker"))
(use-package docker-compose-mode
  :mode ("docker-compose\\.yml\\'" "-compose.yml\\'"))

(use-package docker)

(use-package docker-tramp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package verilog-mode
  :mode
  ("\\.v\\'" . verilog-mode)
  ("\\.sv\\'" . verilog-mode)
  :custom
  ;; (verilog-auto-inst-sort t)
  (verilog-library-directories '("." "../sim" "../rtl"))
  (verilog-auto-declare-nettype "none")
  (verilog-case-fold nil)
  (verilog-auto-newline nil)
  (verilog-tab-always-indent nil)
  (verilog-auto-indent-on-newline t)
  (verilog-case-indent 4)
  (verilog-cexp-indent 4)
  (verilog-indent-begin-after-if nil)
  (verilog-indent-level 4)
  (verilog-indent-level-behavioral 4)
  (verilog-indent-level-directive 4)
  (verilog-indent-level-declaration 4)
  (verilog-indent-level-module 4))


(use-package tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package graphviz-dot-mode
  :custom (graphviz-dot-indent-width 4))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;         LISP        ::
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sly
  :config (sly-setup '(sly-mrepl)))

(use-package smartparens)

(use-package smartparens-config :ensure smartparens)

(use-package lisp-extra-font-lock
  :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))

(use-package comment-or-uncomment-sexp
  :bind ("C-M-;" . comment-or-uncomment-sexp))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Python       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package blacken
  :hook (python-mode . blacken-mode)
  :custom (blacken-line-length 100))

(use-package gitlab-ci-mode
  :mode "\\.gitlab-ci\\.yml\\'")

(use-package aphelia
  :hook (python-mode . aphelia-mode))

(provide 'usta-prog-lang)
