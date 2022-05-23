;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          C++          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cc-mode :ensure nil
  :mode
  ("\\.h\\'" . c++-mode)
  :custom
  (c-offsets-alist '((innamespace . 0)
                         (substatement-open . 0)))
  (c-default-style "stroustrup")
  (c-basic-offset 4)
  (c-indent-level 4)
  (access-label 0)
  (c-noise-macro-names '("constexpr")))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package cmake-mode)

(use-package flycheck-clang-analyzer
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package meson-mode)

(use-package dap-lldb :ensure dap-mode)

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
;;     LISP / ELISP    ::
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elisp-mode
  :hook (emacs-lip-mode . flycheck-mode))

(use-package lisp
  :hook (lisp-mode . auto-highlight-symbol-mode))

(use-package sly
  :config (sly-setup '(sly-fancy))
  :hook
  ((sly-mode . (lambda () (sly-symbol-completion-mode -1)))
   (sly-mode . smartparens-mode))
  :preface
  (defun my-sly-lookup ()
    (interactive)
    (let ((browse-url-browser-function 'eww-browse-url))
      (sly-documentation-lookup)))
  :custom
  (inferior-lisp-program "sbcl")
  (sly-symbol-completion-mode -1)
  (sly-kill-without-query-p t)
  (sly-command-switch-to-existing-lisp 'always)
  :bind
  (:map sly-mode-map ("C-h D" . my-sly-lookup)))

(use-package sly-quicklisp)

(use-package sly-repl-ansi-color
  :after sly
  :init (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(use-package smartparens
  :hook
  ((prog-mode . smartparens-mode)
   (conf-mode . smartparens-mode)
   (text-mode . smartparens-mode))
  :init (require 'smartparens-config)
  :bind (:map smartparens-mode-map
              ("C-c l w" . sp-copy-sexp)
              ("C-c l b" . sp-backward-up-sexp)
              ("C-c l u" . sp-unwrap-sexp)
              ("C-c l f" . sp-forward-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-u" . sp-up-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-c [" . sp-wrap-square)
              ("C-c {" . sp-wrap-curly)
              ("C-c (" . sp-wrap-round)
              ("C-c l SPC" . sp-select-next-thing)
              ("C-c l C-SPC" . sp-select-next-thing-exchange)
              ("C-c l k" . sp-kill-sexp)))

(use-package lisp-extra-font-lock
  :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))

(use-package highlight-function-calls
  :hook (emacs-lisp-mode . highlight-function-calls-mode))

(use-package easy-escape
  :hook (emacs-lisp-mode . easy-escape-minor-mode))

(use-package flycheck-elsa
  :hook (emacs-lisp-mode . flycheck-elsa-setup))

(use-package comment-or-uncomment-sexp
  :bind ("C-M-;" . comment-or-uncomment-sexp))

(use-package elisp-def
  :hook
  (emacs-lisp-mode . elisp-def-mode)
  (ielm-mode . elisp-def-mode))

(use-package refine)

(use-package eval-sexp-fu
  :bind
  ("C-c e s" . eval-sexp-fu-eval-sexp-inner-list))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Python       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package blacken
;;   :hook (python-mode . blacken-mode)
;;   :custom (blacken-line-length 100))

(use-package gitlab-ci-mode
  :mode "\\.gitlab-ci\\.yml\\'")

(use-package apheleia
  :hook (python-mode . apheleia-mode))

(use-package format-all)

(use-package python-mls
  :hook
  (inferior-python-mode . python-mls-mode)
  (python-mode . python-mls-python-setup))

;; Built-in Python utilities
(use-package python
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python")))
  :preface
  (defun yasnippet-radical-snippets--python-split-args (arg-string)
    "Split the python ARG-STRING into ((name, default)..) tuples."
    (mapcar (lambda (x)
              (split-string x "[[:blank:]]*=[[:blank:]]*" t))
            (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

  (defun yasnippet-radical-snippets--python-args-to-google-docstring (text &optional make-fields)
    "Return a Google docstring for the Python arguments in TEXT.
Optional argument MAKE-FIELDS will create yasnippet compatible
field that the can be jumped to upon further expansion."
    (let* ((indent (concat "\n" (make-string (current-column) 32)))
           (args (yasnippet-radical-snippets--python-split-args text))
    	   (nr 0)
           (formatted-args
    	    (mapconcat
    	     (lambda (x)
    	       (concat "   " (nth 0 x)
    		           (if make-fields (format " ${%d:arg%d}" (cl-incf nr) nr))
    		           (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
    	     args
    	     indent)))
      (unless (string= formatted-args "")
        (concat
         (mapconcat 'identity
    		        (list "" "Args:" formatted-args)
    		        indent)
         "\n")))))

(use-package pyvenv
  :hook
  (python-mode . pyvenv-try-activate)
  (pyvenv-post-activate . (lambda () (pyvenv-restart-python)))
  :preface
  (defun pyvenv-try-activate ()
    (pyvenv-mode t)
    (if (not pyvenv-virtual-env)
        (pyvenv-activate (concat (projectile-project-root) "venv"))
        (pyvenv-activate (concat (projectile-project-root) ".venv")))
    (if (not pyvenv-virtual-env)
        (pyvenv-activate (read-directory-name "Activate venv: " nil nil nil
					                          pyvenv-default-virtual-env-name)))))

(use-package pyinspect
  :bind
  (:map python-mode-map
        ("C-c C-i" . pyinspect-inspect-at-point)))

(use-package python-pytest)

(use-package py-isort
  :hook (before-save . py-isort-before-save))

(use-package dap-python :ensure dap-mode)

;; (use-package tox :custom (tox-runner py.test))
;; (use-package poetry)

(use-package perl
  :hook (cperl-mode . lsp-deferred)
  :mode ("\\.pl\\'" . cperl-mode))

(use-package tcl
  :custom
  (tcl-application "tclsh"))

(provide 'usta-prog-lang)
