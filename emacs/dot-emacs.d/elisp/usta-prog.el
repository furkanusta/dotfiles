;; -*- lexical-binding: t; -*-

(use-package prog-mode :ensure nil
  :preface
  (defun eos/previous-function ()
    (interactive)
    (beginning-of-defun))
  (defun eos/next-function ()
    (interactive)
    (beginning-of-defun -1))
  :custom
  (tab-always-indent 'complete)
  :bind
  (:map prog-mode-map
        ("C-c C-p" . eos/previous-function)
        ("C-c C-n" . eos/next-function)))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package projectile
  :commands projectile-project-name projectile-project-root
  :preface (defun my-open-readme ()
          (let* ((project-root (projectile-project-root))
                 (project-files (directory-files project-root nil nil t))
                 (readme-files (seq-filter (lambda (file) (string-prefix-p "readme" file t)) project-files)))
            (if readme-files
                (let ((readme-file (car readme-files)))
                  (find-file (expand-file-name readme-file project-root)))
              (find-file (expand-file-name "README.org" project-root)))))
  :custom
  (projectile-mode 1)
  (projectile-enable-caching t)
  (projectile-auto-discover nil)
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
  (projectile-switch-project-action 'my-open-readme)
  (projectile-known-projects-file (concat no-littering-var-directory "projectile-bookmarks.eld"))
  (projectile-sort-order 'recentf)
  (projectile-inedxing-method 'hybrid)
  (projectile-dynamic-mode-line nil)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package persp-projectile
  :bind (:map projectile-command-map ("p" . projectile-persp-switch-project)))

(use-package perspective
  :hook (after-init . persp-mode)
  :commands persp-current-buffers
  :preface
  (defvar perspective-skip-ignore-list
  '("*dashboard*" "*Messages*" "*Warnings*" "*elfeed-search*" "*Fd*" "*compilation*" "*Bufler*" "*Easy-hugo*"))
  (defvar perspective-skip-prefix-list '("magit-"))
  (defvar perspective-skip-ignore-prefix-list
  '("*vterm" "*scratch" "*shell" "*Customize" "*ielm*" "*helpful" "*org" "*ein" "*Org" "*Embark" "*cardboard" "*eww" "*sly"))
  (defun perspective-my-skip-buffer-p (_ buffer _)
    (let ((name (buffer-name buffer)))
      (or
       (and
        (char-equal ?* (seq-elt name 0))
        (not (seq-contains-p perspective-skip-ignore-list name))
        (cl-every
         (lambda (x) x)
         (mapcar (lambda (pref) (not (string-prefix-p pref name))) perspective-skip-ignore-prefix-list)))
       (cl-some
        (lambda (x) x)
        (mapcar (lambda (pref) (string-prefix-p pref name)) perspective-skip-prefix-list))
       (not (seq-contains-p (persp-current-buffers) buffer)))))
  :custom
  (persp-mode-prefix-key (kbd "C-c w"))
  (persp-modestring-short t)
  ;; (persp-initial-frame-name ".dotfiles")
  (switch-to-prev-buffer-skip #'perspective-my-skip-buffer-p))

(use-package magit
  :bind ("C-c g s" . magit-status)
  :init (setq magit-define-global-key-bindings nil)
  :custom
  (magit-define-global-key-bindings nil)
  (magit-blame-echo-style 'headings)
  (magit-repository-directories (list (cons (file-truename "~/Projects") 1))))

(use-package magit-delta
  ;; :hook (magit-mode . magit-delta-mode)
  :custom (magit-delta-default-dark-theme "Monokai Extended Origin"))

(use-package magit-todos
  :config
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("T " "Todos" magit-todos-jump-to-todos)))

(use-package git-link
  :custom (git-link-use-commit t))

(use-package git-timemachine)

(use-package git-messenger
  :custom
  (git-messenger:show-detail t)
  (git-messenger:use-magit-popup t)
  :bind ("C-c g m" . git-messenger:popup-message))

(use-package why-this
  :config
  (setq why-this-annotate-heat-map-cold "#203448")
  (setq why-this-annotate-heat-map-warm "#382f27")
  :bind
  ("C-c g b a" . why-this-annotate)
  ("C-c g b b" . why-this-mode))

(use-package github-review)

(use-package git-modes)

(use-package magit-pretty-graph
  :quelpa (magit-pretty-graph :fetcher github :repo "georgek/magit-pretty-graph")
  :after magit
  :commands magit-pg-repo
  :preface
  (defun magit-pretty-log ()
    (interactive)
    (magit-pg-repo (or (projectile-project-root) default-directory))
    (with-current-buffer (get-buffer "*magit-prettier-graph*")
      (view-mode +1)))
  :init
  (transient-append-suffix 'magit-log "l" '("p" "Pretty Log" magit-pretty-log)))

(use-package copy-as-format
  :custom (copy-as-format-default "github"))

(use-package diff-hl
  :hook (find-file . diff-hl-mode))

(use-package hl-todo
  :custom (global-hl-todo-mode 1))

(use-package abridge-diff
  :after magit
  :init (abridge-diff-mode 1))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :commands flycheck-add-mode
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc verilog-verilator))
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (flycheck-add-mode 'proselint 'lsp-mode)
  (flycheck-add-mode 'c/c++-cppcheck 'c++-mode)
  (flycheck-add-mode 'python-mypy 'python-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :custom (flycheck-pos-tip-mode 1))

(use-package flymake)

(use-package flymake-collection)

(use-package flymake-cursor)

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))


(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(use-package vterm
  :commands (vterm-next-prompt vterm-prev-prompt)
  :defines (vterm-mode-map vterm-copy-mode-map)
  :hook (vterm-mode . set-no-process-query-on-exit)
  :preface
  (defun vterm-next-prompt () (interactive)
         (re-search-forward "\\] \\$ " nil 'move))
  (defun vterm-prev-prompt () (interactive)
         (move-beginning-of-line nil)
         (re-search-backward "\\] .*\\$ " nil 'move)
         (re-search-forward "\\] \\$ " nil 'move))
  :config
  (add-to-list 'display-buffer-alist (cons "\\*vterm" use-other-window-alist))
  :custom
  (vterm-copy-exclude-prompt t)
  :bind
  (:map vterm-mode-map
        ("C-y" . vterm-yank)
        ("M-y" . vterm-yank-pop))
  (:map vterm-copy-mode-map
        ("C-<" . vterm-prev-prompt)
        ("C->" . vterm-next-prompt)))

(use-package vterm-toggle
  :custom (vterm-toggle-cd-auto-create-buffer nil)
  :bind
  ("<f8>" . vterm-toggle)
  (:map vterm-mode-map
        ("<f8>" . vterm-toggle)
        ("C-c n"  . vterm-toggle-forward)
        ("C-c p"  . vterm-toggle-backward)
        ("C-<return>" . vterm-toggle-insert-cd)))

(use-package shx
  :hook (shell-mode . shx-mode))

(use-package compile :ensure nil
  :preface
  (make-variable-buffer-local 'my-compilation-start-time)
  (defun my-compilation-start-hook (_)
    (setq my-compilation-start-time (current-time)))
  (defun my-compilation-finish-function (buf why)
    (let* ((elapsed  (time-subtract nil my-compilation-start-time))
           (msg (format "Elapsed: %s" (format-time-string "%T.%N" elapsed t))))
      (if (get-buffer-window "*compilation*")
          (when (string-match "finished" why)
  	        (bury-buffer "*compilation*")
            (popper-close-latest))
        (save-excursion (goto-char (point-max)) (insert msg))
        (alert (format "Emacs: %s at %s" why (buffer-name buf)))
        (message "Compilation %s: %s" (string-trim-right why) msg))))
  :hook
  (compilation-start . my-compilation-start-hook)
  :custom (compilation-scroll-output t)  
  :config
  (add-hook 'compilation-finish-functions #'my-compilation-finish-function)
  :bind ("C-c C-r" . recompile))

(use-package isend-mode
  :bind
  ("C-M-<return>" . isend-send))

(use-package repl-toggle
  :custom
  (rtog/mode-repl-alist (list (cons 'emacs-lisp-mode  #'ielm)
                              (cons 'tcl-mode #'(lambda () (call-interactively #'inferior-tcl)))))
  (rtog/goto-buffer-fun 'pop-to-buffer)
  :bind ("C-c C-z" . rtog/toggle-repl))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package electric-operator
  :hook (cc-mode . electric-operator-mode))

(use-package yasnippet-snippets)

(use-package yasnippet
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :bind (:map yas-minor-mode-map ("<tab>" . nil)))

(use-package tramp
  :commands (tramp-cleanup-all-connections tramp-cleanup-all-buffers)
  :config (defun tramp-done ()
            (interactive)
            (tramp-cleanup-all-connections)
            (tramp-cleanup-all-buffers))
  :custom
  (tramp-backup-directory-alist backup-directory-alist))

(use-package auto-highlight-symbol
  :custom (global-auto-highlight-symbol-mode t))

(use-package origami
  :hook (prog-mode . origami-mode)
  :bind
  ("C-c ," . origami-toggle-node)
  ("C-c C-." . origami-close-all-nodes)
  ("C-c C->" . origami-open-all-nodes))

(use-package which-key)

(use-package lsp-mode
  :hook
  ((lsp-completion-mode . my/lsp-mode-setup-completion)
   (lsp-completion-mode . (lambda ()
                            (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))))
  :custom
  (lsp-completion-provider :none)
  (lsp-auto-execute-action nil)
  (lsp-before-save-edits nil)
  (lsp-keymap-prefix "C-c C-l")
  (lsp-enable-snippet t)
  (lsp-enable-xref t)
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-semantic-highlighting nil)
  (lsp-enable-on-type-formatting nil)
  (read-process-output-max (* 2 1024 1024))
  (lsp-enable-on-type-formatting nil)
  :preface
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :init
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :after lsp
  :bind
  (:map lsp-mode-map
        ("C-c C-l h g" . lsp-ui-doc-show)
        ("C-c C-l h i" . lsp-ui-sideline-toggle-symbols-info)
        ("C-c C-l h m" . lsp-ui-imenu)
        ("C-c t i" . lsp-ui-imenu))
  (:map lsp-ui-imenu-mode-map
        ("TAB" . lsp-ui-imenu--next-kind)
        ("<backtab>" . lsp-ui-imenu--prev-kind)))

(use-package dap-mode
  :custom
  (dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip repl)))

(use-package gdb-mi
  :custom
  (gdb-many-windows t))

(use-package hl-prog-extra
  :hook (prog-mode . hl-prog-extra-mode))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ("C-c TAB" . hs-toggle-hiding)))

(use-package text-categories
  :quelpa (text-categories :fetcher github :repo "Dspil/text-categories")
  :init
  (defun my-text-categories-filename ()
    "Return a filename corresponding to the current buffer."
    (concat no-littering-var-directory "text-categories/" (buffer-name) text-categories-file-suffix))
  (advice-add 'text-categories-filename :override #'my-text-categories-filename))

(use-package subword
  :hook ((yaml-mode conf-mode java-mode js-mode) . subword-mode))

(use-package bug-reference
  :hook ((prog-mode org-mode) . bug-reference-prog-mode))

(use-package devdocs
  :hook (devdocs-mode . shrface-mode)
  ;; (python-mode . (lambda () (setq-local devdocs-current-docs '("python~3.9"))))
  :bind
  ("C-h D" . devdocs-lookup))

(use-package fancy-compilation
  :hook (compilation-mode . fancy-compilation-mode))

(use-package obvious
  :quelpa (obvious :fetcher github :repo "alphapapa/obvious.el"))

(provide 'usta-prog)
