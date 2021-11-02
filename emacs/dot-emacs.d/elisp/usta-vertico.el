;; -*- lexical-binding: t; -*-

(use-package vertico
  :demand t
  :defines vertico-mode
  :config (vertico-mode)
  :quelpa (vertico :fetcher github :repo "minad/vertico" :files ("*.el" "extensions/*.el"))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  (vertico-resize 'grow)
  (vertico-count 15)
  (vertico-cycle t)
  (completion-in-region-function
   (lambda (&rest args)
     (apply (if vertico-mode
                #'consult-completion-in-region
              #'completion--in-region)
            args)))
  :preface
  (defun my-vertico-insert-and-exit () (interactive) (progn (vertico-insert) (exit-minibuffer)))
  :bind
  (:map vertico-map
        ("M-q" . vertico-quick-insert)
        ("C-q" . vertico-quick-exit)
        ("TAB" . vertico-insert)
        ("RET" . my-vertico-insert-and-exit)
        ("?" . minibuffer-completion-help)))

(use-package vertico-directory
  :ensure vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :custom
  (completion-styles '(substring orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (orderless partial-completion)))))
  :config
  (set-face-attribute 'completions-first-difference nil :inherit nil))

(use-package savehist :init (savehist-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :preface
  (defvar consult--fd-command nil)
  (defun consult--fd-builder (input)
    (unless consult--fd-command
      (setq consult--fd-command
            (if (eq 0 (call-process-shell-command "fdfind"))
                "fdfind"
              "fd")))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended)))
      (when re
        (list :command (append
                        (list consult--fd-command
                              "--color=never" "--full-path"
                              (consult--join-regexps re 'extended))
                        opts)
              :highlight hl))))
  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
           (default-directory (cdr prompt-dir)))
      (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (defun my-consult-ripgrep ()
    (interactive)
    (consult-ripgrep (or (projectile-project-root) default-directory) (thing-at-point 'symbol)))
  :bind
  ;; ("C-c h" . consult-history)
  ;; ("C-c m" . consult-mode-command)
  ("M-g b" . consult-bookmark)
  ("C-c k" . consult-kmacro)
  ("C-x M-:" . consult-complex-command)
  ("C-x b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-x 5 b" . consult-buffer-other-frame)
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store)
  ("C-M-#" . consult-register)
  ("M-y" . consult-yank-pop)
  ("<help> a" . consult-apropos)
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flymake)
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("M-g o" . consult-outline)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ("M-s f" . consult-find)
  ("M-s F" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("C-c h s" . my-consult-ripgrep)
  ("C-c h f" . consult-fd)
  ("M-s e" . consult-isearch-history)
  ("C-s" . consult-line-symbol-at-point)
  (:map isearch-mode-map
        ("M-e" . consult-isearch)
        ("M-s e" . consult-isearch)
        ("M-s l" . consult-line)
        ("M-s L" . consult-line-multi))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-preview-key (kbd "M-.")
        consult-narrow-key "<")
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :config
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package consult-company
  :bind (:map company-mode-map ([remap completion-at-point] . consult-company)))

(use-package embark
  :bind
  ("C-." . embark-act)
  ("C-;" . embark-dwim)
  ("C-h B" . embark-bindings)
  (:map embark-file-map
        ("s" . sudo-edit))
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config (add-to-list 'display-buffer-alist
                       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                         nil
                         (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :init (marginalia-mode)
  :custom
  (marginalia-command-categories
   '((imenu . imenu)
     (persp-switch-to-buffer . buffer)
     (projectile-find-file . project-file))))

(use-package all-the-icons-completion
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))


(use-package corfu
  :quelpa (corfu :fetcher github :repo "minad/corfu")
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-echo-documentation nil) ;; Do not show documentation in the echo area
  :bind
  ("C-<tab>" . corfu-complete)
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

(use-package citar
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :after (embark bibtex-completion)
  :config
  ;; Make the 'citar' bindings and targets available to `embark'.
  (add-to-list 'embark-target-finders 'citar-citation-key-at-point)
  (add-to-list 'embark-keymap-alist '(bib-reference . citar-map))
  (add-to-list 'embark-keymap-alist '(citation-key . citar-buffer-map))
  (setq citar-bibliography (f-glob "*.bib" my-bibliography-directory)))


;; (use-package citar-org
;;   :quelpa (citar-org :fetcher github :repo "bdarcus/citar" :files ("citar-org.el"))
;;   :bind (("C-c b" . org-cite-insert)
;;          ("M-o" . org-open-at-point)
;;          :map minibuffer-local-map
;;          ("M-b" . citar-insert-preset))
;;   :after (embark oc)
;;   :config
;;   (setq citar-bibliography my/bibs
;;         org-cite-global-bibliography my/bibs
;;         org-cite-insert-processor 'citar;
;         org-cite-follow-processor 'citar
;;         org-cite-activate-processor 'citar))

(setq citar-at-point-function 'embark-act)

;; use consult-completing-read for enhanced interface
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

(provide 'usta-vertico)
