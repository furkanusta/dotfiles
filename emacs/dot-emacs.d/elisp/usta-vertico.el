;; -*- lexical-binding: t; -*-

(use-package vertico
  :defines vertico-mode
  :quelpa (vertico :fetcher github :repo "minad/vertico" :files ("*.el" "extensions/*.el"))
  :hook
  (after-init . vertico-mode)
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  (vertico-resize 'grow)
  (vertico-count 15)
  (vertico-cycle t)
  (vertico-indexed-mode t)
  (completion-in-region-function
   (lambda (&rest args)
     (apply (if vertico-mode
                #'consult-completion-in-region
              #'completion--in-region)
            args)))
  :preface
  (defvar +completion-category-hl-func-overrides
    `((file . ,#'+completion-category-highlight-files))
    "Alist mapping category to highlight functions.")
  (defun +completion-category-highlight-files (cand)
  (let ((len (length cand)))
    (when (and (> len 0)
               (eq (aref cand (1- len)) ?/))
      (add-face-text-property 0 len 'dired-directory 'append cand)))
  cand)
  (defun my-vertico-insert-and-exit ()
    (interactive)
    (progn
      (vertico-insert)
      (exit-minibuffer)))
  (defun auto-minor-mode-enabled-p (minor-mode)
    "Return non-nil if MINOR-MODE is enabled in the current buffer."
    (and (memq minor-mode minor-mode-list)
         (symbol-value minor-mode)))
  (defun +completion-category-highlight-commands (cand)
    (let ((len (length cand)))
      (when (and (> len 0)
                 (with-current-buffer (nth 1 (buffer-list))
                   (or (eq major-mode (intern cand))
                       (auto-minor-mode-enabled-p (intern cand)))))
        (add-face-text-property 0 len '(:foreground "red") 'append cand)))
    cand)
  :config
  (advice-add #'vertico--arrange-candidates :around
            (defun vertico-format-candidates+ (func)
              (let ((hl-func (or (alist-get (vertico--metadata-get 'category)
                                            +completion-category-hl-func-overrides)
                                 #'identity)))
                (cl-letf* (((symbol-function 'actual-vertico-format-candidate)
                            (symbol-function #'vertico--format-candidate))
                           ((symbol-function #'vertico--format-candidate)
                            (lambda (cand &rest args)
                              (apply #'actual-vertico-format-candidate
                                     (funcall hl-func cand) args))))
                  (funcall func)))))
  (add-to-list '+completion-category-hl-func-overrides `(command . ,#'+completion-category-highlight-commands))
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
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (orderless partial-completion)))))
  (orderless-matching-styles  '(orderless-literal orderless-prefixes))
  :config
  (set-face-attribute 'completions-first-difference nil :inherit nil))

(use-package savehist :init (savehist-mode))

(use-package consult
  :after projectile
  :defines projectile-project-root
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
  (defun my-consult-rga-here ()
    (interactive)
    (let ((consult-ripgrep-args (concat "rga" (substring consult-ripgrep-args 2) " --no-ignore")))
      (consult-ripgrep default-directory)))
  (defun my-consult-ripgrep-here ()
    (interactive)
    (let ((consult-ripgrep-args (concat consult-ripgrep-args " --no-ignore")))
    (consult-ripgrep default-directory (thing-at-point 'symbol))))
  (defun my-consult-ripgrep ()
    (interactive)
    (consult-ripgrep (or (projectile-project-root) default-directory) (thing-at-point 'symbol)))
  :bind
  ;; ("C-c h" . consult-history)
  ;; ("C-c m" . consult-mode-command)
  ("M-g b" . consult-bookmark)
  ;; ("C-c k" . consult-kmacro)
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
  ("C-c h a" . my-consult-rga-here)
  ("C-c h S" . my-consult-ripgrep-here)
  ("C-c h f" . consult-fd)
  ("M-s e" . consult-isearch-history)
  ("C-s" . consult-line-symbol-at-point)
  (:map isearch-mode-map
        ("M-e" . consult-isearch)
        ("M-s e" . consult-isearch)
        ("M-s l" . consult-line)
        ("M-s L" . consult-line-multi))
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :custom
  (consult-project-root-function #'projectile-project-root)
  (register-preview-delay 0)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-preview-key (kbd "M-."))
  (consult-narrow-key "<")
  (consult-ripgrep-args
   "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number .")
  :config
  (consult-customize
   consult-line-symbol-at-point my-consult-ripgrep my-consult-ripgrep-here
   consult-ripgrep consult-git-grep consult-grep consult-line
   consult--source-recent-file consult--source-project-recent-file
   consult-bookmark consult-recent-file consult-xref consult--source-bookmark
   :preview-key '(:debounce 0.01 any))
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

(use-package consult-company
  :bind (:map company-mode-map ([remap completion-at-point] . consult-company)))

(use-package embark
  :after vertico
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :custom
  (embark-indicators '(embark-verbose-indicator))
  :preface
  (defun sudo-find-file (file)
    "Open FILE as root."
    (interactive "FOpen file as root: ")
    (when (file-writable-p file)
      (user-error "File is user writeable, aborting sudo"))
    (find-file (if (file-remote-p file)
                   (concat "/" (file-remote-p file 'method) ":"
                           (file-remote-p file 'user) "@" (file-remote-p file 'host)
                           "|sudo:root@"
                           (file-remote-p file 'host) ":" (file-remote-p file 'localname))
                 (concat "/sudo:root@localhost:" file))))
  (defun with-minibuffer-keymap (keymap)
    (lambda (fn &rest args)
      (minibuffer-with-setup-hook
          (:append (lambda ()
                     (use-local-map
                      (make-composed-keymap keymap (current-local-map)))))
        (apply fn args))))
  (defvar embark-completing-read-prompter-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-<tab>") 'abort-recursive-edit)
      map))
  (defun embark-act-with-completing-read (&optional arg)
    (interactive "P")
    (let* ((embark-prompter 'embark-completing-read-prompter)
           (act (propertize "Act" 'face 'highlight))
           (embark-indicators '(embark-minimal-indicator)))
      (embark-act arg)))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (advice-add 'embark-completing-read-prompter :around
              (with-minibuffer-keymap embark-completing-read-prompter-map))
  :bind
  ("C-." . embark-act)
  ("C-;" . embark-dwim)
  ("C-:" . embark-export)
  ("C-h B" . embark-bindings)
  (:map vertico-map
        ("C-<tab>" . embark-act-with-completing-read)
        ("C-." . embark-act))
  (:map embark-file-map
        ("s" . sudo-edit)
        ("S" . sudo-find-file)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :init (marginalia-mode)
  :custom
  (marginalia-align 'center)
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
         (cmake-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-commit-predicate #'corfu-candidate-previewed-p)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-preview-current 'insert)
  :config
  (require 'kind-icon)
  :bind
  ("C-<tab>" . completion-at-point)
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("C-SPC" . corfu-insert-separator)))

(use-package savehist
  :hook (after-init . savehist-mode))

(use-package corfu-history
  :after savehist
  :load-path "elisp/"
  :hook (corfu-mode . corfu-history-mode)
  :custom (corfu-sort-function #'corfu-history--sort)
  :config (add-to-list 'savehist-additional-variables 'corfu-history--list))

(use-package corfu-doc
  :hook (corfu-mode . corfu-doc-mode))

;; Add extensions
(use-package cape
  :config
  (add-to-list 'completion-at-point-functions (cape-super-capf #'cape-file #'cape-keyword #'cape-dabbrev))
  (require 'company-cmake)
  (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-cmake)))


(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package citar
  :after org-roam
  :preface
  (defun my-citar-open-current-pdf ()
    "Open REFs of the node at point."
    (interactive)
    (let ((keys (save-excursion
                  (goto-char (org-roam-node-point (org-roam-node-at-point 'assert)))
                  (when-let* ((p (org-entry-get (point) "ROAM_REFS"))
                              (refs (when p (split-string-and-unquote p)))
                              (refs (if (length> refs 1) (completing-read-multiple "Open: " refs) refs))
                              (oc-cites
                               (seq-map
                                (lambda (ref) (substring ref 7 (- (length ref) 1)))
                                (seq-filter (apply-partially #'string-prefix-p "[cite:@") refs))))
                    oc-cites))))
      (if keys
          (progn
            (other-window 1)
            (citar-open-library-file keys))
        (user-error "No ROAM_REFS found"))))
  :custom
  (citar-bibliography my-bibliographies)
  (citar-open-note-function 'orb-citar-edit-note)
  (citar-library-paths (list my-papers-directory))
  (citar-at-point-function 'embark-act)
  (citar-symbols
   `((file ,(all-the-icons-octicon "file-pdf" :face 'all-the-icons-green :v-adjust -0.1) . " ")
     (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
     (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (citar-symbol-separator " ")
  :bind
  ("C-c o b" . citar-open)
  ("C-c o P" . my-citar-open-current-pdf)
  ("C-c o p" . citar-open-library-file))

(use-package citar-embark
  :no-require t
  :ensure citar
  :after embark
  :preface
  (defvar my-citar-embark-become-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "f") 'citar-open-library-files)
      (define-key map (kbd "x") 'biblio-arxiv-lookup)
      (define-key map (kbd "c") 'biblio-crossref-lookup)
      (define-key map (kbd "i") 'biblio-ieee-lookup)
      (define-key map (kbd "h") 'biblio-hal-lookup)
      (define-key map (kbd "s") 'biblio-dissemin-lookup)
      (define-key map (kbd "b") 'biblio-dblp-lookup)
      (define-key map (kbd "o") 'biblio-doi-insert-bibtex)
      map)
    "Citar Embark become keymap for biblio lookup.")
  (defun my-citar-embark-open-pdf (keys-entries)
    (interactive (list (citar-select-refs :rebuild-cache current-prefix-arg)))
    (let* ((entry (car keys-entries))
           (key (car entry)))
      (citar-file-open
       (car
        (citar-file--files-for-entry key nil citar-library-paths citar-file-extensions)))))
  :config
  (add-to-list 'embark-become-keymaps 'my-citar-embark-become-map)
  (add-to-list 'embark-keymap-alist '(bib-reference . citar-map))
  (add-to-list 'embark-keymap-alist '(citar-reference . citar-map))
  (add-to-list 'embark-keymap-alist '(citation-key . citar-buffer-map))
  :bind (:map citar-map
              ("M-RET" . my-citar-embark-open-pdf)))

(use-package citar-org
  :quelpa (citar-org :fetcher github :repo "bdarcus/citar" :files ("citar-org.el"))
  :after (citar oc)
  :custom
  (org-cite-global-bibliography citar-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-file-note-org-include '(org-id org-roam-ref))
  :bind (("M-o" . org-open-at-point)
         (:map org-mode-map ("C-c i c" . org-cite-insert))
         (:map minibuffer-local-map ("M-b" . citar-insert-preset))))

(use-package consult-flycheck
  :bind ("C-c l f" . consult-flycheck))

(use-package consult-tramp :load-path "elisp/")

(provide 'usta-vertico)
