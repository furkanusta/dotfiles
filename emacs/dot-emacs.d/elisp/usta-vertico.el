;; -*- lexical-binding: t; -*-

(use-package vertico
  :defines vertico-mode
  :quelpa (vertico :fetcher github :repo "minad/vertico" :files ("*.el" "extensions/*.el"))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)
         (vertico-mode . minibuffer-vertico-setup)
         (minibuffer-setup . minibuffer-vertico-setup))
  :preface
  (require 'dired)
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
                       (ignore-errors (auto-minor-mode-enabled-p (intern cand))))))
        (add-face-text-property 0 len '(:foreground "red") 'append cand)))
    cand)
  (defun minibuffer-vertico-setup ()
    (setq truncate-lines t)
    (setq completion-in-region-function
          (if vertico-mode
              #'consult-completion-in-region
            #'completion--in-region)))
  :custom ((vertico-resize 'grow)
           (vertico-count 15)
           (vertico-cycle t)
           (vertico-indexed-mode t)
           (completion-in-region-function (lambda (&rest args)
                                            (apply (if vertico-mode
                                                       #'consult-completion-in-region
                                                     #'completion--in-region)
                                                   args))))
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
  :preface
  ;; From doomemacs
  (defun vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  :custom
  (orderless-matching-styles '(orderless-regexp))
  (orderless-style-dispatchers '(vertico-orderless-dispatch))
  (completion-styles '(substring orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (orderless partial-completion)))))
  ;; (orderless-matching-styles  '(orderless-literal orderless-prefixes))
  :config
  (set-face-attribute 'completions-first-difference nil :inherit nil))

(use-package consult
  :after projectile
  :defines projectile-project-root
  :hook
  ((completion-list-mode . consult-preview-at-point-mode)
   (minibuffer-setup . consult-initial-narrow))
  :preface
  (defvar consult--fd-command (if (eq 0 (call-process-shell-command "fdfind")) "fdfind" "fd"))
  (defvar consult--initial-narrow-list nil)
  (defun consult-initial-narrow ()
    (dolist (l consult--initial-narrow-list)
      (when (eval (car l))
        (setq unread-command-events (append unread-command-events (cdr l))))))
  (defun consult--fd-builder (input)
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler arg 'extended t)))
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

(use-package consult-dir
  :preface
  (defun consult-dir--tramp-docker-hosts ()
    "Get a list of hosts from docker."
    (when (require 'docker-tramp nil t)
      (let ((hosts)
            (docker-tramp-use-names t))
        (dolist (cand (docker-tramp--parse-running-containers))
          (let ((user (unless (string-empty-p (car cand))
                        (concat (car cand) "@")))
                (host (car (cdr cand))))
            (push (concat "/docker:" user host ":/") hosts)))
        hosts)))
  (defvar consult-dir--source-tramp-docker
    `(:name     "Docker"
                :narrow   ?d
                :category file
                :face     consult-file
                :history  file-name-history
                :items    ,#'consult-dir--tramp-docker-hosts)
    "Docker candiadate source for `consult-dir'.")
  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-docker t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  :bind ("C-x C-d" . consult-dir))

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
  ("C-," . embark-dwim)  
  (:map vertico-map
        ("C-<tab>" . embark-act-with-completing-read)
        ("C-." . embark-act)
        ("C-," . embark-dwim)
        ("C-:" . embark-export))
  (:map embark-file-map
        ("L" . vlf)
        ("S" . sudo-find-file)))

(use-package embark-consult
  :after (embark consult)
  :demand t
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
         (comint-mode . corfu-mode)
         (cmake-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-preselect-first t)
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
  :quelpa (corfu-history :fetcher github :repo "minad/corfu" :files ("extensions/corfu-history.el"))
  :after savehist
  :hook (corfu-mode . corfu-history-mode)
  :config (add-to-list 'savehist-additional-variables 'corfu-history--list))

(use-package corfu-doc
  :if (display-graphic-p)
  :hook (corfu-mode . corfu-doc-mode))

(use-package corfu-doc-terminal
  :quelpa (corfu-doc-terminal :fetcher git :url "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
  :unless (display-graphic-p)
  :hook (corfu-mode . corfu-doc-terminal-mode))

;; Add extensions
(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions
               (cape-super-capf #'cape-symbol #'cape-keyword #'cape-dabbrev)))


(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package citar
  :preface
  (defun my-citar-open-current-resource (files notes)
    "Open REFs of the node at point."
    (interactive)
    (if-let ((keys (when-let* ((prop (org-entry-get (point) "ROAM_REFS" t))
                            (refs (when prop (split-string-and-unquote prop)))
                            (oc-cites
                             (seq-map (lambda (ref) (substring ref 7 (- (length ref) 1))) refs)))
                  oc-cites))
             (selected (let* ((actions (bound-and-true-p embark-default-action-overrides))
                              (embark-default-action-overrides `((t . ,#'citar--open-resource) . ,actions)))
                         (citar--select-resource keys
                                                 :files files :notes notes :always-prompt nil))))
        (progn
          (when (= 1 (length (window-list)))
            (split-window-horizontally))
          (other-window 1)
          (citar--open-resource (cdr selected) (car selected))
          (when (eq 'file (car selected))
            (pdf-view-fit-width-to-window)))
      (user-error "No ROAM_REFS found")))
  (defun my-citar-open-current-note () (interactive) (my-citar-open-current-resource nil t))
  (defun my-citar-open-current-file () (interactive) (my-citar-open-current-resource t nil))
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
  ("C-c o N" . citar-open-notes)
  ("C-c o n" . my-citar-open-current-note)
  ("C-c o p" . my-citar-open-current-file)
  ("C-c o P" . citar-open-files))

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

(use-package citar-org-roam
  :quelpa (citar-org-roam :fetcher github :repo "emacs-citar/citar-org-roam"))

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
  :bind
  (:map flycheck-mode-map
        ("C-c h l" . consult-flycheck)))

(use-package consult-flyspell
  :bind
  (:map flyspell-mode-map
        ("C-c h s" . consult-flyspell)))

(use-package vertico-repeat :ensure vertico
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("M-r" . vertico-repeat))

(use-package consult-yasnippet
  :load-path "~/Projects/Contribute/consult-yasnippet/"
  :custom
  (consult-yasnippet-use-thing-at-point t)
  :bind ("M-i" . consult-yasnippet)
  :config
  (consult-customize consult-yasnippet :preview-key '(:debounce 0.01 any)))

(use-package consult-lsp
  :after lsp
  :config
  (add-to-list 'consult--initial-narrow-list (cons `(eq this-command #'consult-lsp-symbols) (list ?< ?c))))

(provide 'usta-vertico)
