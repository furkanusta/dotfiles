;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Helm          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package helm-icons
;;   :after all-the-icons
;;   :init
;;   (setq helm-icons-provider 'all-the-icons)
;;   (helm-icons-enable))

(use-package helm
  ;; :after helm-icons
  :bind
  ((:map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("<left>" . helm-previous-source)
         ("<right>" . helm-next-source)))
  :custom
  (helm-split-window-inside-p t)
  (helm-move-to-line-cycle-in-source t)
  (helm-scroll-amount 8)
  (helm-autoresize-mode 1)
  (helm-mode +1))

(use-package helm-command :ensure helm
  :bind ("M-x" . helm-M-x))

(use-package helm-imenu :ensure helm
  :bind ("<f6>" . helm-imenu))

(use-package helm-ring :ensure helm
  :bind ("M-y" . helm-show-kill-ring))

(use-package helm-buffers :ensure helm
  :custom (helm-boring-buffer-regexp-list (list (rx "*magit-") (rx "*helm") (rx "*flycheck")))
  :bind ("C-x b" . helm-mini))

(use-package helm-files :ensure helm
  :after helm
  :commands (helm-get-selection helm-next-line helm-previous-line helm-ff-move-to-first-real-candidate)
  :preface (defun helm-skip-dots (old-func &rest args)
             (apply old-func args)
             (let ((sel (helm-get-selection)))
               (if (and (stringp sel) (string-match "/\\.$" sel))
                   (helm-next-line 2)))
             (let ((sel (helm-get-selection))) ; if we reached .. move back
               (if (and (stringp sel) (string-match "/\\.\\.$" sel))
                   (helm-previous-line 1))))
  :config (advice-add #'helm-ff-move-to-first-real-candidate :around #'helm-skip-dots)
  :bind ("C-x C-f" . helm-find-files)
  :custom
  (helm-substitute-in-filename-stay-on-remote t)
  (helm-ff-skip-boring-files t)
  (helm-ff-search-library-in-sexp t)
  (helm-ff-file-name-history-use-recentf t)
  (helm-ff-allow-non-existing-file-at-point nil)
  (helm-ff-auto-update-initial-value t)
  (helm-ff-guess-ffap-filenames t)
  (helm-ff-guess-ffap-urls nil))

(use-package helm-bibtex
  :defines my-bibliography
  :custom
  (bibtex-completion-bibliography my-bibliography)
  (bibtex-completion-library-path (concat my-data-directory "/Papers/"))
  (bibtex-completion-find-additional-pdfs t)
  (bibtex-completion-notes-path (concat my-notes-directory "/Papers.org")))

(use-package tramp
  :commands (tramp-cleanup-all-connections tramp-cleanup-all-buffers)
  :config (defun tramp-done ()
            (interactive)
            (tramp-cleanup-all-connections)
            (tramp-cleanup-all-buffers))
  :custom
  (tramp-backup-directory-alist backup-directory-alist))

(use-package helm-tramp)

(use-package helm-swoop
  :bind
  ("C-s" . helm-swoop)
  ("C-c h h" . helm-swoop-back-to-last-point)
  :custom
  (helm-swoop-split-with-multiple-windows nil)
  (helm-swoop-move-to-line-cycle t)
  (helm-swoop-use-fuzzy-match nil)
  (helm-swoop-speed-or-color t))

(use-package helm-rg
  :bind
  ("C-c h S" .  helm-projectile-rg)
  ("C-c h s" .  helm-rg))

(use-package helm-bookmark
  :ensure nil
  :bind ("C-c h b" . helm-bookmarks))

(use-package ace-jump-helm-line
  :after helm
  :bind (:map helm-map ("C-'" . ace-jump-helm-line)))

(use-package flyspell-correct-helm :after flyspell-correct)

(use-package helm-flycheck
  :after flycheck
  :bind (:map flycheck-mode-map ("C-c h f" . helm-flycheck)))

(use-package helm-lsp)

(use-package helm-xref
  :custom (xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package helm-company
  :after helm company
  :bind
  ("C-<tab>" . helm-company)
  (:map shell-mode-map ("<tab>" . helm-company)))

(use-package helm-projectile :after helm projectile
  :custom (projectile-completion-system 'helm)
  :config (helm-projectile-on))

(use-package helm-sly
  :after sly helm-company
  :defines sly-mrepl-mode-map
  :hook (sly-mrepl . company-mode)
  :bind (:map sly-mrepl-mode-map ("<tab>" . helm-company)))

(use-package helm-flx
  :hook (helm-mode . helm-flx-mode)
  :custom (helm-flx-for-helm-locate t))

(use-package helm-flx-historian
  :hook (helm-mode . helm-flx-historian-mode)
  :quelpa (helm-flx-historian :fetcher github :repo "PythonNut/historian.el"))

(use-package helm-pass)

(use-package helm-org-ql)

(use-package helm-bufler)

(provide 'usta-helm)
