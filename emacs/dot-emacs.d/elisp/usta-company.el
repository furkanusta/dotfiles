
;; Generic
(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-backends '(company-cmake company-capf company-clang))
  (company-idle-delay nil))

(use-package company-files :ensure company
  :after company
  :config (push 'company-files company-backends))

(use-package company-statistics
  :after company
  :hook (company-mode . company-statistics-mode)
  :custom (company-statistics-file (concat no-littering-var-directory "company-statistics-cache.el")))

(use-package company-quickhelp
  :custom (company-quickhelp-mode t))

(use-package company-graphviz-dot :ensure nil)

(use-package company-flx
  :hook (company-mode . company-flx-mode))

(use-package company-org-block
  :custom (company-org-block-edit-style 'inline) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(provide 'usta-company)
