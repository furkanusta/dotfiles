;; -*- lexical-binding: t; -*-
;; Generic
(use-package company
  ;; :hook (prog-mode . company-mode)
  :custom
  (company-backends '(company-cmake company-capf company-files))
  (completion-ignore-case t)
  (company-idle-delay nil)
  :bind
  ("C-<tab>" . company-complete))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-files :ensure company
  :after company
  :config (add-to-list 'company-backends 'company-files))

(use-package company-statistics
  :after company
  :hook (company-mode . company-statistics-mode)
  :custom (company-statistics-file (concat no-littering-var-directory "company-statistics-cache.el")))

(use-package company-quickhelp
  :custom (company-quickhelp-mode t))

(use-package company-graphviz-dot :ensure nil)

(use-package company-flx
  :hook (company-mode . company-flx-mode))

(use-package company-reftex
  :after company
  :hook (LaTeX-mode . (lambda (progn
                                (add-to-list 'company-backends #'company-reftex-labels)
                                (add-to-list 'company-backends #'company-reftex-citations)))))

(use-package company-math
  :after company
  :hook (LaTeX-mode . (lambda (progn
                                (add-to-list 'company-backends #'company-math-symbols-unicode)))))

(use-package company-math)

(use-package company-auctex
  :commands company-auctex-init
  :config (company-auctex-init))

(provide 'usta-company)
