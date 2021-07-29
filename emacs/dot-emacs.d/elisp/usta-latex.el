;;;;;;;;;;;;;;;;;;;;;;;;;
;;        LaTeX        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tex :ensure auctex
  :commands TeX-revert-document-buffer
  :config (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :custom
  (TeX-master nil)
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t)
  (TeX-source-correlate-method 'synctex)
  (TeX-electric-math (cons "\\(" "\\)")))

(use-package latex :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :hook ((LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . TeX-PDF-mode))
  :custom (LaTeX-electric-left-right-brace t))

(use-package reftex
  :hook (LaTeX-mode . turn-on-reftex)
  :custom (reftex-plug-into-AUCTeX t))

(use-package cdlatex
  :hook (LaTeX-mode . cdlatex-mode))

(use-package company-auctex
  :commands company-auctex-init
  :config (company-auctex-init))

(use-package auctex-latexmk
  :commands auctex-latexmk-setup
  :config (auctex-latexmk-setup)
  :custom (auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package bibtex
  :custom (bibtex-align-at-equal-sign t))

(use-package biblio)

(use-package ebib
  :custom
  (ebib-preload-bib-files (list my-bibliography))
  (ebib-bibtex-dialect 'biblatex)
  (ebib-default-directory 'first-bib-dir)
  (ebib-index-window-size 20)
  (ebib-bib-search-dirs (list my-papers-directory)))

(provide 'usta-latex)
