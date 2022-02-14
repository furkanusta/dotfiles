;; -*- lexical-binding: t; -*-
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
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . TeX-PDF-mode))
  :custom (LaTeX-electric-left-right-brace t))

(use-package reftex
  :hook (LaTeX-mode . turn-on-reftex)
  :custom (reftex-plug-into-AUCTeX t))

(use-package cdlatex
  :hook (LaTeX-mode . cdlatex-mode))

(use-package auctex-latexmk
  :commands auctex-latexmk-setup
  :config (auctex-latexmk-setup)
  :custom (auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package bibtex
  :custom
  (bibtex-completion-library-path (list my-papers-directory))
  (bibtex-align-at-equal-sign t)
  (bibtex-dialect 'biblatex))

(use-package biblio)

(use-package ebib
  :custom
  (ebib-preload-bib-files my-bibliographies)
  (ebib-bibtex-dialect 'biblatex)
  (ebib-default-directory my-bibliography-directory)
  (ebib-index-window-size 20)
  (ebib-bib-search-dirs (list my-bibliography-directory)))

(use-package magic-latex-buffer
  :hook (latex-mode . magic-latex-buffer))

(use-package latexdiff
  :preface
  (defun latexdiff-wip ()
    "Compile the pdf difference between the choosen commit and the current version of the current file."
    (interactive)
    (let* ((commits (latexdiff--get-commit-hash-alist))
           (commit-hash (cdr (car commits))))
      (latexdiff-vc--compile-diff-with-current commit-hash))))

(use-package lsp-latex
  :demand t
  :hook
  ((latex-mode . lsp)
   (bibtex-mode . lsp))
  :custom
  (lsp-latex-texlab-executable
   (or (executable-find "texlab")
       (expand-file-name "~/.local/prog/texlab/target/release/texlab")))
  (lsp-latex-forward-search-executable "emacsclient")
  (lsp-latex-forward-search-args '("--eval" "(lsp-latex-forward-search-with-pdf-tools \"%f\" \"%p\" \"%l\")")))

(use-package lsp-ltex :demand t
  :custom
  (lsp-ltex-version "15.2.0"))

(use-package xenops
  :hook (latex-mode . xenops-mode)
  :init (setq xenops-reveal-on-entry t))

(provide 'usta-latex)
