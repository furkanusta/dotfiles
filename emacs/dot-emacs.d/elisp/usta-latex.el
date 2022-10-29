;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;
;;        LaTeX        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tex :ensure auctex
  :commands TeX-revert-document-buffer
  :config (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :custom
  ;; (TeX-master nil)
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
  :hook (LaTeX-mode . cdlatex-mode)
  :custom
  (cdlatex-use-dollar-to-ensure-math nil)
  (cdlatex-auto-help-delay 0.5))

(use-package auctex-latexmk
  :after latex
  :functions auctex-latexmk-setup
  :preface
  (defun my-auctex-latexmk-advice (req feature &rest args)
    "Call REQ with FEATURE and ARGS, unless FEATURE is `tex-buf'."
    (unless (eq feature 'tex-buf)
      (apply req feature args)))
  :init
  (unwind-protect
      (progn (advice-add 'require :around #'my-auctex-latexmk-advice)
             (auctex-latexmk-setup))
    (advice-remove 'require #'my-auctex-latexmk-advice))
  :custom (auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package bibtex
  :custom
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

;; (use-package magic-latex-buffer
;;   :hook (LaTeX-mode . magic-latex-buffer))

(use-package latexdiff
  :preface
  (defun latexdiff-wip ()
    "Compile the pdf difference between the choosen commit and the current version of the current file."
    (interactive)
    (let* ((commits (latexdiff--get-commit-hash-alist))
           (commit-hash (cdr (car commits))))
      (latexdiff-vc--compile-diff-with-current commit-hash))))

;; (use-package lsp-latex
;;   :hook
;;   ((LaTeX-mode . lsp-deferred)
;;    (bibtex-mode . lsp-deferred))
;;   :custom
;;   (lsp-latex-texlab-executable
;;    (or (executable-find "texlab")
;;        (expand-file-name "~/.local/prog/texlab/target/release/texlab")))
;;   (lsp-latex-forward-search-executable "emacsclient")
;;   (lsp-latex-forward-search-args '("--eval" "(lsp-latex-forward-search-with-pdf-tools \"%f\" \"%p\" \"%l\")")))

;; (use-package xenops
;;   ;; :hook (LaTeX-mode . xenops-mode)
;;   :init (setq xenops-reveal-on-entry t))

;; (use-package outline
;;   :hook (LaTeX-mode . outline-minor-mode)
;;   :bind (:map outline-minor-mode-map
;;               ("C-c C-n" . )))

;; (use-package lazytab
;;   :quelpa (lazytab :fetcher github :repo "karthink/lazytab"))

;; ;; For some reason calctex skips the first element
;; (use-package calctex
;;   :quelpa (calctex :fetcher github :repo "johnbcoughlin/calctex")
;;   :hook (calc-mode . calctex-mode)
;;   :config
;;   (setq calctex-additional-latex-packages "
;; \\usepackage[usenames]{xcolor}
;; \\usepackage{soul}
;; \\usepackage{adjustbox}
;; \\usepackage{amsmath}
;; \\usepackage{amssymb}
;; \\usepackage{siunitx}
;; \\usepackage{cancel}
;; \\usepackage{mathtools}
;; \\usepackage{mathalpha}
;; \\usepackage{xparse}
;; \\usepackage{arevmath}"
;;         calctex-additional-latex-macros (concat calctex-additional-latex-macros "\n\\let\\evalto\\Rightarrow"))
;;   (defun teco-calctex-fix (orig-fn &rest args)
;;     (let ((inhibit-message t)
;;           message-log-max)
;;       (apply orig-fn args)))
;;   (advice-add #'teco-calctex-fix :around #'calctex-default-dispatching-render-process)
;;   (let ((vendor-folder (concat (file-truename quelpa-build-dir) "/calctex/vendor/")))
;;     (setq calctex-dvichop-sty (concat vendor-folder "texd/dvichop")
;;           calctex-dvichop-bin (concat vendor-folder "texd/dvichop")))
;;   (unless (file-exists-p calctex-dvichop-bin)
;;     (message "CalcTeX: Building dvichop binary")
;;     (let ((default-directory (file-name-directory calctex-dvichop-bin)))
;;       (call-process "make" nil nil nil))))

(use-package laas
  :hook (LaTeX-mode . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "supp" "\\supp"
                    "On" "O(n)"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
                    "Olon" "O(n \\log n)"
                    ;; bind to functions!
                    "Sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                    "Span" (lambda () (interactive)
                             (yas-expand-snippet "\\Span($1)$0"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

(provide 'usta-latex)
