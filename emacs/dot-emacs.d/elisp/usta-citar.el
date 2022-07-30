
(use-package citar
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
  ("C-c o P" . citar-open-files))

(use-package my-citar-org
  :no-require t
  :after org
  :init
  (require 'citar)
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
                         (citar--select-resource keys :files files :notes notes))))
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
  :bind
  (:map org-mode-map
        ("C-c o n" . my-citar-open-current-note)
        ("C-c o p" . my-citar-open-current-file)))

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
      map))
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

(provide 'usta-citar)
