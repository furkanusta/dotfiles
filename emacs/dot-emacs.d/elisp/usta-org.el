;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Org Mode          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :hook
  ((org-mode . turn-on-flyspell)
   (org-mode . auto-fill-mode)
   (org-mode . smartparens-mode))
  :preface
  (defvar org-capture-file (concat my-notes-directory "/Capture.org"))
  (defun my-update-all-bibs ()
    (interactive)
    (let ((buf (current-buffer))
          (files (f-glob "*.org")))
      (dolist (file files)
        (with-current-buffer (find-file file)
          (message "TANGLE: %s" file)
          (org-babel-tangle nil nil "bibtex")))
      (switch-to-buffer buf)))
  :custom
  (org-modules (list 'ol-eww 'org-tempo 'ol-info 'ol-docview 'ol-bibtex 'ol-doi))
  (org-default-notes-file org-capture-file)
  ;; (org-startup-folded 'content)
  (org-adapt-indentation t)
  (org-catch-invisible-edits 'show-and-error)
  (org-cycle-separator-lines 0)
  (org-edit-src-content-indentation 0)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-imenu-depth 2)
  (org-indent-indentation-per-level 1)
  (org-log-done t)
  (org-babel-load-languages '((python . t)
                              (shell . t)
                              (emacs-lisp . t)))
  ;; (org-pretty-entities t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation nil)
  (org-src-tab-acts-natively t)
  (org-yank-adjusted-subtrees t)
  (org-export-with-todo-keywords t)
  (org-export-with-tag nil)
  (org-latex-listings 'minted)
  (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "NEXT" "|" "DONE")
                       (sequence "PAUSED" "SCHEDULED" "WAITING" "|" "CANCELLED")))
  :config
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  :bind
  (:map org-mode-map
        ("C-c C-." . org-time-stamp-inactive)
        ("DEL" . org-delete-backward-char)
        ("M-q" . nil)))

(use-package my-org-bibtex
  :no-require t
  :after (org bibtex)
  :preface
  (defun my-sync-bibtex-org-entry ()
    (interactive)
    (save-excursion
      (call-interactively #'org-next-block)
      (forward-line 1)
      (let* ((entry (bibtex-parse-entry))
             (key (cdr (assoc "=key=" entry)))
             (title (substring (cdr (assoc "title" entry)) 1 -1))
             (fixed-title (replace-regexp-in-string " +" " " (string-replace "\n" " " title))))
        (org-set-property "ROAM_REFS" (format "[cite:@%s]" key))
        (org-back-to-heading)
        (forward-char 3)
        (org-kill-line)
        (insert fixed-title))
      (forward-line 1)
      (org-cycle))))

(use-package my-org-biblio
  :no-require t
  :after (org biblio)
  :preface
  (defun my-insert-bibtex-entry (buffer bibtex-entry)
    (interactive)
    (let* ((title (if bibtex-entry
                      (save-excursion
                        (with-temp-buffer
                          (insert bibtex-entry)
                          (goto-char (point-min))
                          (let* ((title (substring (cdr (assoc "title" (bibtex-parse-entry))) 1 -1))
                                 (title-single (string-replace "\n" " " title))
                                 (title-squeezed (replace-regexp-in-string " +" " " title-single)))
                            title-squeezed)))
                    "Title")))
      (with-current-buffer buffer
        (if (= 1 (org-current-level))
            (org-insert-subheading t)
          (org-insert-heading t))
        (insert title)
        (org-id-get-create)
        (forward-line 1)
        (org-cycle)
        (forward-line 3)
        (insert "   #+begin_src bibtex\n" bibtex-entry "\n   #+end_src")
        (org-edit-src-code)
        (org-edit-src-exit)
        (forward-line 2)
        (insert "\n"))))
  (defun my-insert-bibtex-entry-doi (doi)
    (interactive (list (read-string "DOI: ")))
    (require 'biblio)
    (biblio-doi-forward-bibtex
     (biblio-cleanup-doi doi)
     (apply-partially
      (lambda (buffer result)
        (my-insert-bibtex-entry buffer (biblio-format-bibtex result biblio-bibtex-use-autokey)))
      (current-buffer)))))

(use-package ob-async)

(use-package org-alert
  :commands org-alert-enable
  :config (org-alert-enable))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package org-cliplink
  :bind (:map org-mode-map ("C-c i l" . org-cliplink)))

(use-package org-capture :ensure org
  :custom
  (org-capture-templates '(("t" "TODO" entry (file+headline org-capture-file "Tasks")
  						    "* TODO %?\n	%a\n  %i\n")
  					       ("j" "Journal" entry (file+headline org-capture-file "Journal")
  						    "* %U\n	 %a\n	 %i")
                           ("w" "Web site" entry
                            (file "")
                            "* %a :website:\n\n%U %?\n\n%:initial" :immediate-finish t)
                           ;; Ideas??
                           ;; Habit-Journal (:type table-line)
                           ;; Pocket
                           ;; org-web-tools-insert-web-page-as-entry
                           ;; Snippet
                           ;; Elfeed
  					       ("p" "Protocol" entry (file+headline org-capture-file "Inbox")
  	    				    "* %?\n	 [[%:link][%:description]]\n	%U\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n")
  	    			       ("L" "Protocol Link" entry (file+headline org-capture-file "Inbox")
  						    "* %?\n	 [[%:link][%:description]]\n	%U")))
  :bind ("C-c c" . org-capture))


(use-package org-capture-ref
  :quelpa (org-capture-ref :repo "yantar92/org-capture-ref" :fetcher github)
  :config
  (setq org-capture-templates
        (append org-capture-templates
           (doct '(:group "Browser link"
                          :type entry
                          :headline "REF"
 			              :file org-capture-file
 			              :fetch-bibtex (lambda () (org-capture-ref-process-capture)) ; this must run first
                          :link-type (lambda () (org-capture-ref-get-bibtex-field :type))
                          :extra (lambda ()
                                   (if (org-capture-ref-get-bibtex-field :journal)
					                   (s-join "\n"
                                               '("- [ ] download and attach pdf"
						                         "- [ ] [[elisp:org-attach-open][read paper capturing interesting references]]"
						                         "- [ ] [[elisp:(browse-url (url-encode-url (format \"https://www.semanticscholar.org/search?q=%s\" (org-entry-get nil \"TITLE\"))))][check citing articles]]"
						                         "- [ ] [[elisp:(browse-url (url-encode-url (format \"https://www.connectedpapers.com/search?q=%s\" (org-entry-get nil \"TITLE\"))))][check related articles]]"
                                                 "- [ ] check if bibtex entry has missing fields"))
                                     ""))
                          :org-entry (lambda () (org-capture-ref-get-org-entry))
			              :template
                          ("%{fetch-bibtex}* TODO %?%{space}%{org-entry}"
                           "%{extra}"
                           "- Keywords: #%{link-type}")
			              :children (("Interactive link" :keys "b" :clock-in t :space " " :clock-resume t)
				                     ("Silent link" :keys "B" :space "" :immediate-finish t)))))))

(use-package org-table :ensure org
  :preface
  (defun my-org-copy-table-cell ()
    (interactive)
    (when (org-at-table-p)
      (kill-new
       (string-trim
        (substring-no-properties(org-table-get-field))))))
  :bind
  (:map org-mode-map
        ("M-W" . my-org-copy-table-cell)))

(use-package org-protocol :ensure org)

(use-package org-agenda :ensure org
  :custom
  (org-agenda-show-future-repeats nil)
  (org-agenda-files (list my-notes-directory))
  (org-agenda-include-diary t)
  (org-agenda-span 10)
  (org-agenda-start-day "-2d")
  :bind ("C-c a" . org-agenda))

(use-package org-refile :ensure org
  :custom
  (org-refile-use-outline-path t)
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9)))
  (org-refile-allow-creating-parent-nodes 'confirm))

(use-package org-clock :ensure org
  :custom
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-report-include-clocking-task t)
  (org-clock-out-when-done t))

(use-package org-appear
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t))

(use-package oc :ensure org
  :config
  (require 'oc-csl)
  (require 'oc-biblatex)
  (require 'oc-natbib)
  :custom
  (org-cite-export-processors '((latex biblatex) (t csl)))
  (org-support-shift-select t))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-pretty-table
  :quelpa (org-pretty-table :fetcher github :repo "Fuco1/org-pretty-table")
  :hook (org-mode . org-pretty-table-mode))

;; (use-package org-table-sticky-header)

(use-package org-sticky-header
  ;; :hook (org-mode . org-sticky-header-mode)
  :custom
  (org-sticky-header-always-show-header t)
  (org-sticky-header-show-keyword nil)
  (org-sticky-header-full-path 'full))

(use-package bibtex-completion
  :custom
  (bibtex-completion-bibliography my-bibliographies)
  (bibtex-completion-notes-path my-notes-directory)
  (bibtex-completion-additional-search-fields '(keywords))
  (bibtex-completion-library-path my-paper-directories))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :preface
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))
  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package org-noter
  :quelpa (org-noter :fetcher github :repo "furkanusta/org-noter")
  :custom
  (org-noter-notes-search-path (list my-notes-directory))
  (org-noter-always-create-frame nil)
  (org-noter-doc-split-fraction (cons 0.7  0.3))
  (org-noter-kill-frame-at-session-end nil)
  (org-noter-default-notes-file-names (list "Notes.org"))
  (org-noter-auto-save-last-location t)
  (org-noter-insert-note-no-questions t))

(use-package org-special-block-extras)

(use-package org-remark
  :quelpa (org-remark :fetcher github :repo "nobiot/org-remark")
  :bind
  ("C-c r m" . org-remark-mark)
  (:map org-remark-mode-map
        ("C-c r m" . org-remark-mark)
        ("C-c r o" . org-remark-open)
        ("C-c r k" . org-remark-remove)
        ("C-c r n" . org-remark-next)
        ("C-c r p" . org-remark-prev)))

(use-package org-journal
  :bind ("C-c i j" . org-journal-new-entry)
  :custom
  (org-journal-dir (concat my-notes-directory "/Journal"))
  (org-journal-file-format "%Y-%m.org")
  (org-journal-date-format "%A, %d %B")
  (org-journal-file-type 'monthly)
  (org-journal-time-format nil)
  (org-journal-created-property-timestamp-format "%F"))

(use-package binder
  :custom (binder-default-file-extension "org"))

(use-package inherit-org
  :quelpa (inherit-org :repo "chenyanming/inherit-org" :fetcher github)
  :hook ((eww-mode nov-mode info-mode helpful-mode) . inherit-org-mode))

(use-package shrface
  :after org
  :commands (shrface-basic shrface-trial shrface-outline-cycle org-at-heading-p)
  :config
  (shrface-basic)
  (shrface-trial)
  :preface
  (defun org-tab-or-next-heading ()
    (interactive)
    (if (org-at-heading-p)
        (shrface-outline-cycle)
      (progn
        (org-next-visible-heading 1)
        (unless (org-at-heading-p)
          (org-previous-visible-heading 1)))))
  :hook (inherit-org-mode . shrface-mode)
  :custom (shrface-href-versatile t)
  :bind (:map shrface-mode-map
              ("<tab>" . shrface-outline-cycle)
              ("C-<tab>" . org-tab-or-next-heading)
              ("<backtab>" . shrface-outline-cycle-buffer)
              ("C-c C-n" . shrface-next-headline)
              ("C-c C-p" . shrface-previous-headline)
              ("C-c C-l" . shrface-links-consult)
              ("C-c C-h" . shrface-headline-consult)
              ("M-<up>" . org-previous-visible-heading)))

(use-package shr-tag-pre-highlight
  :after shrface
  :functions (shr-tag-pre-highlight-fontify
              shr-tag-pre-highlight--get-lang-mode
              shr-tag-pre-highlight-guess-language-attr)
  :preface
  (defun shrface-shr-tag-pre-highlight (pre)
    "Highlighting code in PRE."
    (let* ((shr-folding-mode 'none)
           (shr-current-font 'default)
           (code (with-temp-buffer
                   (shr-generic pre)
                   ;; (indent-rigidly (point-min) (point-max) 2)
                   (buffer-string)))
           (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                     (let ((sym (language-detection-string code)))
                       (and sym (symbol-name sym)))))
           (mode (and lang (shr-tag-pre-highlight--get-lang-mode lang)))
           (start)
           (end))
      (shr-ensure-newline)
      (shr-ensure-newline)
      (setq start (point))
      (insert
       (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
       (or (and (fboundp mode)
                (with-demoted-errors "Error while fontifying: %S"
                  (shr-tag-pre-highlight-fontify code mode)))
           code)
       (propertize "#+END_SRC" 'face 'org-block-end-line ))
      (shr-ensure-newline)
      (setq end (point))
      (if (eq 'my-light-theme (car custom-enabled-themes))
          (add-face-text-property start end '(:background "#D8DEE9" :extend t))
        (add-face-text-property start end '(:background "#292b2e" :extend t)))
      (shr-ensure-newline)
      (insert "\n")))
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shrface-shr-tag-pre-highlight)))

(use-package engine-mode
  :custom
  (engine-mode t)
  (engine/browser-function 'eww-browse-url)
  :init
  (defengine github "https://github.com/search?ref=simplesearch&q=%s")
  (defengine google "https://google.com/search?q=%s" :keybinding "g")
  (defengine wikipedia "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s" :keybinding "w")
  (defengine wolfram "http://www.wolframalpha.com/input/?i=%s"))

(use-package org-transclusion
  :quelpa (org-transclusion :fetcher github :repo "nobiot/org-transclusion")
  :custom (org-transclusion-activate-persistent-message nil))

(use-package org-ql)

(use-package org-rich-yank
  :bind (:map org-mode-map ("C-M-y" . org-rich-yank)))

(use-package side-notes
  :custom
  (side-notes-display-alist '((side . right)
                              (window-width . 50)))
  (side-notes-file "Notes.org")
  (side-notes-secondary-file "README.org")
  :bind ("C-c t n" . side-notes-toggle-notes))

(use-package org-roam
  :preface
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))
  :custom
  (org-roam-directory my-notes-directory)
  (org-roam-auto-replace-fuzzy-links nil)
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :target  (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")  :unnarrowed t)
     ("p" "Paper Note" plain "* %^{citekey}" :target (file "Papers.org" "#+title: ${title}"))))
  :bind
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n g" . org-roam-graph)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n I" . org-roam-node-insert-immediate)
  ("C-c n c" . org-roam-capture)
  ;; Dailies
  ("C-c n j" . org-roam-dailies-capture-today)
  :config
  (add-to-list 'completion-at-point-functions 'org-roam-complete-link-at-point)
  (require 'org-roam-protocol))

(use-package org-roam-bibtex
  :after org-roam
  :preface
  (defun my-org-roam-bibtex-fix-property ()
    (interactive)
    (save-excursion
      (while (> (org-current-level) 2)
        (org-up-heading-safe))
      (org-babel-next-src-block)
      (org-set-property "ROAM_REFS"
                        (concat "[cite:@"
                                (buffer-substring (search-forward "{") (1- (search-forward ",")))
                                "]")))))

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package org-roam-timestamps
  :hook (org-roam-mode . org-roam-timestamps-mode))

(use-package highlight
  :preface
  (defun hlt-general()
    (interactive)
    (unless (bound-and-true-p enriched-mode)
      (enriched-mode t))
    (hlt-highlight-region (region-beginning) (region-end) 'highlight))
  (defun highlight-on-capture ()
    (when (equal (plist-get org-capture-plist :key) "f")
      (save-excursion
        (with-current-buffer (plist-get org-capture-plist :original-buffer)
          (hlt-general)))))
  :hook (org-capture-after-finalize . highlight-on-capture)
  :bind (("C-c o h" . hlt-general)
         ("C-c o H" . hlt-unhighlight-region)))

(use-package ox-pandoc)

(use-package org-pandoc-import
  :quelpa (org-pandoc-import
           :fetcher github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

(use-package org-web-tools)

(use-package org-agenda-property
  :custom (org-agenda-property-list '(LOCATION)))

(use-package org-timeline
  :hook (org-agenda-finalize . org-timeline-insert-timeline))

(use-package org-download
  :hook
  (dired-mode . org-download-enable)
  (org-mode . org-download-enable))

(use-package org-mru-clock
  :bind
  ("C-c C-x c" . org-mru-clock-in)
  ("C-c C-x C-c" . org-mru-clock-select-recent-task))

(use-package org-dashboard
  :custom (org-dashboard-files (list (concat my-notes-directory "TODO.org"))))

(use-package orgit)
(use-package orgit-forge)

(use-package ox-hugo)

(use-package org-latex-impatient
  :custom
  (org-latex-impatient-tex2svg-bin "/home/eksi/.local/prog/node_modules/mathjax-node-cli/bin/tex2svg"))

(use-package org-tanglesync
  :bind
  (( "C-c M-i" . org-tanglesync-process-buffer-interactive)
   ( "C-c M-a" . org-tanglesync-process-buffer-automatic)))

(use-package orgdiff
  :quelpa (orgdiff :fetcher github :repo "tecosaur/orgdiff"))

;; (use-package math-at-point
;;   :quelpa (math-at-point :fetcher github :repo "shankar2k/math-at-point"))

(use-package org-clock-reminder
  :custom
  (org-clock-reminder-interval 1200)
  :config (org-clock-reminder-activate))

(use-package org-elp
  :custom
  (org-elp-idle-time 0.5)
  (org-elp-split-fraction 0.25))

(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

(use-package org-books
  :custom
  (org-books-add-to-top nil)
  (org-books-file-depth 1)
  (org-books-file (concat my-notes-directory "/Books.org")))

(use-package ein)

(use-package code-cells
  :config
  (let ((map code-cells-mode-map))
      (define-key map "n" (code-cells-speed-key 'code-cells-forward-cell))
      (define-key map "p" (code-cells-speed-key 'code-cells-backward-cell))
      (define-key map "e" (code-cells-speed-key 'code-cells-eval))
      (define-key map "RET" (code-cells-speed-key 'code-cells-eval))
      (define-key map (kbd "TAB") (code-cells-speed-key 'outline-cycle)))
  :bind
  (:map code-cells-mode-map
        ("C-c n" . code-cells-forward-cell)
        ("C-c p" . code-cells-backward-cell)
        ("M-RET" . code-cells-eval)))

(use-package org-auctex
  :quelpa (org-auctex :fetcher github :repo "karthink/org-auctex"))

(use-package org-protocol-capture-html
  :quelpa (org-protocol-capture-html :fetcher github :repo "alphapapa/org-protocol-capture-html"))

(use-package orca
  :preface
  
  ;; (defun orfu-handle-link-github ()
  ;;   (let ((link (caar org-stored-links))
  ;;         (title (cl-cadar org-stored-links)))
  ;;     (when (string-match orfu-github-project-name link)
  ;;       (let ((project-name (match-string 1 link))
  ;;             (parts (split-string title "·")))
  ;;         (setf (cl-cadar org-stored-links)
  ;;               (concat (car parts)
  ;;                       (substring (cadr parts) 7)))
  ;;         (find-file (orfu-expand "wiki/github.org"))
  ;;         (goto-char (point-min))
  ;;         (re-search-forward (concat "^\\*+ +" project-name) nil t)))))


  
  ;; (defun orfu--handle-link-youtube-1 (link)
  ;;   (setq link (replace-regexp-in-string "time_continue=[0-9]+&" "" link))
  ;;   (let* ((default-directory "~/Downloads/Videos")
  ;;          (json (orfu--youtube-json
  ;;                 (setq orfu--current-cmd
  ;;                       (format "setsid -w yt-dlp --write-sub -f mp4 --write-info-json %s" link)))))
  ;;     (find-file (orfu-expand "wiki/youtube.org"))
  ;;     (zo-goto-headings '("Blogs"))
  ;;     (org-capture-put
  ;;      :immediate-finish t
  ;;      :jump-to-captured t))
  ;;   t)
  ;; 

  ;; org-web-tools-insert-web-page-as-entry
  ;; org-board-new
  ;; https://github.com/alphapapa/org-protocol-capture-html/blob/master/org-protocol-capture-html.sh
  

  (defun orfu-handle-link-youtube ()
    (let ((link (orfu--youtube-link)))
      (when link
        (setq orca-link-hook nil)
        (orfu--handle-link-youtube-1 link))))

  :config
  (setq orca-handler-list
        '(
          (orfu-handle-link-youtube)
          (orca-handler-project)
          (orca-handler-match-url "https://www.reddit.com/emacs/" "~/Dropbox/org/wiki/emacs.org" "Reddit")
          (orca-handler-match-url "https://emacs.stackexchange.com/" "~/Dropbox/org/wiki/emacs.org" "\\* Questions")
          (orca-handler-current-buffer "\\* Tasks")
          (orca-handler-file "~/Dropbox/org/ent.org" "\\* Articles"))))

(use-package zotra
  :quelpa (zotra :fetcher github :repo "mpedramfar/zotra"))

(provide 'usta-org)
