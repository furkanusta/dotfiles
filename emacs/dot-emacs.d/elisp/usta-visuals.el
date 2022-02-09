;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Visual          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dashboard
  :hook (dashboard-mode . page-break-lines-mode)
  :commands dashboard-insert-section dashboard-insert-heading dashboard-subseq
  :preface (defun dashboard-insert-scratch (list-size)
             (dashboard-insert-section
              "Shortcuts:"
              '("*scratch*" "*elfeed*" "init.el" "*vterm*" "Data Folder" "Home")
              list-size
              "Scratch"
              "s"
              `(lambda (&rest ignore)
                 (cond
                  ((string= "*scratch*" ,el) (switch-to-buffer "*scratch*"))
                  ((string= "init.el" ,el) (find-file user-init-file))
                  ((string= "Data Folder" ,el) (find-file my-data-directory))
                  ((string= "*vterm*" ,el) (multi-vterm))
                  ((string= "*elfeed*" ,el)
                   (progn
                     (if (get-buffer "*elfeed-search*")
                         (switch-to-buffer "*elfeed-search*")
                       (progn
                         (elfeed)
                         (elfeed-search-fetch nil)))))
                  ((string= "Home" ,el) (dired (expand-file-name "~/")))
                  (t (message "%s" ,el))))
              (format "%s" el)))
  :init (dashboard-setup-startup-hook)
  :config (add-to-list 'dashboard-item-generators  '(scratch . dashboard-insert-scratch))
  :custom
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-page-separator "\n\f\n")
  (dashboard-projects-backend 'projectile)
  (dashboard-projects-switch-function 'projectile-persp-switch-project)
  (dashboard-items '((scratch . 6)
                     (recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)))
  (dashboard-banner-logo-title "Emacs"))

(use-package all-the-icons)

(use-package doom-modeline
  :custom
  (doom-modeline-mode 1)
  (doom-modeline-icon t)
  (doom-modeline-buffer-encoding nil))

(use-package diminish)

(use-package monokai-theme
  :init (load-theme 'monokai t))

(use-package popper
  :commands popper-select-popup-at-bottom
  :bind (("C-\\"   . popper-toggle-latest)
         ("M-\\"   . popper-cycle)
         ("C-M-\\"   . popper-toggle-type))
  :custom
  (popper-reference-buffers '("\\*Messages\\*" "Output\\*$" help-mode compilation-mode))
  (popper-display-function #'popper-select-popup-at-bottom)
  (popper-mode +1))

(use-package helpful
  :commands (get-buffers-matching-mode helpful-first-buffer-p helpful-not-first-buffer-p)
  :preface (defun alist-switch-or-pop (mode buf  &optional alist)
             (if (derived-mode-p mode)
                 (switch-to-buffer buf)
               (progn
                 (when (= (length (window-list)) 1)
                   (split-window-horizontally))
                 (other-window 1)
                 (switch-to-buffer buf nil))))
  :config
  (add-to-list 'display-buffer-alist `("\\*helpful" (,(apply-partially #'alist-switch-or-pop 'helpful-mode))))
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h h" . helpful-at-point)
  :custom (helpful-max-buffers 5))

(provide 'usta-visuals)
