;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Navigation          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; -*- lexical-binding: t -*-
(use-package buffer-move
  :bind
  ("C-c S-<up>"    . buf-move-up)
  ("C-c S-<down>"  . buf-move-down)
  ("C-c S-<left>"  . buf-move-left)
  ("C-c S-<right>" . buf-move-right))

(use-package drag-stuff
  :custom (drag-stuff-global-mode t)
  :bind (:map drag-stuff-mode-map
              ("M-N" . drag-stuff-down)
              ("M-P" . drag-stuff-up)
              ("<M-up>" . drag-stuff-up)
              ("<M-down>" . drag-stuff-down)))

(use-package hungry-delete
  :custom
  (global-hungry-delete-mode 1)
  (hungry-delete-join-reluctantly t)
  :bind
  ([remap delete-char] . hungry-delete-forward)
  ([remap delete-forward-char] . hungry-delete-forward)
  ([remap delete-backward-char] . hungry-delete-backward))

(use-package writeroom-mode
  ;; :hook (writeroom-mode . toggle-line-numbers)
  :defines display-line-numbers-mode
  :functions display-line-numbers-mode
  :preface (defun toggle-line-numbers () (display-line-numbers-mode (or (not display-line-numbers-mode) 0)))
  :custom
  (writeroom-width 120)
  (writeroom-mode-line nil)
  (writeroom-fullscreen-effect 'maximized)
  :bind
  (:map writeroom-mode-map
        ("C-c o w" . writeroom-mode)))

(use-package focus
  :bind ("C-c C-f" . focus-mode) ;; Might be unnecessary
  (:map focus-mode-map
        ("C-c C-n" . focus-next-thing)
        ("C-c C-p" . focus-prev-thing)))

(use-package expand-region
  :bind
  ("C-}" . er/expand-region)
  ("C-c } {}" . er/contract-region)
  ("C-c } d" . er/mark-defun)
  ("C-c } s" . er/mark-outside-pairs)
  ("C-c } q" . er/mark-outside-quotes))

(use-package annotate
  :bind ("C-c i a" . annotate-annotate))

(use-package goto-last-point
  :config (goto-last-point-mode)
  :bind ("C-x j p" . goto-last-point))

(use-package goto-chg
  :bind
  ("C-x j c" . goto-last-change)
  ("C-x j C" . goto-last-change-reverse))

(use-package link-hint
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

(use-package beacon
  :custom
  (beacon-blink-when-point-moves-vertically 20)
  (beacon-blink-when-buffer-changes nil)
  (beacon-blink-when-window-changes nil)
  (beacon-mode 1))

(use-package goggles
  :hook (prog-mode . goggles-mode)
  :custom
  (goggles-pulse t))


(use-package ctrlf
  :custom
  (ctrlf-mode 1)
  (ctrlf-auto-recenter nil)
  :bind
  (:map ctrlf-minibuffer-mode-map
        ("C-s" . ctrlf-forward-default)
        ("C-r" . ctrlf-backward-default)))


(use-package ibuffer-projectile
  :commands (ibuffer-projectile-set-filter-groups ibuffer-projectile-generate-filter-groups)
  :init
  (defun j-ibuffer-projectile-run ()
    "Set up `ibuffer-projectile'."
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  (add-hook 'ibuffer-sidebar-mode-hook #'j-ibuffer-projectile-run)
  (add-hook 'ibuffer-hook #'j-ibuffer-projectile-run)
  :config (setq ibuffer-projectile-prefix "Project: "))

(use-package ibuffer-sidebar
  :commands (ibuffer-sidebar-toggle-sidebar)
  :bind ("C-c t b" . ibuffer-sidebar-toggle-sidebar))

(use-package all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode 1))

(use-package bufler
  :preface (define-prefix-command 'my-switch-command-map)
  :bind-keymap ("C-x s" . my-switch-command-map)
  :bind
  ("C-x C-b" . bufler)
  (:map my-switch-command-map
        ("b" . bufler)
        ("s" . bufler-switch-buffer)))

(use-package multicolumn)

(use-package zygospore
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))

(use-package bifocal
  :hook (shell-mode . bifocal-mode)
  :custom (bifocal-tail-size 30))

(use-package fill-column-indicator
  :hook (prog-mode . fci-mode))

(use-package beginend
  :config (beginend-global-mode))

(use-package resize-window
  :defines resize-window-dispatch-alist
  :config
  (add-to-list 'resize-window-dispatch-alist '(?- shrink-window-if-larger-than-buffer "Shrink if larger" nil))
  (add-to-list 'resize-window-dispatch-alist '(?+ balance-windows "Balance windows" nil))
  :bind ("C-c ;" . resize-window)
  :custom (resize-window-allow-backgrounds nil))

(use-package dogears
  :quelpa (dogears :fetcher github :repo "alphapapa/dogears.el"
                   :files (:defaults (:exclude "helm-dogears.el")))
  ;; These bindings are optional, of course:
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar)))

(use-package winner
  :hook (after-init . winner-mode))

(use-package buffer-expose)

;; (use-package trivial-copy
;;   :quelpa (trivial-copy :fetcher github :repo "casouri/trivial-copy"))

(use-package imenu-list
  :custom
  (imenu-list-position 'left)
  (imenu-list-size 0.15)
  :bind ("C-c t i" . imenu-list-smart-toggle))

(provide 'usta-navigation)
