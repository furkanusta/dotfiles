
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Navigation          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package buffer-move
  :bind
  ("C-c S-<up>"    . buf-move-up)
  ("C-c S-<down>"  . buf-move-down)
  ("C-c S-<left>"  . buf-move-left)
  ("C-c S-<right>" . buf-move-right))

(use-package drag-stuff
  :custom (drag-stuff-global-mode t)
  :bind (:map drag-stuff-mode-map
              ("<M-up>" . drag-stuff-up)
              ("<M-down>" . drag-stuff-down)))

(use-package frame-movement
  ;; :load-path "elisp/"
  :bind
  ("C-x 5 n" . frame-movement/select-next-frame)
  ("C-x 5 p" . frame-movement/select-prev-frame))

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
  :preface (defun toggle-line-numbers () (display-line-numbers-mode (or (not display-line-numbers-mode) 0)))
  :custom
  (writeroom-width 100)
  (writeroom-mode-line nil)
  :bind ("C-c o w" . writeroom-mode))

(use-package focus
  :bind ("C-c C-f" . focus-mode) ;; Might be unnecessary
  (:map focus-mode-map
        ("C-c C-n" . focus-next-thing)
        ("C-c C-p" . focus-prev-thing)))

(use-package expand-region
  :bind ("C-}" . er/expand-region))

(use-package annotate
  :bind ("C-c i a" . annotate-annotate))

(use-package goto-last-point
  :config (goto-last-point-mode)
  :bind ("C-x g s" . goto-last-point))

(use-package goto-chg
  :bind
  ("C-x g c" . goto-last-change)
  ("C-x g C" . goto-last-change-reverse))

(use-package link-hint
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

(use-package bm
  :commands bm-buffer-save-all bm-repository-save
  :after no-littering
  :preface (defun bm-save-all ()
             (progn (bm-buffer-save-all)
                    (bm-repository-save)))
  :init (setq bm-restore-repository-on-load t)
  :custom
  (bm-cycle-all-buffers t)
  (bm-repository-file (concat no-littering-var-directory "bm-repository"))
  (bm-buffer-persistence t)
  :hook
  ((after-init . bm-repository-load)
   (after-save . bm-buffer-save)
   (vc-before-checkin . bm-buffer-save)
   (find-file . bm-buffer-restore)
   (after-revert . bm-buffer-restore)
   (kill-buffer . bm-buffer-save)
   (kill-emacs . bm-save-all))
  :bind
  ("C-c b n" . bm-next)
  ("C-c b p" . bm-previous)
  ("C-c b b" . bm-toggle))

(use-package beacon
  :custom
  (beacon-blink-when-point-moves-vertically 20)
  (beacon-blink-when-buffer-changes nil)
  (beacon-blink-when-window-changes nil)
  (beacon-mode 1))

(use-package ibuffer-sidebar
  :commands (ibuffer-sidebar-toggle-sidebar)
  :bind ("C-c t b" . ibuffer-sidebar-toggle-sidebar))

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

(use-package exwm-mff :custom (exwm-mff-mode 1))

(provide 'usta-navigation)
