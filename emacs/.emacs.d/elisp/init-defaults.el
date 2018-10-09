;; (use-package desktop
;;   :init (desktop-save-mode 1)
;;   :config (add-to-list 'desktop-modes-not-to-save 'dired-mode))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode)
;; (global-linum-mode t)
(global-display-line-numbers-mode t)
(blink-cursor-mode 0)
(show-paren-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)


(setq-default save-interprogram-paste-before-kill t
              emacs-load-start-time (current-time)
              ad-redefinition-action 'accept
              backup-inhibited t
              calendar-week-start-day 1
              delete-by-moving-to-trash t
              confirm-nonexistent-file-or-buffer nil
              tab-width 4
              tab-stop-list (number-sequence 4 200 4)
              indent-tabs-mode nil
              backup-directory-alist `(("." . "~/.emacs.d/.gen"))
              auto-save-file-name-transforms `((".*" "~/.emacs.d/.gen/" t))
              ediff-window-setup-function 'ediff-setup-windows-plain
              ediff-split-window-function 'split-window-horizontally
              gdb-many-windows t
              use-file-dialog nil
              use-dialog-box nil
              inhibit-startup-screen t
              inhibit-startup-echo-area-message t
              inhibit-startup-screen t
              cursor-type 'bar
              ring-bell-function 'ignore
              scroll-step 1
              sentence-end-double-space -1
              fill-column 100
              scroll-step 1
              scroll-conservatively 10000
              initial-major-mode 'org-mode
              auto-window-vscroll nil
              comint-prompt-read-only t
              font-use-system-font t)

(use-package recentf
  :init (recentf-mode t)
  :config (setq-default recent-save-file "~/.emacs.d/recentf"))

(use-package delsel :init (delete-selection-mode 1))

(use-package saveplace
  :init (save-place-mode 1)
  :config (setq-default server-visit-hook (quote (save-place-find-file-hook))))

(use-package uniquify
  :ensure nil
  :config
  (setq-default uniquify-buffer-name-style 'reverse
                uniquify-separator " • "
                uniquify-after-kill-buffer-p t
                uniquify-ignore-buffers-re "^\\*"))

;; (use-package powerline :init (powerline-default-theme))

(use-package all-the-icons)

(use-package spaceline-config
  :commands spaceline-helm-mode
  :init
  (with-eval-after-load 'helm
    (spaceline-helm-mode t)))

(use-package spaceline-all-the-icons
  :commands spaceline-toggle-all-the-icons-time-off
  :after spaceline
  :init (spaceline-all-the-icons-theme)
  :config (spaceline-toggle-all-the-icons-time-off))

(use-package diminish)

(use-package winner :init (winner-mode 1))
;; (defun my/find-file-hook ()
;;   (when (not (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
;; (add-hook 'find-file-hook 'my/find-file-hook)

(use-package which-func :init (which-function-mode t))

(defun duplicate-line-or-region (arg)
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun my-align-comments (beginning end)
  (interactive "*r")
  (let (indent-tabs-mode align-to-tab-stop)
    (align-regexp beginning end "\\(\\s-*\\)//")))

(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer) (buffer-list))))

(defun xah-cut-line-or-region ()
  (interactive)
  (if current-prefix-arg
      (progn
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun xah-copy-line-or-region ()
  (interactive)
  (let (-p1 -p2)
    (if (use-region-p)
        (setq -p1 (region-beginning) -p2 (region-end))
      (setq -p1 (line-beginning-position) -p2 (line-end-position)))
    (progn
      (kill-ring-save -p1 -p2)
      (message "Text copied"))))


(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil) (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun scroll-down-in-place (n)
  (interactive "p")
  (forward-line (* -1 n))
  (unless (eq (window-start) (point-min))
    (scroll-down n)))

(defun scroll-up-in-place (n)
  (interactive "p")
  (forward-line n)
  (unless (eq (window-end) (point-max))
    (scroll-up n)))

(global-set-key (kbd "M-n") 'scroll-up-in-place)
(global-set-key (kbd "M-p") 'scroll-down-in-place)
(global-set-key (kbd "<f7>") 'eww)
(global-set-key (kbd "<f8>") 'shell)
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "C-M-;") 'my-align-comments)
(global-set-key (kbd "C-c C-k") 'kill-other-buffers)
(global-set-key (kbd "C-c d") 'duplicate-line-or-region)
(global-set-key (kbd "C-c e r") 'eval-region)
(global-set-key (kbd "C-c f") 'find-file-at-point)
(global-set-key (kbd "C-S-d") 'delete-backward-char)
(global-set-key (kbd "M-D") 'backward-kill-word)
(global-set-key (kbd "C-w") 'xah-cut-line-or-region) ; cut
(global-set-key (kbd "C-x <f9>") 'recompile)
(global-set-key (kbd "M-k") 'kill-whole-line)
(global-set-key (kbd "M-w") 'xah-copy-line-or-region) ; copy
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)
(define-key prog-mode-map (kbd "<tab>") 'indent-for-tab-command)

(provide 'init-defaults)
