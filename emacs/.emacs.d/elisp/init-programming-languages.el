;; C++-mode
(use-package cc-mode
  :ensure nil
  :mode ("\\.h\\'" . c++-mode)
  :config
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (setq-default c-default-style "stroustrup"
                c-basic-offset 4
                c-indent-level 4
                access-label 0
                tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
                tab-width 4
                indent-tabs-mode nil))

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

;; (use-package cmake-ide
;;   :after rtags
;;   :init (cmake-ide-setup))

;; requires rtags-install
(use-package rtags
  ;; :hook (c++-mode . rtags-start-process-unless-running)
  :config
  (setq-default rtags-autostart-diagnostics t
                rtags-completions-enabled t
                rtags-use-helm t
                rtags-display-result-backend 'helm))

(use-package company-rtags
  :after company
  :config (add-to-list 'company-backends 'company-rtags))

;; (defun my-flycheck-rtags-setup ()
;;   (flycheck-select-checker 'rtags)
;;   (setq-local flycheck-highlighting-mode nil)
;;   (setq-local flycheck-check-syntax-automatically nil))
;; (use-package flycheck-rtags :after flycheck)

;; Generic
(use-package company
  :diminish company-mode
  :commands company-complete
  :hook (after-init . global-company-mode)
  :bind ("<C-tab>" .(function company-complete)))

(use-package company-flx
  :after company
  :init (company-flx-mode +1))

(use-package company-quickhelp :init (company-quickhelp-mode t))

(use-package company-c-headers
  :after company
  :init (add-to-list 'company-backends 'company-c-headers))

;; requires irony-install-server
(use-package irony
  :diminish irony-mode
  :hook ((irony-mode . irony-cdb-autosetup-compile-options)
         (c++-mode . irony-mode))
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-irony)))

(use-package quickrun
  :bind
  ("C-c e e" . quickrun-region)
  ("C-c e q" . quickrun-replace-region))

(use-package semantic
  :hook ((python-mode . semantic-mode)
         (c++-mode . semantic-mode)))

(use-package srefactor
  :after semantic
  :config (setq-default srefactor-ui-menu-show-help nil)
  :bind
  (:map c++-mode-map
        ("C-c r s" . srefactor-refactor-at-point)))

;; Perl
(use-package cperl-mode
  :ensure nil
  :init (defalias 'perl-mode 'cperl-mode)
  :config
  (setq-default cperl-indent-level 4
                cperl-close-paren-offset -4
                cperl-continued-statement-offset 4
                cperl-indent-parens-as-block t
                cperl-tab-always-indent nil))

;; Scala
(use-package scala-mode)
(use-package ensime
  :config
  (setq-default ensime-eldoc-hints t
                ensime-graphical-tooltips t
                ensime-tooltip-hints t
                ensime-startup-notification nil))
(use-package sbt-mode)

;; Python
(use-package virtualenvwrapper :init (venv-initialize-interactive-shells))
(use-package auto-virtualenvwrapper :after virtualenvwrapper)

(use-package anaconda-mode)

(use-package company-anaconda
  :after company
  :init (add-to-list 'company-backends 'company-anaconda))

(use-package python
  :hook ((python-mode . auto-virtualenvwrapper-activate)
         (python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config)

(use-package flycheck-pycheckers
  :after flycheck
  :hook (flycheck-mode . flycheck-pycheckers-setup)
  :config (setq-default flycheck-pycheckers-checkers '(flake8 pyflakes)
                        flycheck-pycheckers-max-line-length 100))

;; web-mode
(use-package web-mode
  :mode "\\.jinja2\\'"
  :config
  (setq-default web-mode-engines-alist '(("jinja2"    . "\\.jinja2\\'"))
                web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))

(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.blade\\.php\\'" . web-mode)
   ("\\.jsp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.ejs\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tsx\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
  :init
  (flycheck-add-mode 'tsx-tide 'web-mode)
  (flycheck-add-next-checker 'tsx-tide '(t . typescript-tide) 'append)
  (flycheck-add-next-checker 'tsx-tide '(t . typescript-tslint) 'append)
  :config
  (setq-default web-mode-engines-alist '(("jinja2"    . "\\.jinja2\\'"))
                web-mode-attr-indent-offset 2
                web-mode-attr-value-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-enable-auto-closing t
                web-mode-enable-auto-pairing t
                web-mode-enable-comment-keywords t
                web-mode-enable-current-element-highlight t
                web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))

(use-package company-web
  :ensure t
  :hook (web-mode . (lambda ()
    (add-to-list 'company-backends 'company-web-html)
    (add-to-list 'company-backends 'company-web-jade)
    (add-to-list 'company-backends 'company-web-slim))))

(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup))
  :config
  (setq-default typescript-expr-indent-offset -2
                typescript-indent-level 2))

(provide 'init-programming-languages)
