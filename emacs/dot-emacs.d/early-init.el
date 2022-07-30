(setq-default package-enable-at-startup nil)

(setq gc-cons-threshold 64000000
      gc-cons-percentage 0.8
      package-enable-at-startup nil
      load-prefer-newer t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 100
                    :weight 'medium
                    :width 'normal)
