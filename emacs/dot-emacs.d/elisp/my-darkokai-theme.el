;;; my-darkokai-theme.el --- A darker variant on Monokai.

;; URL: http://github.com/sjrmanning/my-darkokai
;; Version: 0.1.2

;; The MIT License (MIT)

;; Copyright (c) 2017 Simon Manning

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:
;;
;; This theme and particular the structure of this file is entirely based
;; on the great work by `oneKelvinSmith' and his port of the monokai theme
;; found at https://github.com/oneKelvinSmith/monokai-emacs.
;;
;; My-Darkokai arose after I found myself making more and more small tweaks
;; to monokai via a separate theme overrides file. Eventually it made more
;; sense to create a new theme as it was diverging significantly from the
;; original theme.
;;
;; Pull requests, and suggestions are most welcome!
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "The my-darkokai theme requires Emacs 24 or later!"))

(deftheme my-darkokai "The My-Darkokai colour theme")

(defgroup my-darkokai nil
  "My-Darkokai theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom my-darkokai-mode-line-padding 8
  "Amount of padding around the mode-line text. Set to 1 for normal look."
  :type 'number
  :group 'my-darkokai)

(defcustom my-darkokai-distinct-fringe-background t
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'my-darkokai)

(defcustom my-darkokai-use-variable-pitch t
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'my-darkokai)

(defcustom my-darkokai-blue-tint nil
  "Use a blue-ish tinted background rather than the flatter black."
  :type 'boolean
  :group 'my-darkokai)

(defcustom my-darkokai-high-contrast-mode-line nil
  "Make the active/inactive mode line stand out more."
  :type 'boolean
  :group 'my-darkokai)

(defcustom my-darkokai-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'my-darkokai)

(defcustom my-darkokai-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'my-darkokai)

(defcustom my-darkokai-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'my-darkokai)

(defcustom my-darkokai-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'my-darkokai)

(defcustom my-darkokai-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'my-darkokai)

(defmacro my-darkokai-with-color-vars (&rest body)
  (declare (indent 0))
  `(let* ((class '((class color) (min-colors 257)))
          ;; Primary colors
          (my-darkokai-yellow           "#E6DB74")
          (my-darkokai-orange           "#ffac4a")
          (my-darkokai-red              "#ff0066")
          (my-darkokai-magenta          "#ff8eff")
          (my-darkokai-violet           "#ab7eff")
          (my-darkokai-blue             "#06d8ff")
          (my-darkokai-cyan             "#53f2dc")
          (my-darkokai-green            "#63de5d")
          (my-darkokai-gray             "#35393b")
          ;; Blue tints
          (my-darkokai-bg-blue          "#282a36")
          (my-darkokai-fringe-blue      "#323342")
          (my-darkokai-pl-ld            "#38394a")
          (my-darkokai-pl-d             "#323342")
          (my-darkokai-pl-dd            "#292a36")
          (my-darkokai-pl-l             "#424458")
          ;; Darker and lighter accented colors
          (my-darkokai-yellow-d         "#BEB244")
          (my-darkokai-yellow-l         "#FFF7A8")
          (my-darkokai-orange-d         "#de8f2d")
          (my-darkokai-orange-l         "#ffc260")
          (my-darkokai-red-d            "#F70057")
          (my-darkokai-red-l            "#FE61A0")
          (my-darkokai-magenta-d        "#FF61FF")
          (my-darkokai-magenta-l        "#FFC4FF")
          (my-darkokai-violet-d         "#9257FF")
          (my-darkokai-violet-l         "#C9ACFF")
          (my-darkokai-blue-d           "#40CAE4")
          (my-darkokai-blue-l           "#92E7F7")
          (my-darkokai-cyan-d           "#00b2ac")
          (my-darkokai-cyan-l           "#BBF7EF")
          (my-darkokai-green-d          "#86C30D")
          (my-darkokai-green-l          "#BBEF53")
          (my-darkokai-gray-ld          "#383c3d")
          (my-darkokai-gray-d           "#35393b")
          (my-darkokai-gray-dd          "#2B2F31")
          (my-darkokai-gray-l           "#515759")
          (my-darkokai-green-plain      "#2AD043")
          (my-darkokai-red-plain        "#FF6159")
          ;; Adaptive colors
          (my-darkokai-fg               "#f8fbfc")
          (my-darkokai-bg               "#242728")
          (my-darkokai-highlight-line   "#323342")
          (my-darkokai-highlight        "#5D6365")
          (my-darkokai-emph             "#ffffff")
          (my-darkokai-comments         "#6A6D70")
          ;; Adaptive higher/lower contrast accented colors
          (my-darkokai-fg-hc            "#141414")
          (my-darkokai-fg-lc            "#171A0B")
          ;; High contrast colors
          (my-darkokai-yellow-hc        "#FFFACE")
          (my-darkokai-yellow-lc        "#9A8F21")
          (my-darkokai-orange-hc        "#FFBE74")
          (my-darkokai-orange-lc        "#A75B00")
          (my-darkokai-red-hc           "#FEB0CC")
          (my-darkokai-red-lc           "#F20055")
          (my-darkokai-magenta-hc       "#FEC6F9")
          (my-darkokai-magenta-lc       "#F309DF")
          (my-darkokai-violet-hc        "#F0E7FF")
          (my-darkokai-violet-lc        "#7830FC")
          (my-darkokai-blue-hc          "#CAF5FD")
          (my-darkokai-blue-lc          "#1DB4D0")
          (my-darkokai-cyan-hc          "#D3FBF6")
          (my-darkokai-cyan-lc          "#4BBEAE")
          (my-darkokai-green-hc         "#CCF47C")
          (my-darkokai-green-lc         "#63de5d")
          ;; customize based face properties
          (s-primary-bg             (if my-darkokai-blue-tint
                                        my-darkokai-bg-blue my-darkokai-bg))
          (s-variable-pitch         (if my-darkokai-use-variable-pitch
                                        'variable-pitch 'default))
          (s-distinct-fringe        (if my-darkokai-blue-tint
                                        my-darkokai-fringe-blue my-darkokai-gray-dd))
          (s-fringe-bg              (if my-darkokai-distinct-fringe-background
                                        s-distinct-fringe my-darkokai-bg))
          (s-mode-line-fg           (if my-darkokai-high-contrast-mode-line
                                        my-darkokai-bg my-darkokai-fg))
          (s-mode-line-bg           (if my-darkokai-high-contrast-mode-line
                                        my-darkokai-fg (if my-darkokai-blue-tint
                                                        my-darkokai-pl-d my-darkokai-gray)))
          (s-mode-line-buffer-id-fg (if my-darkokai-high-contrast-mode-line
                                        'unspecified my-darkokai-green-lc))
          (s-mode-line-inactive-fg  (if my-darkokai-high-contrast-mode-line
                                        my-darkokai-fg my-darkokai-comments))
          (s-mode-line-inactive-bg  (if my-darkokai-high-contrast-mode-line
                                        my-darkokai-gray-dd (if my-darkokai-blue-tint
                                                             my-darkokai-pl-dd my-darkokai-bg)))
          (s-mode-line-inactive-bc  (if my-darkokai-high-contrast-mode-line
                                        my-darkokai-fg my-darkokai-gray))

          ;; powerline default
          (s-powerline-default-active1-bg   (if my-darkokai-high-contrast-mode-line
                                                my-darkokai-gray-l my-darkokai-gray))
          (s-powerline-default-active2-bg   (if my-darkokai-high-contrast-mode-line
                                                my-darkokai-gray my-darkokai-gray-l))
          (s-powerline-default-inactive1-bg (if my-darkokai-high-contrast-mode-line
                                                my-darkokai-gray my-darkokai-gray-d))
          (s-powerline-default-inactive2-bg (if my-darkokai-high-contrast-mode-line
                                                my-darkokai-bg my-darkokai-gray))

          ;; powerline blue versions
          (s-powerline-active1-blue-bg   (if my-darkokai-high-contrast-mode-line
                                             my-darkokai-pl-l my-darkokai-pl-d))
          (s-powerline-active2-blue-bg   (if my-darkokai-high-contrast-mode-line
                                             my-darkokai-pl-d my-darkokai-pl-l))
          (s-powerline-inactive1-blue-bg (if my-darkokai-high-contrast-mode-line
                                             my-darkokai-pl-d my-darkokai-pl-d))
          (s-powerline-inactive2-blue-bg (if my-darkokai-high-contrast-mode-line
                                             my-darkokai-bg my-darkokai-pl-d))

          ;; powerline conditional
          (s-powerline-active1-bg   (if my-darkokai-blue-tint
                                        s-powerline-active1-blue-bg s-powerline-default-active1-bg))
          (s-powerline-active2-bg   (if my-darkokai-blue-tint
                                        s-powerline-active2-blue-bg s-powerline-default-active2-bg))
          (s-powerline-inactive1-bg   (if my-darkokai-blue-tint
                                          s-powerline-inactive1-blue-bg s-powerline-default-inactive1-bg))
          (s-powerline-inactive2-bg   (if my-darkokai-blue-tint
                                          s-powerline-inactive2-blue-bg s-powerline-default-inactive2-bg))

          ;; Definitions for terminals that do not support 256 colors
          (terminal-class                    '((class color) (min-colors 89)))
          ;; Primary colors
          (terminal-my-darkokai-yellow           "#CDC673")
          (terminal-my-darkokai-orange           "#FF8C00")
          (terminal-my-darkokai-red              "#FF1493")
          (terminal-my-darkokai-magenta          "#D700D7")
          (terminal-my-darkokai-violet           "#AF87FF")
          (terminal-my-darkokai-blue             "#5FD7FF")
          (terminal-my-darkokai-cyan             "#5FFFFF")
          (terminal-my-darkokai-green            "#87D700")
          (terminal-my-darkokai-gray             "#3D3D3D")
          ;; Darker and lighter accented colors
          (terminal-my-darkokai-yellow-d         "#878700")
          (terminal-my-darkokai-yellow-l         "#FFFF87")
          (terminal-my-darkokai-orange-d         "#AF5F00")
          (terminal-my-darkokai-orange-l         "#FFAF5F")
          (terminal-my-darkokai-red-d            "#870000")
          (terminal-my-darkokai-red-l            "#FF5F87")
          (terminal-my-darkokai-magenta-d        "#AF0087")
          (terminal-my-darkokai-magenta-l        "#FF87DF")
          (terminal-my-darkokai-violet-d         "#5F00AF")
          (terminal-my-darkokai-violet-l         "#AF87D7")
          (terminal-my-darkokai-blue-d           "#008787")
          (terminal-my-darkokai-blue-l           "#87D7FF")
          (terminal-my-darkokai-cyan-d           "#5FAFAF")
          (terminal-my-darkokai-cyan-l           "#AFFFFF")
          (terminal-my-darkokai-green-d          "#5F8700")
          (terminal-my-darkokai-green-l          "#AFD700")
          (terminal-my-darkokai-gray-d           "#333333")
          (terminal-my-darkokai-gray-l           "#707070")
          ;; Adaptive colors
          (terminal-my-darkokai-fg               "#F5F5F5")
          (terminal-my-darkokai-bg               nil)
          (terminal-my-darkokai-highlight-line   "#474747")
          (terminal-my-darkokai-highlight        "#F4A460")
          (terminal-my-darkokai-emph             "#FFFAFA")
          (terminal-my-darkokai-comments         "#8B8878")
          ;; Adaptive higher/lower contrast accented colors
          (terminal-my-darkokai-fg-hc            "#171A0B")
          (terminal-my-darkokai-fg-lc            "#141414")
          ;; High contrast colors
          (terminal-my-darkokai-yellow-hc        terminal-my-darkokai-yellow-d)
          (terminal-my-darkokai-yellow-lc        terminal-my-darkokai-yellow-l)
          (terminal-my-darkokai-orange-hc        terminal-my-darkokai-orange-d)
          (terminal-my-darkokai-orange-lc        terminal-my-darkokai-orange-l)
          (terminal-my-darkokai-red-hc           terminal-my-darkokai-red-d)
          (terminal-my-darkokai-red-lc           terminal-my-darkokai-red-l)
          (terminal-my-darkokai-magenta-hc       terminal-my-darkokai-magenta-d)
          (terminal-my-darkokai-magenta-lc       terminal-my-darkokai-magenta-l)
          (terminal-my-darkokai-violet-hc        terminal-my-darkokai-violet-d)
          (terminal-my-darkokai-violet-lc        terminal-my-darkokai-violet-l)
          (terminal-my-darkokai-blue-hc          terminal-my-darkokai-blue-d)
          (terminal-my-darkokai-blue-lc          terminal-my-darkokai-blue-l)
          (terminal-my-darkokai-cyan-hc          terminal-my-darkokai-cyan-d)
          (terminal-my-darkokai-cyan-lc          terminal-my-darkokai-cyan-l)
          (terminal-my-darkokai-green-hc         terminal-my-darkokai-green-d)
          (terminal-my-darkokai-green-lc         terminal-my-darkokai-green-l)
          ;; customize based face properties
          (terminal-s-variable-pitch         (if my-darkokai-use-variable-pitch
                                                 'variable-pitch 'default))
          (terminal-s-fringe-bg              (if my-darkokai-distinct-fringe-background
                                                 terminal-my-darkokai-gray terminal-my-darkokai-bg))
          (terminal-s-mode-line-fg           (if my-darkokai-high-contrast-mode-line
                                                 terminal-my-darkokai-bg terminal-my-darkokai-fg))
          (terminal-s-mode-line-bg           (if my-darkokai-high-contrast-mode-line
                                                 terminal-my-darkokai-fg terminal-my-darkokai-gray))
          (terminal-s-mode-line-buffer-id-fg (if my-darkokai-high-contrast-mode-line
                                                 'unspecified terminal-my-darkokai-green))
          (terminal-s-mode-line-inactive-fg  (if my-darkokai-high-contrast-mode-line
                                                 terminal-my-darkokai-fg terminal-my-darkokai-comments))
          (terminal-s-mode-line-inactive-bg  (if my-darkokai-high-contrast-mode-line
                                                 terminal-my-darkokai-highlight-line terminal-my-darkokai-bg))
          (terminal-s-mode-line-inactive-bc  (if my-darkokai-high-contrast-mode-line
                                                 terminal-my-darkokai-fg terminal-my-darkokai-gray))
          )
     ,@body))

(my-darkokai-with-color-vars
  ;; Define faces
  (custom-theme-set-faces
   'my-darkokai

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face
     ((,class (:foreground ,my-darkokai-red
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(font-lock-comment-face
     ((,class (:foreground ,my-darkokai-comments
                           :background nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(font-lock-constant-face
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))

   `(font-lock-doc-face
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(font-lock-function-name-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(font-lock-keyword-face
     ((,class (:foreground ,my-darkokai-red
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :weight normal))))

   `(font-lock-negation-char-face
     ((,class (:foreground ,my-darkokai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight bold))))

   `(font-lock-preprocessor-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(font-lock-regexp-grouping-construct
     ((,class (:foreground ,my-darkokai-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,class (:foreground ,my-darkokai-violet
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet
                                    :weight normal))))

   `(font-lock-string-face
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(font-lock-type-face
     ((,class (:foreground ,my-darkokai-blue
                           :italic nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :italic nil))))

   `(font-lock-variable-name-face
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(font-lock-warning-face
     ((,class (:foreground ,my-darkokai-orange
                           :weight bold
                           :italic t
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange
                                    :weight bold
                                    :italic t
                                    :underline t))))

   `(c-annotation-face
     ((,class (:inherit font-lock-constant-face))
      (,terminal-class (:inherit font-lock-constant-face))))

   ;; general colouring
   '(button ((t (:underline t))))

   `(default
      ((,class (:foreground ,my-darkokai-fg
                            :background ,s-primary-bg))
       (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                     :background ,terminal-my-darkokai-bg))))

   `(highlight
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-highlight))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-highlight))))

   `(lazy-highlight
     ((,class (:inherit highlight
                        :background ,my-darkokai-comments))
      (,terminal-class (:inherit highlight
                                 :background ,terminal-my-darkokai-comments))))

   `(region
     ((,class (:inherit highlight
                        :background ,my-darkokai-highlight))
      (,terminal-class (:inherit highlight
                                 :background ,terminal-my-darkokai-highlight))))

   `(secondary-selection
     ((,class (:inherit region
                        :background ,my-darkokai-blue))
      (,terminal-class (:inherit region
                                 :background ,terminal-my-darkokai-blue))))

   `(shadow
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(match
     ((,class (:foreground ,my-darkokai-cyan-l
                           :background ,my-darkokai-cyan-d
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-cyan-l
                                    :foreground ,terminal-my-darkokai-cyan-d
                                    :weight bold))))

   `(cursor
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-fg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-fg
                                    :inverse-video t))))

   `(mouse
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-fg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-fg
                                    :inverse-video t))))

   `(escape-glyph
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(escape-glyph-face
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(fringe
     ((,class (:foreground ,my-darkokai-fg
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :background ,terminal-s-fringe-bg))))

   `(link
     ((,class (:foreground ,my-darkokai-blue
                           :underline t
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :underline t
                                    :weight bold))))

   `(link-visited
     ((,class (:foreground ,my-darkokai-violet
                           :underline t
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet
                                    :underline t
                                    :weight normal))))

   `(success
     ((,class (:foreground ,my-darkokai-green ))
      (,terminal-class (:foreground ,terminal-my-darkokai-green ))))

   `(warning
     ((,class (:foreground ,my-darkokai-yellow ))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow ))))

   `(error
     ((,class (:foreground ,my-darkokai-red-plain))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(eval-sexp-fu-flash
     ((,class (:foreground ,my-darkokai-cyan-l
                           :background ,my-darkokai-cyan-d))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan-l
                                    :background ,terminal-my-darkokai-cyan-d))))

   `(eval-sexp-fu-flash-error
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-red))))

   `(trailing-whitespace
     ((,class (:background ,my-darkokai-red))
      (,terminal-class (:background ,terminal-my-darkokai-red))))

   `(vertical-border
     ((,class (:foreground ,my-darkokai-gray))
      (,terminal-class (:foreground ,terminal-my-darkokai-gray))))

   `(menu
     ((,class (:foreground ,my-darkokai-fg
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :background ,terminal-my-darkokai-bg))))

   `(minibuffer-prompt
     ((,class (:foreground ,my-darkokai-violet-l))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))

   ;; menus and mode line
   `(mode-line
     ((,class (:inverse-video unspecified
                              :underline unspecified
                              :foreground ,s-mode-line-fg
                              :background ,s-mode-line-bg
                              :box (:line-width ,my-darkokai-mode-line-padding
                                                :color ,s-mode-line-bg
                                                :style unspecified)))
      (,terminal-class (:inverse-video unspecified
                                       :underline unspecified
                                       :foreground ,terminal-s-mode-line-fg
                                       :background ,terminal-s-mode-line-bg))))

   `(mode-line-buffer-id
     ((,class (:foreground ,s-mode-line-buffer-id-fg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-s-mode-line-buffer-id-fg
                                    :weight bold))))

   `(mode-line-inactive
     ((,class (:inverse-video unspecified
                              :underline unspecified
                              :foreground ,s-mode-line-inactive-fg
                              :background ,s-mode-line-inactive-bg
                              :box (:line-width ,my-darkokai-mode-line-padding
                                                :color ,s-mode-line-inactive-bg
                                                :style unspecified)))
      (,terminal-class (:inverse-video unspecified
                                       :underline unspecified
                                       :foreground ,terminal-s-mode-line-inactive-fg
                                       :background ,terminal-s-mode-line-inactive-bg))))

   `(header-line
     ((,class (:inverse-video unspecified
                              :underline unspecified
                              :foreground ,my-darkokai-emph
                              :background ,my-darkokai-highlight-line
                              :box (:line-width 1
                                                :color ,my-darkokai-gray
                                                :style unspecified)))
      (,terminal-class (:inverse-video unspecified
                                       :underline unspecified
                                       :foreground ,terminal-my-darkokai-emph
                                       :background ,terminal-my-darkokai-highlight-line
                                       :box (:line-width 1
                                                         :color ,terminal-my-darkokai-gray
                                                         :style unspecified)))))

   ;; cua
   `(cua-global-mark
     ((,class (:background ,my-darkokai-yellow
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-yellow
                                    :foreground ,terminal-my-darkokai-bg))))

   `(cua-rectangle
     ((,class (:inherit region))
      (,terminal-class (:inherit region))))

   `(cua-rectangle-noselect
     ((,class (:inherit secondary-selection))
      (,terminal-class (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   ;; dired
   `(dired-directory
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(dired-flagged
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(dired-header
     ((,class (:foreground ,my-darkokai-blue
                           :background ,my-darkokai-bg
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :background ,terminal-my-darkokai-bg
                                    :inherit bold))))

   `(dired-ignored
     ((,class (:inherit shadow))
      (,terminal-class (:inherit shadow))))

   `(dired-mark
     ((,class (:foreground ,my-darkokai-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :weight bold))))

   `(dired-marked
     ((,class (:foreground ,my-darkokai-violet
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet
                                    :inherit bold))))

   `(dired-perm-write
     ((,class (:foreground ,my-darkokai-fg
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :underline t))))

   `(dired-symlink
     ((,class (:foreground ,my-darkokai-cyan
                           :slant italic))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :slant italic))))

   `(dired-warning
     ((,class (:foreground ,my-darkokai-orange
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange
                                    :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-blue))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-blue))))

   `(dropdown-list-selection-face
     ((,class (:background ,my-darkokai-green
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-green
                                    :foreground ,terminal-my-darkokai-bg))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,class (:background ,my-darkokai-blue
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-blue
                                    :foreground ,terminal-my-darkokai-bg))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,class (:inherit ecb-history-bucket-node-face
                        :foreground ,my-darkokai-yellow))
      (,terminal-class (:inherit ecb-history-bucket-node-face
                                 :foreground ,terminal-my-darkokai-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,class (:inherit ecb-directories-general-face
                        :foreground ,my-darkokai-fg))
      (,terminal-class (:inherit ecb-directories-general-face
                                 :foreground ,terminal-my-darkokai-fg))))

   `(ecb-history-dead-buffer-face
     ((,class (:inherit ecb-history-general-face
                        :foreground ,my-darkokai-comments))
      (,terminal-class (:inherit ecb-history-general-face
                                 :foreground ,terminal-my-darkokai-comments))))

   `(ecb-directory-not-accessible-face
     ((,class (:inherit ecb-directories-general-face
                        :foreground ,my-darkokai-comments))
      (,terminal-class (:inherit ecb-directories-general-face
                                 :foreground ,terminal-my-darkokai-comments))))

   `(ecb-bucket-node-face
     ((,class (:inherit ecb-default-general-face
                        :weight normal
                        :foreground ,my-darkokai-blue))
      (,terminal-class (:inherit ecb-default-general-face
                                 :weight normal
                                 :foreground ,terminal-my-darkokai-blue))))

   `(ecb-tag-header-face
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))

   `(ecb-analyse-bucket-element-face
     ((,class (:inherit ecb-analyse-general-face
                        :foreground ,my-darkokai-green))
      (,terminal-class (:inherit ecb-analyse-general-face
                                 :foreground ,terminal-my-darkokai-green))))

   `(ecb-directories-general-face
     ((,class (:inherit ecb-default-general-face
                        :height 1.0))
      (,terminal-class (:inherit ecb-default-general-face
                                 :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,class (:inherit ecb-methods-general-face
                        :foreground ,my-darkokai-cyan))
      (,terminal-class (:inherit ecb-methods-general-face
                                 :foreground ,terminal-my-darkokai-cyan))))

   `(ecb-mode-line-prefix-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(ecb-tree-guide-line-face
     ((,class (:inherit ecb-default-general-face
                        :foreground ,my-darkokai-gray
                        :height 1.0))
      (,terminal-class (:inherit ecb-default-general-face
                                 :foreground ,terminal-my-darkokai-gray
                                 :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,class (:foreground ,my-darkokai-emph))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph))))

   `(ee-category
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(ee-link
     ((,class (:inherit link))
      (,terminal-class (:inherit link))))

   `(ee-link-visited
     ((,class (:inherit link-visited))
      (,terminal-class (:inherit link-visited))))

   `(ee-marked
     ((,class (:foreground ,my-darkokai-magenta
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta
                                    :weight bold))))

   `(ee-omitted
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(ee-shadow
     ((,class (:inherit shadow))
      (,terminal-class (:inherit shadow))))

   ;; elixir
   `(elixir-atom-face
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))

   `(elixir-attribute-face
     ((,class (:foreground ,my-darkokai-violet-l))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet-l))))

   ;; grep
   `(grep-context-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(grep-error-face
     ((,class (:foreground ,my-darkokai-red
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :weight bold
                                    :underline t))))

   `(grep-hit-face
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(grep-match-face
     ((,class (:foreground ,my-darkokai-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :weight bold))))

   ;; isearch
   `(isearch
     ((,class (:foreground ,my-darkokai-cyan-l
                           :background ,my-darkokai-cyan-d))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan-l
                                    :background ,terminal-my-darkokai-cyan-d))))

   `(isearch-fail
     ((,class (:inherit isearch
                        :foreground ,my-darkokai-red
                        :background ,my-darkokai-bg
                        :bold t))
      (,terminal-class (:inherit isearch
                                 :foreground ,terminal-my-darkokai-red
                                 :background ,terminal-my-darkokai-bg
                                 :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,class (:foreground ,my-darkokai-comments
                           :background ,my-darkokai-bg
                           :inverse-video nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :background ,terminal-my-darkokai-bg
                                    :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,class (:foreground ,my-darkokai-yellow
                           :background ,my-darkokai-bg
                           :inverse-video nil
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :background ,terminal-my-darkokai-bg
                                    :inverse-video nil
                                    :weight bold))))

   ;; alchemist
   `(alchemist-test--failed-face
     ((,class (:foreground ,my-darkokai-red-hc
                           :background ,my-darkokai-red-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-red-hc
                                    :background ,terminal-my-darkokai-red-lc
                                    :weight bold))))

   `(alchemist-test--success-face
     ((,class (:foreground ,my-darkokai-fg
                           :background ,my-darkokai-green-plain
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :background ,terminal-my-darkokai-green-l
                                    :weight bold))))


   ;; auctex
   `(font-latex-bold-face
     ((,class (:inherit bold
                        :foreground ,my-darkokai-emph))
      (,terminal-class (:inherit bold
                                 :foreground ,terminal-my-darkokai-emph))))

   `(font-latex-doctex-documentation-face
     ((,class (:background unspecified))
      (,terminal-class (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,class (:inherit italic :foreground ,my-darkokai-emph))
      (,terminal-class (:inherit italic :foreground ,terminal-my-darkokai-emph))))

   `(font-latex-math-face
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))

   `(font-latex-sectioning-0-face
     ((,class (:inherit font-latex-sectioning-1-face
                        :height ,my-darkokai-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-1-face
                                 :height ,my-darkokai-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,class (:inherit font-latex-sectioning-2-face
                        :height ,my-darkokai-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-2-face
                                 :height ,my-darkokai-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,class (:inherit font-latex-sectioning-3-face
                        :height ,my-darkokai-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-3-face
                                 :height ,my-darkokai-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,class (:inherit font-latex-sectioning-4-face
                        :height ,my-darkokai-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-4-face
                                 :height ,my-darkokai-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,class (:inherit font-latex-sectioning-5-face
                        :height ,my-darkokai-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-5-face
                                 :height ,my-darkokai-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,my-darkokai-yellow
                        :weight bold))
      (,terminal-class (:inherit ,terminal-s-variable-pitch :
                                 foreground ,terminal-my-darkokai-yellow
                                 :weight bold))))

   `(font-latex-sedate-face
     ((,class (:foreground ,my-darkokai-emph))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph))))

   `(font-latex-slide-title-face
     ((,class (:inherit (,s-variable-pitch font-lock-type-face)
                        :weight bold
                        :height ,my-darkokai-height-plus-3))
      (,terminal-class (:inherit (,terminal-s-variable-pitch font-lock-type-face)
                                 :weight bold
                                 :height ,my-darkokai-height-plus-3))))

   `(font-latex-string-face
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   `(font-latex-subscript-face
     ((,class (:height ,my-darkokai-height-minus-1))
      (,terminal-class (:height ,my-darkokai-height-minus-1))))

   `(font-latex-superscript-face
     ((,class (:height ,my-darkokai-height-minus-1))
      (,terminal-class (:height ,my-darkokai-height-minus-1))))

   `(font-latex-verbatim-face
     ((,class (:inherit fixed-pitch
                        :foreground ,my-darkokai-fg
                        :slant italic))
      (,terminal-class (:inherit fixed-pitch
                                 :foreground ,terminal-my-darkokai-fg
                                 :slant italic))))

   `(font-latex-warning-face
     ((,class (:inherit bold
                        :foreground ,my-darkokai-orange))
      (,terminal-class (:inherit bold
                                 :foreground ,terminal-my-darkokai-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-blue))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-blue))))

   `(ac-selection-face
     ((,class (:background ,my-darkokai-blue
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-blue
                                    :foreground ,terminal-my-darkokai-bg))))

   `(ac-candidate-mouse-face
     ((,class (:background ,my-darkokai-blue
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-blue
                                    :foreground ,terminal-my-darkokai-bg))))

   `(ac-completion-face
     ((,class (:foreground ,my-darkokai-emph
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :underline t))))

   `(ac-gtags-candidate-face
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-blue))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-blue))))

   `(ac-gtags-selection-face
     ((,class (:background ,my-darkokai-blue
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-blue
                                    :foreground ,terminal-my-darkokai-bg))))

   `(ac-yasnippet-candidate-face
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-yellow))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-yellow))))

   `(ac-yasnippet-selection-face
     ((,class (:background ,my-darkokai-yellow
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-yellow
                                    :foreground ,terminal-my-darkokai-bg))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-blue))))

   `(ahs-edit-mode-face
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-highlight))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-highlight))))

   `(ahs-face
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta
                                    :background unspecified))))

   `(ahs-plugin-bod-face
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-violet ))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-cyan ))))

   `(ahs-plugin-defalt-face
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-green))))

   `(ahs-warning-face
     ((,class (:foreground ,my-darkokai-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(android-mode-error-face
     ((,class (:foreground ,my-darkokai-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange
                                    :weight bold))))

   `(android-mode-info-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(android-mode-verbose-face
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(android-mode-warning-face
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,class (:foreground ,my-darkokai-violet
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet
                                    :weight bold))))

   `(anzu-replace-to
     ((,class (:foreground ,my-darkokai-magenta-l
                           :background ,my-darkokai-violet-d))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta-l
                                    :background ,terminal-my-darkokai-violet-d))))

   ;; bm
   `(bm-face
     ((,class (:background ,my-darkokai-yellow-lc
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-yellow-lc
                                    :foreground ,terminal-my-darkokai-bg))))

   `(bm-fringe-face
     ((,class (:background ,my-darkokai-yellow-lc
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-yellow-lc
                                    :foreground ,terminal-my-darkokai-bg))))

   `(bm-fringe-persistent-face
     ((,class (:background ,my-darkokai-green-lc
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-green-lc
                                    :foreground ,terminal-my-darkokai-bg))))

   `(bm-persistent-face
     ((,class (:background ,my-darkokai-green-lc
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-green-lc
                                    :foreground ,terminal-my-darkokai-bg))))

   ;; calfw
   `(cfw:face-day-title
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))

   `(cfw:face-annotation
     ((,class (:inherit cfw:face-day-title
                        :foreground ,my-darkokai-yellow))
      (,terminal-class (:inherit cfw:face-day-title
                                 :foreground ,terminal-my-darkokai-yellow))))

   `(cfw:face-default-content
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(cfw:face-default-day
     ((,class (:inherit cfw:face-day-title
                        :weight bold))
      (,terminal-class (:inherit cfw:face-day-title
                                 :weight bold))))

   `(cfw:face-disable
     ((,class (:inherit cfw:face-day-title
                        :foreground ,my-darkokai-comments))
      (,terminal-class (:inherit cfw:face-day-title
                                 :foreground ,terminal-my-darkokai-comments))))

   `(cfw:face-grid
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(cfw:face-header
     ((,class (:foreground ,my-darkokai-blue-hc
                           :background ,my-darkokai-blue-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue-hc
                                    :background ,terminal-my-darkokai-blue-lc
                                    :weight bold))))

   `(cfw:face-holiday
     ((,class (:background nil
                           :foreground ,my-darkokai-red
                           :weight bold))
      (,terminal-class (:background nil
                                    :foreground ,terminal-my-darkokai-red
                                    :weight bold))))

   `(cfw:face-periods
     ((,class (:foreground ,my-darkokai-magenta))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta))))

   `(cfw:face-select
     ((,class (:background ,my-darkokai-magenta-lc
                           :foreground ,my-darkokai-magenta-hc))
      (,terminal-class (:background ,terminal-my-darkokai-magenta-lc
                                    :foreground ,terminal-my-darkokai-magenta-hc))))

   `(cfw:face-saturday
     ((,class (:foreground ,my-darkokai-cyan-hc
                           :background ,my-darkokai-cyan-lc))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan-hc
                                    :background ,terminal-my-darkokai-cyan-lc))))

   `(cfw:face-sunday
     ((,class (:foreground ,my-darkokai-red-hc
                           :background ,my-darkokai-red-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-red-hc
                                    :background ,terminal-my-darkokai-red-lc
                                    :weight bold))))

   `(cfw:face-title
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,my-darkokai-yellow
                        :weight bold
                        :height ,my-darkokai-height-plus-4))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-my-darkokai-yellow
                                 :weight bold
                                 :height ,my-darkokai-height-plus-4))))

   `(cfw:face-today
     ((,class (:weight bold
                       :background ,my-darkokai-highlight-line
                       :foreground nil))
      (,terminal-class (:weight bold
                                :background ,terminal-my-darkokai-highlight-line
                                :foreground nil))))

   `(cfw:face-today-title
     ((,class (:background ,my-darkokai-yellow-lc
                           :foreground ,my-darkokai-yellow-hc
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-yellow-lc
                                    :foreground ,terminal-my-darkokai-yellow-hc
                                    :weight bold))))

   `(cfw:face-toolbar
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-fg))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-fg))))

   `(cfw:face-toolbar-button-off
     ((,class (:background ,my-darkokai-yellow-lc
                           :foreground ,my-darkokai-yellow-hc
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-yellow-lc
                                    :foreground ,terminal-my-darkokai-yellow-hc
                                    :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,class (:background ,my-darkokai-yellow-hc
                           :foreground ,my-darkokai-yellow-lc
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-yellow-hc
                                    :foreground ,terminal-my-darkokai-yellow-lc
                                    :weight bold))))

   ;; cider
   `(cider-enlightened
     ((,class (:foreground ,my-darkokai-yellow
                           :background nil
                           :box (:color ,my-darkokai-yellow :line-width -1 :style nil)))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :background nil
                                    :box (:color ,terminal-my-darkokai-yellow :line-width -1 :style nil))) ))

   `(cider-enlightened-local
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(cider-instrumented-face
     ((,class (:foreground ,my-darkokai-violet
                           :background nil
                           :box (:color ,my-darkokai-violet :line-width -1 :style nil)))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet
                                    :background nil
                                    :box (:color ,terminal-my-darkokai-violet :line-width -1 :style nil)))))

   `(cider-result-overlay-face
     ((,class (:foreground ,my-darkokai-blue
                           :background nil
                           :box (:color ,my-darkokai-blue :line-width -1 :style nil)))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :background nil
                                    :box (:color ,terminal-my-darkokai-blue :line-width -1 :style nil)))))

   `(cider-test-error-face
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-orange))))

   `(cider-test-failure-face
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-red))))

   `(cider-test-success-face
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-green))))

   `(cider-traced-face
     ((,class :box (:color ,my-darkokai-blue :line-width -1 :style nil))
      (,terminal-class :box (:color ,terminal-my-darkokai-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,class (:foreground ,my-darkokai-red
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :weight bold
                                    :underline t))))

   `(clojure-test-error-face
     ((,class (:foreground ,my-darkokai-orange
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :weight bold
                                    :underline t))))

   `(clojure-test-success-face
     ((,class (:foreground ,my-darkokai-green
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :weight bold
                                    :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-emph))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-emph))))

   `(company-tooltip-selection
     ((,class (:background ,my-darkokai-cyan-d
                           :foreground ,my-darkokai-cyan-l))
      (,terminal-class (:background ,terminal-my-darkokai-cyan-d
                                    :foreground ,terminal-my-darkokai-cyan-l))))

   `(company-tooltip-mouse
     ((,class (:background ,my-darkokai-blue
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-blue
                                    :foreground ,terminal-my-darkokai-bg))))

   `(company-tooltip-common
     ((,class (:foreground ,my-darkokai-violet
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet
                                    :underline t))))

   `(company-tooltip-common-selection
     ((,class (:foreground ,my-darkokai-fg
                           :background ,my-darkokai-cyan-d
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :background ,terminal-my-darkokai-cyan-d
                                    :underline t))))

   `(company-preview
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-emph))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-emph))))

   `(company-preview-common
     ((,class (:foreground ,my-darkokai-blue
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :underline t))))

   `(company-scrollbar-bg
     ((,class (:background ,my-darkokai-gray))
      (,terminal-class (:background ,terminal-my-darkokai-gray))))

   `(company-scrollbar-fg
     ((,class (:background ,my-darkokai-comments))
      (,terminal-class (:background ,terminal-my-darkokai-comments))))

   `(company-tooltip-annotation
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(company-template-field
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-blue))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-blue))))

   ;; compilation
   `(compilation-column-face
     ((,class (:foreground ,my-darkokai-cyan
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :underline nil))))

   `(compilation-column-number
     ((,class (:inherit font-lock-doc-face
                        :foreground ,my-darkokai-cyan
                        :underline nil))
      (,terminal-class (:inherit font-lock-doc-face
                                 :foreground ,terminal-my-darkokai-cyan
                                 :underline nil))))

   `(compilation-enter-directory-face
     ((,class (:foreground ,my-darkokai-green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :underline nil))))

   `(compilation-error
     ((,class (:inherit error
                        :underline nil))
      (,terminal-class (:inherit error
                                 :underline nil))))

   `(compilation-error-face
     ((,class (:foreground ,my-darkokai-red
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :underline nil))))

   `(compilation-face
     ((,class (:foreground ,my-darkokai-fg
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :underline nil))))

   `(compilation-info
     ((,class (:foreground ,my-darkokai-comments
                           :underline nil
                           :bold nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :underline nil
                                    :bold nil))))

   `(compilation-info-face
     ((,class (:foreground ,my-darkokai-blue
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :underline nil))))

   `(compilation-leave-directory-face
     ((,class (:foreground ,my-darkokai-green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :underline nil))))

   `(compilation-line-face
     ((,class (:foreground ,my-darkokai-green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :underline nil))))

   `(compilation-line-number
     ((,class (:foreground ,my-darkokai-green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :underline nil))))

   `(compilation-warning
     ((,class (:inherit warning
                        :underline nil))
      (,terminal-class (:inherit warning
                                 :underline nil))))

   `(compilation-warning-face
     ((,class (:foreground ,my-darkokai-yellow
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight normal
                                    :underline nil))))

   `(compilation-mode-line-exit
     ((,class (:inherit compilation-info
                        :foreground ,my-darkokai-green
                        :weight bold))
      (,terminal-class (:inherit compilation-info
                                 :foreground ,terminal-my-darkokai-green
                                 :weight bold))))

   `(compilation-mode-line-fail
     ((,class (:inherit compilation-error
                        :foreground ,my-darkokai-red
                        :weight bold))
      (,terminal-class (:inherit compilation-error
                                 :foreground ,terminal-my-darkokai-red
                                 :weight bold))))

   `(compilation-mode-line-run
     ((,class (:foreground ,my-darkokai-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange
                                    :weight bold))))

   ;; CSCOPE
   `(cscope-file-face
     ((,class (:foreground ,my-darkokai-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :weight bold))))

   `(cscope-function-face
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(cscope-line-number-face
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(cscope-line-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(cscope-mouse-face
     ((,class (:background ,my-darkokai-blue
                           :foreground ,my-darkokai-fg))
      (,terminal-class (:background ,terminal-my-darkokai-blue
                                    :foreground ,terminal-my-darkokai-fg))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-emph
                           :underline ,my-darkokai-emph
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-emph
                                    :underline ,terminal-my-darkokai-emph
                                    :weight bold))))

   `(ctbl:face-continue-bar
     ((,class (:background ,my-darkokai-gray
                           :foreground ,my-darkokai-yellow))
      (,terminal-class (:background ,terminal-my-darkokai-gray
                                    :foreground ,terminal-my-darkokai-yellow))))

   `(ctbl:face-row-select
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-fg
                           :underline t))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-fg
                                    :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,class (:foreground ,my-darkokai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight bold))))

   `(coffee-mode-function-param
     ((,class (:foreground ,my-darkokai-violet
                           :slant italic))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet
                                    :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,class (:inherit ,s-variable-pitch
                        :height ,my-darkokai-height-plus-3
                        :foreground ,my-darkokai-violet
                        :weight bold))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,my-darkokai-height-plus-3
                                 :foreground ,terminal-my-darkokai-violet
                                 :weight bold))))

   `(custom-variable-tag
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,my-darkokai-cyan
                        :height ,my-darkokai-height-plus-3))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-my-darkokai-cyan
                                 :height ,my-darkokai-height-plus-3))))

   `(custom-comment-tag
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(custom-group-tag
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,my-darkokai-blue
                        :height ,my-darkokai-height-plus-3))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-my-darkokai-blue
                                 :height ,my-darkokai-height-plus-3))))

   `(custom-group-tag-1
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,my-darkokai-red
                        :height ,my-darkokai-height-plus-3))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-my-darkokai-red
                                 :height ,my-darkokai-height-plus-3))))

   `(custom-state
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   ;; diff
   `(diff-added
     ((,class (:foreground ,my-darkokai-green-plain
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :background ,terminal-my-darkokai-bg))))

   `(diff-changed
     ((,class (:foreground ,my-darkokai-blue
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :background ,terminal-my-darkokai-bg))))

   `(diff-removed
     ((,class (:foreground ,my-darkokai-red-plain
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :background ,terminal-my-darkokai-bg))))

   `(diff-header
     ((,class (:background ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-bg))))

   `(diff-file-header
     ((,class (:background ,my-darkokai-bg
                           :foreground ,my-darkokai-fg
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-bg
                                    :foreground ,terminal-my-darkokai-fg
                                    :weight bold))))

   `(diff-refine-added
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-green-plain))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-green))))

   `(diff-refine-change
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-blue))))

   `(diff-refine-removed
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-red-plain))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-red))))

   ;; diff-hl
   `(diff-hl-change
     ((,class (:background ,my-darkokai-blue-lc
                           :foreground ,my-darkokai-blue-hc))
      (,terminal-class (:background ,terminal-my-darkokai-blue-lc
                                    :foreground ,terminal-my-darkokai-blue-hc))))

   `(diff-hl-delete
     ((,class (:background ,my-darkokai-red-lc
                           :foreground ,my-darkokai-red-hc))
      (,terminal-class (:background ,terminal-my-darkokai-red-lc
                                    :foreground ,terminal-my-darkokai-red-hc))))

   `(diff-hl-insert
     ((,class (:background ,my-darkokai-green-lc
                           :foreground ,my-darkokai-green-hc))
      (,terminal-class (:background ,terminal-my-darkokai-green-lc
                                    :foreground ,terminal-my-darkokai-green-hc))))

   `(diff-hl-unknown
     ((,class (:background ,my-darkokai-violet-lc
                           :foreground ,my-darkokai-violet-hc))
      (,terminal-class (:background ,terminal-my-darkokai-violet-lc
                                    :foreground ,terminal-my-darkokai-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,class (:background ,my-darkokai-orange-lc))
      (,terminal-class (:background ,terminal-my-darkokai-orange-lc))))

   `(ediff-fine-diff-B
     ((,class (:background ,my-darkokai-green-lc))
      (,terminal-class (:background ,terminal-my-darkokai-green-lc))))

   `(ediff-fine-diff-C
     ((,class (:background ,my-darkokai-yellow-lc))
      (,terminal-class (:background ,terminal-my-darkokai-yellow-lc))))

   `(ediff-current-diff-C
     ((,class (:background ,my-darkokai-blue-lc))
      (,terminal-class (:background ,terminal-my-darkokai-blue-lc))))

   `(ediff-even-diff-A
     ((,class (:background ,my-darkokai-comments
                           :foreground ,my-darkokai-fg-lc ))
      (,terminal-class (:background ,terminal-my-darkokai-comments
                                    :foreground ,terminal-my-darkokai-fg-lc ))))

   `(ediff-odd-diff-A
     ((,class (:background ,my-darkokai-comments
                           :foreground ,my-darkokai-fg-hc ))
      (,terminal-class (:background ,terminal-my-darkokai-comments
                                    :foreground ,terminal-my-darkokai-fg-hc ))))

   `(ediff-even-diff-B
     ((,class (:background ,my-darkokai-comments
                           :foreground ,my-darkokai-fg-hc ))
      (,terminal-class (:background ,terminal-my-darkokai-comments
                                    :foreground ,terminal-my-darkokai-fg-hc ))))

   `(ediff-odd-diff-B
     ((,class (:background ,my-darkokai-comments
                           :foreground ,my-darkokai-fg-lc ))
      (,terminal-class (:background ,terminal-my-darkokai-comments
                                    :foreground ,terminal-my-darkokai-fg-lc ))))

   `(ediff-even-diff-C
     ((,class (:background ,my-darkokai-comments
                           :foreground ,my-darkokai-fg ))
      (,terminal-class (:background ,terminal-my-darkokai-comments
                                    :foreground ,terminal-my-darkokai-fg ))))

   `(ediff-odd-diff-C
     ((,class (:background ,my-darkokai-comments
                           :foreground ,my-darkokai-bg ))
      (,terminal-class (:background ,terminal-my-darkokai-comments
                                    :foreground ,terminal-my-darkokai-bg ))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) class)
       (:underline (:style line :color ,my-darkokai-red)
                   :inherit unspecified))
      (,class (:foreground ,my-darkokai-red-hc
                           :background ,my-darkokai-red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style line))) terminal-class)
       (:underline (:style line :color ,terminal-my-darkokai-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-my-darkokai-red-hc
                                    :background ,terminal-my-darkokai-red-lc
                                    :weight bold
                                    :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) class)
       (:underline (:style line :color ,my-darkokai-yellow)
                   :inherit unspecified))
      (,class (:foreground ,my-darkokai-yellow-hc
                           :background ,my-darkokai-yellow-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style line))) terminal-class)
       (:underline (:style line :color ,terminal-my-darkokai-yellow)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow-hc
                                    :background ,terminal-my-darkokai-yellow-lc
                                    :weight bold
                                    :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,class (:foreground ,my-darkokai-red
                           :background unspecified
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :background unspecified
                                    :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,class (:foreground ,my-darkokai-yellow
                           :background unspecified
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :background unspecified
                                    :weight bold))))

   `(edts-face-error-mode-line
     ((,class (:background ,my-darkokai-red
                           :foreground unspecified))
      (,terminal-class (:background ,terminal-my-darkokai-red
                                    :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,class (:background ,my-darkokai-yellow
                           :foreground unspecified))
      (,terminal-class (:background ,terminal-my-darkokai-yellow
                                    :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(elfeed-search-feed-face
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(elfeed-search-tag-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(elfeed-search-title-face
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   ;; ein
   `(ein:cell-input-area
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))
   `(ein:cell-input-prompt
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))
   `(ein:cell-output-prompt
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))
   `(ein:notification-tab-normal
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))
   `(ein:notification-tab-selected
     ((,class (:foreground ,my-darkokai-orange :inherit bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,class (:inherit font-lock-keyword-face))
      (,terminal-class (:inherit font-lock-keyword-face))))

   ;; erm-syn
   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,my-darkokai-red)
                   :inherit unspecified))
      (,class (:foreground ,my-darkokai-red-hc
                           :background ,my-darkokai-red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-my-darkokai-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-my-darkokai-red-hc
                                    :background ,terminal-my-darkokai-red-lc
                                    :weight bold
                                    :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,my-darkokai-orange)
                   :inherit unspecified))
      (,class (:foreground ,my-darkokai-orange-hc
                           :background ,my-darkokai-orange-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-my-darkokai-orange)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange-hc
                                    :background ,terminal-my-darkokai-orange-lc
                                    :weight bold
                                    :underline t))))

   ;; epc
   `(epc:face-title
     ((,class (:foreground ,my-darkokai-blue
                           :background ,my-darkokai-bg
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :background ,terminal-my-darkokai-bg
                                    :weight normal
                                    :underline nil))))

   ;; erc
   `(erc-action-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-bold-face
     ((,class (:weight bold))
      (,terminal-class (:weight bold))))

   `(erc-current-nick-face
     ((,class (:foreground ,my-darkokai-blue :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :weight bold))))

   `(erc-dangerous-host-face
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(erc-highlight-face
     ((,class (:inherit erc-default-face
                        :background ,my-darkokai-highlight))
      (,terminal-class (:inherit erc-default-face
                                 :background ,terminal-my-darkokai-highlight))))

   `(erc-direct-msg-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-error-face
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-input-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-keyword-face
     ((,class (:foreground ,my-darkokai-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :weight bold))))

   `(erc-nick-default-face
     ((,class (:foreground ,my-darkokai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight bold))))

   `(erc-my-nick-face
     ((,class (:foreground ,my-darkokai-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :weight bold))))

   `(erc-nick-msg-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-notice-face
     ((,class (:inherits erc-default-face))
      (,terminal-class (:inherits erc-default-face))))

   `(erc-pal-face
     ((,class (:foreground ,my-darkokai-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange
                                    :weight bold))))

   `(erc-prompt-face
     ((,class (:foreground ,my-darkokai-blue
                           :background ,my-darkokai-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :background ,terminal-my-darkokai-bg
                                    :weight bold))))

   `(erc-timestamp-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,class (:foreground ,my-darkokai-blue
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :inherit bold))))

   `(eshell-ls-archive
     ((,class (:foreground ,my-darkokai-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :inherit bold))))

   `(eshell-ls-backup
     ((,class (:inherit font-lock-comment-face))
      (,terminal-class (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,class (:inherit font-lock-comment-face))
      (,terminal-class (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,class (:foreground ,my-darkokai-blue
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :inherit bold))))

   `(eshell-ls-executable
     ((,class (:foreground ,my-darkokai-green
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :inherit bold))))

   `(eshell-ls-unreadable
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(eshell-ls-missing
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,class (:inherit font-lock-doc-face))
      (,terminal-class (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,class (:foreground ,my-darkokai-yellow
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :inherit bold))))

   `(eshell-ls-symlink
     ((,class (:foreground ,my-darkokai-cyan
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :inherit bold))))

   ;; evil-ex-substitute
   `(evil-ex-substitute-matches
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-red-l
                           :inherit italic))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-red-l
                                    :inherit italic))))
   `(evil-ex-substitute-replacement
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-green-l
                           :inherit italic))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line :foreground ,terminal-my-darkokai-green-l :inherit italic))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face
     ((,class (:inherit region))
      (,terminal-class (:inherit region))))

   ;; fic
   `(fic-author-face
     ((,class (:background ,my-darkokai-bg
                           :foreground ,my-darkokai-orange
                           :underline t
                           :slant italic))
      (,terminal-class (:background ,terminal-my-darkokai-bg
                                    :foreground ,terminal-my-darkokai-orange
                                    :underline t
                                    :slant italic))))

   `(fic-face
     ((,class (:background ,my-darkokai-bg
                           :foreground ,my-darkokai-orange
                           :weight normal
                           :slant italic))
      (,terminal-class (:background ,terminal-my-darkokai-bg
                                    :foreground ,terminal-my-darkokai-orange
                                    :weight normal
                                    :slant italic))))

   `(font-lock-fic-face
     ((,class (:background ,my-darkokai-bg
                           :foreground ,my-darkokai-orange
                           :weight normal
                           :slant italic))
      (,terminal-class (:background ,terminal-my-darkokai-bg
                                    :foreground ,terminal-my-darkokai-orange
                                    :weight normal
                                    :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,class (:foreground ,my-darkokai-cyan
                           :weight normal
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :weight normal
                                    :underline t))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,my-darkokai-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,class (:foreground ,my-darkokai-red-hc
                           :background ,my-darkokai-red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-my-darkokai-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,terminal-class (:foreground ,terminal-my-darkokai-red-hc
                                    :background ,terminal-my-darkokai-red-lc
                                    :weight bold
                                    :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,my-darkokai-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,class (:foreground ,my-darkokai-green-hc
                           :background ,my-darkokai-green-lc))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-my-darkokai-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,terminal-class (:foreground ,terminal-my-darkokai-green-hc
                                    :background ,terminal-my-darkokai-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,my-darkokai-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,class (:foreground ,my-darkokai-yellow-hc
                           :background ,my-darkokai-yellow-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-my-darkokai-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow-hc
                                    :background ,terminal-my-darkokai-yellow-lc
                                    :weight bold
                                    :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,my-darkokai-red)
                   :inherit unspecified))
      (,class (:foreground ,my-darkokai-red-hc
                           :background ,my-darkokai-red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-my-darkokai-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-my-darkokai-red-hc
                                    :background ,terminal-my-darkokai-red-lc
                                    :weight bold
                                    :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,my-darkokai-yellow)
                   :inherit unspecified))
      (,class (:foreground ,my-darkokai-yellow-hc
                           :background ,my-darkokai-yellow-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-my-darkokai-yellow)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow-hc
                                    :background ,terminal-my-darkokai-yellow-lc
                                    :weight bold
                                    :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,my-darkokai-blue)
                   :inherit unspecified))
      (,class (:foreground ,my-darkokai-blue-hc
                           :background ,my-darkokai-blue-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-my-darkokai-blue)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue-hc
                                    :background ,terminal-my-darkokai-blue-lc
                                    :weight bold
                                    :underline t))))

   `(flycheck-fringe-error
     ((,class (:foreground ,my-darkokai-red-hc
                           :background ,my-darkokai-red-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-red-hc
                                    :background ,terminal-my-darkokai-red-lc
                                    :weight bold))))

   `(flycheck-fringe-warning
     ((,class (:foreground ,my-darkokai-yellow-hc
                           :background ,my-darkokai-yellow-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow-hc
                                    :background ,terminal-my-darkokai-yellow-lc
                                    :weight bold))))

   `(flycheck-fringe-info
     ((,class (:foreground ,my-darkokai-blue-hc
                           :background ,my-darkokai-blue-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue-hc
                                    :background ,terminal-my-darkokai-blue-lc
                                    :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,my-darkokai-yellow)
                   :inherit unspecified))
      (,class (:foreground ,my-darkokai-yellow
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-my-darkokai-yellow)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight bold
                                    :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,my-darkokai-red)
                   :inherit unspecified))
      (,class (:foreground ,my-darkokai-red
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-my-darkokai-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :weight bold
                                    :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,class (:background ,my-darkokai-green
                           :foreground ,my-darkokai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-my-darkokai-green
                                    :foreground ,terminal-my-darkokai-bg
                                    :inherit bold))))

   `(git-gutter:deleted
     ((,class (:background ,my-darkokai-red
                           :foreground ,my-darkokai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-my-darkokai-red
                                    :foreground ,terminal-my-darkokai-bg
                                    :inherit bold))))

   `(git-gutter:modified
     ((,class (:background ,my-darkokai-blue
                           :foreground ,my-darkokai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-my-darkokai-blue
                                    :foreground ,terminal-my-darkokai-bg
                                    :inherit bold))))

   `(git-gutter:unchanged
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-bg
                                    :inherit bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,class (:foreground ,my-darkokai-green
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :inherit bold))))

   `(git-gutter-fr:deleted
     ((,class (:foreground ,my-darkokai-red
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :inherit bold))))

   `(git-gutter-fr:modified
     ((,class (:foreground ,my-darkokai-blue
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,class (:background ,my-darkokai-green
                           :foreground ,my-darkokai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-my-darkokai-green
                                    :foreground ,terminal-my-darkokai-bg
                                    :inherit bold))))

   `(git-gutter+-deleted
     ((,class (:background ,my-darkokai-red
                           :foreground ,my-darkokai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-my-darkokai-red
                                    :foreground ,terminal-my-darkokai-bg
                                    :inherit bold))))

   `(git-gutter+-modified
     ((,class (:background ,my-darkokai-blue
                           :foreground ,my-darkokai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-my-darkokai-blue
                                    :foreground ,terminal-my-darkokai-bg
                                    :inherit bold))))

   `(git-gutter+-unchanged
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-bg
                                    :inherit bold))))

   `(git-gutter-fr+-added
     ((,class (:foreground ,my-darkokai-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :weight bold))))

   `(git-gutter-fr+-deleted
     ((,class (:foreground ,my-darkokai-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :weight bold))))

   `(git-gutter-fr+-modified
     ((,class (:foreground ,my-darkokai-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :weight bold))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,class (:foreground ,my-darkokai-blue
                           :background ,my-darkokai-highlight-line
                           :inherit bold))
      (,terminal-class (:foreground ,my-darkokai-blue
                                    :background ,terminal-my-darkokai-highlight-line
                                    :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(guide-key/key-face
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(guide-key/prefix-command-face
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))

   ;; gnus
   `(gnus-group-mail-1
     ((,class (:weight bold
                       :inherit gnus-group-mail-1-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,class (:inherit gnus-group-news-1-empty))
      (,terminal-class (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,class (:weight bold
                       :inherit gnus-group-mail-2-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,class (:inherit gnus-group-news-2-empty))
      (,terminal-class (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,class (:weight bold
                       :inherit gnus-group-mail-3-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,class (:inherit gnus-group-news-3-empty))
      (,terminal-class (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,class (:weight bold
                       :inherit gnus-group-mail-low-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,class (:inherit gnus-group-news-low-empty))
      (,terminal-class (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,class (:weight bold
                       :inherit gnus-group-news-1-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,class (:weight bold
                       :inherit gnus-group-news-2-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,class (:weight bold
                       :inherit gnus-group-news-3-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,class (:weight bold
                       :inherit gnus-group-news-4-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,class (:weight bold
                       :inherit gnus-group-news-5-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,class (:weight bold
                       :inherit gnus-group-news-6-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,class (:weight bold
                       :inherit gnus-group-news-low-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,class (:inherit message-header-other))
      (,terminal-class (:inherit message-header-other))))

   `(gnus-header-from
     ((,class (:inherit message-header-other))
      (,terminal-class (:inherit message-header-other))))

   `(gnus-header-name
     ((,class (:inherit message-header-name))
      (,terminal-class (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,class (:inherit message-header-other))
      (,terminal-class (:inherit message-header-other))))

   `(gnus-header-subject
     ((,class (:inherit message-header-subject))
      (,terminal-class (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(gnus-summary-high-ancient
     ((,class (:foreground ,my-darkokai-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :weight bold))))

   `(gnus-summary-high-read
     ((,class (:foreground ,my-darkokai-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :weight bold))))

   `(gnus-summary-high-ticked
     ((,class (:foreground ,my-darkokai-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange
                                    :weight bold))))

   `(gnus-summary-high-unread
     ((,class (:foreground ,my-darkokai-fg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :weight bold))))

   `(gnus-summary-low-ancient
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(gnus-summary-low-read
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(gnus-summary-low-ticked
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(gnus-summary-low-unread
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(gnus-summary-normal-ancient
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(gnus-summary-normal-read
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(gnus-summary-normal-ticked
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(gnus-summary-normal-unread
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(gnus-summary-selected
     ((,class (:foreground ,my-darkokai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight bold))))

   `(gnus-cite-1
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(gnus-cite-2
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(gnus-cite-3
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(gnus-cite-4
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(gnus-cite-5
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(gnus-cite-6
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(gnus-cite-7
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(gnus-cite-8
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(gnus-cite-9
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(gnus-cite-10
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(gnus-cite-11
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(gnus-group-news-1-empty
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(gnus-group-news-2-empty
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(gnus-group-news-3-empty
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(gnus-group-news-4-empty
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(gnus-group-news-5-empty
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(gnus-group-news-6-empty
     ((,class (:foreground ,my-darkokai-blue-lc))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue-lc))))

   `(gnus-group-news-low-empty
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(gnus-signature
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(gnus-x-face
     ((,class (:background ,my-darkokai-fg
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-fg
                                    :foreground ,terminal-my-darkokai-bg))))


   ;; helm
   `(helm-apt-deinstalled
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(helm-apt-installed
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(helm-bookmark-directory
     ((,class (:inherit helm-ff-directory))
      (,terminal-class (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(helm-bookmark-gnus
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   `(helm-bookmark-info
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(helm-bookmark-man
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))

   `(helm-bookmark-w3m
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(helm-bookmarks-su
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(helm-buffer-file
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(helm-buffer-directory
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(helm-buffer-process
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(helm-buffer-saved-out
     ((,class (:foreground ,my-darkokai-red
                           :background ,my-darkokai-bg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :background ,terminal-my-darkokai-bg
                                    :inverse-video t))))

   `(helm-buffer-size
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(helm-candidate-number
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-emph
                           :bold t))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-emph
                                    :bold t))))

   `(helm-ff-directory
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(helm-ff-executable
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(helm-ff-file
     ((,class (:background ,my-darkokai-bg
                           :foreground ,my-darkokai-fg))
      (,terminal-class (:background ,terminal-my-darkokai-bg
                                    :foreground ,terminal-my-darkokai-fg))))

   `(helm-ff-invalid-symlink
     ((,class (:background ,my-darkokai-bg
                           :foreground ,my-darkokai-orange
                           :slant italic))
      (,terminal-class (:background ,terminal-my-darkokai-bg
                                    :foreground ,terminal-my-darkokai-orange
                                    :slant italic))))

   `(helm-ff-prefix
     ((,class (:background ,my-darkokai-green
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-green
                                    :foreground ,terminal-my-darkokai-bg))))

   `(helm-ff-symlink
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   `(helm-grep-file
     ((,class (:foreground ,my-darkokai-cyan
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :underline t))))

   `(helm-grep-finish
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(helm-grep-lineno
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(helm-grep-match
     ((,class (:inherit helm-match)))
     ((,terminal-class (:inherit helm-match))))

   `(helm-grep-running
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(helm-header
     ((,class (:inherit header-line))
      (,terminal-class (:inherit terminal-header-line))))

   `(helm-lisp-completion-info
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(helm-lisp-show-completion
     ((,class (:foreground ,my-darkokai-yellow
                           :background ,my-darkokai-highlight-line
                           :bold t))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :background ,terminal-my-darkokai-highlight-line
                                    :bold t))))

   `(helm-M-x-key
     ((,class (:foreground ,my-darkokai-orange
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange
                                    :underline t))))

   `(helm-moccur-buffer
     ((,class (:foreground ,my-darkokai-cyan
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :underline t))))

   `(helm-match
     ((,class (:foreground ,my-darkokai-green :inherit bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-green :inherit bold))))

   `(helm-match-item
     ((,class (:inherit helm-match))
      (,terminal-class (:inherit helm-match))))

   `(helm-selection
     ((,class (:background ,my-darkokai-highlight-line
                           :inherit bold
                           :underline nil))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :inherit bold
                                    :underline nil))))

   `(helm-selection-line
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-emph
                           :underline nil))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-emph
                                    :underline nil))))

   `(helm-separator
     ((,class (:foreground ,my-darkokai-gray))
      (,terminal-class (:foreground ,terminal-my-darkokai-gray))))

   `(helm-source-header
     ((,class (:background ,my-darkokai-violet-l
                           :foreground ,my-darkokai-bg
                           :underline nil))
      (,terminal-class (:background ,terminal-my-darkokai-violet-l
                                    :foreground ,terminal-my-darkokai-bg
                                    :underline nil))))

   `(helm-swoop-target-line-face
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))

   `(helm-swoop-target-line-block-face
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))

   `(helm-swoop-target-word-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(helm-time-zone-current
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(helm-time-zone-home
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(helm-visible-mark
     ((,class (:background ,my-darkokai-bg
                           :foreground ,my-darkokai-magenta :bold t))
      (,terminal-class (:background ,terminal-my-darkokai-bg
                                    :foreground ,terminal-my-darkokai-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,class :foreground ,my-darkokai-blue)
      (,terminal-class :foreground ,terminal-my-darkokai-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,class :foreground ,my-darkokai-blue-l)
      (,terminal-class :foreground ,terminal-my-darkokai-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,class :foreground ,my-darkokai-blue-l)
      (,terminal-class :foreground ,terminal-my-darkokai-blue-l)))

   `(helm-ls-git-untracked-face
     ((,class :foreground ,my-darkokai-orange)
      (,terminal-class :foreground ,terminal-my-darkokai-orange)))

   `(helm-ls-git-added-copied-face
     ((,class :foreground ,my-darkokai-green)
      (,terminal-class :foreground ,terminal-my-darkokai-green)))

   `(helm-ls-git-added-modified-face
     ((,class :foreground ,my-darkokai-green-l)
      (,terminal-class :foreground ,terminal-my-darkokai-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,class :foreground ,my-darkokai-red)
      (,terminal-class :foreground ,terminal-my-darkokai-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,class :foreground ,my-darkokai-red-l)
      (,terminal-class :foreground ,terminal-my-darkokai-red-l)))

   `(helm-ls-git-conflict-face
     ((,class :foreground ,my-darkokai-yellow)
      (,terminal-class :foreground ,terminal-my-darkokai-yellow)))

   ;; hi-lock-mode
   `(hi-yellow
     ((,class (:foreground ,my-darkokai-yellow-lc
                           :background ,my-darkokai-yellow-hc))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow-lc
                                    :background ,terminal-my-darkokai-yellow-hc))))

   `(hi-pink
     ((,class (:foreground ,my-darkokai-magenta-lc
                           :background ,my-darkokai-magenta-hc))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta-lc
                                    :background ,terminal-my-darkokai-magenta-hc))))

   `(hi-green
     ((,class (:foreground ,my-darkokai-green-lc
                           :background ,my-darkokai-green-hc))
      (,terminal-class (:foreground ,terminal-my-darkokai-green-lc
                                    :background ,terminal-my-darkokai-green-hc))))

   `(hi-blue
     ((,class (:foreground ,my-darkokai-blue-lc
                           :background ,my-darkokai-blue-hc))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue-lc
                                    :background ,terminal-my-darkokai-blue-hc))))

   `(hi-black-b
     ((,class (:foreground ,my-darkokai-emph
                           :background ,my-darkokai-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :background ,terminal-my-darkokai-bg
                                    :weight bold))))

   `(hi-blue-b
     ((,class (:foreground ,my-darkokai-blue-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue-lc
                                    :weight bold))))

   `(hi-green-b
     ((,class (:foreground ,my-darkokai-green-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-green-lc
                                    :weight bold))))

   `(hi-red-b
     ((,class (:foreground ,my-darkokai-red
                           :weight bold))))

   `(hi-black-hb
     ((,class (:foreground ,my-darkokai-emph
                           :background ,my-darkokai-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :background ,terminal-my-darkokai-bg
                                    :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(highlight-changes-delete
     ((,class (:foreground ,my-darkokai-red
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,class (:background ,my-darkokai-gray))
      (,terminal-class (:background ,terminal-my-darkokai-gray))))

   `(highlight-indentation-current-column-face
     ((,class (:background ,my-darkokai-gray))
      (,terminal-class (:background ,terminal-my-darkokai-gray))))

   ;; hl-line-mode
   `(hl-line
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))

   `(hl-line-face
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,class (:foreground ,my-darkokai-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight normal))))

   `(ido-only-match
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-yellow
                                    :weight normal))))

   `(ido-subdir
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(ido-incomplete-regexp
     ((,class (:foreground ,my-darkokai-red
                           :weight bold ))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :weight bold ))))

   `(ido-indicator
     ((,class (:background ,my-darkokai-red
                           :foreground ,my-darkokai-bg
                           :width condensed))
      (,terminal-class (:background ,terminal-my-darkokai-red
                                    :foreground ,terminal-my-darkokai-bg
                                    :width condensed))))

   `(ido-virtual
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   ;; ivy
   `(ivy-subdir
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(ivy-minibuffer-match-face-1
     ((,class (:foreground ,my-darkokai-comments
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :underline nil))))

   `(ivy-minibuffer-match-face-2
     ((,class (:foreground ,my-darkokai-violet-l
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet-l
                                    :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,class (:foreground ,my-darkokai-magenta-l
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta-l
                                    :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,class (:foreground ,my-darkokai-cyan-l
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan-l
                                    :underline t))))

   `(ivy-match-required-face
     ((,class (:foreground ,my-darkokai-red-plain))
      (,terminal-class (:foreground ,my-darkokai-red-plain))))

   `(ivy-current-match
     ((,class (:background ,my-darkokai-cyan-d
                           :foreground ,my-darkokai-cyan-l
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-cyan-d
                                    :foreground ,terminal-my-darkokai-cyan-l
                                    :weight bold))))

   ;; jabber

   `(jabber-activity-face
     ((,class (:weight bold
                       :foreground ,my-darkokai-red))
      (,terminal-class (:weight bold
                                :foreground ,terminal-my-darkokai-red))))

   `(jabber-activity-personal-face
     ((,class (:weight bold
                       :foreground ,my-darkokai-blue))
      (,terminal-class (:weight bold
                                :foreground ,terminal-my-darkokai-blue))))

   `(jabber-chat-error
     ((,class (:weight bold
                       :foreground ,my-darkokai-red))
      (,terminal-class (:weight bold
                                :foreground ,terminal-my-darkokai-red))))

   `(jabber-chat-prompt-foreign
     ((,class (:weight bold
                       :foreground ,my-darkokai-red))
      (,terminal-class (:weight bold
                                :foreground ,terminal-my-darkokai-red))))

   `(jabber-chat-prompt-local
     ((,class (:weight bold
                       :foreground ,my-darkokai-blue))
      (,terminal-class (:weight bold
                                :foreground ,terminal-my-darkokai-blue))))

   `(jabber-chat-prompt-system
     ((,class (:weight bold
                       :foreground ,my-darkokai-green))
      (,terminal-class (:weight bold
                                :foreground ,terminal-my-darkokai-green))))

   `(jabber-chat-text-foreign
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(jabber-chat-text-local
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(jabber-chat-rare-time-face
     ((,class (:underline t
                          :foreground ,my-darkokai-green))
      (,terminal-class (:underline t
                                   :foreground ,terminal-my-darkokai-green))))

   `(jabber-roster-user-away
     ((,class (:slant italic
                      :foreground ,my-darkokai-green))
      (,terminal-class (:slant italic
                               :foreground ,terminal-my-darkokai-green))))

   `(jabber-roster-user-chatty
     ((,class (:weight bold
                       :foreground ,my-darkokai-orange))
      (,terminal-class (:weight bold
                                :foreground ,terminal-my-darkokai-orange))))

   `(jabber-roster-user-dnd
     ((,class (:slant italic
                      :foreground ,my-darkokai-red))
      (,terminal-class (:slant italic
                               :foreground ,terminal-my-darkokai-red))))

   `(jabber-roster-user-error
     ((,class (:weight light
                       :slant italic
                       :foreground ,my-darkokai-red))
      (,terminal-class (:weight light
                                :slant italic
                                :foreground ,terminal-my-darkokai-red))))

   `(jabber-roster-user-offline
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(jabber-roster-user-online
     ((,class (:weight bold
                       :foreground ,my-darkokai-blue))
      (,terminal-class (:weight bold
                                :foreground ,terminal-my-darkokai-blue))))

   `(jabber-roster-user-xa
     ((,class (:slant italic
                      :foreground ,my-darkokai-magenta))
      (,terminal-class (:slant italic
                               :foreground ,terminal-my-darkokai-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(js2-external-variable
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(js2-function-param
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(js2-instance-member
     ((,class (:foreground ,my-darkokai-magenta))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta))))

   `(js2-jsdoc-html-tag-delimiter
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   `(js2-jsdoc-html-tag-name
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(js2-object-property
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(js2-function-call
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(js2-jsdoc-tag
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   `(js2-jsdoc-type
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(js2-jsdoc-value
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))

   `(js2-magic-paren
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(js2-private-function-call
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(js2-private-member
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(js2-warning
     ((,class (:underline ,my-darkokai-orange))
      (,terminal-class (:underline ,terminal-my-darkokai-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,class (:inherit bold))
      (,terminal-class (:inherit bold))))

   ;; Emacs native line numbers
   `(line-number
     ((,class (:foreground ,my-darkokai-highlight
                           :background ,s-fringe-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :background ,terminal-s-fringe-bg))))
   `(line-number-current-line
     ((,class (:foreground ,my-darkokai-fg
                           :background ,s-fringe-bg
                           :inherit default
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :background ,terminal-s-fringe-bg
                                    :inherit default
                                    :underline nil))))

   ;; linum-mode
   `(linum
     ((,class (:foreground ,my-darkokai-highlight
                           :background ,s-fringe-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :background ,terminal-s-fringe-bg))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,class (:inherit dimy-darkokai-red-directory))
      (,terminal-class (:inherit dimy-darkokai-red-directory))))

   `(lusty-file-face
     ((,class nil)
      (,terminal-class nil)))

   `(lusty-match-face
     ((,class (:inherit ido-first-match))
      (,terminal-class (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,class (:foreground ,my-darkokai-cyan
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :weight bold))))

   ;; magit
   ;;
   ;; TODO: Add supports for all magit faces
   ;; https://github.com/magit/magit/search?utf8=%E2%9C%93&q=face
   ;;
   `(magit-diff-added
     ((,class (:foreground ,my-darkokai-green-plain
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :background ,terminal-my-darkokai-bg))))

   `(magit-diff-added-highlight
     ((,class (:foreground ,my-darkokai-green-plain
                           :background ,my-darkokai-highlight-line))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :background ,terminal-my-darkokai-highlight-line))))

   `(magit-diff-removed
     ((,class (:foreground ,my-darkokai-red-plain
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :background ,terminal-my-darkokai-bg))))

   `(magit-diff-removed-highlight
     ((,class (:foreground ,my-darkokai-red-plain
                           :background ,my-darkokai-highlight-line))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :background ,terminal-my-darkokai-highlight-line))))

   `(magit-section-highlight
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))

   `(magit-section-title
     ((,class (:foreground ,my-darkokai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight bold))))

   `(magit-branch
     ((,class (:foreground ,my-darkokai-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange
                                    :weight bold))))

   `(magit-cherry-equivalent
     ((,class (:foreground ,my-darkokai-magenta))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta))))

   `(magit-cherry-unmatched
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   `(magit-head
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))

   `(magit-branch-local
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))

   `(magit-branch-remote
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(magit-section-heading
     ((,class (:foreground ,my-darkokai-yellow :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow :weight bold))))

   `(magit-process-ok
     ((,class (:foreground ,my-darkokai-green-plain
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :weight bold))))

   `(magit-process-ng
     ((,class (:foreground ,my-darkokai-red-plain
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :weight bold))))

   `(magit-item-highlight
     ((,class (:background ,my-darkokai-highlight-line
                           :weight unspecified))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :weight unspecified))))

   `(magit-log-author
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   `(magit-log-graph
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(magit-log-head-label-bisect-bad
     ((,class (:background ,my-darkokai-red-hc
                           :foreground ,my-darkokai-red-lc
                           :box 1))
      (,terminal-class (:background ,terminal-my-darkokai-red-hc
                                    :foreground ,terminal-my-darkokai-red-lc
                                    :box 1))))

   `(magit-log-head-label-bisect-good
     ((,class (:background ,my-darkokai-green-hc
                           :foreground ,my-darkokai-green-lc
                           :box 1))
      (,terminal-class (:background ,terminal-my-darkokai-green-hc
                                    :foreground ,terminal-my-darkokai-green-lc
                                    :box 1))))

   `(magit-log-head-label-default
     ((,class (:background ,my-darkokai-highlight-line
                           :box 1))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :box 1))))

   `(magit-log-head-label-local
     ((,class (:background ,my-darkokai-blue-lc
                           :foreground ,my-darkokai-blue-hc
                           :box 1))
      (,terminal-class (:background ,terminal-my-darkokai-blue-lc
                                    :foreground ,terminal-my-darkokai-blue-hc
                                    :box 1))))

   `(magit-log-head-label-patches
     ((,class (:background ,my-darkokai-red-lc
                           :foreground ,my-darkokai-red-hc
                           :box 1))
      (,terminal-class (:background ,terminal-my-darkokai-red-lc
                                    :foreground ,terminal-my-darkokai-red-hc
                                    :box 1))))

   `(magit-log-head-label-remote
     ((,class (:background ,my-darkokai-green-lc
                           :foreground ,my-darkokai-green-hc
                           :box 1))
      (,terminal-class (:background ,terminal-my-darkokai-green-lc
                                    :foreground ,terminal-my-darkokai-green-hc
                                    :box 1))))

   `(magit-log-head-label-tags
     ((,class (:background ,my-darkokai-yellow-lc
                           :foreground ,my-darkokai-yellow-hc
                           :box 1))
      (,terminal-class (:background ,terminal-my-darkokai-yellow-lc
                                    :foreground ,terminal-my-darkokai-yellow-hc
                                    :box 1))))

   `(magit-log-sha1
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(magit-reflog-amend
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))
   `(magit-reflog-rebase
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))
   `(magit-reflog-checkout
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))
   `(magit-reflog-reset
     ((,class (:foreground ,my-darkokai-red-plain))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))
   `(magit-reflog-commit
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))
   `(magit-reflog-merge
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))
   `(magit-reflog-cherry-pick
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))
   `(magit-reflog-other
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))
   `(magit-reflog-remote
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   ;; man
   `(Man-overstrike
     ((,class (:foreground ,my-darkokai-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :weight bold))))

   `(Man-reverse
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(Man-underline
     ((,class (:foreground ,my-darkokai-green :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,class (:foreground ,my-darkokai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight bold))))

   `(monky-diff-title
     ((,class (:foreground ,my-darkokai-cyan-l
                           :background ,my-darkokai-gray-ld))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan-l
                                    :background ,terminal-my-darkokai-gray-d))))

   `(monky-diff-add
     ((,class (:foreground ,my-darkokai-green-plain))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(monky-diff-del
     ((,class (:foreground ,my-darkokai-red-plain))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(markdown-header-face-1
     ((,class (:inherit markdown-header-face
                        :height ,my-darkokai-height-plus-4))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,my-darkokai-height-plus-4))))

   `(markdown-header-face-2
     ((,class (:inherit markdown-header-face
                        :height ,my-darkokai-height-plus-3))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,my-darkokai-height-plus-3))))

   `(markdown-header-face-3
     ((,class (:inherit markdown-header-face
                        :height ,my-darkokai-height-plus-2))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,my-darkokai-height-plus-2))))

   `(markdown-header-face-4
     ((,class (:inherit markdown-header-face
                        :height ,my-darkokai-height-plus-1))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,my-darkokai-height-plus-1))))

   `(markdown-header-face-5
     ((,class (:inherit markdown-header-face))
      (,terminal-class (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,class (:inherit markdown-header-face))
      (,terminal-class (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(message-header-name
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(message-header-other
     ((,class (:foreground ,my-darkokai-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :weight normal))))

   `(message-header-to
     ((,class (:foreground ,my-darkokai-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :weight normal))))

   `(message-header-cc
     ((,class (:foreground ,my-darkokai-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :weight normal))))

   `(message-header-newsgroups
     ((,class (:foreground ,my-darkokai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight bold))))

   `(message-header-subject
     ((,class (:foreground ,my-darkokai-cyan
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :weight normal))))

   `(message-header-xheader
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   `(message-mml
     ((,class (:foreground ,my-darkokai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight bold))))

   `(message-separator
     ((,class (:foreground ,my-darkokai-comments
                           :slant italic))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(mew-face-header-from
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(mew-face-header-date
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(mew-face-header-to
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(mew-face-header-key
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(mew-face-header-private
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(mew-face-header-important
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(mew-face-header-marginal
     ((,class (:foreground ,my-darkokai-fg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :weight bold))))

   `(mew-face-header-warning
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(mew-face-header-xmew
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(mew-face-header-xmew-bad
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(mew-face-body-url
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(mew-face-body-comment
     ((,class (:foreground ,my-darkokai-fg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :slant italic))))

   `(mew-face-body-cite1
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(mew-face-body-cite2
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(mew-face-body-cite3
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(mew-face-body-cite4
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(mew-face-body-cite5
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(mew-face-mark-review
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(mew-face-mark-escape
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(mew-face-mark-delete
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(mew-face-mark-unlink
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(mew-face-mark-refile
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(mew-face-mark-unread
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(mew-face-eof-message
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(mew-face-eof-part
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(mingus-pausing-face
     ((,class (:foreground ,my-darkokai-magenta))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta))))

   `(mingus-playing-face
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   `(mingus-playlist-face
     ((,class (:foreground ,my-darkokai-cyan ))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan ))))

   `(mingus-song-file-face
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(mingus-stopped-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   ;; mmm
   `(mmm-init-submode-face
     ((,class (:background ,my-darkokai-violet-d))
      (,terminal-class (:background ,terminal-my-darkokai-violet-d))))

   `(mmm-cleanup-submode-face
     ((,class (:background ,my-darkokai-orange-d))
      (,terminal-class (:background ,terminal-my-darkokai-orange-d))))

   `(mmm-declaration-submode-face
     ((,class (:background ,my-darkokai-cyan-d))
      (,terminal-class (:background ,terminal-my-darkokai-cyan-d))))

   `(mmm-comment-submode-face
     ((,class (:background ,my-darkokai-blue-d))
      (,terminal-class (:background ,terminal-my-darkokai-blue-d))))

   `(mmm-output-submode-face
     ((,class (:background ,my-darkokai-red-d))
      (,terminal-class (:background ,terminal-my-darkokai-red-d))))

   `(mmm-special-submode-face
     ((,class (:background ,my-darkokai-green-d))
      (,terminal-class (:background ,terminal-my-darkokai-green-d))))

   `(mmm-code-submode-face
     ((,class (:background ,my-darkokai-gray))
      (,terminal-class (:background ,terminal-my-darkokai-gray))))

   `(mmm-default-submode-face
     ((,class (:background ,my-darkokai-gray-d))
      (,terminal-class (:background ,terminal-my-darkokai-gray-d))))

   ;; moccur
   `(moccur-current-line-face
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(moccur-edit-done-face
     ((,class (:foreground ,my-darkokai-comments
                           :background ,my-darkokai-bg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :background ,terminal-my-darkokai-bg
                                    :slant italic))))

   `(moccur-edit-face
     ((,class (:background ,my-darkokai-yellow
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-yellow
                                    :foreground ,terminal-my-darkokai-bg))))

   `(moccur-edit-file-face
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))

   `(moccur-edit-reject-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(moccur-face
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-emph
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-emph
                                    :weight bold))))

   `(search-buffers-face
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-emph
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-emph
                                    :weight bold))))

   `(search-buffers-header-face
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-yellow
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-yellow
                                    :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,class (:foreground ,my-darkokai-green
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-2-face
     ((,class (:foreground ,my-darkokai-blue
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-3-face
     ((,class (:foreground ,my-darkokai-orange
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-4-face
     ((,class (:foreground ,my-darkokai-yellow
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-5-face
     ((,class (:foreground ,my-darkokai-cyan
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-6-face
     ((,class (:foreground ,my-darkokai-green
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-7-face
     ((,class (:foreground ,my-darkokai-blue
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :slant italic
                                    :weight normal))))

   `(mu4e-flagged-face
     ((,class (:foreground ,my-darkokai-magenta
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta
                                    :weight bold))))

   `(mu4e-view-url-number-face
     ((,class (:foreground ,my-darkokai-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight normal))))

   `(mu4e-warning-face
     ((,class (:foreground ,my-darkokai-red
                           :slant normal
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :slant normal
                                    :weight bold))))

   `(mu4e-header-highlight-face
     ((,class (:inherit unspecified
                        :foreground unspecified
                        :background ,my-darkokai-highlight-line
                        :underline ,my-darkokai-emph
                        :weight normal))
      (,terminal-class (:inherit unspecified
                                 :foreground unspecified
                                 :background ,terminal-my-darkokai-highlight-line
                                 :underline ,terminal-my-darkokai-emph
                                 :weight normal))))


   `(mu4e-draft-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,class (:inherit font-lock-comment-face))
      (,terminal-class (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,class (:inherit font-lock-builtin-face
                        :weight normal))
      (,terminal-class (:inherit font-lock-builtin-face
                                 :weight normal))))

   `(mu4e-header-face
     ((,class (:inherit default))
      (,terminal-class (:inherit default))))

   `(mu4e-header-marks-face
     ((,class (:inherit font-lock-preprocessor-face))
      (,terminal-class (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,class (:inherit font-lock-type-face))
      (,terminal-class (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,class (:inherit font-lock-pseudo-keyword-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-pseudo-keyword-face
                                 :weight bold))))

   `(mu4e-moved-face
     ((,class (:inherit font-lock-comment-face
                        :slant italic))
      (,terminal-class (:inherit font-lock-comment-face
                                 :slant italic))))

   `(mu4e-ok-face
     ((,class (:inherit font-lock-comment-face
                        :slant normal
                        :weight bold))
      (,terminal-class (:inherit font-lock-comment-face
                                 :slant normal
                                 :weight bold))))

   `(mu4e-replied-face
     ((,class (:inherit font-lock-function-name-face
                        :weight normal))
      (,terminal-class (:inherit font-lock-function-face
                                 :weight normal))))

   `(mu4e-system-face
     ((,class (:inherit font-lock-comment-face
                        :slant italic))
      (,terminal-class (:inherit font-lock-comment-face
                                 :slant italic))))

   `(mu4e-title-face
     ((,class (:inherit font-lock-type-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-type-face
                                 :weight bold))))

   `(mu4e-trashed-face
     ((,class (:inherit font-lock-comment-face
                        :strike-through t))
      (,terminal-class (:inherit font-lock-comment-face
                                 :strike-through t))))

   `(mu4e-unread-face
     ((,class (:inherit font-lock-keyword-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-keyword-face
                                 :weight bold))))

   `(mu4e-view-attach-number-face
     ((,class (:inherit font-lock-variable-name-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-variable-name-face
                                 :weight bold))))

   `(mu4e-view-contact-face
     ((,class (:foreground ,my-darkokai-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :weight normal))))

   `(mu4e-view-header-key-face
     ((,class (:inherit message-header-name
                        :weight normal))
      (,terminal-class (:inherit message-header-name
                                 :weight normal))))

   `(mu4e-view-header-value-face
     ((,class (:foreground ,my-darkokai-cyan
                           :weight normal
                           :slant normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :weight normal
                                    :slant normal))))

   `(mu4e-view-link-face
     ((,class (:inherit link))
      (,terminal-class (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,class (:foreground ,my-darkokai-blue
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :weight normal
                                    :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(nav-face-button-num
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   `(nav-face-dir
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(nav-face-hdir
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(nav-face-file
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(nav-face-hfile
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,class (:foreground ,my-darkokai-blue
                           :background ,my-darkokai-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :background ,terminal-my-darkokai-bg
                                    :weight bold))))


   `(neo-header-face
     ((,class (:foreground ,my-darkokai-emph
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :background ,terminal-my-darkokai-bg))))

   `(neo-root-dir-face
     ((,class (:foreground ,my-darkokai-green
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :background ,terminal-my-darkokai-bg))))

   `(neo-dir-link-face
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :background ,terminal-my-darkokai-bg))))

   `(neo-file-link-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(neo-button-face
     ((,class (:underline nil))
      (,terminal-class (:underline nil))))

   `(neo-expand-btn-face
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(neo-vc-default-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(neo-vc-user-face
     ((,class (:foreground ,my-darkokai-red
                           :slant italic))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :slant italic))))

   `(neo-vc-up-to-date-face
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(neo-vc-edited-face
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(neo-vc-needs-update-face
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(neo-vc-needs-merge-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(neo-vc-unlocked-changes-face
     ((,class (:foreground ,my-darkokai-red
                           :background ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :background ,terminal-my-darkokai-comments))))

   `(neo-vc-added-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(neo-vc-removed-face
     ((,class (:strike-through t))
      (,terminal-class (:strike-through t))))

   `(neo-vc-conflict-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(neo-vc-missing-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(neo-vc-ignored-face
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))


   ;; org-mode
   `(org-agenda-structure
     ((,class (:foreground ,my-darkokai-emph
                           :background ,my-darkokai-highlight-line
                           :weight bold
                           :slant normal
                           :inverse-video nil
                           :height ,my-darkokai-height-plus-1
                           :underline nil
                           :box (:line-width 2 :color ,my-darkokai-bg)))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :background ,terminal-my-darkokai-highlight-line
                                    :weight bold
                                    :slant normal
                                    :inverse-video nil
                                    :height ,my-darkokai-height-plus-1
                                    :underline nil
                                    :box (:line-width 2 :color ,terminal-my-darkokai-bg)))))

   `(org-agenda-calendar-event
     ((,class (:foreground ,my-darkokai-emph))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph))))

   `(org-agenda-calendar-sexp
     ((,class (:foreground ,my-darkokai-fg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :slant italic))))

   `(org-agenda-date
     ((,class (:foreground ,my-darkokai-comments
                           :background ,my-darkokai-bg
                           :weight normal
                           :inverse-video nil
                           :overline nil
                           :slant normal
                           :height 1.0
                           :box (:line-width 2 :color ,my-darkokai-bg)))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :background ,terminal-my-darkokai-bg
                                    :weight normal
                                    :inverse-video nil
                                    :overline nil
                                    :slant normal
                                    :height 1.0
                                    :box (:line-width 2 :color ,terminal-my-darkokai-bg)))) t)

   `(org-agenda-date-weekend
     ((,class (:inherit org-agenda-date
                        :inverse-video nil
                        :background unspecified
                        :foreground ,my-darkokai-comments
                        :weight unspecified
                        :underline t
                        :overline nil
                        :box unspecified))
      (,terminal-class (:inherit org-agenda-date
                                 :inverse-video nil
                                 :background unspecified
                                 :foreground ,terminal-my-darkokai-comments
                                 :weight unspecified
                                 :underline t
                                 :overline nil
                                 :box unspecified))) t)

   `(org-agenda-date-today
     ((,class (:inherit org-agenda-date
                        :inverse-video t
                        :weight bold
                        :underline unspecified
                        :overline nil
                        :box unspecified
                        :foreground ,my-darkokai-blue
                        :background ,my-darkokai-bg))
      (,terminal-class (:inherit org-agenda-date
                                 :inverse-video t
                                 :weight bold
                                 :underline unspecified
                                 :overline nil
                                 :box unspecified
                                 :foreground ,terminal-my-darkokai-blue
                                 :background ,terminal-my-darkokai-bg))) t)

   `(org-agenda-done
     ((,class (:foreground ,my-darkokai-comments
                           :slant italic))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :slant italic))) t)

   `(org-archived
     ((,class (:foreground ,my-darkokai-comments
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :weight normal))))

   `(org-block
     ((,class (:foreground ,my-darkokai-emph
                           :background, my-darkokai-highlight))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :background, terminal-my-darkokai-highlight))))

   `(org-block-background
     ((,class (:background ,my-darkokai-highlight))
      (,terminal-class (:background ,terminal-my-darkokai-highlight))))

   `(org-block-begin-line
     ((,class (:foreground ,my-darkokai-comments
                           :background ,my-darkokai-gray-d
                           :slant italic))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :slant italic))))
   `(org-block-end-line
     ((,class (:foreground ,my-darkokai-comments
                           :background ,my-darkokai-gray-d
                           :slant italic))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :slant italic))))

   `(org-checkbox
     ((,class (:background ,my-darkokai-gray
                           :foreground ,my-darkokai-violet
                           :box nil))
      (,terminal-class (:background ,terminal-my-darkokai-gray
                                    :foreground ,terminal-my-darkokai-violet
                                    :box nil))))

   `(org-code
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(org-date
     ((,class (:foreground ,my-darkokai-blue
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :underline t))))

   `(org-done
     ((,class (:weight bold
                       :foreground ,my-darkokai-green))
      (,terminal-class (:weight bold
                                :foreground ,terminal-my-darkokai-green))))

   `(org-ellipsis
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(org-formula
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(org-headline-done
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(org-hide
     ((,class (:foreground ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg))))

   `(org-level-1
     ((,class (:inherit ,s-variable-pitch
                        :height ,my-darkokai-height-plus-4
                        :foreground ,my-darkokai-orange))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,my-darkokai-height-plus-4
                                 :foreground ,terminal-my-darkokai-orange))))

   `(org-level-2
     ((,class (:inherit ,s-variable-pitch
                        :height ,my-darkokai-height-plus-3
                        :foreground ,my-darkokai-green))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,my-darkokai-height-plus-3
                                 :foreground ,terminal-my-darkokai-green))))

   `(org-level-3
     ((,class (:inherit ,s-variable-pitch
                        :height ,my-darkokai-height-plus-2
                        :foreground ,my-darkokai-blue))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,my-darkokai-height-plus-2
                                 :foreground ,terminal-my-darkokai-blue))))

   `(org-level-4
     ((,class (:inherit ,s-variable-pitch
                        :height ,my-darkokai-height-plus-1
                        :foreground ,my-darkokai-yellow))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :height ,my-darkokai-height-plus-1
                                 :foreground ,terminal-my-darkokai-yellow))))

   `(org-level-5
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,my-darkokai-cyan))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-my-darkokai-cyan))))

   `(org-level-6
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,my-darkokai-green))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-my-darkokai-green))))

   `(org-level-7
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,my-darkokai-red))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-my-darkokai-red))))

   `(org-level-8
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,my-darkokai-blue))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-my-darkokai-blue))))

   `(org-link
     ((,class (:foreground ,my-darkokai-yellow
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :underline t))))

   `(org-sexp-date
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))

   `(org-scheduled
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(org-scheduled-previously
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   `(org-scheduled-today
     ((,class (:foreground ,my-darkokai-blue
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :weight normal))))

   `(org-special-keyword
     ((,class (:foreground ,my-darkokai-comments
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :weight bold))))

   `(org-table
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(org-tag
     ((,class (:weight bold))
      (,terminal-class (:weight bold))))

   `(org-time-grid
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(org-todo
     ((,class (:foreground ,my-darkokai-red
                           :weight bold)))
     ((,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :weight bold))))

   `(org-upcoming-deadline
     ((,class (:foreground ,my-darkokai-yellow
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight normal
                                    :underline nil))))

   `(org-warning
     ((,class (:foreground ,my-darkokai-orange
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange
                                    :weight normal
                                    :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,class (:background ,my-darkokai-blue-lc
                           :foreground ,my-darkokai-blue-hc))
      (,terminal-class (:background ,terminal-my-darkokai-blue-lc
                                    :foreground ,terminal-my-darkokai-blue-hc))))

   `(org-habit-clear-future-face
     ((,class (:background ,my-darkokai-blue-l))
      (,terminal-class (:background ,terminal-my-darkokai-blue-l))))

   `(org-habit-ready-face
     ((,class (:background ,my-darkokai-green-plain))
      (,terminal-class (:background ,terminal-my-darkokai-green))))

   `(org-habit-ready-future-face
     ((,class (:background ,my-darkokai-green-lc))
      (,terminal-class (:background ,terminal-my-darkokai-green-lc))))

   `(org-habit-alert-face
     ((,class (:background ,my-darkokai-yellow))
      (,terminal-class (:background ,terminal-my-darkokai-yellow))))

   `(org-habit-alert-future-face
     ((,class (:background ,my-darkokai-yellow-lc))
      (,terminal-class (:background ,terminal-my-darkokai-yellow-lc))))

   `(org-habit-overdue-face
     ((,class (:background ,my-darkokai-red-plain))
      (,terminal-class (:background ,terminal-my-darkokai-red))))

   `(org-habit-overdue-future-face
     ((,class (:background ,my-darkokai-red-hc))
      (,terminal-class (:background ,terminal-my-darkokai-red-hc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(org-agenda-restriction-lock
     ((,class (:background ,my-darkokai-yellow))
      (,terminal-class (:background ,terminal-my-darkokai-yellow))))

   `(org-clock-overlay
     ((,class (:background ,my-darkokai-yellow))
      (,terminal-class (:background ,terminal-my-darkokai-yellow))))

   `(org-column
     ((,class (:background ,my-darkokai-highlight-line
                           :strike-through nil
                           :underline nil
                           :slant normal
                           :weight normal
                           :inherit default))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :strike-through nil
                                    :underline nil
                                    :slant normal
                                    :weight normal
                                    :inherit default))))

   `(org-column-title
     ((,class (:background ,my-darkokai-highlight-line
                           :underline t
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :underline t
                                    :weight bold))))

   `(org-date-selected
     ((,class (:foreground ,my-darkokai-red
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :inverse-video t))))

   `(org-document-info
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(org-document-title
     ((,class (:foreground ,my-darkokai-emph
                           :weight bold
                           :height ,my-darkokai-height-plus-4))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :weight bold
                                    :height ,my-darkokai-height-plus-4))))

   `(org-drawer
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   `(org-footnote
     ((,class (:foreground ,my-darkokai-magenta
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta
                                    :underline t))))

   `(org-latex-and-export-specials
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(org-mode-line-clock-overrun
     ((,class (:inherit mode-line))
      (,terminal-class (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,class (:inherit org-level-1))
      (,terminal-class (:inherit org-level-1))))

   `(outline-2
     ((,class (:inherit org-level-2))
      (,terminal-class (:inherit org-level-2))))

   `(outline-3
     ((,class (:inherit org-level-3))
      (,terminal-class (:inherit org-level-3))))

   `(outline-4
     ((,class (:inherit org-level-4))
      (,terminal-class (:inherit org-level-4))))

   `(outline-5
     ((,class (:inherit org-level-5))
      (,terminal-class (:inherit org-level-5))))

   `(outline-6
     ((,class (:inherit org-level-6))
      (,terminal-class (:inherit org-level-6))))

   `(outline-7
     ((,class (:inherit org-level-7))
      (,terminal-class (:inherit org-level-7))))

   `(outline-8
     ((,class (:inherit org-level-8))
      (,terminal-class (:inherit org-level-8))))

   ;; parenface
   `(paren-face
     ((,terminal-class (:foreground ,my-darkokai-comments))))

   ;; perspective
   `(persp-selected-face
     ((,class (:foreground ,my-darkokai-violet
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet
                                    :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,class (:foreground ,my-darkokai-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight normal))))

   ;; popup
   `(popup-face
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-fg))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-fg))))

   `(popup-isearch-match
     ((,class (:background ,my-darkokai-green))
      (,terminal-class (:background ,terminal-my-darkokai-green))))

   `(popup-menu-face
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-fg))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-fg))))

   `(popup-menu-mouse-face
     ((,class (:background ,my-darkokai-blue
                           :foreground ,my-darkokai-fg))
      (,terminal-class (:background ,terminal-my-darkokai-blue
                                    :foreground ,terminal-my-darkokai-fg))))

   `(popup-menu-selection-face
     ((,class (:background ,my-darkokai-magenta
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-magenta
                                    :foreground ,terminal-my-darkokai-bg))))

   `(popup-scroll-bar-background-face
     ((,class (:background ,my-darkokai-comments))
      (,terminal-class (:background ,terminal-my-darkokai-comments))))

   `(popup-scroll-bar-foreground-face
     ((,class (:background ,my-darkokai-emph))
      (,terminal-class (:background ,terminal-my-darkokai-emph))))

   `(popup-tip-face
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-fg))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-fg))))

   ;; powerline
   `(powerline-active1
     ((,class (:background ,s-powerline-active1-bg :foreground ,my-darkokai-fg))
      (,terminal-class (:background ,terminal-my-darkokai-gray :foreground ,my-darkokai-fg))))

   `(powerline-active2
     ((,class (:background ,s-powerline-active2-bg :foreground ,my-darkokai-fg))
      (,terminal-class (:background ,terminal-my-darkokai-gray-l :foreground ,my-darkokai-fg))))

   `(powerline-inactive1
     ((,class (:background ,s-powerline-inactive1-bg))
      (,terminal-class (:background ,terminal-my-darkokai-gray-d))))

   `(powerline-inactive2
     ((,class (:background ,s-powerline-inactive2-bg))
      (,terminal-class (:background ,terminal-my-darkokai-gray))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(rainbow-delimiters-depth-4-face
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(rainbow-delimiters-depth-5-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(rainbow-delimiters-depth-6-face
     ((,class (:foreground ,my-darkokai-magenta-d))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta))))

   `(rainbow-delimiters-depth-7-face
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(rainbow-delimiters-unmatched-face
     ((,class (:foreground ,my-darkokai-fg
                           :background ,my-darkokai-bg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :background ,terminal-my-darkokai-bg
                                    :inverse-video t))))

   ;; rhtm-mode
   `(erb-face
     ((,class (:foreground ,my-darkokai-emph
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :background ,terminal-my-darkokai-bg))))

   `(erb-delim-face
     ((,class (:foreground ,my-darkokai-cyan
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :background ,terminal-my-darkokai-bg))))

   `(erb-exec-face
     ((,class (:foreground ,my-darkokai-emph
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :background ,terminal-my-darkokai-bg))))

   `(erb-exec-delim-face
     ((,class (:foreground ,my-darkokai-cyan
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :background ,terminal-my-darkokai-bg))))

   `(erb-out-face
     ((,class (:foreground ,my-darkokai-emph
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :background ,terminal-my-darkokai-bg))))

   `(erb-out-delim-face
     ((,class (:foreground ,my-darkokai-cyan
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :background ,terminal-my-darkokai-bg))))

   `(erb-comment-face
     ((,class (:foreground ,my-darkokai-emph
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :background ,terminal-my-darkokai-bg))))

   `(erb-comment-delim-face
     ((,class (:foreground ,my-darkokai-cyan
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :background ,terminal-my-darkokai-bg))))

   ;; rst-mode
   `(rst-level-1-face
     ((,class (:background ,my-darkokai-yellow
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-yellow
                                    :foreground ,terminal-my-darkokai-bg))))

   `(rst-level-2-face
     ((,class (:background ,my-darkokai-cyan
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-cyan
                                    :foreground ,terminal-my-darkokai-bg))))

   `(rst-level-3-face
     ((,class (:background ,my-darkokai-blue
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-blue
                                    :foreground ,terminal-my-darkokai-bg))))

   `(rst-level-4-face
     ((,class (:background ,my-darkokai-violet
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-violet
                                    :foreground ,terminal-my-darkokai-bg))))

   `(rst-level-5-face
     ((,class (:background ,my-darkokai-magenta
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-magenta
                                    :foreground ,terminal-my-darkokai-bg))))

   `(rst-level-6-face
     ((,class (:background ,my-darkokai-red
                           :foreground ,my-darkokai-bg))
      (,terminal-class (:background ,terminal-my-darkokai-red
                                    :foreground ,terminal-my-darkokai-bg))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(rpm-spec-doc-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(rpm-spec-ghost-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(rpm-spec-macro-face
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(rpm-spec-package-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(rpm-spec-section-face
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(rpm-spec-tag-face
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(rpm-spec-var-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,class (:foreground ,my-darkokai-violet
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet
                                    :weight bold))))

   `(sh-escaped-newline
     ((,class (:foreground ,my-darkokai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight bold))))

   `(sh-heredoc
     ((,class (:foreground ,my-darkokai-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight bold))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))

   `(sp-wrap-overlay-face
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))

   `(sp-show-pair-enclosing
     ((,class (:inherit highlight))
      (,terminal-class (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,class (:background ,my-darkokai-cyan-l
                           :foreground ,my-darkokai-cyan-d
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :background ,terminal-my-darkokai-bg
                                    :weight normal
                                    :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,class (:foreground ,my-darkokai-red
                           :background ,my-darkokai-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :background ,terminal-my-darkokai-bg
                                    :weight normal
                                    :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,class (:foreground ,my-darkokai-cyan-d
                           :background ,my-darkokai-cyan-l
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan-d
                                    :background ,terminal-my-darkokai-cyan-l
                                    :weight normal
                                    :inverse-video t))))

   `(show-paren-mismatch
     ((,class (:foreground ,my-darkokai-red
                           :background ,my-darkokai-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :background ,terminal-my-darkokai-bg
                                    :weight normal
                                    :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,class (:foreground ,my-darkokai-green
                           :background ,my-darkokai-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :background ,terminal-my-darkokai-bg
                                    :weight normal
                                    :inverse-video t))))

   `(paren-face-mismatch
     ((,class (:foreground ,my-darkokai-red
                           :background ,my-darkokai-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :background ,terminal-my-darkokai-bg
                                    :weight normal
                                    :inverse-video t))))

   `(paren-face-no-match
     ((,class (:foreground ,my-darkokai-red
                           :background ,my-darkokai-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :background ,terminal-my-darkokai-bg
                                    :weight normal
                                    :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   ;; speedbar
   `(speedbar-button-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,my-darkokai-comments))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-my-darkokai-comments))))

   `(speedbar-directory-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,my-darkokai-blue))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-my-darkokai-blue))))

   `(speedbar-file-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,my-darkokai-fg))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-my-darkokai-fg))))

   `(speedbar-highlight-face
     ((,class (:inherit ,s-variable-pitch
                        :background ,my-darkokai-highlight-line))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :background ,terminal-my-darkokai-highlight-line))))

   `(speedbar-selected-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,my-darkokai-yellow
                        :underline t))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-my-darkokai-yellow
                                 :underline t))))

   `(speedbar-separator-face
     ((,class (:inherit ,s-variable-pitch
                        :background ,my-darkokai-blue
                        :foreground ,my-darkokai-bg
                        :overline ,my-darkokai-cyan-lc))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :background ,terminal-my-darkokai-blue
                                 :foreground ,terminal-my-darkokai-bg
                                 :overline ,terminal-my-darkokai-cyan-lc))))

   `(speedbar-tag-face
     ((,class (:inherit ,s-variable-pitch
                        :foreground ,my-darkokai-green))
      (,terminal-class (:inherit ,terminal-s-variable-pitch
                                 :foreground ,terminal-my-darkokai-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,class (:background ,my-darkokai-blue
                           :foreground ,my-darkokai-bg
                           :height ,my-darkokai-height-plus-1
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-blue
                                    :foreground ,terminal-my-darkokai-bg
                                    :height ,my-darkokai-height-plus-1
                                    :weight bold))))

   `(sr-editing-path-face
     ((,class (:background ,my-darkokai-yellow
                           :foreground ,my-darkokai-bg
                           :weight bold
                           :height ,my-darkokai-height-plus-1))
      (,terminal-class (:background ,terminal-my-darkokai-yellow
                                    :foreground ,terminal-my-darkokai-bg
                                    :weight bold
                                    :height ,my-darkokai-height-plus-1))))

   `(sr-highlight-path-face
     ((,class (:background ,my-darkokai-green
                           :foreground ,my-darkokai-bg
                           :weight bold
                           :height ,my-darkokai-height-plus-1))
      (,terminal-class (:background ,terminal-my-darkokai-green
                                    :foreground ,terminal-my-darkokai-bg
                                    :weight bold
                                    :height ,my-darkokai-height-plus-1))))

   `(sr-passive-path-face
     ((,class (:background ,my-darkokai-comments
                           :foreground ,my-darkokai-bg
                           :weight bold
                           :height ,my-darkokai-height-plus-1))
      (,terminal-class (:background ,terminal-my-darkokai-comments
                                    :foreground ,terminal-my-darkokai-bg
                                    :weight bold
                                    :height ,my-darkokai-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,class (:inherit dimy-darkokai-red-marked))
      (,terminal-class (:inherit dimy-darkokai-red-marked))))

   `(sr-marked-file-face
     ((,class (:inherit dimy-darkokai-red-marked))
      (,terminal-class (:inherit dimy-darkokai-red-marked))))

   `(sr-alt-marked-dir-face
     ((,class (:background ,my-darkokai-magenta
                           :foreground ,my-darkokai-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-magenta
                                    :foreground ,terminal-my-darkokai-bg
                                    :weight bold))))

   `(sr-alt-marked-file-face
     ((,class (:background ,my-darkokai-magenta
                           :foreground ,my-darkokai-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-magenta
                                    :foreground ,terminal-my-darkokai-bg
                                    :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,class (:inherit dimy-darkokai-red-directory
                        :weight normal))
      (,terminal-class (:inherit dimy-darkokai-red-directory
                                 :weight normal))))

   `(sr-symlink-directory-face
     ((,class (:inherit dimy-darkokai-red-directory
                        :slant italic
                        :weight normal))
      (,terminal-class (:inherit dimy-darkokai-red-directory
                                 :slant italic
                                 :weight normal))))

   `(sr-symlink-face
     ((,class (:inherit dimy-darkokai-red-symlink
                        :slant italic
                        :weight normal))
      (,terminal-class (:inherit dimy-darkokai-red-symlink
                                 :slant italic
                                 :weight normal))))

   `(sr-broken-link-face
     ((,class (:inherit dimy-darkokai-red-warning
                        :slant italic
                        :weight normal))
      (,terminal-class (:inherit dimy-darkokai-red-warning
                                 :slant italic
                                 :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(sr-encrypted-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(sr-log-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(sr-packaged-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(sr-html-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(sr-xml-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,class (:background ,my-darkokai-red
                           :foreground ,my-darkokai-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-my-darkokai-red
                                    :foreground ,terminal-my-darkokai-bg
                                    :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,class (:background unspecified
                           :foreground ,my-darkokai-yellow))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-yellow))))

   `(syslog-hour-face
     ((,class (:background unspecified
                           :foreground ,my-darkokai-green))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-green))))

   `(syslog-error-face
     ((,class (:background unspecified
                           :foreground ,my-darkokai-red
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-red
                                    :weight bold))))

   `(syslog-warn-face
     ((,class (:background unspecified
                           :foreground ,my-darkokai-orange
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-orange
                                    :weight bold))))

   `(syslog-info-face
     ((,class (:background unspecified
                           :foreground ,my-darkokai-blue
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-blue
                                    :weight bold))))

   `(syslog-debug-face
     ((,class (:background unspecified
                           :foreground ,my-darkokai-cyan
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-cyan
                                    :weight bold))))

   `(syslog-su-face
     ((,class (:background unspecified
                           :foreground ,my-darkokai-magenta))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-magenta))))

   ;; table
   `(table-cell
     ((,class (:foreground ,my-darkokai-fg
                           :background ,my-darkokai-highlight-line))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :background ,terminal-my-darkokai-highlight-line))))

   ;; term
   `(term-color-black
     ((,class (:foreground ,my-darkokai-bg
                           :background ,my-darkokai-highlight-line))
      (,terminal-class (:foreground ,terminal-my-darkokai-bg
                                    :background ,terminal-my-darkokai-highlight-line))))

   `(term-color-red
     ((,class (:foreground ,my-darkokai-red
                           :background ,my-darkokai-red-d))
      (,terminal-class (:foreground ,terminal-my-darkokai-red
                                    :background ,terminal-my-darkokai-red-d))))

   `(term-color-green
     ((,class (:foreground ,my-darkokai-green
                           :background ,my-darkokai-green-d))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :background ,terminal-my-darkokai-green-d))))

   `(term-color-yellow
     ((,class (:foreground ,my-darkokai-yellow
                           :background ,my-darkokai-yellow-d))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :background ,terminal-my-darkokai-yellow-d))))

   `(term-color-blue
     ((,class (:foreground ,my-darkokai-blue
                           :background ,my-darkokai-blue-d))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :background ,terminal-my-darkokai-blue-d))))

   `(term-color-magenta
     ((,class (:foreground ,my-darkokai-magenta
                           :background ,my-darkokai-magenta-d))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta
                                    :background ,terminal-my-darkokai-magenta-d))))

   `(term-color-cyan
     ((,class (:foreground ,my-darkokai-cyan
                           :background ,my-darkokai-cyan-d))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :background ,terminal-my-darkokai-cyan-d))))

   `(term-color-white
     ((,class (:foreground ,my-darkokai-emph
                           :background ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :background ,terminal-my-darkokai-fg))))

   `(term-default-fg-color
     ((,class (:inherit term-color-white))
      (,terminal-class (:inherit term-color-white))))

   `(term-default-bg-color
     ((,class (:inherit term-color-black))
      (,terminal-class (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,class (:background ,my-darkokai-bg
                           :foreground ,my-darkokai-yellow
                           :inherit ,s-variable-pitch))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,class (:foreground ,my-darkokai-magenta
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta
                                    :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,class (:foreground ,my-darkokai-blue
                           :background ,my-darkokai-highlight-line
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :background ,terminal-my-darkokai-highlight-line
                                    :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,class (:foreground ,my-darkokai-emph))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph))))

   `(tuareg-font-lock-error-face
     ((,class (:foreground ,my-darkokai-yellow
                           :background ,my-darkokai-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :background ,terminal-my-darkokai-red
                                    :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,class (:foreground ,my-darkokai-comments
                           :background ,my-darkokai-bg))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :background ,terminal-my-darkokai-bg))))

   `(undo-tree-visualizer-unmodified-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(undo-tree-visualizer-current-face
     ((,class (:foreground ,my-darkokai-blue
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,class (:foreground ,my-darkokai-emph
                           :background ,my-darkokai-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :background ,terminal-my-darkokai-bg
                                    :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   ;; volatile highlights
   `(vhl/default-face
     ((,class (:background ,my-darkokai-green-lc
                           :foreground ,my-darkokai-green-hc))
      (,terminal-class (:background ,terminal-my-darkokai-green-lc
                                    :foreground ,terminal-my-darkokai-green-hc))))

   ;; w3m
   `(w3m-anchor
     ((,class (:inherit link))
      (,terminal-class (:inherit link))))

   `(w3m-arrived-anchor
     ((,class (:inherit link-visited))
      (,terminal-class (:inherit link-visited))))

   `(w3m-form
     ((,class (:background ,my-darkokai-bg
                           :foreground ,my-darkokai-fg))
      (,terminal-class (:background ,terminal-my-darkokai-bg
                                    :foreground ,terminal-my-darkokai-fg))))

   `(w3m-header-line-location-title
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-yellow))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-yellow))))

   `(w3m-header-line-location-content

     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-fg))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-fg))))

   `(w3m-bold
     ((,class (:foreground ,my-darkokai-emph
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :weight bold))))

   `(w3m-image-anchor
     ((,class (:background ,my-darkokai-bg
                           :foreground ,my-darkokai-cyan
                           :inherit link))
      (,terminal-class (:background ,terminal-my-darkokai-bg
                                    :foreground ,terminal-my-darkokai-cyan
                                    :inherit link))))

   `(w3m-image
     ((,class (:background ,my-darkokai-bg
                           :foreground ,my-darkokai-cyan))
      (,terminal-class (:background ,terminal-my-darkokai-bg
                                    :foreground ,terminal-my-darkokai-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,class (:foreground ,my-darkokai-emph))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph))))

   `(w3m-lnum-match
     ((,class (:background ,my-darkokai-highlight-line))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line))))

   `(w3m-lnum
     ((,class (:underline nil
                          :bold nil
                          :foreground ,my-darkokai-red))
      (,terminal-class (:underline nil
                                   :bold nil
                                   :foreground ,terminal-my-darkokai-red))))

   `(w3m-session-select
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(w3m-session-selected
     ((,class (:foreground ,my-darkokai-emph
                           :bold t
                           :underline t))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :bold t
                                    :underline t))))

   `(w3m-tab-background
     ((,class (:background ,my-darkokai-bg
                           :foreground ,my-darkokai-fg))
      (,terminal-class (:background ,terminal-my-darkokai-bg
                                    :foreground ,terminal-my-darkokai-fg))))

   `(w3m-tab-selected-background
     ((,class (:background ,my-darkokai-bg
                           :foreground ,my-darkokai-fg))
      (,terminal-class (:background ,terminal-my-darkokai-bg
                                    :foreground ,terminal-my-darkokai-fg))))

   `(w3m-tab-mouse
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-yellow))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-yellow))))

   `(w3m-tab-selected
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-emph
                           :bold t))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-emph
                                    :bold t))))

   `(w3m-tab-unselected
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-fg))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-fg))))

   `(w3m-tab-selected-retrieving
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-red))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-red))))

   `(w3m-tab-unselected-retrieving
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-orange))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-orange))))

   `(w3m-tab-unselected-unseen
     ((,class (:background ,my-darkokai-highlight-line
                           :foreground ,my-darkokai-violet))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :foreground ,terminal-my-darkokai-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(web-mode-comment-face
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   `(web-mode-constant-face
     ((,class (:foreground ,my-darkokai-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :weight bold))))

   `(web-mode-current-element-highlight-face
     ((,class (:underline unspecified
                          :weight unspecified
                          :background ,my-darkokai-highlight-line))
      (,terminal-class (:underline unspecified
                                   :weight unspecified
                                   :background ,terminal-my-darkokai-highlight-line))))

   `(web-mode-css-at-rule-face
     ((,class (:foreground ,my-darkokai-violet
                           :slant italic))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet
                                    :slant italic))))

   `(web-mode-css-pseudo-class-face
     ((,class (:foreground ,my-darkokai-green
                           :slant italic))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :slant italic))))

   `(web-mode-doctype-face
     ((,class (:foreground ,my-darkokai-comments
                           :slant italic
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :slant italic
                                    :weight bold))))

   `(web-mode-folded-face
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(web-mode-function-name-face
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(web-mode-html-attr-name-face
     ((,class (:foreground ,my-darkokai-blue
                           :slant normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue
                                    :slant normal))))

   `(web-mode-html-attr-value-face
     ((,class (:foreground ,my-darkokai-cyan
                           :slant italic))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan
                                    :slant italic))))

   `(web-mode-html-tag-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(web-mode-keyword-face
     ((,class (:foreground ,my-darkokai-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :weight normal))))

   `(web-mode-preprocessor-face
     ((,class (:foreground ,my-darkokai-yellow
                           :slant normal
                           :weight unspecified))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow
                                    :slant normal
                                    :weight unspecified))))

   `(web-mode-string-face
     ((,class (:foreground ,my-darkokai-cyan))
      (,terminal-class (:foreground ,terminal-my-darkokai-cyan))))

   `(web-mode-type-face
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(web-mode-variable-name-face
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(web-mode-warning-face
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(web-mode-block-attr-name-face
     ((,class (:inherit web-mode-html-attr-name-face))
      (,terminal-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-block-attr-value-face
     ((,class (:inherit web-mode-html-attr-value-face))
      (,terminal-class (:inherit web-mode-html-attr-value-face))))

   `(web-mode-block-comment-face
     ((,class (:inherit web-mode-comment-face))
      (,terminal-class (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,class (:inherit font-lock-preprocessor-face))
      (,terminal-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-face
     ((,class (:background unspecified))
      (,terminal-class (:background unspecified))))

   `(web-mode-block-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,class (:box 1
                    :weight bold))
      (,terminal-class (:box 1
                             :weight bold))))

   `(web-mode-css-color-face
     ((,class (:inherit font-lock-builtin-face))
      (,terminal-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-face
     ((,class (:inherit font-lock-builtin-face))
      (,terminal-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-priority-face
     ((,class (:inherit font-lock-builtin-face))
      (,terminal-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,class (:inherit font-lock-variable-name-face))
      (,terminal-class (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,class (:inherit font-lock-keyword-face))
      (,terminal-class (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-json-context-face
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))

   `(web-mode-json-key-face
     ((,class (:foreground ,my-darkokai-violet))
      (,terminal-class (:foreground ,terminal-my-darkokai-violet))))

   `(web-mode-json-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(web-mode-part-comment-face
     ((,class (:inherit web-mode-comment-face))
      (,terminal-class (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,class (:inherit web-mode-block-face))
      (,terminal-class (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(web-mode-whitespace-face
     ((,class (:background ,my-darkokai-red))
      (,terminal-class (:background ,terminal-my-darkokai-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,class (:background unspecified
                           :foreground ,my-darkokai-comments
                           :inverse-video unspecified
                           :slant italic))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-comments
                                    :inverse-video unspecified
                                    :slant italic))))

   `(whitespace-hspace
     ((,class (:background unspecified
                           :foreground ,my-darkokai-emph
                           :inverse-video unspecified))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-emph
                                    :inverse-video unspecified))))

   `(whitespace-tab
     ((,class (:background unspecified
                           :foreground ,my-darkokai-red
                           :inverse-video unspecified
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-red
                                    :inverse-video unspecified
                                    :weight bold))))

   `(whitespace-newline
     ((,class(:background unspecified
                          :foreground ,my-darkokai-comments
                          :inverse-video unspecified))
      (,terminal-class(:background unspecified
                                   :foreground ,terminal-my-darkokai-comments
                                   :inverse-video unspecified))))

   `(whitespace-trailing
     ((,class (:background unspecified
                           :foreground ,my-darkokai-orange-lc
                           :inverse-video t))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-orange-lc
                                    :inverse-video t))))

   `(whitespace-line
     ((,class (:background unspecified
                           :foreground ,my-darkokai-magenta
                           :inverse-video unspecified))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-magenta
                                    :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,class (:background ,my-darkokai-red-lc
                           :foreground unspecified
                           :inverse-video unspecified))
      (,terminal-class (:background ,terminal-my-darkokai-red-lc
                                    :foreground unspecified
                                    :inverse-video unspecified))))

   `(whitespace-indentation
     ((,class (:background unspecified
                           :foreground ,my-darkokai-yellow
                           :inverse-video unspecified
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-yellow
                                    :inverse-video unspecified
                                    :weight bold))))

   `(whitespace-empty
     ((,class (:background unspecified
                           :foreground ,my-darkokai-red-lc
                           :inverse-video t))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-red-lc
                                    :inverse-video t))))

   `(whitespace-space-after-tab
     ((,class (:background unspecified
                           :foreground ,my-darkokai-orange
                           :inverse-video t
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-my-darkokai-orange
                                    :inverse-video t
                                    :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(wl-highlight-folder-many-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(wl-highlight-folder-path-face
     ((,class (:foreground ,my-darkokai-orange))
      (,terminal-class (:foreground ,terminal-my-darkokai-orange))))

   `(wl-highlight-folder-unread-face
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(wl-highlight-folder-zero-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(wl-highlight-folder-unknown-face
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(wl-highlight-message-citation-header
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(wl-highlight-message-cited-text-1
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(wl-highlight-message-cited-text-2
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(wl-highlight-message-cited-text-3
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(wl-highlight-message-cited-text-4
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(wl-highlight-message-header-contents-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(wl-highlight-message-headers-face
     ((,class (:foreground ,my-darkokai-red))
      (,terminal-class (:foreground ,terminal-my-darkokai-red))))

   `(wl-highlight-message-important-header-contents
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(wl-highlight-message-header-contents
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(wl-highlight-message-important-header-contents2
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(wl-highlight-message-signature
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(wl-highlight-summary-answemy-darkokai-red-face
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(wl-highlight-summary-disposed-face
     ((,class (:foreground ,my-darkokai-fg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg
                                    :slant italic))))

   `(wl-highlight-summary-new-face
     ((,class (:foreground ,my-darkokai-blue))
      (,terminal-class (:foreground ,terminal-my-darkokai-blue))))

   `(wl-highlight-summary-normal-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(wl-highlight-summary-thread-top-face
     ((,class (:foreground ,my-darkokai-yellow))
      (,terminal-class (:foreground ,terminal-my-darkokai-yellow))))

   `(wl-highlight-thread-indent-face
     ((,class (:foreground ,my-darkokai-magenta))
      (,terminal-class (:foreground ,terminal-my-darkokai-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,class (:foreground ,my-darkokai-fg))
      (,terminal-class (:foreground ,terminal-my-darkokai-fg))))

   `(wl-highlight-summary-displaying-face
     ((,class (:underline t
                          :weight bold))
      (,terminal-class (:underline t
                                   :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,class (:inherit error))
      (,terminal-class (:inherit error))))

   `(weechat-highlight-face
     ((,class (:foreground ,my-darkokai-emph
                           :weight bold))
      (,terminal-class (:foreground ,terminal-my-darkokai-emph
                                    :weight bold))))

   `(weechat-nick-self-face
     ((,class (:foreground ,my-darkokai-green
                           :weight unspecified
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-my-darkokai-green
                                    :weight unspecified
                                    :inverse-video t))))

   `(weechat-prompt-face
     ((,class (:inherit minibuffer-prompt))
      (,terminal-class (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,class (:foreground ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments))))

   ;; which-func-mode
   `(which-func
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   ;; window-number-mode
   `(window-number-face
     ((,class (:foreground ,my-darkokai-green))
      (,terminal-class (:foreground ,terminal-my-darkokai-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,class (:foreground ,my-darkokai-comments
                           :background ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :background ,terminal-my-darkokai-comments))))

   `(yascroll:thumb-fringe
     ((,class (:foreground ,my-darkokai-comments
                           :background ,my-darkokai-comments))
      (,terminal-class (:foreground ,terminal-my-darkokai-comments
                                    :background ,terminal-my-darkokai-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,class (:background ,my-darkokai-highlight-line
                           :box ,my-darkokai-emph))
      (,terminal-class (:background ,terminal-my-darkokai-highlight-line
                                    :box ,terminal-my-darkokai-emph)))))

  (custom-theme-set-variables
   'my-darkokai
   `(ansi-color-names-vector [,my-darkokai-bg ,my-darkokai-red ,my-darkokai-green ,my-darkokai-yellow
                                           ,my-darkokai-blue ,my-darkokai-magenta ,my-darkokai-cyan ,my-darkokai-fg])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,my-darkokai-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,my-darkokai-magenta ,my-darkokai-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,my-darkokai-highlight-line . 0)
       (,my-darkokai-green-lc . 20)
       (,my-darkokai-cyan-lc . 30)
       (,my-darkokai-blue-lc . 50)
       (,my-darkokai-yellow-lc . 60)
       (,my-darkokai-orange-lc . 70)
       (,my-darkokai-magenta-lc . 85)
       (,my-darkokai-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,my-darkokai-bg)
   `(pos-tip-background-color ,my-darkokai-yellow)

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,my-darkokai-red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,my-darkokai-yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,my-darkokai-green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,my-darkokai-cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,my-darkokai-blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     (unspecified ,my-darkokai-bg ,my-darkokai-highlight-line
                  ,my-darkokai-red-d ,my-darkokai-red
                  ,my-darkokai-green-d ,my-darkokai-green
                  ,my-darkokai-yellow-d ,my-darkokai-yellow
                  ,my-darkokai-blue-d ,my-darkokai-blue
                  ,my-darkokai-magenta-d ,my-darkokai-magenta
                  ,my-darkokai-cyan-d ,my-darkokai-cyan
                  ,my-darkokai-fg ,my-darkokai-emph))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'my-darkokai)

;; Local Variables:
;; no-byte-compile: t
;; fill-column: 95
;; End:

;;; my-darkokai-theme.el ends here
