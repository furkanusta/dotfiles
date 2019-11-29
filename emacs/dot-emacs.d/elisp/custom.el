(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242728" "#ff0066" "#63de5d" "#E6DB74" "#06d8ff" "#ff8eff" "#53f2dc" "#f8fbfc"])
 '(beacon-color "#dc322f")
 '(beacon-mode t)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(c-noise-macro-names '("constexpr"))
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes '(my-darkokai))
 '(custom-safe-themes
   '("3869bfbc4eaa519489b7adf3e04f9b9436aee18838ca5de1fab704106909ef2c" "a154a59e6fab592a5493e0439e649c96fab1cbb2197f4165b91fd269166e5030" "53670bf6c50c5a0351cfcf8e5d923b0d094da99023ad8654323672f16f6ac8dc" "846c015a794cf5859059518539f847f5a82529f3f9c1647d49689a6f3d36b87b" "f1582e4d10daedf7b6b02e63de03be3561daf4beed2d02bbb100d1d0141fbea2" "83f3aa2d9303c7ac4d86b5d5951c7905bc8323d898461cb7eeee9282e0c6770d" "239e798add00c4a41459a25299514cd2608814eac60536b27ebe18e1765a1039" "1f70bd77ca2b8a8a7ba101d7ec0de17524456db6aa9beef76bb8442f45b08517" "937c6bec49c213bed836e47ab6473745354935b7e3eb127365e379d442bf8657" "423435c7b0e6c0942f16519fa9e17793da940184a50201a4d932eafe4c94c92d" "0fe9f7a04e7a00ad99ecacc875c8ccb4153204e29d3e57e9669691e6ed8340ce" "fe76f3d5094967034192f6a505085db8db6deb0e135749d9a54dc488d6d3ee2f" "4a9f595fbffd36fe51d5dd3475860ae8c17447272cf35eb31a00f9595c706050" "5c9a906b076fe3e829d030a404066d7949e2c6c89fc4a9b7f48c054333519ee7" "e7666261f46e2f4f42fd1f9aa1875bdb81d17cc7a121533cad3e0d724f12faf2" "e47c0abe03e0484ddadf2ae57d32b0f29f0b2ddfe7ec810bd6d558765d9a6a6c" "32fd809c28baa5813b6ca639e736946579159098d7768af6c68d78ffa32063f4" "dc677c8ebead5c0d6a7ac8a5b109ad57f42e0fe406e4626510e638d36bcc42df" "eb94e44599a45c495ad472ad551a40b87cbc4bae9631211e7a78d72b102c61b1" "947190b4f17f78c39b0ab1ea95b1e6097cc9202d55c73a702395fc817f899393" "41c8c11f649ba2832347fe16fe85cf66dafe5213ff4d659182e25378f9cfc183" "1897b97f63e91a792e8540c06402f29d5edcbfb0aafd64b1b14270663d6868ee" "4b0b568d63b1c6f6dddb080b476cfba43a8bbc34187c3583165e8fb5bbfde3dc" "a4fa3280ffa1f2083c5d4dab44a7207f3f7bcb76e720d304bd3bd640f37b4bef" "c6b93ff250f8546c7ad0838534d46e616a374d5cb86663a9ad0807fd0aeb1d16" "92d8a13d08e16c4d2c027990f4d69f0ce0833c844dcaad3c8226ae278181d5f3" "11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "37ba833442e0c5155a46df21446cadbe623440ccb6bbd61382eb869a2b9e9bf9" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "a8c210aa94c4eae642a34aaf1c5c0552855dfca2153fa6dd23f3031ce19453d4" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "9e31aff9afe3c20a33dd966b4c54c6a5151f07659362e4b06bde38ded5370dae" "392395ee6e6844aec5a76ca4f5c820b97119ddc5290f4e0f58b38c9748181e8d" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "9954ed41d89d2dcf601c8e7499b6bb2778180bfcaeb7cdfc648078b8e05348c6" "fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "ab564a7ce7f2b2ad9e2cfe9fe1019b5481809dd7a0e36240c9139e230cc2bc32" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "dd43ce1171324a8e47f9e4ca9f49773c4b4960706171ab951130c668adc59f53" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "5f4e4c9f5de8156f964fdf8a1b8f8f659efbfeff88b38f49ce13953a84272b77" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" "6ee6f99dc6219b65f67e04149c79ea316ca4bcd769a9e904030d38908fd7ccf9" "4e21fb654406f11ab2a628c47c1cbe53bab645d32f2c807ee2295436f09103c6" "a866134130e4393c0cad0b4f1a5b0dd580584d9cf921617eee3fd54b6f09ac37" "0598de4cc260b7201120b02d580b8e03bd46e5d5350ed4523b297596a25f7403" "463a24ebf2922855c36637f46c53903a4737cf57038e7a9fba700456e3bd27f2" "53d1bb57dadafbdebb5fbd1a57c2d53d2b4db617f3e0e05849e78a4f78df3a1b" "90bd0eb20a1cb155b5a076f698b3c72cfe775aa7ea93b7bfbc171eb250db5e20" "77c3f5f5acaa5a276ca709ff82cce9b303f49d383415f740ba8bcc76570718b9" "e4a6fc5d9f4bc63b6ce9743396b68098ae7011d29e9876082ef3969c18b0ea93" "18eea36d8ecd6e236d25c4cc22d1a772cd34b32d83356a86d3eaf0865788c426" "bbb4a4d39ed6551f887b7a3b4b84d41a3377535ccccf901a3c08c7317fad7008" "a8c927cf1acf19ae27bd971894fa94c8114c31496c3d9cdfcc4d373dab34e4ef" "3d20cf0dbc6465a02c468abf2d9b8c17e88b20fbc05a04205a829285da28799d" "a9d2ed6e4266ea7f8c1f4a0d1af34a6282ad6ff91754bee5ec7c3b260ec721f4" "7f6796a9b925f727bbe1781dc65f7f23c0aa4d4dc19613aa3cf96e41a96651e4" "65d4e1535e8fa5d40b9a95d30ed0e95b3bac2b905e905f4397e0425a51addc55" "fec6c786b1d3088091715772839ac6051ed972b17991af04b50e9285a98c7463" "8ad35d6c2b35eacc328b732f0a4fe263abd96443a5075aa53b8535a9e8cb7eaf" "293b55c588c56fe062afe4b7a3a4b023712a26d26dc69ee89c347b30283a72eb" "9a58c408a001318ce9b4eab64c620c8e8ebd55d4c52327e354f24d298fb6978f" "12b204c8fcce23885ce58e1031a137c5a14461c6c7e1db81998222f8908006af" "d3a406c5905923546d8a3ad0164a266deaf451856eca5f21b36594ffcb08413a" "d5cdb20cc31dfd701ee4ac5fed09d0e1898ee828c6036c4ee00fdc1e50eb7558" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" "ecb9fe1d5b165a35499191a909b2b5710a52935614058b327a39bfbbb07c7dc8" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "fad38808e844f1423c68a1888db75adf6586390f5295a03823fa1f4959046f81" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "7557aa0d3854c7e910121ba2ef94f4c4e70de7d32ddebb609719f545f7f7be0d" "db210d68e7231e5f2361cd8f6e6bc261f4ea02a72e695a5166eee940e7530a76" "357d5abe6f693f2875bb3113f5c031b7031f21717e8078f90d9d9bc3a14bcbd8" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" "880f541eabc8c272d88e6a1d8917fe743552f17cedd8f138fe85987ee036ad08" "cb39485fd94dabefc5f2b729b963cbd0bac9461000c57eae454131ed4954a8ac" "0ca71d3462db28ebdef0529995c2d0fdb90650c8e31631e92b9f02bd1bfc5f36" "dc758223066a28f3c6ef6c42c9136bf4c913ec6d3b710794252dc072a3b92b14" "5009505c383a33f82cd92f6bd322200083675ffcca900120efd467973378166d" "b2db1708af2a7d50cac271be91908fffeddb04c66cb1a853fff749c7ad6926ae" "a09d85c7c69df280084db979e97bf3520fc6b1cf152aff579f7c166c78efeb58" "46dc033b0a5ac1d0ecc68269032774265f249115df8b49fce4b9cf66db66bc90" "70403e220d6d7100bae7775b3334eddeb340ba9c37f4b39c189c2c29d458543b" "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "e8825f26af32403c5ad8bc983f8610a4a4786eb55e3a363fa9acb48e0677fe7e" "cdd26fa6a8c6706c9009db659d2dffd7f4b0350f9cc94e5df657fa295fffec71" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "c567c85efdb584afa78a1e45a6ca475f5b55f642dfcd6277050043a568d1ac6f" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" default))
 '(delete-selection-mode t)
 '(dired-recursive-deletes 'always)
 '(doom-modeline-mode t)
 '(elfeed-feeds
   '("http://research.swtch.com/feeds/posts/default" "http://bitbashing.io/feed.xml" "http://planet.emacsen.org/atom.xml" "http://lemire.me/blog/feed/" "http://preshing.com/feed" "http://danluu.com/atom.xml" "http://tenderlovemaking.com/atom.xml" "http://feeds.feedburner.com/codinghorror/" "https://www.discoverdev.io/rss.xml" "http://planet.gnome.org/rss20.xml" "http://www.snarky.ca/feed" "http://blog.regehr.org/feed" "http://arne-mertz.de/feed/" "http://xkcd.com/rss.xml"))
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django))
 '(ensime-sem-high-faces
   '((var :foreground "#9876aa" :underline
          (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
                         (:color "#808080"))
     (implicitParams :underline
                     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6")))
 '(fci-rule-color "#323342")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flycheck-flake8-maximum-line-length 100)
 '(flycheck-python-flake8-executable "/home/eksi/.local/bin/flake8")
 '(flycheck-python-pycompile-executable "python3")
 '(frame-background-mode 'dark)
 '(helm-completion-style 'emacs)
 '(helm-ff-lynx-style-map t)
 '(helm-follow-mode-persistent t)
 '(highlight-changes-colors '("#ff8eff" "#ab7eff"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#323342" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#323342" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-paren-background-colors '("#2492db" "#95a5a6" nil))
 '(hl-paren-colors '("#ecf0f1" "#ecf0f1" "#c0392b"))
 '(hl-sexp-background-color "#efebe9")
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2aa198")
     ("PROG" . "#268bd2")
     ("OKAY" . "#268bd2")
     ("DONT" . "#d70008")
     ("FAIL" . "#d70008")
     ("DONE" . "#00af00")
     ("NOTE" . "#875f00")
     ("KLUDGE" . "#875f00")
     ("HACK" . "#875f00")
     ("TEMP" . "#875f00")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(jdee-db-active-breakpoint-face-colors (cons "#000000" "#fd971f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#000000" "#b6e63e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#000000" "#525254"))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(objed-cursor-color "#dc322f")
 '(org-agenda-files nil)
 '(org-babel-load-languages '((emacs-lisp . t) (C . t)))
 '(package-selected-packages
   '(darkokai-theme mutt-mode notmuch evil-tutor evil wanderlust doom-modeline pocket-reader cmake-mode helm helm-bm image+ helm-bibtex ox org-agenda ox-hugo helm-lsp ibuffer-projectile wgrep-helm auctex lsp-origami buffer-manage comment-or-uncomment-sexp deadgrep dired-single goto-chg pdf-view-restore rmsbolt smart-hungry-delete origami forge meson-mode exwm vala-mode elfeed helm-xref ccls elpy company-jedi treemacs company-web company-lsp lsp-ui magit-todos company-quickhelp ace-jump-buffer eyebrowse bm fzf interleave benchmark-init org-noter orgit org-ref graphviz-dot-mode srefactor company-anaconda auto-virtualenvwrapper virtualenvwrapper scala-mode helm-flycheck helm-ag helm-tramp helm-projectile helm-swoop spaceline-all-the-icons all-the-icons init-programming-languages init-others init-helm uniquify init-defaults init-bindings c++-mode shell-mode fci persp hs-minor-mode helm-semantic diff-hl-dired-mode diff-hl-mode writegood dired fill-column-indicator restart-emacs writegood-mode projectile ibuffer-vc company-c-headers use-package focus cmake-font-lock ace-window buffer-move diff-hl company-irony-c-headers flycheck-irony pydoc-info terminal-here pdf-tools color-theme-buffer-local magit markdown-mode+ web-mode gist helm-gitignore gitignore-mode tuareg dylan-mode fsharp-mode guile-scheme helm-flyspell vlf modern-cpp-font-lock anaconda-mode peep-dired diminish org-journal company-irony company irony yaml-mode sr-speedbar org-cliplink org-download visual-fill-column writeroom-mode org-bullets immortal-scratch f3 powerline swiper swiper-helm highlight-symbol visual-regexp-steroids ag drag-stuff hideshow-org indent-guide evil-nerd-commenter undo-tree helm-c-yasnippet flycheck yasnippet))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(python-shell-exec-path nil)
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   '((cmake-ide-build-dir expand-file-name "./build")
     (cmake-ide-build-dir . build)))
 '(send-mail-function 'mailclient-send-it)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#ff0066")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#63de5d")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#53f2dc")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#06d8ff")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#242728" "#323342" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(zoom-size '(0.618 . 0.618)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ensime-implicit-highlight ((t nil))))
