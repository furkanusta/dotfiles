;; -*- lexical-binding: t; -*-
(use-package elfeed
  :hook ((elfeed-new-entry . ime-elfeed-podcast-tagger))
  :preface
  (defun ime-elfeed-podcast-tagger (entry)
    (when (elfeed-entry-enclosures entry)
      (elfeed-tag entry 'podcast)))
  (defun +rss/delete-pane ()
    "Delete the *elfeed-entry* split pane."
    (interactive)
    (let* ((buf (get-buffer "*elfeed-entry*"))
           (window (get-buffer-window buf)))
      (delete-window window)
      (when (buffer-live-p buf) (kill-buffer buf))))
  :custom
  (elfeed-feeds
   '(("http://research.swtch.com/feeds/posts/default" other)
     ("http://bitbashing.io/feed.xml" other)
     ("http://preshing.com/feed" other)
     ("http://danluu.com/atom.xml" other)
     ("http://tenderlovemaking.com/atom.xml" other)
     ("http://feeds.feedburner.com/codinghorror/" other)
     ("http://www.snarky.ca/feed" other)
     ("http://blog.regehr.org/feed" cpp)
     ("https://www.joelonsoftware.com/feed/" prog)
     ("https://blog.paranoidcoding.com/atom.xml" prog)
     ("http://matt.might.net/articles/feed.rss" prog)
     ("https://blog.stephenmarz.com/feed/" prog)
     ("https://web.eecs.utk.edu/~azh/blog/feed.rss" prog)
     ("https://thume.ca/atom.xml" prog)
     ("https://blog.acolyer.org/feed/" prog)
     ;; ("https://www.johndcook.com/blog/feed/" math)
     ("https://benhoyt.com/writings/rss.xml" prog)
     ("https://lemire.me/blog/feed/" prog)
     ("https://notes.eatonphil.com/rss.xml" prog)
     ("https://www.hillelwayne.com/index.xml" prog)
     ("https://blog.acolyer.org/feed/" other)
     ("https://randomascii.wordpress.com/feed/" other)
     ("http://planet.gnome.org/rss20.xml" gnome)
     ("http://arne-mertz.de/feed/" cpp)
     ("http://zipcpu.com/" fpga)
     ("https://code-cartoons.com/feed" other)
     ("https://eli.thegreenplace.net/feeds/all.atom.xml" cpp)
     ("https://www.evanjones.ca/index.rss" other)
     ("https://jvns.ca/atom.xml" other)
     ("https://aphyr.com/posts.atom" other)
     ("https://brooker.co.za/blog/rss.xml" other)
     ("https://rachelbythebay.com/w/atom.xml" other)
     ("https://mrale.ph/feed.xml" other)
     ("https://research.swtch.com/" other)
     ("http://aras-p.info/atom.xml" other)
     ("https://www.adriancourreges.com/atom.xml" prog)
     ("https://what-if.xkcd.com/feed.atom" xkcd)
     ("http://xkcd.com/rss.xml" xkcd)
     ("https://esoteric.codes/rss" other)
     ("http://irreal.org/blog/?feed=rss2" emacs)
     ("https://drewdevault.com/feed.xml" other)
     ("https://jacobian.org/index.xml" other)
     ("https://old.reddit.com/r/cpp/top.rss?t=week" cpp)
     ("https://old.reddit.com/r/emacs/top.rss?t=week" emacs)
     ("https://old.reddit.com/r/orgmode/top.rss?t=week" emacs)
     ("https://old.reddit.com/r/python/top.rss?t=month" python)
     ("https://old.reddit.com/r/fpga/top.rss?t=month" fpga)
     ("https://old.reddit.com/r/ruby/top.rss?t=month" ruby)
     ("https://old.reddit.com/r/java/top.rss?t=month" java)
     ("https://old.reddit.com/r/linux/top.rss?t=month" linux)
     ("https://old.reddit.com/r/programming/top.rss?t=week" prog)
     ("https://old.reddit.com/r/selfhosted/top.rss?t=month" prog)
     ("https://old.reddit.com/r/commandline/top.rss?t=month" prog)
     ;; ("https://old.reddit.com/r/simpleprompts/top.rss?t=week" writing)
     ;; ("https://old.reddit.com/r/promptoftheday/top.rss?t=week" writing)
     ;; ("https://old.reddit.com/r/askhistorians/top.rss?t=month" hist)
     ;; ("https://old.reddit.com/r/badhistory/top.rss?t=month" hist)
     ("https://dave.cheney.net/feed/atom" go)
     ("https://blog.theincredibleholk.org/atom.xml" prog)
     ("https://ferd.ca/feed.rss" prog)
     ("https://benjamincongdon.me/blog/feed.xml" blog)
     ("https://erikbern.com/index.xml" blog)
     ("https://www.benkuhn.net/index.xml" blog)
     ("https://rjlipton.wpcomstaging.com/feed/" blog)
     ("http://journal.stuffwithstuff.com/rss.xml" prog)
     ("https://thephd.dev/feed.xml" prog cpp)
     ("https://knowingless.com/feed/" blog)
     ("https://gwern.substack.com/feed/" blog)
     ("https://aella.substack.com/feed/" blog)
     ("https://www.brendangregg.com/blog/rss.xml" blog)
     ("https://blog.acolyer.org/feed/" prog)
     ("https://blog.trailofbits.com/feed/" prog)
     ("https://www.justinobeirne.com/new-items-feed?format=rss" prog)
     ("https://zserge.com/rss.xml" prog)
     ("http://blog.cleancoder.com/atom.xml" prog)
     ("https://blog.nelhage.com/atom.xml" prog)
     ("https://matklad.github.io/feed.xml" prog)
     ("http://samsaffron.com/posts.rss" prog)
     ("https://fasterthanli.me/index.xml" prog)
     ("https://dragan.rocks/feed.xml" prog)
     ("https://blog.reverberate.org/feed.xml" prog)
     ("https://guzey.substack.com/feed/" blog)
     ("https://www.darkcoding.net/feed/" prog)
     ("https://bernsteinbear.com/feed.xml" prog)
     ("https://www.adriancourreges.com/atom.xml" prog)
     ("https://simoncoenen.com/feed.xml" prog)
     ("https://zipcpu.com/feed.xml" prog fpga)
     ("https://xania.org/feed.atom" prog cpp)
     ("https://emacsredux.com/atom.xml" prog emacs)
     ("https://ciechanow.ski/atom.xml" prog other)
     ("http://journal.stuffwithstuff.com/rss.xml" prog other)
     ("https://mtlynch.io/posts/index.xml" prog other)
     ("https://blog.jessfraz.com/index.xml" prog other)
     ("http://endlessparentheses.com/atom.xml" emacs)
     ("https://dynomight.net/feed.xml" blog other)
     ;; Podcast
     ;; ("https://www.omnycontent.com/d/playlist/5af088d6-01f8-4b3d-b372-acb600f45df6/bfc2b445-eb3b-4241-b228-ad950098be04/fa5fa5d2-e117-4161-8c59-ad95009915be/podcast.rss" mesut)
     ;; ("https://karnaval.com/programlar/rabarba/rss" mesut)
     ;; ("https://anchor.fm/s/37c8ef88/podcast/rss")
     ;; ("https://www.omnycontent.com/d/playlist/5af088d6-01f8-4b3d-b372-acb600f45df6/bfc2b445-eb3b-4241-b228-ad950098be04/fa5fa5d2-e117-4161-8c59-ad95009915be/podcast.rss")
     ;; ("https://feeds.megaphone.fm/revisionisthistory")
     ;; ("https://feeds.simplecast.com/BqbsxVfO")
     ;; ("https://feeds.megaphone.fm/VMP8871377602")
     ;; ("https://feeds.simplecast.com/EZwoW5Ys")
     ;; ("https://feeds.megaphone.fm/ep-wswb")
     ;; ("https://feeds.simplecast.com/dHoohVNH")
     ;; Elfeed
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCPZUQqtVDmcjm4NY5FkzqLA" youtube)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UClJ7gpJ9MRXDnbA8N_5NSKQ" youtube)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsvn_Po0SmunchJYOWpOxMg" youtube)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCCpTaib_e5C6Q95qwazq8OA" youtube)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCO-_F5ZEUhy0oKrSa69DLMw" youtube)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCdakEeTJHMPz9MdejLKDRhg" youtube)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eEGT06FrWFU6VBnPOR9lg" youtube)))
  (elfeed-show-entry-switch #'pop-to-buffer)
  (elfeed-show-entry-delete #'+rss/delete-pane)
  (elfeed-search-title-max-width 100))

(use-package elfeed-search :ensure elfeed
  :after elfeed
  :defines elfeed-show-entry
  :commands (pocket-reader-elfeed-search-add-link elfeed-search-selected elfeed-search-untag-all-unread)
  :preface
  (defun elfeed-get-show-or-search-entry ()
    (let* ((search-entries (elfeed-search-selected))
           (search-entry (when search-entries (car search-entries)))
           (elfeed-entry (or elfeed-show-entry search-entry)))
      elfeed-entry))
  (defun elfeed-youtube-dl ()
    "Youtube-DL link"
    (interactive)
    (let* ((entry (elfeed-get-show-or-search-entry))
           (url (when entry (elfeed-entry-link entry)))
           (default-directory (expand-file-name "~/Downloads")))
      (when url (async-shell-command (format "youtube-dl %s" url)))
      (elfeed-search-untag-all-unread)))
  (defun elfeed-mpv ()
    (interactive)
    (let* ((entry (elfeed-get-show-or-search-entry))
           (url (when entry (elfeed-entry-link entry))))
      (when url (start-process "elfeed-mpv" nil "mpv" url))
      (elfeed-search-untag-all-unread)))
  (defun elfeed-open-eww ()
    (interactive)
    (let* ((entry (elfeed-get-show-or-search-entry))
           (url (elfeed-entry-link entry)))
      (eww url)
      (elfeed-search-untag-all-unread)))
  :bind (:map elfeed-search-mode-map
              ("P" . pocket-reader-elfeed-entry-add-link)
              ("d" . elfeed-youtube-dl)
              ("e" . elfeed-open-eww)
              ("m" . elfeed-mpv)))

(use-package elfeed-show :ensure elfeed
  :after elfeed
  :commands (pocket-reader-elfeed-search-add-link)
  :bind (:map elfeed-show-mode-map
              ("q" . +rss/delete-pane)
              ("P" . pocket-reader-elfeed-search-add-link)))

(use-package reddigg
  :defines elfeed-search-mode-map
  :commands promise-finally
  :after elfeed-search
  :preface (defun elfeed-open-reddit ()
             (interactive)
             (require 'promise-finally)
             (let* ((entry (elfeed-get-show-or-search-entry))
                    (url (elfeed-entry-link entry))
                    (existing-buffer (get-buffer "*reddigg-comments*")))
               (progn
                 (when existing-buffer
                   (kill-buffer existing-buffer))
                 (elfeed-search-untag-all-unread)
                 (promise-finally (reddigg-view-comments url)
                                  (lambda ()
                                    (pop-to-buffer "*reddigg-comments*")
                                    (use-local-map (copy-keymap org-mode-map))
                                    (local-set-key "q" #'+rss/delete-pane)
                                    (read-only-mode +1))))))
  :bind (:map elfeed-search-mode-map ("R" . elfeed-open-reddit)))

(use-package feed-discovery)

(use-package pocket-reader)

(use-package elfeed-tube
  :quelpa (efeed-tube :fetcher github :repo "karthink/elfeed-tube")
  :after elfeed
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ;; t is auto-save (not default)
  ;; (setq elfeed-tube-auto-fetch-p t) ;;  t is auto-fetch (default)
  (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

;; (use-package elfeed-tube-mpv
;;   :quelpa (elfeed-tube-mpv :fetcher github :repo "karthink/elfeed-tube")
;;   :bind (:map elfeed-show-mode-map
;;               ("C-c C-f" . elfeed-tube-mpv-follow-mode)
;;               ("C-c C-w" . elfeed-tube-mpv-where)))

;; (use-package elfeed-score
;;   :config
;;   (progn
;;     (elfeed-score-enable)
;;     (define-key elfeed-search-mode-map "=" elfeed-score-map)))

;; (defun concatenate-authors (authors-list)
;;   "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
;;   (mapconcat (lambda (author) (plist-get author :name)) authors-list ", "))

;; (defun my-search-print-fn (entry)
;;   "Print ENTRY to the buffer."
;;   (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
;; 	     (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
;; 	     (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
;; 	     (feed (elfeed-entry-feed entry))
;; 	     (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
;; 	     (entry-authors (concatenate-authors (elfeed-meta entry :authors)))
;; 	     (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
;; 	     (tags-str (mapconcat (lambda (s) (propertize s 'face 'elfeed-search-tag-face))e tags ","))
;; 	     (title-width (- (window-width) 10 elfeed-search-trailing-width))
;; 	     (title-column (elfeed-format-column
;; 			            title (elfeed-clamp
;; 			                   elfeed-search-title-min-width
;; 			                   title-width
;; 			                   elfeed-search-title-max-width)
;; 			            :left))
;; 	     (authors-width 135)
;; 	     (authors-column (elfeed-format-column
;; 			              entry-authors (elfeed-clamp
;; 			                             elfeed-search-title-min-width
;; 			                             authors-width
;; 			                             131)
;; 			              :left)))
;;     (insert (propertize date 'face 'elfeed-search-date-face) " ")
;;     (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
;;     (insert (propertize authors-column 'face 'elfeed-search-date-face 'kbd-help entry-authors) " ")
;;     ;; (when feed-title (insert (propertize entry-authors 'face 'elfeed-search-feed-face) " "))
;;     (when entry-authors (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
;;     (when tags (insert "(" tags-str ")"))))

;; (setq elfeed-search-print-entry-function #'my-search-print-fn)

;; ;;; Elfeed score file                                     -*- lisp -*-
;; (("title"
;;   (:text "OPEN THREAD" :value -1000 :type S)
;;   (:text "raymond c\\(hen\\)?" :value 250 :type r) :tags (t .(@dev)))
;;  ("content"
;;   (:text "type erasure" :value 500 :type s))
;;  ("title-or-content"
;;   (:text "california" 150 100 :type s)
;;   (:text "china" 150 100 :type w))
;;  ("feed"
;;   (:text "Essays in Idleness" :value 250 :type S :attr t)
;;   (:text "Irreal" :value 250 :type S :attr t)
;;   (:text "Julia Evans" :value 100 :type :type s :attr t)
;;   (:text "National Weather Service" :value 400 :type S :attr t)
;;   (:text "emacs-news – sacha chua" :value 350 :type S :attr t :comment "Essential!"))
;;  ("authors"
;;    (:text "Jim Geraghty" :value 500 :type s))
;;  ("tag"
;;   (:tags (t . reddit-question)
;;    :value 750
;;    :comment "Add 750 points to any entry with a tag of reddit-question"))
;;  (mark -2500))

(provide 'usta-elfeed)
