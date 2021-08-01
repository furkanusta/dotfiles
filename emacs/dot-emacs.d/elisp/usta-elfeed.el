
(use-package elfeed
  :preface
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
     ("https://what-if.xkcd.com/feed.atom" xkcd)
     ("http://xkcd.com/rss.xml" xkcd)
     ("https://esoteric.codes/rss" other)
     ("http://irreal.org/blog/?feed=rss2" emacs)
     ("https://drewdevault.com/feed.xml" other)
     ("https://jacobian.org/index.xml" other)
     ("https://old.reddit.com/r/cpp/top.rss?t=month" cpp)
     ("https://old.reddit.com/r/emacs/top.rss?t=week" emacs)
     ("https://old.reddit.com/r/orgmode/top.rss?t=month" emacs)
     ("https://old.reddit.com/r/python/top.rss?t=month" python)
     ("https://old.reddit.com/r/fpga/top.rss?t=month" fpga)
     ("https://old.reddit.com/r/ruby/top.rss?t=month" ruby)
     ("https://old.reddit.com/r/java/top.rss?t=month" java)
     ("https://old.reddit.com/r/linux/top.rss?t=month" linux)
     ("https://old.reddit.com/r/programming/top.rss?t=week" prog)
     ("https://old.reddit.com/r/selfhosted/top.rss?t=month" prog)
     ("https://old.reddit.com/r/commandline/top.rss?t=month" prog)
     ("https://old.reddit.com/r/askhistorians/top.rss?t=month" hist)
     ("https://old.reddit.com/r/badhistory/top.rss?t=month" hist)
     ("https://dave.cheney.net/feed/atom" go)
     ("https://blog.theincredibleholk.org/atom.xml" prog)
     ("https://ferd.ca/feed.rss" prog)
     ("https://benjamincongdon.me/blog/feed.xml" blog)
     ("https://erikbern.com/index.xml" blog)
     ("https://www.benkuhn.net/index.xml" blog)
     ("https://rjlipton.wpcomstaging.com/feed/" blog)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-xTvXTm-lrLWYk308-Km3A" youtube)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsvn_Po0SmunchJYOWpOxMg" youtube)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCCpTaib_e5C6Q95qwazq8OA" youtube)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCO-_F5ZEUhy0oKrSa69DLMw" youtube)
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
                                  (lambda () (with-current-buffer (get-buffer "*reddigg-comments*")
                                               (local-set-key "q" #'+rss/delete-pane)
                                               (read-only-mode +1)))))))
  :bind (:map elfeed-search-mode-map ("R" . elfeed-open-reddit)))

(use-package feed-discovery)

(use-package pocket-reader)

(provide 'usta-elfeed)