;;
;; Replace counsel with consult
;;
;;
;;; consult-tramp.el --- Tramp ivy interface for ssh, docker, vagrant -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019 by Masashı Mıyaura

;; Author: Masashı Mıyaura
;; URL: https://github.com/masasam/emacs-consult-tramp
;; Version: 0.7.5
;; Package-Requires: ((emacs "24.3") (consult "0.10"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; consult-tramp provides interfaces of Tramp
;; You can also use tramp with consult interface as root
;; If you use it with docker-tramp, you can also use docker with consult interface
;; If you use it with vagrant-tramp, you can also use vagrant with consult interface

;;; Code:

(require 'consult)
(require 'tramp)
(require 'cl-lib)

(defgroup consult-tramp nil
  "Tramp with ivy interface for ssh, docker, vagrant"
  :group 'consult)

(defcustom consult-tramp-default-method "ssh"
  "Default method when use tramp multi hop."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-docker-user nil
  "If you want to use login user name when `docker-tramp' used, set variable."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-localhost-directory "/"
  "Initial directory when connecting with /sudo:root@localhost:."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-control-master nil
  "If you want to put out a candidate for completion from ssh controlmaster, please set to t."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-control-master-path "~/.ssh/"
  "Path where ssh controlmaster exists."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-control-master-prefix "master-"
  "Prefix of ssh controlmaster."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-pre-command-hook nil
  "Hook run before `consult-tramp'.
The hook is called with one argument that is non-nil."
  :type 'hook)

(defcustom consult-tramp-post-command-hook nil
  "Hook run after `consult-tramp'.
The hook is called with one argument that is non-nil."
  :type 'hook)

(defcustom consult-tramp-quit-hook nil
  "Hook run when `consult-tramp-quit'.
The hook is called with one argument that is non-nil."
  :type 'hook)

(defcustom consult-tramp-custom-connections '()
  "A list to manually add extra connections.
E.g.: '(\"/ssh:domain|sudo:user@localhost:/\")."
  :type 'string)

(defun consult-tramp-quit ()
  "Quit consult-tramp.
Kill all remote buffers."
  (interactive)
  (run-hooks 'consult-tramp-quit-hook)
  (tramp-cleanup-all-buffers))

(defun consult-tramp--candidates (&optional file)
  "Collect candidates for consult-tramp from FILE."
  (let ((source (split-string
                 (with-temp-buffer
                   (insert-file-contents (or file "~/.ssh/config"))
                   (buffer-string))
                 "\n"))
        (hosts (if file '() consult-tramp-custom-connections)))
    (dolist (host source)
      (when (string-match "[H\\|h]ost +\\(.+?\\)$" host)
	(setq host (match-string 1 host))
	(if (string-match "[ \t\n\r]+\\'" host)
	    (replace-match "" t t host))
	(if (string-match "\\`[ \t\n\r]+" host)
	    (replace-match "" t t host))
        (unless (string= host "*")
	  (if (string-match "[ ]+" host)
	      (let ((result (split-string host " ")))
		(while result
		  (push
		   (concat "/" tramp-default-method ":" (car result) ":")
		   hosts)
		  (push
		   (concat "/" consult-tramp-default-method ":" (car result) "|sudo:root@" (car result) ":/")
		   hosts)
		  (pop result)))
	    (push
	     (concat "/" tramp-default-method ":" host ":")
	     hosts)
	    (push
	     (concat "/" consult-tramp-default-method ":" host "|sudo:root@" host ":/")
	     hosts))))
      (when (string-match "Include +\\(.+\\)$" host)
        (setq include-file (match-string 1 host))
        (when (not (file-name-absolute-p include-file))
          (setq include-file (concat (file-name-as-directory "~/.ssh") include-file)))
        (when (file-exists-p include-file)
          (setq hosts (append hosts (consult-tramp--candidates include-file))))))
    (when consult-tramp-control-master
      (let ((files (consult-tramp--directory-files
		    (expand-file-name
		     consult-tramp-control-master-path)
		    consult-tramp-control-master-prefix))
	    (hostuser nil)
	    (hostname nil)
	    (port nil))
	(dolist (controlmaster files)
	  (let ((file (file-name-nondirectory controlmaster)))
	    (when (string-match
		   (concat consult-tramp-control-master-prefix "\\(.+?\\)@\\(.+?\\):\\(.+?\\)$")
		   file)
	      (setq hostuser (match-string 1 file))
	      (setq hostname (match-string 2 file))
	      (setq port (match-string 3 file))
	      (push
	       (concat "/" tramp-default-method ":" hostuser "@" hostname "#" port ":")
	       hosts)
	      (push
	       (concat "/" consult-tramp-default-method ":" hostuser "@" hostname "#" port "|sudo:root@" hostname ":/")
	       hosts))))))
    (when (and (require 'docker-tramp nil t) (ignore-errors (apply #'process-lines "pgrep" (list "-f" "docker"))))
      (cl-loop for line in (cdr (ignore-errors (apply #'process-lines "docker" (list "ps"))))
	       for info = (reverse (split-string line "[[:space:]]+" t))
	       collect (progn (push
			       (concat "/docker:" (car info) ":/")
			       hosts)
			      (when consult-tramp-docker-user
				(if (listp consult-tramp-docker-user)
				    (let ((docker-user consult-tramp-docker-user))
				      (while docker-user
					(push
					 (concat "/docker:" (car docker-user) "@" (car info) ":/")
					 hosts)
					(pop docker-user)))
				  (push
				   (concat "/docker:" consult-tramp-docker-user "@" (car info) ":/")
				   hosts))))))
    (when (require 'vagrant-tramp nil t)
      (cl-loop for box-name in (cl-map 'list 'cadr (vagrant-tramp--completions))
	       do (progn
		    (push (concat "/vagrant:" box-name ":/") hosts)
		    (push (concat "/vagrant:" box-name "|sudo:" box-name ":/") hosts))))
    (push (concat "/sudo:root@localhost:" consult-tramp-localhost-directory) hosts)
    (reverse hosts)))

(defun consult-tramp--directory-files (dir regexp)
  "Return list of all files under DIR that have file names matching REGEXP."
  (let ((result nil)
	(files nil)
	(tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
			'string<))
      (unless (member file '("./" "../"))
	(if (not (consult-tramp--directory-name-p file))
	    (when (string-match regexp file)
	      (push (expand-file-name file dir) files)))))
    (nconc result (nreverse files))))

(defsubst consult-tramp--directory-name-p (name)
  "Return non-nil if NAME ends with a directory separator character."
  (let ((len (length name))
        (lastc ?.))
    (if (> len 0)
        (setq lastc (aref name (1- len))))
    (or (= lastc ?/)
        (and (memq system-type '(windows-nt ms-dos))
             (= lastc ?\\)))))

;;;###autoload
(defun consult-tramp ()
  "Open your ~/.ssh/config with consult interface.
You can connect your server with tramp"
  (interactive)
  (unless (file-exists-p "~/.ssh/config")
    (error "There is no ~/.ssh/config"))
  (when (require 'docker-tramp nil t)
    (unless (executable-find "docker")
      (error "'docker' is not installed")))
  (when (require 'vagrant-tramp nil t)
    (unless (executable-find "vagrant")
      (error "'vagrant' is not installed")))
  (run-hooks 'consult-tramp-pre-command-hook)
  (find-file (completing-read "Tramp: " (consult-tramp--candidates)))
  (run-hooks 'consult-tramp-post-command-hook))

(provide 'consult-tramp)

;;; consult-tramp.el ends here
