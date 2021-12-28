;;; corfu-history.el --- Sorting by history for Corfu -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Sort candidates by their history position. Maintain a list of
;; recently selected candidates.

;;; Code:

(require 'corfu)

(defcustom corfu-history-length nil
  "Corfu history length."
  :type '(choice (const nil) integer)
  :group 'corfu)

(defvar corfu-history--hash nil
  "Hash table of Corfu candidates.")

(defvar corfu-history--list nil
  "History of Corfu candidates.")

(defun corfu-history--sort-predicate (x y)
  "Sorting predicate which compares X and Y."
  (or (< (cdr x) (cdr y))
      (and (= (cdr x) (cdr y))
           (string< (car x) (car y)))))

(defun corfu-history--sort (candidates)
  "Sort CANDIDATES by history."
  (unless corfu-history--hash
    (let ((index 0))
      (setq corfu-history--hash (make-hash-table :test #'equal :size (length corfu-history--list)))
      (dolist (elem corfu-history--list)
        (unless (gethash elem corfu-history--hash)
          (puthash elem index corfu-history--hash))
        (setq index (1+ index)))))
  ;; Decorate each candidate with (index<<13) + length. This way we sort first by index and then by
  ;; length. We assume that the candidates are shorter than 2**13 characters and that the history is
  ;; shorter than 2**16 entries.
  (let ((cand candidates))
    (while cand
      (setcar cand (cons (car cand)
                         (+ (lsh (gethash (car cand) corfu-history--hash #xFFFF) 13)
                            (length (car cand)))))
      (pop cand)))
  (setq candidates (sort candidates #'corfu-history--sort-predicate))
  ;; Drop decoration from the candidates
  (let ((cand candidates))
    (while cand
      (setcar cand (caar cand))
      (pop cand)))
  candidates)

(defun corfu-history--insert (&rest _)
  "Advice for `corfu--insert'."
  (when (>= corfu--index 0)
    ;; TODO add history variable to `savehist-additional-variables' if bound?
    (add-to-history 'corfu-history--list
                    (nth corfu--index corfu--candidates)
                    corfu-history-length)
    (setq corfu-history--hash nil)))

;;;###autoload
(define-minor-mode corfu-history-mode
  "Update Corfu history and sort completions by history."
  :global t
  (if corfu-history-mode
      (progn
        (advice-add #'corfu--insert :before #'corfu-history--insert)
        (advice-add #'corfu--sort :override #'corfu-history--sort))
    (advice-remove #'corfu--insert #'corfu-history--insert)
    (advice-remove #'corfu--sort #'corfu-history--sort)))

(provide 'corfu-history)
;;; corfu-history.el ends here
