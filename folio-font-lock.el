;;; folio-font-lock.el --- FontLock support for Folio mode

;; Copyright (C) 2012, 2013  Christoph W. Kluge

;; Author: Christoph W. Kluge <shift.in.emphasis@gmail.com>
;; Keywords: wp

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Basic FontLock support for Folio mode.

;;; Code:

(require 'font-lock)

(declare-function fast-lock-after-fontify-buffer "fast-lock")
(declare-function fast-lock-mode "fast-lock")
(declare-function lazy-lock-after-fontify-buffer "lazy-lock")
(declare-function lazy-lock-mode "lazy-lock")

;;;###autoload
(defun folio-font-lock-fontify-region (beg end)
  "Fontify the text region between BEG and END.
Call after-fontify functions of the respective font-lock-mode.
Note that fontify functions might move point."
  (font-lock-fontify-region beg end)
  (cond
   ((bound-and-true-p fast-lock-mode)
    (fast-lock-after-fontify-buffer))
   ((bound-and-true-p jit-lock-mode)
    (jit-lock-refontify beg end))
   ((bound-and-true-p lazy-lock-mode)
    (lazy-lock-after-fontify-buffer))))

;;;###autoload
(defun folio-font-lock-fontify-window (&optional window)
  "Fontify the text region within the current window boundaries.
If WINDOW is non-nil fontify that instead.  Conditions and
restrictions are those of `folio-font-lock-fontify-region'."
  (folio-font-lock-fontify-region
   (window-start window) (window-end window)))

;;;###autoload
(defconst folio-font-lock-keywords-default
  `(("^/\\*\\|^/#\\|^\\*/\\|^#/"
     . font-lock-keyword-face)
    (,(let ((tags '("i" "b" "g" "f" "u" "sc" "tb")))
        (regexp-opt
         (append (mapcar (lambda (x) (concat "<" x ">")) tags)
                 (mapcar (lambda (x) (concat "</" x ">")) tags))))
     . font-lock-type-face)
    (,(regexp-opt '("Blank Page" "Illustration"
                    "Footnote" "Sidenote:"
                    "Greek:" "Hebrew:" "Arabic:" "Chaldee:" "Syriac:"))
     . font-lock-builtin-face)
    ("\\(?:^-----File: .+$\\)"
     . font-lock-preprocessor-face)
    ("\\[\\(\\*\\*[^]]+\\)"
     . (1 font-lock-warning-face))
    ("\\(?:\\*?\\[\\|\\]\\*?\\|[{}^]\\)"
     . font-lock-negation-char-face))
  "Default FontLock 'keywords'.")

;; (font-lock-add-keywords nil
;;    '(("“[^”]*”" 0 font-lock-string-face)))


(provide 'folio-font-lock)

;;; folio-font-lock.el ends here
