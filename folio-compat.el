;;; folio-compat.el --- Folio mode Emacs 23 compatibility layer

;; Copyright (C) 2013  Christoph W. Kluge

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

;; Folio mode glue code for backwards compatibility with Emacs 23.

;;; Code:

(eval-when-compile (require 'cl))

;;; add reverse alias to the old cl functions if the cl-prefix is not
;;; known
(dolist (old-fun '(coerce
                   copy-list
                   delete-duplicates
                   find
                   remove-duplicate
                   subseq))
  (let ((new-fun (unless (fboundp (intern-soft
                                   (format "cl-%s" old-fun)))
                   (intern (format "cl-%s" old-fun)))))
    (when new-fun
      (defalias new-fun old-fun))))

;;;###autoload
(defmacro folio-called-interactively-p (&optional kind)
  "Return t if the containing function was called by `call-interactively'.
With Emacs versions 23.2 or later this is equivalent to calling
`called-interactively-p'."
  (if (or (> emacs-major-version 23)
          (and (= emacs-major-version 23)
               (>= emacs-minor-version 2)))
      ;; The KIND argument to called-interactively-p was introduced
      ;; with Emacs 23.2.
      `(with-no-warnings (called-interactively-p ,kind))
    `(interactive-p)))

;;;###autoload
(defun folio-user-error (format &rest args)
  "Signal a pilot error, making error message by passing all args
to `format'.  This defun is identical to `user-error' that was
introduced with Emacs 24."
  (if (fboundp 'user-error)
      (user-error format args)
    (while t
      (signal 'folio-user-error (list (apply #'format format args))))))



(provide 'folio-compat)

;;; folio-compat.el ends here
