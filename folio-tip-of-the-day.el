;;; folio-tip-of-the-day.el --- Folio mode tip of the day

;; Copyright (C) 2013  Christoph W. Kluge

;; Author: Christoph W. Kluge <shift.in.emphasis@gmail.com>
;; Keywords: wp

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

;; Tip of the day for Folio mode.

;;; Code:

(defconst folio-totd
  (purecopy
   '("Discovered a funny character at point? \"C-u C-x \
=\" to investigate."
     "Input Unicode characters typing \"C-x 8 RET \
HEX-CODEPOINT\" or \"C-x 8 RET NAME.\""
     "Type \"C-h k\" to display documentation of the \
function invoked by a certain key."
     "Type \"C-h m\" to show information about current \
major and minor modes."
     "Remember TAB-completion and input history when \
being prompted at the mini-buffer."
     "Incrementally search forward with \"C-s\", \
backward \"C-r\", regexp \"C-M-s\", and \"C-M-r.\""
     "Having troubles getting a regexp to work? \”M-x \
re-builder\" to construct a regexp interactively.  Make sure \
syntax is set to \ \"string.\""
     "\"C-x 3\" splits the selected window horizontally; \"C-x 2\" \
to split vertically, \"C-x 1\" to unsplit, \"C-x o\" to jump \
between windows."
     "\"M-x list-faces-display\" lets you pick and customize any \
known face to suit your needs."
     "To list lines with more than 75 characters: \"M-x occur \
RET ^.\{75,\} RET\".")))

;; XXX TODO more stuff, visualization in minibuffer/splash screen


(provide 'folio-tip-of-the-day)

;;; folio-tip-of-the-day.el ends here
