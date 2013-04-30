;;; folio-faces.el --- Folio mode faces

;; Copyright (C) 2012, 2013  Christoph Kluge

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Faces for Folio mode.

;;; Code:

(defgroup folio-faces nil
  "Customize faces used in Folio mode."
  :tag "Faces"
  :group 'folio)

;; XXX font-family-list
;; XXX dark/light background

(defface folio-mode-default
  '((t (:family "DPCustomMono2" :height 120)))
  "Default face for buffers in Folio mode."
  :group 'folio-faces)

(defface folio-highlight-page-join
  '((((class color)
      (background dark))
     (:underline
      (:color "OrangeRed1" :style line)
      :overline "OrangeRed1")))
  "*Face used for highlighting the current page join."
  :group 'folio-faces)

(defface folio-misspelled
  '((((class color) (min-colors 88) (background dark))
     (:underline
      (:color foreground-color :style line)
      :foreground "#bc8383" :inherit (error)))
    (((class color) (min-colors 88) (background light))
     (:underline
      (:color foreground-color :style line)
      :foreground "#bc8383" :inherit (error))))
  "Face for misspelled words."
  :group 'folio-faces)

(defface folio-doublon
  '((((class color) (min-colors 88) (background dark))
     (:underline
      (:color foreground-color :style wave)
      :foreground "#e0cf9f" :inherit (error)))
    (((class color) (min-colors 88) (background light))
     (:underline
      (:color foreground-color :style wave)
      :foreground "#e0cf9f" :inherit (error))))
  "Face for adjacent duplicated words."
  :group 'folio-faces)

(defface folio-word-marker
  '((((class color)
      (background dark))
     (:foreground "black" :background "LightSalmon1"))
    (((class color)
      (background light))
     (:foreground "black" :background "LightSalmon1")))
  "Face for the current highlighted misspelling."
  :group 'folio-faces)

(defface folio-spellcheck-active
  '((((class color)) :foreground "orange")
    (t nil))
  "Face for the lighter when spellchecking is active."
  :group 'folio-faces
  :group 'folio-spellcheck)

(defface folio-spellcheck-inactive
  '((((class color)) :foreground "white")
    (t nil))
  "Face for the lighter when spellchecking is inactive."
  :group 'folio-faces
  :group 'folio-spellcheck)

(defface folio-spellcheck-done
  '((((class color)) :foreground "green")
    (t nil))
  "Face for the lighter when spellchecking is done."
  :group 'folio-faces
  :group 'folio-spellcheck)

(defface folio-link
  '((t
      (:inherit
       (link))))
  "Face for cross references and links."
  :group 'folio-faces)

(defface folio-page-label
  '((((class color)
      (background dark))
     (:box
      (:line-width 2 :color "grey75" :style pressed-button)
      :foreground "DarkOliveGreen3")))
  "Face for page labels in dialogs."
  :group 'folio-faces)

(defface folio-dict-entry
  '((t
     (:height 1.1 :underline
              (:color foreground-color :style line)
              :inherit
              (error))))
 "Face for a dictionary entry in corresponding views."
 :group 'folio-faces)

(defface folio-frequency-tag
  '((((min-colors 88) (class color) (background dark)) :foreground "#93e0e3")
    (((min-colors 88) (class color) (background light)) :foreground "red1"))
  "Face for word frequency tags in dictionary views."
  :group 'folio-faces)

(defface folio-dict
  '((((min-colors 88) (class color) (background dark)) :foreground "#dfaf8f")
    (((min-colors 88) (class color) (background light)) :foreground "red1"))
  "Face for a dictionary heading in corresponding views."
  :group 'folio-faces)

(defface folio-dict-tag
  '((((min-colors 88) (class color) (background dark)) :foreground "CadetBlue1")
    (((min-colors 88) (class color) (background light)) :foreground "red1"))
  "Face for dictionary tags in corresponding views."
  :group 'folio-faces)

(defface folio-gwl-tag
  '((((min-colors 88) (class color) (background dark)) :foreground "green")
    (((min-colors 88) (class color) (background light)) :foreground "green"))
  "Face for `good word' tags in dictionary views."
  :group 'folio-faces)

(defface folio-soundslike
  '((((min-colors 88) (class color) (background dark)) :foreground "#dfaf8f")
    (((min-colors 88) (class color) (background light)) :foreground "red1"))
  "Face for a dictionary heading in corresponding views."
  :group 'folio-faces)

(defface folio-widget-field
  '((((class color)
      (background dark))
     (:weight normal :box
      (:line-width 2 :color "grey75" :style pressed-button)
      :foreground "black" :background "gray68")))
  "Face for input fields."
  :group 'folio-faces)

(defun folio-faces-default ()
  "Setup buffer-specific faces for Folio mode.
In particular remap the default face to allow for face
definitions in the text buffer that are more suitable for
spotting misspelled words and scannos."
  (interactive)
  (make-local-variable 'face-remapping-alist)
  (assq-delete-all 'default face-remapping-alist)
  (add-to-list 'face-remapping-alist (cons 'default 'folio-mode-default))
  (redraw-display))



(provide 'folio-faces)

;;; folio-faces.el ends here
