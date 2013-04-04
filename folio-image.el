;;; folio-image.el --- Folio mode support for images

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

;; Folio mode support for image files.

;;; Code:

(require 'image)
(require 'image-mode)

(require 'folio-base)
(require 'folio-core)

(defgroup folio-image nil
  "Settings for the handling of images."
  :tag "Image handling"
  :group 'folio
  :version "24.1")

(defcustom folio-page-scan-directory-default "pngs"
  "Name of the directory containing the page scan images.
This must be a relative path to the project directory, but can be
a symbolic link, also.  The option sets the default; a project
local value can be set in the project setup."
  :tag "Folio Default directory for page scans"
  :type 'string
  :group 'folio-image)

(defvar folio-page-scan-directory nil
  "Directory containing the page scan images.
If set this variable overrides `folio-page-scan-directory-default'.")
(make-variable-buffer-local 'folio-page-scan-directory)

(defcustom folio-image-directory-default "images"
  "Name of the directory containing the book images.
This must be a relative path to the project directory, but can be a
symbolic link, also.  The option sets the default; a project
local value can be set in the project setup."
  :tag "Folio Default directory for book images"
  :type 'string
  :group 'folio-image)

(defvar folio-image-directory nil
  "Directory containing the book images.
If set this variable overrides `folio-image-directory-default'.")
(make-variable-buffer-local 'folio-image-directory)

(defcustom folio-page-scan-external-viewer-p t
  "Whether to use an external image viewer.
If enabled an external image viewer is used to display page
scans.  Otherwise page scans are displayed in an Emacs buffer
which may or may not work, depending on the Emacs version and
what image support actually is compiled in."
  :tag "Folio Whether to use an external image viewer"
  :type 'boolean
  :group 'folio-image
  :group 'folio-external)

(defcustom folio-image-viewer nil
  "Location of an external image viewer program.
This should be capable of displaying PNG and JPEG images."
  :tag "Folio External image viewer program"
  :type '(file :must-match t)
  :group 'folio-image
  :group 'folio-external)

(defcustom folio-image-viewer-args nil
  "Arguments to pass to the external image viewer program."
  :tag "Folio External image viewer program arguments"
  :type 'string
  :group 'folio-image
  :group 'folio-external)

;; XXX:TODO folio-image-viewer-args-alist --- map image type to args (png . "-foo png")

(defcustom folio-page-scan-follow-point-p nil
  "Whether to let page scans follow cursor movements.
If enabled the page scan display is maintained synchroneous to
the current page or cursor location.  Enabling this option for
image display using an external viewer may have issues outside of
the control of Emacs, for instance with respect to performance,
or loss of window focus."
  :tag "Folio Whether the page scan follows point"
  :type 'boolean
  :group 'folio-image)

;; XXX:TODO maintain timer, poll folio-image-at-point--redisplay on
;; change, with some lag

(defcustom folio-max-image-proportion 0.9
  "Specifies how large pictures displayed are in relation to the
window they're in.  A value of 0.7 means that they are allowed to
take up 70% of the width and height of the window.  If they are
larger than this, and Emacs supports it, then the images will be
rescaled down to fit these criteria."
  :tag "Folio Image maximal proportion"
  :type 'float
  :group 'folio-image
  :version "24.1")

(defcustom folio-imagemagick-identify-command "identify"
  "Name or full-path to the ImageMagick identify command.
Set this to the appropriate file path and name if `identify' is
not the desirable way to invoke this program."
  :tag "Folio Name of the ImageMagick identify command"
  :type 'file
  :group 'folio-image
  :group 'folio-external
  :version "24.1")

(defcustom folio-imagemagick-convert-program
  (or (locate-file "convert" exec-path exec-suffixes
                   'file-executable-p)
      "convert")
  "Name or full path to the ImageMagick `convert' program.
Set this to the appropriate file path and name if `convert' is
not the desirable way to invoke this program.  On Windows the
full path should be used because of the system program of the
same name."
  :tag "Folio Name of the ImageMagick `convert' program"
  :type 'file
  :group 'folio-image
  :group 'folio-external
  :version "24.1")

(defvar folio-image-default-transform '(fit-width)
  "Default transform to apply when loading a new image.")

(defsubst folio-create-image (type file &optional props)
  "Create and return an image of type TYPE from FILE.
Assign the image the properties specified by PROPS."
  (apply 'create-image `(,file ,type nil ,@props)))

(defun folio-image-size/imagemagick (file)
  "Return the size of FILE as a cons."
  (with-temp-buffer
    (call-process folio-imagemagick-identify-command
                  nil t nil "-format" "%wx%h" file)
    (goto-char (point-min))
    (re-search-forward "^\\([0-9]+\\)x\\([0-9]+\\)")
    (cons (string-to-number (match-string 1))
          (string-to-number (match-string 2)))))

(defun folio-get-image-spec ()
  "Get image specification at point."
  (let ((display (get-text-property (point) 'display)))
    (cond
     ((eq 'image (car display))
      display)
     ((and (listp (cdr display))
           (eq 'image (car (cadr display))))
      (cadr display)))))

(defun folio-get-image-data (&optional pos)
  "Get data for image at POS or point, if POS is nil."
  (save-excursion
    (goto-char (or pos (point)))
    (let ((image-spec (folio-get-image-spec)))
      (or (cadr (member :data image-spec))
          (let ((file (cadr (member :file image-spec))))
            (when (and file (file-readable-p file))
              (with-temp-buffer
                (insert-file-contents-literally file)
                (string-make-unibyte
                 (buffer-substring-no-properties
                  (point-min) (point-max))))))))))

(defvar folio-image-preserve-aspect-ratio t
  "Whether to preserve the image aspect ratio in transforms.")

(defsubst folio-image-prop (prop &optional spec)
  "Return the value of the image property PROP.
SPEC nil or omitted means return the property value for the
currently displayed image, or of the image SPEC otherwise."
  (plist-get (cdr (or spec
                      (image-get-display-property)))
             prop))

(defun folio-transform-image-args (&optional spec)
  (let* ((image (or spec (image-get-display-property)))
         (transform-props
          (append '(image) (image-transform-properties
                            image)))
         args)
    (let ((size (image-size image 'px))
          (width (folio-image-prop :width transform-props))
          (height (folio-image-prop :height transform-props))
          resize)
      ;; (message "XXX %s TTTT %s" transform-props size)
      (when (and width (not (= width (car size))))
        (setq resize (concat resize (format "%d" width))))
      (when (and height (not (= height (cdr size))))
        (setq resize (concat resize (format "x%d" height))))
      (unless (or folio-image-preserve-aspect-ratio
                  (null resize))
        (setq resize (concat resize "!")))
      (when resize
        (push resize args)
        (push "-resize" args)))
    (let ((rotation (folio-image-prop :rotation transform-props)))
      (when rotation
        (push (format "%d"
                      (if (< rotation 0) (+ rotation 360) rotation))
              args)
        (push "-rotate" args)))
    (when (or args spec)
      (let ((type (folio-image-prop :type image))
            (file (or (folio-image-prop :file image)
                      (buffer-file-name))))
        (unless (or file (not (file-readable-p file)))
          (error "Unrecognized image"))
        (if type
            (setq args (cons (format "%s:%s" type file) args))
          (setq args (cons (format "%s" file) args)))
        (setq args (nconc args '("-")))))
    args))

(defun folio-transform-image (&optional spec)
  "Transform current buffer image."
  (let* ((image (or spec (image-get-display-property))) ;; XXX remove
         (args (folio-transform-image-args spec)))
    ;; (message "XXX ---- args %s type %s" args (folio-image-prop :type image))
    (when args
      (let* ((program folio-imagemagick-convert-program)
             (coding-system-for-read 'no-conversion)
             (process-adaptive-read-buffering t)
             (data (with-temp-buffer
                     (if (eq 0 (apply #'call-process
                                      program nil t nil args))
                         (string-as-unibyte
                          (buffer-substring-no-properties
                           (point-min) (point-max)))
                       (error "Failure transforming image")))))
        (let ((inhibit-read-only t)
              (buffer-undo-list t)
              (modified (buffer-modified-p)))
          (erase-buffer)
          (insert data)
          (folio-with-muted-message
           (folio-image-mode))
          (restore-buffer-modified-p modified)))
      (image-bob))))

(defun folio-image-fit-to-width ()
  "Fit the current image to the width of the current window."
  (interactive)
  (setq image-transform-resize 'fit-width)
  (folio-transform-image))

(defun folio-image-fit-to-height ()
  "Fit the current image to the height of the current window."
  (interactive)
  (setq image-transform-resize 'fit-height)
  (folio-transform-image))

(defun folio-image-reload ()
  "Reload the current image."
  (interactive)
  (let* ((image (image-get-display-property))
         (type (folio-image-prop :type image))
         (file (or (folio-image-prop :file image)
                   (buffer-file-name)))
         (spec (create-image file type)))
    (setq image-transform-resize
          (car-safe folio-image-default-transform)
          image-transform-rotation
          (or (cdr-safe folio-image-default-transform) 0.0))
    (image-flush image)
    (folio-transform-image spec)))

(defun folio-rescale-image (data &optional type)
  (if (or (not (fboundp 'imagemagick-types))
          (not (get-buffer-window (current-buffer))))
      (create-image data type)
    (let* ((image (create-image data type))
          (size (image-size image))
          (width (car size))
          (height (cdr size))
          (edges (window-inside-pixel-edges
                  (get-buffer-window (current-buffer))))
          (window-width (truncate (* folio-max-image-proportion
                                     (- (nth 2 edges) (nth 0 edges)))))
          (window-height (truncate (* folio-max-image-proportion
                                      (- (nth 3 edges) (nth 1 edges)))))
          scaled-image)
     (when (> height window-height)
       (setq image (or (create-image data 'imagemagick t
                                     :height window-height)
                       image))
       (setq size (image-size image t)))
     (when (> (car size) window-width)
       (setq image (or
                    (create-image data 'imagemagick t
                                  :width window-width)
                    image)))
     image)))

(defun folio-find-page-scan (file-name)
  "Return the full path-name for a page scan image.
FILE-NAME should be the file name sans path but including
extension such as `047.png'."
  (expand-file-name file-name
                    (expand-file-name
                     (or folio-page-scan-directory
                         folio-page-scan-directory-default)
                                      default-directory)))

(defalias 'folio-image-forward-hscroll
  (symbol-function 'image-forward-hscroll))

(defalias 'folio-image-backward-hscroll
  (symbol-function 'image-backward-hscroll))

(defconst folio-image-hscroll-fast-amount 2
  "Scroll amount for scrolling fast in number of characters.")

(defun folio-image-forward-hscroll-fast ()
  "Scroll right fast by `folio-image-hscroll-fast-amount'."
  (interactive)
  (folio-image-forward-hscroll folio-image-hscroll-fast-amount))

(defun folio-image-backward-hscroll-fast ()
  "Scroll left fast by `folio-image-hscroll-fast-amount'."
  (interactive)
  (folio-image-backward-hscroll folio-image-hscroll-fast-amount))

(defvar folio-image-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map image-mode-map)
    ;; Get rid of the inappropriate `image-toggle-display'.
    (define-key map (kbd "C-c C-c") 'undefined)

    ;; Remap all mouse wheel events.
    (define-key map [remap mwheel-scroll] 'folio-image-mwheel-scroll)
    (define-key map [remap mac-mwheel-scroll] 'folio-image-mwheel-scroll)

    (define-key map (kbd "r") 'folio-image-reload)

    (define-key map (kbd "f") 'folio-image-fit-to-width)
    (define-key map (kbd "F") 'folio-image-fit-to-height)

    ;; XXX C-+ zoom in
    ;; XXX C-- zoom out

    ;; Remap `forward-char', `backward-char' to left/right panning.
    (define-key map [remap right] 'folio-image-forward-hscroll)
    (define-key map [remap left] 'folio-image-backward-hscroll)
    map)
  "Mode keymap for `folio-image-mode'.")

;;;###autoload
(defun folio-image-mwheel-scroll (event)
  "Handle mouse wheel events according to EVENT."
  ;; With shift S- pan left, right.
  ;; With meta XXX M- zoom.
  ;; With control XXX C- next/previous scan.
  (interactive (list last-input-event))
  (let ((button (mwheel-event-button event))
        ;; Drop click, double, triple modifiers.
        (modifiers (cl-intersection (event-modifiers event)
                                    '(shift control meta alt)))
        (window (mwheel-event-window event)))
    ;; Scroll events are produced for the window under the cursor,
    ;; whether selected or not (which is a good thing).
    (with-selected-window window
      (cond
       ((eq button mouse-wheel-up-event)
        (cond
         ((null modifiers)
          (if folio-inverted-scrolling
              (image-next-line 1)
            (image-previous-line 1)))
         ((equal modifiers '(shift))
          (folio-image-backward-hscroll-fast))
         (t
          (ignore))))
       ((eq button mouse-wheel-down-event)
        (cond
         ((null modifiers)
          (if folio-inverted-scrolling
              (image-previous-line 1)
            (image-next-line 1)))
         ((equal modifiers '(shift))
          (folio-image-forward-hscroll-fast))
         (t
          (ignore))))
       ;; To be verified: the wheel-left and wheel-right events
       ;; apparently are not produced on the author's development
       ;; platform.
       ((eq button 'wheel-left)
        (folio-image-backward-hscroll-fast))
       ((eq button 'wheel-right)
        (folio-image-forward-hscroll-fast))
       (t (error "Bad binding in folio-image-mwheel-scroll"))))))

;;;###autoload
(define-derived-mode folio-image-mode image-mode "Folio"
  "A major mode for displaying page scan images."
  (use-local-map folio-image-mode-map))

;;;###autoload
(defun folio-show-page-scan-external (file-name)
  "Show the page scan with file name FILE-NAME using an external viewer.
FILE-NAME should be the complete path to the scan image.  The
return value is that of `call-process'."
  (let ((args (mapconcat #'identity
                         `(,(folio-chomp folio-image-viewer)
                           ,@(remove "" (split-string
                                         (or folio-image-viewer-args
                                             "") " "))
                           ,file-name) " ")))
    (call-process shell-file-name
                  nil "*Messages*" nil shell-command-switch args)))

;;;###autoload
(defun folio-show-page-scan (file-name)
  "Show the page scan with file name FILE-NAME."
  (let ((dot-png (folio-find-page-scan file-name)))
    (unwind-protect
        (save-selected-window
          (cond
           (nil ;;folio-page-scan-external-viewer-p
            (unless folio-image-viewer
              (error "No external image viewer configured"))
            (unless (= 0 (folio-show-page-scan-external dot-png))
              (error "Failure calling external image viewer")))
           ((fboundp 'image-mode)
            (unless (and (image-type-available-p 'png)
                         (display-images-p))
              (error "This version of Emacs does not \
appear to support PNG images"))
            (switch-to-buffer-other-window "*Page Scan*")
            ;; (insert-file-contents dot-png t nil nil t)
            (erase-buffer)
            (goto-char (point-min))
            (insert-image (folio-rescale-image dot-png 'png))
;            (insert-image (create-image dot-png 'png))
            (set-buffer-modified-p nil)
            (folio-image-mode)))))))

(defun folio-sync-page-scan (&optional point)
  "XXX"
  (interactive)
  (let ((scan (folio-page-scan-at-point point)))
    (or (and scan (folio-show-page-scan scan) t) nil)))


(provide 'folio-image)

;;; folio-image.el ends here
