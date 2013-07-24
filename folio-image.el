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
(require 'folio-compat)
(require 'folio-core)

;; XXX:TODO add support for ezimage
;; XXX:TODO maintain text line detector & sync with point

(defgroup folio-image nil
  "Settings for the handling of images."
  :group 'folio
  :tag "Folio Image handling"
  :version "24.1")

(defcustom folio-page-scan-refresh-delay 0.2
  "Time in seconds to delay refreshing of the current page scan."
  :group 'folio-image
  :tag "Page scan refresh delay"
  :type 'number)

(defvar folio-page-scan nil
  "Full path to the page scan image currently displayed.")
(make-variable-buffer-local 'folio-page-scan)

(defvar folio-page-scan-last-config nil
  "Time-stamp of the last window configuration or size change.")
(make-local-variable 'folio-page-scan-last-config)

(defvar folio-page-scan-last-cmd nil
  "Time-stamp of the last command executed.")
(make-local-variable 'folio-page-scan-last-cmd)

(defvar folio-page-scan-tracking nil
  "Buffer for which scroll events are being tracked.")
(make-local-variable 'folio-page-scan-tracking)

(defvar folio-page-scan-last-scroll nil
  "Time-stamp of the window scroll event.")
(make-local-variable 'folio-page-scan-last-scroll)

(defcustom folio-page-scan-directory-default "pngs"
  "Name of the directory containing the page scan images.
This must be a relative path to the project directory, but can be
a symbolic link, also.  This option sets the default; a project
local value can be set in the project setup."
  :tag "Default directory for page scans"
  :type 'string
  :group 'folio-image)

(defvar folio-page-scan-timer nil
  "Idle timer run for updating the page scan display.")

(defvar folio-page-scan-directory nil
  "Directory containing the page scan images.
If set this variable overrides `folio-page-scan-directory-default'.")
(make-variable-buffer-local 'folio-page-scan-directory)

(defcustom folio-image-directory-default "images"
  "Name of the directory containing the book images.
This must be a relative path to the project directory, but can be a
symbolic link, also.  The option sets the default; a project
local value can be set in the project setup."
  :tag "Default directory for book images"
  :type 'string
  :group 'folio-image)

(defvar folio-image-directory nil
  "Directory containing the book images.
If set this variable overrides `folio-image-directory-default'.")
(make-variable-buffer-local 'folio-image-directory)

(defcustom folio-page-scan-external-viewer-p t
  "If enabled an external image viewer is used to display page
scans.  Otherwise page scans are displayed in an Emacs buffer
which may or may not work, depending on the Emacs version and
what image support actually is compiled in."
  :tag "Whether to use an external image viewer"
  :type 'boolean
  :group 'folio-image
  :group 'folio-external)

(defcustom folio-image-viewer nil
  "Location of an external image viewer program.
This should be capable of displaying PNG and JPEG images."
  :tag "External image viewer program"
  :type '(file :must-match t)
  :group 'folio-image
  :group 'folio-external)

(defcustom folio-image-viewer-args nil
  "Arguments to pass to the external image viewer program."
  :tag "External image viewer program arguments"
  :type 'string
  :group 'folio-image
  :group 'folio-external)

;; XXX:TODO folio-image-viewer-args-alist --- map image type to args
;; (png . "-foo png")

(defcustom folio-page-scan-follows-point-p nil
  "Whether to let page scans follow cursor movements.
If enabled the page scan display is maintained synchroneous to
the current page or cursor location.  Enabling this option for
image display using an external viewer may have issues outside of
the control of Emacs, for instance with respect to performance,
or loss of window focus."
  :tag "Whether the page scan follows point"
  :type 'boolean
  :group 'folio-image)

;; XXX:TODO maintain timer, poll folio-image-at-point--redisplay on
;; change, with some lag

(defun folio-toggle-page-scan-follows-point (&optional force-disable)
  "Toggle whether the page scan follows point.
If called interactively the customized value of
`folio-page-scan-follows-point-p' is toggled for the current
session only.  If called from Lisp a non-nil value for
FORCE-DISABLE causes tracking of point to be disabled, again, for
the current session only."
  (interactive)
  (let ((new-value (and (not force-disable)
                        (not folio-page-scan-follows-point-p))))
    (if new-value
        (progn
          (add-hook 'post-command-hook
                    'folio-page-scan-post-command nil t)
          (add-hook 'window-scroll-functions
                    'folio-page-scan-after-scroll nil t))
      (remove-hook 'post-command-hook
                   'folio-page-scan-post-command t)
      (remove-hook 'window-scroll-functions
                   'folio-page-scan-after-scroll t))
    (customize-set-variable
     'folio-page-scan-follows-point-p new-value)
    (customize-save-customized)
    (message (concat "Page scan following point was "
                     (if new-value "enabled" "disabled")))))

(defcustom folio-max-image-proportion 0.9
  "Specifies how large pictures displayed are in relation to the
window they're in.  A value of 0.7 means that they are allowed to
take up 70% of the width and height of the window.  If they are
larger than this, and Emacs supports it, then the images will be
rescaled down to fit these criteria."
  :tag "Image maximal proportion"
  :type 'float
  :group 'folio-image
  :version "24.1")

(defcustom folio-imagemagick-identify-command "identify"
  "Name or full-path to the ImageMagick identify command.
Set this to the appropriate file path and name if `identify' is
not the desirable way to invoke this program."
  :tag "Name of the ImageMagick identify command"
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
  :tag "Name of the ImageMagick `convert' program"
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
  (let ((args (folio-transform-image-args spec)))
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
       ((eq button mouse-wheel-down-event)
        (cond
         ((null modifiers) ;; vscroll
          (if folio-inverted-scrolling
              (image-next-line 1)
            (image-previous-line 1)))
         ((equal modifiers '(shift)) ;; hscroll
          (folio-image-backward-hscroll-fast))
         (t
          (ignore))))
       ((eq button mouse-wheel-up-event)
        (cond
         ((null modifiers) ;; vscroll
          (if folio-inverted-scrolling
              (image-previous-line 1)
            (image-next-line 1)))
         ((equal modifiers '(shift)) ;; hscroll
          (folio-image-forward-hscroll-fast))
         (t
          (ignore))))
       ;; To be verified: the wheel-left and wheel-right events aren't
       ;; used on OS X 10.8.4 dev-platform.
       ((eq button 'wheel-left)
        (folio-image-backward-hscroll-fast))
       ((eq button 'wheel-right)
        (folio-image-forward-hscroll-fast))
       (t (error "Bad binding in folio-image-mwheel-scroll"))))))

(defun folio-page-scan-update (buffer &optional force)
  "Update the page scan display for BUFFER.
If FORCE is non-nil refresh the display even if the current scan
already is displayed to allow for changes in window
configuration, size, etc."
  (when (buffer-live-p buffer) ;; avoid race
    (with-current-buffer buffer
      (when (folio-mode-p)
        (let ((scan (folio-find-page-scan
                     (folio-page-scan-at-point (point)))))
          (when (or force (not (equal scan folio-page-scan)))
            (folio-show-page-scan scan)))
        (setq folio-page-scan-tracking nil
              folio-page-scan-last-cmd nil
              folio-page-scan-last-scroll nil
              folio-page-scan-last-config nil)))))

(defun folio-page-scan-post-command ()
  "Post command hook function for updating the page scan display."
  (if folio-page-scan-follows-point-p
      (if (zerop (or folio-page-scan-refresh-delay 0))
          (folio-page-scan-update (current-buffer))
        (when folio-page-scan-timer
          (cancel-timer folio-page-scan-timer))
        (setq folio-page-scan-last-cmd (current-time)
              folio-page-scan-timer
              (run-with-idle-timer folio-page-scan-refresh-delay nil
                                   'folio-page-scan-post-command-fired)))
    (setq folio-page-scan-last-cmd nil)
    (remove-hook
     'post-command-hook 'folio-page-scan-post-command t)))

(defun folio-page-scan-post-command-fired ()
  "Timer function for asynchronously updating the page scan view
in `post-command' with a time lag as specified by
`folio-page-scan-refresh-delay'."
  (unless (or (null folio-page-scan-last-cmd)
              (zerop (or folio-page-scan-refresh-delay 0)))
    (let ((now (current-time))
          (later (timer-relative-time folio-page-scan-last-cmd
                                      folio-page-scan-refresh-delay)))
      (when (time-less-p later now)
        (folio-page-scan-update (current-buffer))))))

(defun folio-page-scan-after-scroll (win start)
  "After scroll hook function for updating the page scan display."
  (if folio-page-scan-follows-point-p
      (if (zerop (or folio-page-scan-refresh-delay 0))
          (folio-page-scan-update (window-buffer win))
        (when folio-page-scan-timer
          (cancel-timer folio-page-scan-timer))
        (setq folio-page-scan-tracking (window-buffer win)
              folio-page-scan-last-scroll (current-time)
              folio-page-scan-timer
              (run-with-idle-timer folio-page-scan-refresh-delay nil
                                   'folio-page-scan-after-scroll-fired)))
    (setq folio-page-scan-last-scroll nil)
    (remove-hook
     'window-scroll-functions 'folio-page-scan-after-scroll t)))

(defun folio-page-scan-after-scroll-fired ()
  "Timer function for asynchronously updating the page scan view
in `after-scroll' with a time lag as specified by
`folio-page-scan-refresh-delay'."
  (when (and folio-page-scan-last-scroll
             folio-page-scan-follows-point-p)
    (let ((now (current-time))
          (later (timer-relative-time folio-page-scan-last-scroll
                                      folio-page-scan-refresh-delay)))
      (when (and folio-page-scan-tracking (time-less-p later now))
        (folio-page-scan-update folio-page-scan-tracking)))))

(defun folio-image-mode-p ()
  "Return non-nil if the current buffer is in `folio-image-mode'."
  (derived-mode-p 'folio-image-mode))

(defun folio-page-scan-after-config ()
  "After scroll hook function for updating the page scan display."
  (let (buffer)
    (walk-windows (lambda (x)
                    (unless buffer
                      (with-current-buffer (window-buffer x)
                        (when (folio-image-mode-p)
                          ;; there should be only one window for
                          ;; displaying page scans
                          (setq buffer folio-parent-buffer)))))
                  nil 'visible)
    (if buffer
        (if (zerop (or folio-page-scan-refresh-delay 0))
            (folio-page-scan-update buffer)
          (when folio-page-scan-timer
            (cancel-timer folio-page-scan-timer))
          (setq folio-page-scan-tracking buffer
                folio-page-scan-last-config (current-time)
                folio-page-scan-timer
                (run-with-idle-timer folio-page-scan-refresh-delay nil
                                     'folio-page-scan-after-config-fired)))
      (setq folio-page-scan-last-config nil)
      (unless folio-page-scan-follows-point-p
        (remove-hook 'window-size-change-functions
                     'folio-page-scan-after-size)
        (remove-hook 'window-configuration-change-hook
                     'folio-page-scan-after-config t)))))

(defun folio-page-scan-after-config-fired ()
  "Timer function for asynchronously updating the page scan view
in `after-config' with a time lag as specified by
`folio-page-scan-refresh-delay'."
  (setq folio-page-scan-last-config nil)
  (when folio-page-scan-follows-point-p
    (folio-page-scan-update
     folio-page-scan-tracking 'force-update)))

(defun folio-page-scan-after-size (_frame)
  "After resize hook function for refreshing the page scan
display."
  (folio-page-scan-after-config))

;;;###autoload
(defun folio-show-page-scan-external (file-name)
  "Given a page scan image file display it using an external viewer.
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
  (unwind-protect
      (save-selected-window
        (cond
         (nil ;;folio-page-scan-external-viewer-p
          (unless folio-image-viewer
            (error "No external image viewer configured"))
          (unless (= 0 (folio-show-page-scan-external file-name))
            (error "Failure calling external image viewer")))
         ((fboundp 'image-mode)
          (unless (and (image-type-available-p 'png)
                       (display-images-p))
            (error "This version of Emacs does not \
appear to support PNG images"))
          (let* ((parent (when (folio-mode-p)
                           (current-buffer)))
                 (buffer-name "*Page Scan*")
                 (buffer (get-buffer-create buffer-name)))
            (pop-to-buffer buffer nil 'norecord)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (goto-char (point-min))
              (insert-image (folio-create-image 'png file-name))
              ;; XXX actually apply the 'default' transformation
              (folio-image-fit-to-width)
              (when parent
                (folio-set-parent-buffer parent))
              (set-buffer-modified-p nil))))))
    (setq folio-page-scan file-name)))

;;;###autoload
(defun folio-sync-page-scan (&optional point)
  "Synchronize page scan with POINT position.
Return t if the display is up-to-date, or nil otherwise."
  (interactive)
  (let ((scan (folio-find-page-scan
               (folio-page-scan-at-point point))))
    (or (and scan (folio-show-page-scan scan) t) nil)))

(defvar folio-image-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map image-mode-map)
    ;; Get rid of the inappropriate `image-toggle-display'.
    (define-key map (kbd "C-c C-c") 'undefined)

    ;; Remap all mouse wheel events.
    (define-key map [remap mwheel-scroll]
      'folio-image-mwheel-scroll)
    (define-key map [remap mac-mwheel-scroll]
      'folio-image-mwheel-scroll)

    (define-key map (kbd "r") 'folio-image-reload)

    (define-key map (kbd "f") 'folio-image-fit-to-width)
    (define-key map (kbd "F") 'folio-image-fit-to-height)

    ;; XXX C-+ zoom in
    ;; XXX C-- zoom out
    ;; XXX Emacs MacPort: gesture events <magnify-up> <magnify-down>
    ;;(when (fboundp 'mac-magnify-text-scale)
    ;;  (define-key map [remap mac-magnify-text-scale] 'folio-image-zoom))

    ;; Remap `forward-char', `backward-char' to left/right panning.
    (define-key map [remap right] 'folio-image-forward-hscroll)
    (define-key map [remap left] 'folio-image-backward-hscroll)
    map)
  "Mode keymap for `folio-image-mode'.")

(defun folio-image-mode-teardown ()
  (remove-hook 'window-size-change-functions
               'folio-page-scan-after-size)
  (remove-hook 'window-configuration-change-hook
               'folio-page-scan-after-config t)
  ;; (folio-toggle-page-scan-follows-point 'disable)
  )

;;;###autoload
(define-derived-mode folio-image-mode image-mode "Folio-Image"
  "A major mode for displaying page scan images."
  (use-local-map folio-image-mode-map)
  (add-hook 'window-size-change-functions
            'folio-page-scan-after-size) ;; global
  (add-hook 'window-configuration-change-hook
            'folio-page-scan-after-config nil t)
  (add-hook 'change-major-mode-hook
            'folio-image-mode-teardown nil t)
  (add-hook 'kill-buffer-hook
            'folio-image-mode-teardown nil t))


(provide 'folio-image)

;;; folio-image.el ends here
