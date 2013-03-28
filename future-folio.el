(defun folio-highlight-region ()
  "Highlight the region without using Transient Mark mode."
  (interactive) ;; XXX remove
  ;; XXX
  (remove-overlays nil nil 'kg t) ;; XXX
  (when (mark)
    (let* ((beg (region-beginning))
           (end (region-end))
           (ovs (overlays-in beg end))
           ov highlighted)
      (while ovs
        (setq ov (pop ovs))
        (when (overlay-get ov 'kg)
          (setq highlighted t)
          (setq ovs nil)))
      (if highlighted
          (move-overlay ov beg end)
        (setq ov (make-overlay beg end))
        (overlay-put ov 'kg t)
        (overlay-put ov 'face 'region)))))
