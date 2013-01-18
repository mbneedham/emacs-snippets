;--narrowing commands for narrowing in an indirect buffer

;`narrow-to-region-indirect' and `narrow-to-defun-indirect'
;create an indirect buffer with name "[buffername]:narrowed[<n>]"
;and applies the narrowing to the indirect so that other
;windows showing the original buffer are unaffected.
;It's also possible to have multiple windows with different
;narrowings of the same file, for example, to examine 
;different functions simultaneously

;`widen-indirect' kills indirect buffers created by 
;narrow-to-region-indirect and narrow-to-defun-indirect
;and returns the user to the original buffer
;(or performs a regular widen if the buffer was not created
; with an indirect narrowing)

(setq-default indirect-buffer-parent nil)

(defmacro do-in-indirect-buffer (action &optional suffix)
  `(if indirect-buffer-parent
       ,action
       (let* ((parent-name (buffer-name))
              (new-name (generate-new-buffer-name (format "%s%s" parent-name ,suffix))))
         (switch-to-buffer (clone-indirect-buffer new-name nil))
         (set (make-local-variable 'indirect-buffer-parent) parent-name)
         ,action
         (setq mark-active nil))))

(defun narrow-to-region-indirect ()
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (do-in-indirect-buffer 
     (narrow-to-region start end) ":narrowed")))

(defun narrow-to-defun-indirect ()
  (interactive)
  (do-in-indirect-buffer (narrow-to-defun) ":narrowed"))

(defun widen-indirect ()
  (interactive)
  (if (not indirect-buffer-parent)
      (widen)
    (let ((buf (current-buffer))
          (p (point)))
      (switch-to-buffer indirect-buffer-parent)
      (kill-buffer buf)
      (goto-char p))))

(global-set-key (kbd "C-x n d") 'narrow-to-defun-indirect)
(global-set-key (kbd "C-x n n") 'narrow-to-region-indirect)
(global-set-key (kbd "C-x n w") 'widen-indirect) 
