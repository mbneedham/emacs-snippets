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

(setq-default narrow-buffer-parent nil)

(defun make-indirect-narrow-buffer-name (parent-name)
  (do* ((buf-count 0 (+ buf-count 1))
        (name (format "%s:narrowed" parent-name)
              (format "%s:narrowed<%d>" parent-name buf-count)))
      ((not (get-buffer name)) name)))

(defmacro do-indirect-narrow (action)
  `(let* ((buf (current-buffer))
          (parent-name (buffer-name buf))
          (p (point)))
     (if narrow-buffer-parent
         ,action
       (let ((new-name (make-indirect-narrow-buffer-name parent-name)))
         (switch-to-buffer
          (or (get-buffer new-name) (make-indirect-buffer buf new-name t)))
         (set (make-local-variable 'narrow-buffer-parent) parent-name)
         (goto-char p)
         ,action
         (setq mark-active nil)))))

(defun narrow-to-region-indirect ()
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (do-indirect-narrow 
     (narrow-to-region start end))))

(defun narrow-to-defun-indirect ()
  (interactive)
  (do-indirect-narrow (narrow-to-defun)))

(defun widen-indirect ()
  (interactive)
  (let* ((buf (current-buffer))
         (buffername (buffer-name buf)))
    (if narrow-buffer-parent      
        (progn (switch-to-buffer narrow-buffer-parent)
               (kill-buffer buf))
      (widen))))

(global-set-key (kbd "C-x n d") 'narrow-to-defun-indirect)
(global-set-key (kbd "C-x n n") 'narrow-to-region-indirect)
(global-set-key (kbd "C-x n w") 'widen-indirect)  
