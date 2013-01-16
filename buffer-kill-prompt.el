;Normally when killing a buffer, emacs only prompts the 
;user to save if the buffer is associated with a file.
;If you frequently create buffers without immediately
;associating them with files, it's easy to accidentally
;lose the contents of those buffers.
;
;This code warns the user when killing a buffer or exiting
;emacs exit if the action would destroy a modified buffer
;that isn't associate with a file.

(defun modified-user-temp-buffer-p (buf)
  (let ((buffername (buffer-name buf)))
    (and (not (buffer-file-name buf))
         (buffer-modified-p buf)
          ;Emacs creates its own temporary buffers.
          ;These are safe to destroy.
         (not (string-equal "*" (format "%.1s" buffername)))
         (not (string-equal " *" (format "%.2s" buffername))))))

(defun modified-user-temp-buffer-prompt ()
  (interactive)
  (if (modified-user-temp-buffer-p (current-buffer))
      (y-or-n-p (format "Buffer %s is modified; kill without saving? "
                        (buffer-name)))
    t))

(defun check-for-unsaved-user-temp-buffers ()
  (interactive)
  (if (memq t (mapcar 'modified-user-temp-buffer-p
                      (buffer-list)))
      (y-or-n-p (format "Modified user temp buffers exist; exit anyway? "))
    t))

(add-to-list 'kill-emacs-query-functions 'check-for-unsaved-user-temp-buffers)
(add-to-list 'kill-buffer-query-functions 'modified-user-temp-buffer-prompt)
