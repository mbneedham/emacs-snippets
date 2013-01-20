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
    (and (buffer-modified-p buf)
         (not (or (buffer-file-name buf)
                  (eq major-mode 'dired-mode)
                  (string-equal "*" (format "%.1s" buffername))
                  (string-equal " *" (format "%.2s" buffername))
                  (string-equal " info dir" buffername)
                  (buffer-base-buffer))))))

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
      (progn (ibuffer nil "*modified user temp buffers*" nil nil t)
             (ibuffer-filter-by-predicate '(modified-user-temp-buffer-p (current-buffer)))
             (y-or-n-p (format "Modified user temp buffers exist; exit anyway? ")))
    t))

(add-to-list 'kill-emacs-query-functions 'check-for-unsaved-user-temp-buffers)
(add-to-list 'kill-buffer-query-functions 'modified-user-temp-buffer-prompt)
