;;; package --- Summary


;;; Commentary:

;;; Code:
(defconst hangups-buffer-name "hangups - all conversations")
(defconst hangups-dir "~/.local/share/hangups_cli/")
(defconst hangups-conversation_file (concat hangups-dir "conversation_list.txt"))

(defvar hangups-mode-hook nil)


(defvar hangups-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for hangups major mode.")

(define-derived-mode hangups-mode special-mode "hangups"
  "Major mode for viewing conversations from hangouts"
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t))

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath.
FILEPATH: the path to the file to read"
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun hangups-conversations ()
  "View all converations."
  (interactive "*")
  (deactivate-mark)
  (async-shell-command "hangups_cli")
  (let ((conversation_list (read-lines hangups-conversation_file)))
    (insert (mapconcat (lambda (line) (cadr (split-string line ":"))) conversation_list "\n"))))

(defun hangups-conversation (name number)
  "View *number* messages from *name*  conversation.
NAME: user
NUMBER: number of messages"
  (interactive "*")
  (deactivate-mark)

  (switch-to-buffer-other-window
   (get-buffer-create hangups-buffer-name))
  (shell-command (concat "hangups_cli get -c " name " -n " (number-to-string number)) (current-buffer) (get-buffer-create "hangups-error"))
  (hangups-mode))

(provide 'hangups)
;;; hangups.el ends here
