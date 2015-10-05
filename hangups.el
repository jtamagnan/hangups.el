;;; package --- Summary


;;; Commentary:

;;; Code:

;;; Potentially useful code:
(require 'cl)

(defun jat/async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the background.

Return the temporary output buffer which command is writing to
during execution.

When the command is finished, call CALLBACK with the resulting
output as a string."
  (lexical-let
      ((output-buffer (generate-new-buffer " *temp*"))
       (callback-fun callback))
    (set-process-sentinel

(start-process "Shell" output-buffer shell-file-name shell-command-switch command)

     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback-fun output-string)))
         (kill-buffer output-buffer))))
output-buffer))

(defun jat/chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                        (: (* (any " \t\n")) eos)))
                                ""
                                str))

;;; My code:

(defconst hangups-list-buffer-name "hangups - all conversations")
(defconst hangups-conv-buffer-name "hangups - conversation")
(defconst hangups-dir "~/.local/share/hangups_cli/")
(defconst hangups-conversation_file (concat hangups-dir "conversation_list.txt"))
(defconst hangups-messages 50)

(defvar hangups-mode-hook nil)


(defvar hangups-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'hangups-open-conversation)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    map)
  "Keymap for `hangups-list-mode' major mode.")

(defvar hangups-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for `hangups-conv-mode' major mode.")

(define-derived-mode hangups-list-mode special-mode "hangups"
  "Major mode for viewing conversations from hangouts

\\{hangups-list-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t))

(define-derived-mode hangups-conv-mode special-mode "hangups"
  "Major mode for viewing conversations from hangouts

\\{hangups-conv-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t))

(defun hangups-conversations-helper (string)
  "View all converations (STRING)."
  (deactivate-mark)
  (switch-to-buffer-other-window
   (get-buffer-create hangups-list-buffer-name))
  (insert string)
  (hangups-list-mode))

(defun hangups-conversation-helper (string)
  "Puts STRING in a new buffer."
  (deactivate-mark)
  (switch-to-buffer-other-window
   (get-buffer-create hangups-conv-buffer-name))
  (insert string)
  (hangups-mode))

(defun hangups ()
  "View all conversations."
  (interactive "")
  (jat/async-shell-command-to-string "hangups_cli" 'hangups-conversations-helper))

(defun hangups-conversation (name number)
  "View *number* messages from *name*  conversation.
NAME: user
NUMBER: number of messages"
  (jat/async-shell-command-to-string (concat "hangups_cli get -c " name " -n " (number-to-string number)) `hangups-conversation-helper))

(defun hangups-open-conversation ()
  "Open conversation at point."
  (interactive "")
  (hangups-conversation (jat/chomp (thing-at-point 'line)) hangups-messages))

(provide 'hangups)
;;; hangups.el ends here
