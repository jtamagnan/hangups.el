;;; package --- Summary


;;; Commentary:

;;; Code:

;;; Potentially useful code:
(require 'cl)

(defun jat/async-shell-command-to-string (command callback &rest args)
  "Execute shell command COMMAND asynchronously in the background.

Return the temporary output buffer which command is writing to
during execution.

When the command is finished, call CALLBACK with the resulting
output as a string and extra argment ARGS."
  (lexical-let
      ((output-buffer (generate-new-buffer " *temp*"))
       (callback-fun callback)
       (margs args))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (apply callback-fun (cons output-string margs))))
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
    (define-key map (kbd "g") 'hangups-list-refresh)
    map)
  "Keymap for `hangups-list-mode' major mode.")

(defvar hangups-conv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "g") 'hangups-conv-refresh)
    (define-key map (kbd "r") 'hangups-send-to-conv)
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

(defun hangups-list-helper (string)
  "View all converations (STRING)."
  (deactivate-mark)
  (switch-to-buffer
   (get-buffer-create hangups-list-buffer-name))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert string))
  (goto-char (point-min))
  (hangups-list-mode))

(defvar hangups-name)
(defvar hangups-number)

(defun hangups-conv-helper (string name number)
  "Puts STRING in a new buffer.

Title is affected by NAME, NUMBER is saved"
  (deactivate-mark)
  (switch-to-buffer
   (get-buffer-create (concat hangups-conv-buffer-name " - " name)))
  (message (buffer-name))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert string))
  (hangups-conv-mode)
  (setq-local hangups-name name)
  (setq-local hangups-number number))

(defun hangups ()
  "View all conversations."
  (interactive "")
  (jat/async-shell-command-to-string "hangups_cli" 'hangups-list-helper))

(defun hangups-conversation (name number)
  "View *number* messages from *name*  conversation.
NAME: user
NUMBER: number of messages"
  (jat/async-shell-command-to-string
   (concat "hangups_cli get -c " name " -n " (number-to-string number))
   'hangups-conv-helper name number))

(defun hangups-open-conversation ()
  "Open conversation at point."
  (interactive "")
  (hangups-conversation (jat/chomp (thing-at-point 'line)) hangups-messages))

(defun hangups-conv-refresh ()
  "Refresh conversation."
  (interactive "")
  (hangups-conversation hangups-name hangups-number))

(defun hangups-list-refresh ()
  "Refresh list of conversations."
  (interactive "")
  (hangups))

(defun message-sent (string)
  "Show that message was sent.

Success is based off of STRING contents"
  (message "Message sent succesfully"))

(defun hangups-send-to-conv ()
"Send a message to current conversation."
(interactive "")
(let ((string (read-from-minibuffer "Message: ")))
  (jat/async-shell-command-to-string
   (concat "hangups_cli send -c " hangups-name " -m \"" string "\"")
   'message-sent)))

(provide 'hangups)
;;; hangups.el ends here
