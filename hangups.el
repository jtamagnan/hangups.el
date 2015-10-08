;;; hangups.el --- Chat using google voice and google hangouts

;; Copyright (C) 2015 Jules Tamagnan
;; Author: Jules Tamagnan <jtamagnan@gmail.com>
;; Maintainer: Jules Tamagnan <jtamagnan@gmail.com>
;; Created: 8 Oct 2015
;; Version: 0.1
;; Package-Requires: ((adaptive-wrap "0.5"))

;; Keywords: Chat, sms, hangouts, voice
;; Homepage: http://www.github.com/jtamagnan/hangups.el

;; This file is not a part of GNU Emacs.

;; This file is free software (GPLv3 License)

;;; Commentary:

;; This package allows the user (YOU!) to communicate to his/her
;; google contacts through google hangouts and google voice (if he/she
;; has a google voice phone number). It is very much a work in
;; progress but is being constantly updated.

;;; Usage

;; First you must install hangups_cli
;; <http://www.github.com/jtamagnan/hangups_cli>.

;; After installation call hangups_cli once from terminal to make sure
;; that it works and to register your computer with google.

;; Add this file to your load path :(add-to-list 'load-path "~/Path/To/hangups.el/")
;; Require the file                :(require 'hangups)
;; Call hangups!                   : M-x hangups
;; Enjoy!

;;; Code:

;; * Side functions:

(require 'cl)
(require 'adaptive-wrap)


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

(defun jat/split-string-line (str delim)
  "Splits a STR into lines and then into sections broken at DELIM."
  (mapcar (lambda (line)
	    (split-string line delim))
	  (split-string str "\n")))

(defun hangups/buffer-name (name)
  "Create the conversation NAME."
  (concat hangups-conv-buffer-name " - " name))

;;; My code:

;; * External variables and constants

(defconst hangups-list-buffer-name "hangups - all conversations")
(defconst hangups-conv-buffer-name "hangups - conversation")
(defconst hangups-dir "~/.local/share/hangups_cli/")
(defconst hangups-conversation_file (concat hangups-dir "conversation_list.txt"))
(defconst hangups-messages 50)

(defvar hangups-mode-hook nil)
(defvar hangups/convs-unread)

;; * keymaps

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

;; * major mode definitions

(define-derived-mode hangups-list-mode special-mode "hangups-list"
  "Major mode for viewing conversations from hangouts

\\{hangups-list-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  (toggle-truncate-lines 1))

(define-derived-mode hangups-conv-mode special-mode "hangups-conv"
  "Major mode for viewing conversations from hangouts

\\{hangups-conv-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  (adaptive-wrap-prefix-mode)
  (setq adaptive-wrap-extra-indent 42)
  (toggle-word-wrap 1))

;; * buffer handling functions

(defun hangups/clean-string-count-unread (string)
  "Strip STRING of unread messages count,
propertize unread conversation
Count unread messages -> hangups/convs-unread"
  (let* ((string-list (jat/split-string-line string "|"))
	 (numbers (mapcar (lambda (str) (string-to-number (jat/chomp (car str)))) string-list))
	 (strings (mapcar 'cadr string-list)))
    (setq hangups/convs-unread (apply '+ numbers))
    (apply 'concat (loop for num in numbers
				   for str in strings
				   collect (if (> num 0)
					       (propertize (concat str "\n") 'face 'bold-italic)
					     (concat str "\n"))))))

(defun hangups-list-helper (string)
  "View all converations (STRING)."
  (deactivate-mark)
  (save-current-buffer
    (set-buffer
     (get-buffer-create hangups-list-buffer-name))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (hangups/clean-string-count-unread string)))
    (goto-char (point-min))))

(defvar hangups-name)
(defvar hangups-number)

(defun hangups-conv-helper (string name number)
  "Puts STRING in a new buffer.

Title is affected by NAME, NUMBER is saved
SWITCH-BUFFER toggles whether to switch or set the buffer"
  (deactivate-mark)
  (save-current-buffer
    (set-buffer
     (get-buffer-create (hangups/buffer-name name)))
    (message (buffer-name))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert string))
    (setq-local hangups-name name)
    (setq-local hangups-number number)))

;; * open conversation list

;; ** main function

(defun hangups ()
  "View all conversations."
  (interactive "")
  (run-hooks 'hangups-mode-hook)
  (switch-to-buffer (get-buffer-create hangups-list-buffer-name))
  (hangups-list-mode)
  (message "Opening hangups")
  (hangups-list))

;; ** open conversation helper function

(defun hangups-list ()
  "Go to hangups-list."
  (jat/async-shell-command-to-string "hangups_cli" 'hangups-list-helper))

;; * Open conversation

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
  (let ((name (jat/chomp (thing-at-point 'line))))
    (switch-to-buffer (get-buffer-create (hangups/buffer-name name)))
    (hangups-conv-mode)
    (message "Opening conversation")
    (hangups-conversation name hangups-messages)))

;; * refresh functions

(defun hangups-conv-refresh ()
  "Refresh conversation."
  (interactive "")
  (message "Refreshing conversation")
  (hangups-conversation hangups-name hangups-number))

(defun hangups-list-refresh ()
  "Refresh list of conversations."
  (interactive "")
  (message "Refreshing hangups-list")
  (hangups-list))

;; * send message

(defun message-sent (string name number)
  "Show that message was sent.

STRING is the messages in the conversation
NAME is the conversation name
NUMBER is the number of messages to reload"
  (message "Message sent succesfully")
  (hangups-conv-helper string name number))

(defun hangups-send-to-conv ()
"Send a message to current conversation."
(interactive "")
(let ((string (read-from-minibuffer "Message: ")))
  (message "Sending message")
  (jat/async-shell-command-to-string
   (concat "hangups_cli send -c " hangups-name " -m \"" string "\"")
   'message-sent hangups-name hangups-number)))


;; * provide statement
(provide 'hangups)
;;; hangups.el ends here
