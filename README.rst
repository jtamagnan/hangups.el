.. role:: lisp(code)
   :language: lisp

============
 Hangups.el
============

Hangups.el is a hangouts interface for emacs.

It uses the `hangups library <https://github.com/tdryer/hangups>`_ and
the `hangups_cli library <https://github.com/jtamagnan/hangups_cli>`_
to send and recieve hangouts messages and google voice sms messages.

Installation
------------

First you must install `hangups_cli
<https://github.com/jtamagnan/hangups_cli>`_ which will soon be
available through the python package installer pip but is currently
only installable through source.

Then you must include the package in your init file, an example of
which is shown below.

.. code-block:: lisp

   (add-to-list 'load-path "~/path/to/hangups.el/")
   (require 'hangups)

The package will be added to melpa when it is in a more polished state
(soon)!

Info
----

There is a variable called :lisp:`hangups/convs-unread` that is an
integer representing how many conversations have unread messages. It
is updated everytime that :lisp:`hangups` is called or that the list
of conversations is updated :lisp:`hangups-list-refresh`. It is then
possible to refresh the conversation list every minute on a timer and
then display the variable in your modeline, to have a constant view of
how many messages are unread.

Here is a quick example, directly from my init file of what I
mean. This code appends the number of unread messages to the end of
the time string that is displayed in the modeline:

.. code-block:: lisp

   (setq display-time-24hr-format t)

   (setq display-time-default-load-average nil)

   ;; refresh conversation list every minute
   (add-hook 'hangups-mode-hook (lambda () (run-with-timer 120 (* 1 60) 'hangups-list-refresh)))

   ;; set base display string
   (setq display-time-string-forms
      '(" " 24-hours ":" minutes " "))

   ;; add to the display-string the number of unread messages
   (setq display-time-string-forms
     (append
       display-time-string-forms
       '((if (boundp 'hangups/convs-unread)
             (propertize
	       (concat "Texts(" (int-to-string hangups/convs-unread) ")") ;; surround the number by "Texts()"
	       'font-lock-face '(:background "black" :foreground "white")))))) ;; what the font style should be

   ;; display the time
   (display-time-mode)

The biggest issue with this is that it checks for updates every minute
and then the visual is only updated every minute therefore it can take
up to two minutes before you are notified of a new message.

Disclaimer
----------

This software is still very much a work in progress; I have plans: to
implement more features, and clean up the code. Please bare with me
and don't forget to report any issues or new requested features!
