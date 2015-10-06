============
 Hangups.el
============

Hangups.el is a hangouts interface for emacs.

It uses the `hangups library <https://github.com/tdryer/hangups>`_ and
the `hangups_cli library <https://github.com/jtamagnan/hangups_cli>`_
to send and recieve messages.

Installation
------------

First you must install `hangups_cli
<https://github.com/jtamagnan/hangups_cli>`_ which will soon be
available through the python package installer pip but is currently
only installable through source.

Then you must include the package in your init file, an example of
which is shown below.

::
   (add-to-list 'load-path "~/path/to/hangups.el/")
   (require 'hangups)

The package will be added to melpa when it is in a more polished state (soon)!

Disclaimer
----------

This software is still very much a work in progress; I have plans: to
implement more features, and clean up the code. Please bare with me
and don't forget to report any issues or new requested features!
