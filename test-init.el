;;; test-init.el --- Test initialization -*- lexical-binding: t; -*-
;;
;; This is free and unencumbered software released into the public
;; domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.


;;; Commentary:
;;
;; This file contains generic initialization for a Cask project’s test
;; suite.  Unless you’re hacking on it you shouldn’t need to edit or run
;; this file.


;;; Code:

(require 'cask "~/.cask/cask.el")
(cask-initialize (file-name-directory load-file-name))

(require 'package-lint)

;;; test-init.el ends here
