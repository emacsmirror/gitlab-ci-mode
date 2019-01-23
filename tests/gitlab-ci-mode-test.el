;;; gitlab-ci-mode-test.el --- Tests gitlab-ci-mode  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017 Joe Wreschnig
;;
;; Author: Joe Wreschnig
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.


;;; Commentary:
;;
;; This file contains test cases for ‘gitlab-ci-mode’.  Unless you’re
;; hacking on it you shouldn’t need to edit or run this file.


;;; Code:

(require 'ert)
(require 'gitlab-ci-mode)

(defmacro with-gitlab-ci-test-buffer (contents &rest body)
  "Create a buffer with CONTENTS and evaluate BODY in ‘gitlab-ci-mode’."
  (declare (indent 1))
  `(with-temp-buffer
     (rename-buffer ".gitlab-ci.yml")
     (gitlab-ci-mode)
     (insert ,contents)
     (goto-char 0)
     ,@body))

(ert-deftest gitlab-ci-mode-test-complete-free-keyword ()
  (with-gitlab-ci-test-buffer "imag"
    (re-search-forward "a")
    (completion-at-point)
    (should (equal (buffer-string) "image:"))))

(ert-deftest gitlab-ci-mode-test-complete-terminated-keyword ()
  (with-gitlab-ci-test-buffer
      "test:\n  scri:\n    - echo"
    (re-search-forward "scri")
    (completion-at-point)
    (should (equal (buffer-string) "test:\n  script:\n    - echo"))))

(ert-deftest gitlab-ci-mode-test-complete-variable ()
  (with-gitlab-ci-test-buffer
      "variables:\n  - X: $CI_JOB_N"
    (goto-char (point-max))
    (completion-at-point)
    (should (equal (buffer-string) "variables:\n  - X: $CI_JOB_NAME"))))

(ert-deftest gitlab-ci-mode-test-complete-bounded-variable ()
  (with-gitlab-ci-test-buffer
      "variables:\n  - X: ${CI_JOB_N"
    (goto-char (point-max))
    (completion-at-point)
    (should (equal (buffer-string) "variables:\n  - X: ${CI_JOB_NAME}"))))

(ert-deftest gitlab-ci-mode-test-complete-partial-variable ()
  (with-gitlab-ci-test-buffer
      "variables:\n  - X: ${CI_JOB_N}Y"
    (re-search-forward "CI_JOB")
    (completion-at-point)
    (should (equal (buffer-string) "variables:\n  - X: ${CI_JOB_NAME}Y"))))

(ert-deftest gitlab-ci-mode-test-complete-special-value ()
  (with-gitlab-ci-test-buffer
      "some_job:\n  - when: pi"
    (goto-char (point-max))
    (completion-at-point)
    (should (equal (buffer-string) "some_job:\n  - when: pipelines"))))

;;; gitlab-ci-mode-test.el ends here
