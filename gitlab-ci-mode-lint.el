;;; gitlab-ci-mode-lint.el --- Linting for ‘gitlab-ci-mode’ -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018  Joe Wreschnig
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.


;;; Commentary:
;;
;; Linting support for ‘gitlab-ci-mode’.


;;; Code:

(require 'json)
(require 'url)

(defgroup gitlab-ci nil
  "Support for editing GitLab CI configuration files.

For more information about GitLab CI, see URL
‘https://about.gitlab.com/features/gitlab-ci-cd/’."
  :group 'convenience)

(defcustom gitlab-ci-url nil
  "URL to use for GitLab CI API files.

If nil, use URL ‘https://gitlab.com’.")

(defcustom gitlab-ci-api-token nil
  "Private token to use for linting GitLab CI files.")

(defun gitlab-ci--lint-to-buffer (status data)
  "Show linting errors in a temporary buffer.

STATUS and DATA are passed from ‘gitlab-ci-request-lint’, which see."
  (if (eq status 'errored)
      (error data)
    (let ((errors (alist-get 'errors data)))
      (if errors
          (with-output-to-temp-buffer "*GitLab CI Lint*"
            (dolist (error errors)
              (princ error) (princ "\n")))
        (message "No errors found")))))

(defvar url-http-end-of-headers)        ; defined in ‘url/url-http.el’
(defun gitlab-ci--lint-results (_ callback buffer)
  "Translate lint API result into data and pass it on.

CALLBACK and BUFFER are passed from ‘gitlab-ci-request-lint’,
which see."
  ;; TODO: Handle HTTP errors.
  (when (buffer-live-p buffer) ; Don’t bother if the source is dead.
    (goto-char url-http-end-of-headers)
    (condition-case err
        (let ((json-array-type 'list)
              (result (json-read)))
          (with-current-buffer buffer
            (funcall callback 'finished result)))
      (error (funcall callback 'errored (error-message-string err))))))

(defun gitlab-ci-request-lint (callback &optional silent)
  "Run the current buffer against the GitLab CI linter.

This function uploads the contents of the current buffer to
‘gitlab-ci-url’, authorizing the request with
‘gitlab-ci-api-token’ (if set).

When the request is complete, CALLBACK receives two arguments.
If the first is ‘finished’, then the second is the decoded JSON
response from the API.  If it is ‘errored’, then the second is an
error message.  SILENT is as to ‘url-retrieve’."
  (let* ((url-request-method "POST")
         (url-request-data
          (let ((h (make-hash-table)))
            (puthash "content" (substring-no-properties (buffer-string)) h)
            (json-encode h)))
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url (concat (or gitlab-ci-url "https://gitlab.com")
                      "/api/v4/ci/lint")))

    (when gitlab-ci-api-token
      (add-to-list 'url-request-extra-headers
                   `("Private-Token" . ,gitlab-ci-api-token)))

    ;; TODO: Handle HTTP 429 responses and delay internally. (However,
    ;; GitLab’s API limit is fairly high relative to human typing speed,
    ;; so the idle timers within e.g. Flycheck may be enough for now.)
    (url-retrieve url #'gitlab-ci--lint-results
                  (list callback (current-buffer))
                  silent)))

(defun gitlab-ci-lint ()
  "Lint the current buffer using the GitLab API.

Running this command will upload your buffer to the site
specified in ‘gitlab-ci-url’, which see.  If your buffer contains
sensitive data, this is not recommended.  (Storing sensitive data
in your CI configuration file is also not recommended.)

If your GitLab API requires a private token, set
‘gitlab-ci-api-token’."
  (interactive)
  (gitlab-ci-request-lint #'gitlab-ci--lint-to-buffer))


(provide 'gitlab-ci-mode-lint)
;;; gitlab-ci-mode-lint.el ends here
