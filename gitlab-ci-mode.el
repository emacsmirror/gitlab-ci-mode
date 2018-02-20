;;; gitlab-ci-mode.el --- mode for editing GitLab CI files  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018  Joe Wreschnig
;;
;; Author: Joe Wreschnig
;; URL: https://git.korewanetadesu.com/gitlab-ci-mode.git
;; Version: 0
;; Keywords: convenience
;; Package-Requires: ((emacs "25") (yaml-mode "0.0.12"))
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; ‘gitlab-ci-mode’ is a major mode for editing GitLab CI files.  It
;; provides syntax highlighting and completion for keywords and
;; special variables.
;;
;; For more information about GitLab CI, see URL
;; https://about.gitlab.com/features/gitlab-ci-cd/


;;; Code:

(require 'subr-x)
(require 'yaml-mode)

(defgroup gitlab-ci nil
  "Support for editing GitLab CI configuration files.

For more information about GitLab CI, see URL
https://about.gitlab.com/features/gitlab-ci-cd/."
  :group 'convenience)

(defconst gitlab-ci-keywords
  '("action"
    "after_script"
    "allow_failure"
    "artifacts"
    "before_script"
    "cache"
    "coverage"
    "dependencies"
    "environment"
    "except"
    "expire_in"
    "image"
    "key"
    "name"
    "on_stop"
    "only"
    "pages"
    "paths"
    "policy"
    "retry"
    "script"
    "services"
    "stage"
    "stages"
    "url"
    "tags"
    "untracked"
    "variables"
    "when"
    "GIT_CHECKOUT"
    "GIT_DEPTH"
    "GIT_STRATEGY"
    "GIT_SUBMODULE_STRATEGY")
  "YAML keys with special meaning used in GitLab CI files.")

(defconst gitlab-ci-variables
  '("ARTIFACT_DOWNLOAD_ATTEMPTS"
    "CI"
    "CI_COMMIT_PATH"
    "CI_COMMIT_REF_NAME"
    "CI_COMMIT_REF_SLUG"
    "CI_COMMIT_SHA"
    "CI_COMMIT_TAG"
    "CI_DEBUG_TRACE"
    "CI_DISPOSABLE_ENVIRONMENT"
    "CI_ENVIRONMENT_NAME"
    "CI_ENVIRONMENT_SLUG"
    "CI_ENVIRONMENT_URL"
    "CI_JOB_ID"
    "CI_JOB_MANUAL"
    "CI_JOB_NAME"
    "CI_JOB_STAGE"
    "CI_PIPELINE_ID"
    "CI_PIPELINE_SOURCE"
    "CI_PIPELINE_TRIGGERED"
    "CI_PROJECT_DIR"
    "CI_PROJECT_ID"
    "CI_PROJECT_NAME"
    "CI_PROJECT_NAMESPACE"
    "CI_PROJECT_PATH"
    "CI_PROJECT_PATH_SLUG"
    "CI_PROJECT_URL"
    "CI_PROJECT_VISIBILITY"
    "CI_REGISTRY"
    "CI_REGISTRY_IMAGE"
    "CI_REGISTRY_PASSWORD"
    "CI_REGISTRY_USER"
    "CI_REPOSITORY_URL"
    "CI_RUNNER_DESCRIPTION"
    "CI_RUNNER_ID"
    "CI_RUNNER_TAGS"
    "CI_SERVER"
    "CI_SERVER_NAME"
    "CI_SERVER_REVISION"
    "CI_SERVER_VERSION"
    "CI_SHARED_ENVIRONMENT"
    "GET_SOURCES_ATTEMPTS"
    "GITLAB_CI"
    "GITLAB_USER_EMAIL"
    "GITLAB_USER_ID"
    "GITLAB_USER_LOGIN"
    "GITLAB_USER_NAME"
    "RESTORE_CACHE_ATTEMPTS")
  "Environment variables defined by GitLab CI.")

(defconst gitlab-ci-deprecated
  '("CI_BUILD_ID"
    "CI_BUILD_MANUAL"
    "CI_BUILD_NAME"
    "CI_BUILD_REF_NAME"
    "CI_BUILD_REF_SLUG"
    "CI_BUILD_REPO"
    "CI_BUILD_STAGE"
    "CI_BUILD_TAG"
    "CI_BUILD_TOKEN"
    "CI_BUILD_TRIGGERED")
  "Deprecated environment variables defined by GitLab CI.")

;; TODO: Run through linter. First, this requires writing a linter, or
;; rather an interface to the GitLab “linter”.  Also, there are some
;; security concerns - it should be possible to configure a per-buffer
;; lint URL so you don’t have to send private code to public GitLab.

;; TODO: Parse file to extract stages for highlighting?

(defface gitlab-ci-builtin-variable
  '((t (:inherit font-lock-builtin-face)))
  "Face for built-in GitLab CI variables (e.g. “$CI_COMMIT_TAG”)."
  :group 'gitlab-ci)

(defface gitlab-ci-custom-variable
  '((t (:inherit font-lock-variable-name-face)))
  "Face for custom GitLab CI variables."
  :group 'gitlab-ci)

(defun gitlab-ci--post-completion (_string status)
  "Handle special syntax after keywords/variables.

If STATUS is ‘finished’ then a “}” may be added to the end of a
variable use (if it began with “${”), and a “:” may be added to
the end of a keyword used as a key."
  (when (eq status 'finished)
    (cond
     ((and (looking-back "\\${[A-Za-z0-9_]+" (line-beginning-position))
           (not (eql (char-after) ?})))
      (insert "}"))
     ((and (looking-back "^ *[a-z_]+" (line-beginning-position))
           (not (eql (char-after) ?:)))
      (insert ":")))))

(defun gitlab-ci--attempt-completion (expect match candidates)
  "Attempt to offer completions at the point.

When looking backwards at EXPECT, scan forward from the start of
the EXPECT match, capturing MATCH.  Present any elements of
CANDIDATES with that match string as a prefix.

If successful, returns the bounds and matched candidates."
  (save-mark-and-excursion
   (when (looking-back expect (line-beginning-position))
     (goto-char (match-beginning 0))
     (re-search-forward match)
     (let* ((start (match-beginning 0))
            (end (match-end 0))
            (prefix (match-string-no-properties 0))
            (matching))
       (dolist (var (reverse candidates))
         (when (string-prefix-p prefix var)
           (push var matching)))
       (and matching
            (list start end matching))))))

(defun gitlab-ci--completion-for-keyword ()
  "Attempt to offer completions for a keyword at the point.

If successful, returns the bounds and matching keywords."
  (gitlab-ci--attempt-completion
   "^ *[a-z_]+" "[a-z_]+" gitlab-ci-keywords))

(defun gitlab-ci--completion-for-variable ()
  "Attempt to offer completions for a variable at the point.

If successful, returns the bounds and matching variables."
  (gitlab-ci--attempt-completion
   "\\${?[A-Za-z0-9_]+" "[A-Za-z0-9_]+" gitlab-ci-variables))

(defun gitlab-ci--completion-candidates ()
  "Return candidates for completion at the point."
  (or (gitlab-ci--completion-for-variable)
      (gitlab-ci--completion-for-keyword)))

(defun gitlab-ci-complete-at-point ()
  "`completion-at-point-functions' function for GitLab CI files."
  (when-let (completion (gitlab-ci--completion-candidates))
    (append completion
            '(:exclusive yes
              :company-docsig identity
              :exit-function gitlab-ci--post-completion))))

;;;###autoload
(define-derived-mode gitlab-ci-mode yaml-mode "GitLab CI"
  "Major mode for editing GitLab CI (“.gitlab-ci.yml”) files."
  :group 'gitlab-ci
  (font-lock-add-keywords
   nil
   `((,(format "^ *\\(%s\\) *:" (regexp-opt gitlab-ci-keywords))
      (1 'font-lock-keyword-face))
     (,(format "\\<\\${?\\(%s\\)\\>" (regexp-opt gitlab-ci-deprecated))
      (1 'warning))
     (,(format "\\<\\${?\\(%s\\)\\>[^A-Za-z0-9_]"
               (regexp-opt gitlab-ci-variables))
      (1 'gitlab-ci-builtin-variable))
     ("\\<\\${?\\([A-Za-z0-9_]+\\)\\>"
      (1 'gitlab-ci-custom-variable))))

  (setq-local completion-at-point-functions
              '(gitlab-ci-complete-at-point)))

;;;###autoload
(add-to-list 'auto-mode-alist '(".gitlab-ci.yml\\'" . gitlab-ci-mode))

(provide 'gitlab-ci-mode)
;;; gitlab-ci-mode.el ends here