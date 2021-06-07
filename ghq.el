;;; ghq.el --- Ghq interface for emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015 Roman Coedo
;; Copyright (C) 2021 Joseph LaFreniere

;; Author: Roman Coedo <romancoedo@gmail.com>
;; Created 28 November 2015
;; Version: 0.1.3
;; Package-Requires: ((emacs "26.1") (dash "2.18.0") (s "1.7.0"))

;; Keywords: ghq

;;; Commentary:

;; This package provides a set of functions wrapping ghq.

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'dash)
(require 'rx)
(require 's)
(require 'simple)

(defun ghq--find-root ()
  "Find the ghq root directory."
  (car (split-string (shell-command-to-string "ghq root"))))

(defvar ghq--root
  (ghq--find-root))

(defun ghq--find-projects ()
  "Find the list of ghq projects relative to ghq root."
  (split-string (shell-command-to-string "ghq list")))

(defun ghq--find-projects-full-path ()
  "Find the list of ghq projects."
  (split-string (shell-command-to-string "ghq list --full-path")))

;;;###autoload
(defun ghq (repository &optional ssh)
  "Clone REPOSITORY via ghq, optionally over SSH."
  (interactive "MEnter the repository: \nP")
  (let* ((command (-non-nil `("ghq" "get" ,(when ssh "-p") ,repository)))
         (command-string (s-join " " command))
         (buffer (generate-new-buffer (s-wrap command-string "*")))
         path)
    (set-process-sentinel
     (apply #'start-process command-string buffer command)
     (lambda (p e)
       (with-current-buffer buffer
         (goto-char (point-min))
         (re-search-forward
          (rx (seq line-start (one-or-more whitespace)
                   (or (seq "exists "
                            (group-n 1 (one-or-more not-newline)))
                       (seq "clone "
                            (one-or-more (not whitespace))
                            " -> "
                            (group-n 1 (one-or-more not-newline))))
                   line-end)))
         (setq path (match-string 1))
         (message "%s cloned to %s" repository path))))))

;;;###autoload
(defun ghq-ssh ()
  "Clone a repository via ghq over SSH."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'ghq)))

(defvar ghq--helm-action
  '(("Open Dired"              . (lambda (dir) (dired              (concat ghq--root "/" dir))))
    ("Open Dired other window" . (lambda (dir) (dired-other-window (concat ghq--root "/" dir))))
    ("Open Dired other frame"  . (lambda (dir) (dired-other-frame  (concat ghq--root "/" dir))))))

;(defun ghq--build-helm-source ()
  ;"Build a helm source."
  ;(when (fboundp 'helm-build-sync-source)
  ;(helm-build-async-source "Search ghq projects with helm"
    ;:candidates-process (lambda () (start-process "ghq-list-process" nil "ghq" "list" helm-pattern))
    ;:action ghq--helm-action)))

(defun ghq--build-helm-source ()
  "Build a helm source."
  (helm-make-source "Search ghq projects with helm" 'helm-source-sync
    :candidates (ghq--find-projects)
    :action ghq--helm-action))

(defun ghq-list ()
  "Display the ghq project list in a message."
  (interactive)
  (message (shell-command-to-string "ghq list")))

(defun ghq-list-full-path ()
  "Display the ghq project list in a message."
  (interactive)
  (message (shell-command-to-string "ghq list --full-path")))

(defun helm-ghq-list ()
  "Opens a helm buffer with ghq projects as source."
  (interactive)
  (when (and (fboundp 'ghq--build-helm-source) (fboundp 'helm))
    (helm :sources (ghq--build-helm-source) :prompt "Select repository: " :buffer "*ghq-helm*")))

(provide 'ghq)
;;; ghq.el ends here
