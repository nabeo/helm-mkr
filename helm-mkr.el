;;; helm-mkr.el --- mkr interface for emacs-helm

;; Copyright (C) 2018 nabeo

;; Author: nabeo
;; URL: https://github.com/nabeo/helm-mkr
;; Version: 0.0.0
;; X-Original-Version: 0.0.0
;; Package-Requires: ((helm "1.5.3")(cl-lib "0.5")(s "1.9.0"))
;; Keywords:

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; mkr interface for emacs-helm

;; A call to `helm-mkr' will show a list of host on your mackerel account
;; 
;; Requires a configured installation of mkr (https://github.com/mackerelio/mkr)


;;; Code:
(require 'json)
(require 'cl-lib)
(require 's)

(defgroup helm-mkr-faces nil
  "Customize the appearance of helm-mkr."
  :prefix "helm-"
  :group 'helm-mkr
  :group 'helm-faces)

(defface helm-mkr-host-status-working
  '((t :inherit font-lock-builtin-face))
  "Face used for working host in `helm-mkr'."
  :group 'helm-mkr-faces)

(defface helm-mkr-host-status-standby
  '((t :inhertit font-lock-builtin-face
      :foreground "green"))
  "Face used for standby host in `helm-mkr'."
  :group 'helm-mkr-faces)

(defface helm-mkr-host-state-maintenance
  '((t :inhertit font-lock-builtin-face
      :foreground "yellow"))
  "Face used for maintenance host in `helm-mkr'."
  :group 'helm-mkr-faces)

(defface helm-mkr-host-state-poweroff
  '((t :inhertit font-lock-comment-face
      :slant italic
      :foreground "gray"))
  "Face used for poweroff host in `helm-mkr'."
  :group 'helm-mkr-faces)

(defvar mkr-hosts-command
  "mkr hosts"
  "Command to list hosts.")

(defvar mkr-orgs
  "default"
  "Your mackerel org name.")

(defun mkr-run-hosts-command ()
  "Execute mkr hosts."
  (let ((mkr-resut-buffer (generate-new-buffer-name "*mkr-hosts*")))
    (with-temp-buffer
      (shell-command mkr-hosts-command (current-buffer) nil)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun mkr-parse-hosts-list (input)
  "Extract hosts list.
Argument INPUT json input in string from."
  (let* ((json-object-type 'plist)
          (mkr-hosts-json (json-read-from-string input)))
    mkr-hosts-json))

(defun mkr-format-hosts-helm-row (host)
  "Constracts a human-readable string of a host.
show: <name>, <status>,  <create date>.
Argument HOST is the mkr json in plist form."
  (let* ((id (plist-get host :id))
          (name (plist-get host :name))
          (status (plist-get host :status))
          (retired-p (plist-get host :isRetired))
          (create-at (plist-get host :createdAt))
          (create-date (car (split-string create-at "T")))
          (format-string
            (concat
              (propertize (format "%-30s" (s-truncate 30 name))
                'face (cond
                        ((string= status "standby")
                          'helm-mkr-host-status-standby)
                        ((string= status "maintenance")
                          'helm-mkr-host-status-maintenance)
                        ((string= status "poweroff")
                          'helm-mkr-host-status-poweroff)
                        (t
                          'helm-mkr-host-status-working)))
              " | " (format "%11s" status)
              " | " create-date)))
    (cons format-string host)))

(defun mkr-sort-helm-rows (a b)
  "Compare results from `mkr-format-hosts-helm-row' A and B."
  (string< (downcase (car a)) (downcase (car b))))

(defun mkr-get-id-from-host (host-json)
  "Extracts ID from HOST-JSON."
  (plist-get host-json :id))

(defun mkr-get-name-from-host (host-json)
  "Extracts name from HOST-JSON."
  (plist-get host-json :name))

(defun mkr-browse-host (host-json)
  "Browse mackerel.io from HOST-JSON with `browse-url-browser-function'."
    (browse-url (concat
                  "https://mackerel.io/orgs/"
                  mkr-orgs
                  "/hosts/"
                  (mkr-get-id-from-host host-json)))
  )

(defun mkr-get-hosts ()
  "Create host list from mackerel.io."
  (let* ((mkr-command-result (mkr-run-hosts-command))
          (hosts-info-list (mkr-parse-hosts-list mkr-command-result))
          (hosts-info-list (mapcar 'mkr-format-hosts-helm-row hosts-info-list))
          )
    hosts-info-list))

;;;###autoload
(defun helm-mkr ()
  "Show helm with a table of aws information."
  (interactive)
  (let ((choices (mkr-get-hosts)))
    (helm
      :buffer "*helm-mkr*"
      :sources `(
                  (name . "Hosts")
                  (candidates . ,choices)
                  (candidate-number-limit . 99999)
                  (action . (
                              ("Copy hostname" .
                                (lambda (host-json)
                                  (x-select-text (mkr-get-name-from-host host-json))))
                              ("Copy HostId" .
                                (lambda (host-json)
                                  (x-select-text (mkr-get-id-from-host host-json))))
                              ("Browse mackerel.io" .
                                (lambda (host-json)
                                  (mkr-browse-host host-json)))
                              )))))
  )

(provide 'helm-mkr)

;;; helm-mkr.el ends here
