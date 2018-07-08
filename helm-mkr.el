;;; helm-mkr.el --- mkr interface for emacs-helm -*- lexical-binding: t; -*-

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
  '((t
      :inherit font-lock-builtin-face
      :foreground "blue"))
  "Face used for working host in `helm-mkr'.")

(defface helm-mkr-host-status-standby
  '((t
      :inhertit font-lock-builtin-face
      :foreground "green"))
  "Face used for standby host in `helm-mkr'.")

(defface helm-mkr-host-status-maintenance
  '((t :inhertit font-lock-builtin-face
      :foreground "yellow"))
  "Face used for maintenance host in `helm-mkr'.")

(defface helm-mkr-host-status-poweroff
  '((t :inhertit font-lock-comment-face
      :slant italic
      :foreground "gray"))
  "Face used for poweroff host in `helm-mkr'.")

(defface helm-mkr-alert-status-critical
  '((t :inhertit font-lock-builtin-face
      :foreground "red"))
  "Face used for critical alert in `helm-mkr'.")

(defface helm-mkr-alert-status-warning
  '((t :inhertit font-lock-builtin-face
      :foreground "orange"))
  "Face used for warning alert in `helm-mkr'.")

(defface helm-mkr-alert-status-unknown
  '((t :inhertit font-lock-builtin-face
      :slant italic
      :foreground "gray"))
  "Face used for unknown alert in `helm-mkr'.")

(defface helm-mkr-alert-status-default
  '((t :inhertit font-lock-builtin-face
      :foreground "gray"))
  "Face used for default alert in `helm-mkr'.")

(defvar helm-mkr-hosts-command
  "mkr hosts"
  "Command to list hosts.")

(defvar helm-mkr-alerts-command
  "mkr alerts"
  "Command to list alerts.")

(defvar helm-mkr-status-command
  "mkr status"
  "Command to get host info.")

(defvar helm-mkr-services-command
  "mkr services"
  "Command to get service list.")

(defvar helm-mkr-orgs
  "default"
  "Your mackerel org name.")

(defun helm-mkr-run-hosts-command ()
  "Execute mkr hosts."
  (let ((helm-mkr-resut-buffer (generate-new-buffer-name "*helm-mkr-hosts*")))
    (with-temp-buffer
      (shell-command helm-mkr-hosts-command (current-buffer) nil)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun helm-mkr-run-alerts-command ()
  "Execute mkr alerts."
  (let ((helm-mkr-result-buffer (generate-new-buffer-name "*helm-mkr-alerts*")))
    (with-temp-buffer
      (shell-command helm-mkr-alerts-command (current-buffer) nil)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun helm-mkr-run-services-command ()
  "Execute mkr services."
  (let ((helm-mkr-result-buffer (generate-new-buffer-name "*helm-mkr-services*")))
    (with-temp-buffer
      (shell-command helm-mkr-services-command (current-buffer) nil)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun helm-mkr-run-status-command (host-id)
  "Execute mkr status HOST-ID."
  (with-temp-buffer
    (shell-command-on-region (point-min) (point-max)
      (concat helm-mkr-status-command " " host-id) t)
    (buffer-string)))

(defun helm-mkr-parse-hosts-list (input)
  "Extract hosts list.
Argument INPUT json input in string from."
  (let* ((json-object-type 'plist)
          (helm-mkr-hosts-json (json-read-from-string input)))
    helm-mkr-hosts-json))

(defun helm-mkr-parse-alerts-list (input)
  "Extract alerts list.
Artgument INPUT json input in straing form."
  (let* ((json-object-type 'plist)
          (helm-mkr-alerts-json (json-read-from-string input)))
    helm-mkr-alerts-json))

(defun helm-mkr-parse-services-list (input)
  "Extract services list.
Artgument INPUT json input in straing form."
  (let* ((json-object-type 'plist)
          (helm-mkr-services-json (json-read-from-string input)))
    helm-mkr-services-json))

(defun helm-mkr-get-host-status-from-id (host-id)
  "Get host status from HOST-ID."
  (let ((host-status (helm-mkr-parse-status
                    (helm-mkr-run-status-command host-id))))
    host-status))

(defun helm-mkr-parse-status (input)
  "Extract host info.
Artgument INPUT json input in straing form."
  (let* ((json-object-type 'plist)
          (helm-mkr-host-info-json (json-read-from-string input)))
    helm-mkr-host-info-json))

(defun helm-mkr-get-host-name-from-host-id (host-id)
  "Get hostname from HOST-ID."
  (let* ((host-status (helm-mkr-get-host-status-from-id host-id))
          (host-name (plist-get host-status :name)))
    host-name))

(defun helm-mkr-format-hosts-helm-row (host)
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
              name
              " "
              (propertize (format "%s" status)
                'face (cond
                        ((string= status "standby")
                          'helm-mkr-host-status-standby)
                        ((string= status "maintenance")
                          'helm-mkr-host-status-maintenance)
                        ((string= status "poweroff")
                          'helm-mkr-host-status-poweroff)
                        ((string= status "working")
                          'helm-mkr-host-status-working)))
              " " create-date)))
    (cons format-string host)))

(defun helm-mkr-format-alerts-helm-row (alert)
  "Constracts a human-readable string of a host.
show: <create date time>, <alert status>, <hostname>, <message>.
Argument ALERT is the mkr json in plist form."
  (let* (
          (id (plist-get alert :id))
          (status (plist-get alert :status))
          (host-id (plist-get alert :hostId))
          (alert-type (plist-get alert :type))
          (host-info (cond
                       ((plist-member alert :hostId)
                         (helm-mkr-get-host-status-from-id host-id))))
          (host-name (cond
                       ((plist-member alert :hostId)
                         (plist-get host-info :name))))
          (host-status (cond
                         ((plist-member alert :hostId)
                           (plist-get host-info :status))))
          (message (cond
                     ((string= alert-type "check")
                         (car (s-lines (plist-get alert :message))))
                     ((string= alert-type "host")
                       (plist-get alert :value))
                     (t
                       ""
                     )))
          (opened-date-time (format-time-string "%Y-%m-%d %H:%M:%S"
                              (seconds-to-time
                                (plist-get alert :openedAt))))
          (format-string
            (concat
              opened-date-time " "
              (propertize (format "%s" status)
                'face (cond
                        ((string= status "WARNING")
                          'helm-mkr-alert-status-warning)
                        ((string= status "CRITICAL")
                          'helm-mkr-alert-status-critical)
                        ((string= status "UNKNOWN")
                          'helm-mkr-alert-status-unknown)
                        (t
                          'helm-mkr-alert-status-default)))
              " " host-name
              " "
              (propertize (format "%s" host-status)
                'face (cond
                        ((string= host-status "standby")
                          'helm-mkr-host-status-standby)
                        ((string= host-status "maintenance")
                          'helm-mkr-host-status-maintenance)
                        ((string= host-status "poweroff")
                          'helm-mkr-host-status-poweroff)
                        ((string= host-status "working")
                          'helm-mkr-host-status-working)))
              " " (format "%s" message))))
    (cons format-string alert)
    ))

(defun helm-mkr-format-services-helm-row (service)
  "Constracts a human-readable string of a host.
show: <name>.
Argument SERVICE is the mkr json in plist form."
  (let* (
          (name (plist-get service :name))
          (memo (cond
                  ((plist-member service :memo)
                    (plist-get service :memo))))
          (format-string
            (concat
              name " " memo)))
    (cons format-string service)))

(defun helm-mkr-sort-helm-rows (a b)
  "Compare results from `helm-mkr-format-hosts-helm-row' A and B."
  (string< (downcase (car a)) (downcase (car b))))

(defun helm-mkr-get-id-from-host (host-json)
  "Extracts ID from HOST-JSON."
  (plist-get host-json :id))

(defun helm-mkr-get-name-from-host (host-json)
  "Extracts name from HOST-JSON."
  (plist-get host-json :name))

(defun helm-mkr-browse-host (host-json)
  "Browse mackerel.io from HOST-JSON with `browse-url-browser-function'."
    (browse-url (concat
                  "https://mackerel.io/orgs/"
                  helm-mkr-orgs
                  "/hosts/"
                  (helm-mkr-get-id-from-host host-json)))
  )

(defun helm-mkr-browse-alert (alert-json)
  "Browse mackerel.io from ALERT-JSON with `browse-url-browser-function'."
  (browse-url (concat
                "https://mackerel.io/orgs/"
                helm-mkr-orgs
                "/alerts/"
                (plist-get alert-json :id)))
  )

(defun helm-mkr-browse-service (service-json)
  "Browse mackerel.io from SERVICE-JSON with `browse-url-browser-function'."
  (browse-url (concat
                "https://mackerel.io/orgs/"
                helm-mkr-orgs
                "/services/"
                (plist-get service-json :name)))
  )

(defun helm-mkr-get-hosts ()
  "Create host list from mackerel.io."
  (let* ((helm-mkr-command-result (helm-mkr-run-hosts-command))
          (hosts-info-list (helm-mkr-parse-hosts-list helm-mkr-command-result))
          (hosts-info-list (mapcar 'helm-mkr-format-hosts-helm-row hosts-info-list))
          )
    hosts-info-list))

(defun helm-mkr-get-alerts ()
  "Create alert list from mackerel.io."
  (let* (
          (helm-mkr-command-result (helm-mkr-run-alerts-command))
          (alerts-info-list (helm-mkr-parse-alerts-list helm-mkr-command-result))
          (alerts-info-list (mapcar 'helm-mkr-format-alerts-helm-row alerts-info-list)))
    alerts-info-list))

(defun helm-mkr-get-services ()
  "Create service list from mackerel.io."
  (let* (
          (helm-mkr-command-result (helm-mkr-run-services-command))
          (services-info-list (helm-mkr-parse-services-list helm-mkr-command-result))
          (services-info-list (mapcar 'helm-mkr-format-services-helm-row services-info-list)))
    services-info-list))

;;;###autoload
(defun helm-mkr ()
  "Show helm with a table of host information."
  (interactive)
  (let ((choices (helm-mkr-get-hosts)))
    (helm
      :buffer "*helm-mkr*"
      :sources `(
                  (name . "Hosts")
                  (candidates . ,choices)
                  (candidate-number-limit . 99999)
                  (action . (
                              ("Copy hostname" .
                                (lambda (host-json)
                                  (x-select-text (helm-mkr-get-name-from-host host-json))))
                              ("Copy HostId" .
                                (lambda (host-json)
                                  (x-select-text (helm-mkr-get-id-from-host host-json))))
                              ("Browse mackerel.io" .
                                (lambda (host-json)
                                  (helm-mkr-browse-host host-json)))
                              )))))
  )

(defun helm-mkr-alert ()
  "Show helm with a table of alert infomation."
  (interactive)
  (let ((choices (helm-mkr-get-alerts)))
    (helm
      :buffer "*helm-mkr-alert*"
      :sources `(
                  (name . "Alerts")
                  (candidates . ,choices)
                  (candidate-number-limit . 99999)
                  (action . (
                              ("Browse Alert mackerel.io" .
                                (lambda (alert-json)
                                  (helm-mkr-browse-alert alert-json)))
                              ("Browse Host at mackerel.io" .
                                (lambda (alert-json)
                                  (browse-url (concat
                                                "https://mackerel.io/orgs/"
                                                helm-mkr-orgs
                                                "/hosts/"
                                                (plist-get alert-json :hostId)))))
                              ("Browse monitor at mackerel.io" .
                                (lambda (alert-json)
                                  (browse-url (concat
                                                "https://mackerel.io/orgs/"
                                                helm-mkr-orgs
                                                "/monitors#monitor="
                                                (plist-get alert-json :monitorId)))))
                              ))
                  ))))

(defun helm-mkr-service ()
  "Show helm with a table of alert infomation."
  (interactive)
  (let ((choices (helm-mkr-get-services)))
    (helm
      :buffer "*helm-mkr-service*"
      :sources `(
                  (name . "Services")
                  (candidates . ,choices)
                  (candidate-number-limit . 99999)
                  (action . (
                              ("Browse Service mackerel.io" .
                                (lambda (service-json)
                                  (helm-mkr-browse-service service-json)))
                              ))
                  ))))

(provide 'helm-mkr)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-mkr.el ends here
