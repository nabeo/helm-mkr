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
  '((t
      :inherit font-lock-builtin-face
      :foreground "blue"))
  "Face used for working host in `helm-mkr'."
  :group 'helm-mkr-faces)

(defface helm-mkr-host-status-standby
  '((t
      :inhertit font-lock-builtin-face
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

(defface helm-mkr-alert-status-critical
  '((t :inhertit font-lock-builtin-face
      :foreground "red"))
  "Face used for critical alert in `helm-mkr'."
  :group 'helm-mkr-faces)

(defface helm-mkr-alert-status-warning
  '((t :inhertit font-lock-builtin-face
      :foreground "orange"))
  "Face used for warning alert in `helm-mkr'."
  :group 'helm-mkr-faces)

(defface helm-mkr-alert-status-unknown
  '((t :inhertit font-lock-builtin-face
      :slant italic
      :foreground "gray"))
  "Face used for unknown alert in `helm-mkr'."
  :group 'helm-mkr-faces)

(defface helm-mkr-alert-status-default
  '((t :inhertit font-lock-builtin-face
      :foreground "gray"))
  "Face used for default alert in `helm-mkr'."
  :group 'helm-mkr-faces)

(defvar mkr-hosts-command
  "mkr hosts"
  "Command to list hosts.")

(defvar mkr-alerts-command
  "mkr alerts"
  "Command to list alerts.")

(defvar mkr-status-command
  "mkr status"
  "Command to get host info.")

(defvar mkr-services-command
  "mkr services"
  "Command to get service list.")

(defvar mkr-orgs
  "default"
  "Your mackerel org name.")

(defun mkr-run-hosts-command ()
  "Execute mkr hosts."
  (let ((mkr-resut-buffer (generate-new-buffer-name "*mkr-hosts*")))
    (with-temp-buffer
      (shell-command mkr-hosts-command (current-buffer) nil)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun mkr-run-alerts-command ()
  "Execute mkr alerts."
  (let ((mkr-result-buffer (generate-new-buffer-name "*mkr-alerts*")))
    (with-temp-buffer
      (shell-command mkr-alerts-command (current-buffer) nil)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun mkr-run-services-command ()
  "Execute mkr services."
  (let ((mkr-result-buffer (generate-new-buffer-name "*mkr-services*")))
    (with-temp-buffer
      (shell-command mkr-services-command (current-buffer) nil)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun mkr-run-status-command (host-id)
  "Execute mkr status HOST-ID."
  (with-temp-buffer
    (shell-command-on-region (point-min) (point-max)
      (concat mkr-status-command " " host-id) t)
    (buffer-string)))

(defun mkr-parse-hosts-list (input)
  "Extract hosts list.
Argument INPUT json input in string from."
  (let* ((json-object-type 'plist)
          (mkr-hosts-json (json-read-from-string input)))
    mkr-hosts-json))

(defun mkr-parse-alerts-list (input)
  "Extract alerts list.
Artgument INPUT json input in straing form."
  (let* ((json-object-type 'plist)
          (mkr-alerts-json (json-read-from-string input)))
    mkr-alerts-json))

(defun mkr-parse-services-list (input)
  "Extract services list.
Artgument INPUT json input in straing form."
  (let* ((json-object-type 'plist)
          (mkr-services-json (json-read-from-string input)))
    mkr-services-json))

(defun mkr-get-host-status-from-id (host-id)
  "Get host status from HOST-ID."
  (let ((host-status (mkr-parse-status
                    (mkr-run-status-command host-id))))
    host-status))

(defun mkr-parse-status (input)
  "Extract host info.
Artgument INPUT json input in straing form."
  (let* ((json-object-type 'plist)
          (mkr-host-info-json (json-read-from-string input)))
    mkr-host-info-json))

(defun mkr-get-host-name-from-host-id (host-id)
  "Get hostname from HOST-ID."
  (let* ((host-status (mkr-get-host-status-from-id host-id))
          (host-name (plist-get host-status :name)))
    host-name))

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
                        ((string= status "working")
                          'helm-mkr-host-status-working)))
              " | " (format "%11s" status)
              " | " create-date)))
    (cons format-string host)))

(defun mkr-format-alerts-helm-row (alert)
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
                         (mkr-get-host-status-from-id host-id))))
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

(defun mkr-format-services-helm-row (service)
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

(defun mkr-browse-alert (alert-json)
  "Browse mackerel.io from ALERT-JSON with `browse-url-browser-function'."
  (browse-url (concat
                "https://mackerel.io/orgs/"
                mkr-orgs
                "/alerts/"
                (plist-get alert-json :id)))
  )

(defun mkr-browse-service (service-json)
  "Browse mackerel.io from SERVICE-JSON with `browse-url-browser-function'."
  (browse-url (concat
                "https://mackerel.io/orgs/"
                mkr-orgs
                "/services/"
                (plist-get service-json :name)))
  )

(defun mkr-get-hosts ()
  "Create host list from mackerel.io."
  (let* ((mkr-command-result (mkr-run-hosts-command))
          (hosts-info-list (mkr-parse-hosts-list mkr-command-result))
          (hosts-info-list (mapcar 'mkr-format-hosts-helm-row hosts-info-list))
          )
    hosts-info-list))

(defun mkr-get-alerts ()
  "Create alert list from mackerel.io."
  (let* (
          (mkr-command-result (mkr-run-alerts-command))
          (alerts-info-list (mkr-parse-alerts-list mkr-command-result))
          (alerts-info-list (mapcar 'mkr-format-alerts-helm-row alerts-info-list)))
    alerts-info-list))

(defun mkr-get-services ()
  "Create service list from mackerel.io."
  (let* (
          (mkr-command-result (mkr-run-services-command))
          (services-info-list (mkr-parse-services-list mkr-command-result))
          (services-info-list (mapcar 'mkr-format-services-helm-row services-info-list)))
    services-info-list))

;;;###autoload
(defun helm-mkr ()
  "Show helm with a table of host information."
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

(defun helm-mkr-alert ()
  "Show helm with a table of alert infomation."
  (interactive)
  (let ((choices (mkr-get-alerts)))
    (helm
      :buffer "*helm-mkr-alert*"
      :sources `(
                  (name . "Alerts")
                  (candidates . ,choices)
                  (candidate-number-limit . 99999)
                  (action . (
                              ("Browse Alert mackerel.io" .
                                (lambda (alert-json)
                                  (mkr-browse-alert alert-josn)))
                              ("Browse Host at mackerel.io" .
                                (lambda (alert-json)
                                  (browse-url (concat
                                                "https://mackerel.io/orgs/"
                                                mkr-orgs
                                                "/hosts/"
                                                (plist-get alert-json :hostId)))))
                              ("Browse monitor at mackerel.io" .
                                (lambda (alert-json)
                                  (browse-url (concat
                                                "https://mackerel.io/orgs/"
                                                mkr-orgs
                                                "/monitors#monitor="
                                                (plist-get alert-json :monitorId)))))
                              ))
                  ))))

(defun helm-mkr-service ()
  "Show helm with a table of alert infomation."
  (interactive)
  (let ((choices (mkr-get-services)))
    (helm
      :buffer "*helm-mkr-alert*"
      :sources `(
                  (name . "Services")
                  (candidates . ,choices)
                  (candidate-number-limit . 99999)
                  (action . (
                              ("Browse Service mackerel.io" .
                                (lambda (service-json)
                                  (mkr-browse-service service-json)))
                              ))
                  ))))

(provide 'helm-mkr)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-mkr.el ends here
