;;; unison-daemon.el --- Run Unison in the background.   -*- lexical-binding: t; -*-

;; Copyright (C) 2024 JM Ibanez

;; Author: JM Ibanez <jm@jmibanez.com>
;; Keywords: lisp convenience unison
;; Version: 0.1

;;; Commentary:

;; Run Unison as a daemon in the background.

;;; Code:


(require 'cl)
(require 'seq)

(defgroup unison-daemon nil "Unison daemon customization group"
  :group 'convenience)

(defcustom unison-daemon-profile-alist nil
  "Additional arguments when running specific profiles."
  :group 'unison-daemon
  :type '(alist :key-type (string :tag "Name of the profile.")
                :value-type (repeat string)))

(defcustom unison-daemon-common-args
  '("-ui" "text"
    "-dumbtty"
    "-terse")
  "Common arguments to Unison when running"
  :group 'unison-daemon
  :type '(repeat string))

(defcustom unison-executable "unison"
  "Unison CLI executable. Can be a full path."
  :group 'unison-daemon
  :type 'file)

(defcustom unison-daemon-default-profile "Default"
  "Default Unison profile."
  :group 'unison-daemon
  :type 'string)

(defcustom unison-daemon-profile-directory 'detect
  "Directory where your Unison installation reads its profiles. Default is
to autodetect this by based on your operating system."
  :group 'unison-daemon
  :type '(choice (directory :tag "Path to Unison profile directory")
                 (const :tag "Autodetect based on your operating system" detect)))


(defun unison-daemon--buffer-name (unison-profile-name)
  "Get the Unison daemon buffer name for a given profile name."
  (format "*Unison:%s*" unison-profile-name))

(defun unison-daemon--args (unison-profile-name)
  "Construct args for running given Unison profile."
  (let ((unison-profile-args
         (or (cdr (assoc unison-profile-name
                         unison-daemon-profile-alist))
             '())))
    (append (list unison-profile-name)
            unison-daemon-common-args unison-profile-args)))

(defun unison-daemon--get-proc (unison-profile-name)
  "Get the running Unison process (or nil if daemon isn't running)."
  (let ((b (get-buffer (unison-daemon--buffer-name unison-profile-name))))
    (and (buffer-live-p b)
         (get-buffer-process b))))

(defun unison-daemon--os-profile-directory ()
  (pcase system-type
    ('darwin  (expand-file-name "~/Library/Application Support/Unison"))
    (_  (expand-file-name ".unison" "~/"))))

(defun unison-daemon--all-profiles ()
  (let ((profile-dir (or (and (eq unison-daemon-profile-directory 'detect)
                              (unison-daemon--os-profile-directory))
                         unison-daemon-profile-directory)))
    (mapcar #'file-name-sans-extension
            (directory-files profile-dir nil "\\.prf$"))))


(defvar unison-show-buffer t)

(defun unison-daemon-for-profile (unison-profile-name)
  "Run `unison' as a daemon, outputing its sync messages in a buffer in the
background. If SHOW-BUFFER, switch to its process buffer as well. If the
daemon is already running, switch to its buffer."
  (interactive
   (list (completing-read "Profile: "
                          (unison-daemon--all-profiles))))

  (let* ((daemon-buffer-name (unison-daemon--buffer-name unison-profile-name))
         (daemon-args  (unison-daemon--args unison-profile-name)))
    (if (unison-daemon--get-proc unison-profile-name)
        (set-window-buffer (selected-window)
                           (process-buffer (unison-daemon--get-proc unison-profile-name)))

      (progn
        (let ((dummy (when (get-buffer daemon-buffer-name))))
          (apply 'start-process
                 daemon-buffer-name
                 daemon-buffer-name
                 unison-executable
                 daemon-args))
        (when unison-show-buffer
          (set-window-buffer (selected-window)
                             (process-buffer (unison-daemon--get-proc unison-profile-name))))))))


(defun unison-daemon (&optional all-profiles)
  (interactive "P")
  (if all-profiles
      (let ((unison-show-buffer nil))
        (cl-loop for profile in (unison-daemon--all-profiles)
                 collect (unison-daemon-for-profile profile)))
    (unison-daemon-for-profile unison-daemon-default-profile)))

(provide 'unison-daemon)
;;; unison-daemon.el ends here
