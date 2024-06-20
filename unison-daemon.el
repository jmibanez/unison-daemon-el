;;; unison-daemon.el --- Run Unison in the background   -*- lexical-binding: t; -*-

;; Copyright (C) 2024 JM Ibanez

;; Homepage: https://github.com/jmibanez/unison-daemon-el
;; Author: JM Ibanez <jm@jmibanez.com>
;; Keywords: lisp convenience unison
;; Version: 0.1

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Run Unison as a daemon in the background.

;;; Code:


(require 'cl)
(require 'seq)

(defgroup unison-daemon nil "Unison daemon customization group."
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
  "Common arguments to Unison when running."
  :group 'unison-daemon
  :type '(repeat string))

(defcustom unison-executable "unison"
  "Unison CLI executable; can be a full path."
  :group 'unison-daemon
  :type 'file)

(defcustom unison-daemon-default-profile "Default"
  "Default Unison profile."
  :group 'unison-daemon
  :type 'string)

(defcustom unison-daemon-profile-directory 'detect
  "Directory where your Unison installation reads its profiles.
Default is to autodetect this by based on your operating system."
  :group 'unison-daemon
  :type '(choice (directory :tag "Path to Unison profile directory")
                 (const :tag "Autodetect based on your operating system" detect)))


(defun unison-daemon--buffer-name (unison-profile-name)
  "Get the Unison daemon buffer name for a profile named UNISON-PROFILE-NAME."
  (format "*Unison:%s*" unison-profile-name))

(defun unison-daemon--args (unison-profile-name)
  "Construct args for running given Unison profile named UNISON-PROFILE-NAME."
  (let ((unison-profile-args
         (or (cdr (assoc unison-profile-name
                         unison-daemon-profile-alist))
             '())))
    (append (list unison-profile-name)
            unison-daemon-common-args unison-profile-args)))

(defun unison-daemon--get-proc (unison-profile-name)
  "Get Unison process for UNISON-PROFILE-NAME (or nil if daemon isn't running)."
  (let ((b (get-buffer (unison-daemon--buffer-name unison-profile-name))))
    (and (buffer-live-p b)
         (get-buffer-process b))))

(defun unison-daemon--os-profile-directory ()
  "Get Unison config and profile directory."
  (pcase system-type
    ('darwin  (expand-file-name "~/Library/Application Support/Unison"))
    (_  (expand-file-name ".unison" "~/"))))

(defun unison-daemon--all-profiles ()
  "Run Unison for all profiles."
  (let ((profile-dir (or (and (eq unison-daemon-profile-directory 'detect)
                              (unison-daemon--os-profile-directory))
                         unison-daemon-profile-directory)))
    (mapcar #'file-name-sans-extension
            (directory-files profile-dir nil "\\.prf$"))))


(defvar unison-show-buffer t)

(defun unison-daemon-for-profile (unison-profile-name)
  "Run `unison' as a daemon for UNISON-PROFILE-NAME, with its messages in a buffer.
If SHOW-BUFFER, switch to its process buffer as well.  If the daemon is
already running, switch to its buffer."
  (interactive
   (list (completing-read "Profile: "
                          (unison-daemon--all-profiles))))

  (let* ((daemon-buffer-name (unison-daemon--buffer-name unison-profile-name))
         (daemon-args  (unison-daemon--args unison-profile-name)))
    (if (unison-daemon--get-proc unison-profile-name)
        (set-window-buffer (selected-window)
                           (process-buffer (unison-daemon--get-proc unison-profile-name)))

      (progn
        (apply 'start-process
               daemon-buffer-name
               daemon-buffer-name
               unison-executable
               daemon-args)
        (when unison-show-buffer
          (set-window-buffer (selected-window)
                             (process-buffer (unison-daemon--get-proc unison-profile-name))))))))


(defun unison-daemon (&optional all-profiles)
  "Run Unison in the background.
By default, this runs Unison for your default profile
UNISON-DAEMON-DEFAULT-PROFILE.  However if you want to run all your
profiles in the background, you can run this with a prefix
argument (passing t to ALL-PROFILES)."
  (interactive "P")
  (if all-profiles
      (let ((unison-show-buffer nil))
        (cl-loop for profile in (unison-daemon--all-profiles)
                 collect (unison-daemon-for-profile profile)))
    (unison-daemon-for-profile unison-daemon-default-profile)))

(provide 'unison-daemon)
;;; unison-daemon.el ends here
