;;; emacs-totp --- time based one time password implementation for emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Bas Alberts

;; Author: Bas Alberts <bas@mokusatsu.org>

;; Contains work by:
;; Derek Upham <sand@blarg.net> (hmac-sha1.el, unmodified)
;; Simon Josefsson <simon@josefsson.org> (base32.el, modified)

;; Keywords: 2fa

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Example use with authinfo entry of:
;;
;; "machine account@somewhere login user password JBSWY3DPEHPK3PXP"
;;
;; ELISP> (totp-for "account@somewhere")
;; "508128"
;; ELISP>
;;
;; OR
;;
;; M-x totp-for

(require 'bindat)
(require 'unibyte-base32)
(require 'hmac-sha1)
(require 'netrc)
(require 'cl-lib)

(defun totp-pull-authinfo (machine)
  "Pull a base32 encoded secret for MACHINE from authinfo"
  (let* ((netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
         (machine-entry (netrc-machine netrc machine)))
    (when machine-entry (netrc-get machine-entry "password"))))

(defun totp-for (account)
  "Return a totp for ACCOUNT"
  (interactive "sAccount: ")
  (format "%06d" (totp (totp-pull-authinfo account))))

(defun totp-dump-hex (s)
  "Return a hex representation of s"
  (mapconcat #'(lambda (x) (format "%.2x" x)) s ""))

(defun totp (totp-secret)
  "Return a 6 digit totp seeded by a base32 encoded TOTP-SECRET"
  (let* ((totp-secret (unibyte-base32-decode-string (upcase totp-secret)))
         (totp-interval 30)
         ;; remind me to update this in 2038
         (totp-time (bindat-pack
                     '((:high u32)
                       (:low u32))
                     `((:high . 0)
                       ;; calculate C as the number of times TI has elapsed after TO
                       (:low . ,(/ (truncate (time-to-seconds)) totp-interval))))))
    ;; 11:00AM, restate my assumptions
    (cl-assert (and (stringp totp-secret) (not (string-empty-p totp-secret))))
    (cl-assert (and (stringp totp-time) (not (string-empty-p totp-time))))
    ;; 1) HMAC hash H with C as the message and K as the key
    ;; 2) Take the least 4 significant bits of H and use it as an offset O
    ;; 3) Take 4 bytes from H starting at O bytes MSB (network order)
    ;; 4) Discard the most significant bit and store as unsigned 32-bit integer I
    ;; 5) Token is lowest N digits of I in base 10, zero pad up to N if required
    (let* ((totp-hmac (hmac-sha1 totp-secret totp-time))
           (totp-offset (logand (aref totp-hmac (- (length totp-hmac) 1)) #xf))
           (totp-pin (bindat-unpack '((:totp-pin u32)) (substring totp-hmac totp-offset (+ totp-offset 4))))
           (totp-pin (bindat-get-field totp-pin ':totp-pin))
           (totp-pin (logand totp-pin #x7fffffff))
           (totp-pin (% totp-pin 1000000)))
      (message (format "totp-pin: %06d (remaining: %d secs)"
                       totp-pin
                       (- totp-interval (% (truncate (time-to-seconds)) totp-interval))
                       ))
      totp-pin)
    ))

(provide 'emacs-totp)
