;;; base32.el --- Base32 encoding functions
;; Copyright (c) 2000, 2006 Simon Josefsson

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: encoding

;; This file is not a part of GNU Emacs, but the same permissions apply.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is based on base64.el which is Copyright (c) by Kyle E. Jones.

;; This is more than trivially different from base64, since LCM(8,5) =
;; 40 so we have to cludge around the elisp integer 28 bit limit.

;; Tested with Emacs 20.5 and XEmacs 21.1.10.

;; Patched to operate in unibyte buffer mode for hmac-sha1 compatibility
;; and re-namespaced to unibyte-base32 bas@mokusatsu.org 01/26/2018

;;; Code:

(eval-when-compile
  (require 'cl))

;; For non-MULE
(eval-and-compile
  (defalias 'unibyte-base32-char-int
    (if (fboundp 'char-int)
	'char-int
      'identity)))

(defvar unibyte-base32-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")

(defvar unibyte-base32-hex-alphabet "0123456789ABCDEFGHIJKLMNOPQRSTUV")

(defvar unibyte-base32-decoder-program nil
  "*Non-nil value should be a string that names a MIME base32 decoder.
The program should expect to read base32 data on its standard
input and write the converted data to its standard output.")

(defvar unibyte-base32-decoder-switches nil
  "*List of command line flags passed to the command named by
unibyte-base32-decoder-program.")

(defvar unibyte-base32-encoder-program nil
  "*Non-nil value should be a string that names a MIME base32 encoder.
The program should expect arbitrary data on its standard
input and write base32 data to its standard output.")

(defvar unibyte-base32-encoder-switches nil
  "*List of command line flags passed to the command named by
unibyte-base32-encoder-program.")

(defconst unibyte-base32-alphabet-decoding-alist
  '((?A . 00) (?B . 01) (?C . 02) (?D . 03) (?E . 04) (?F . 05)
    (?G . 06) (?H . 07) (?I . 08) (?J . 09) (?K . 10) (?L . 11)
    (?M . 12) (?N . 13) (?O . 14) (?P . 15) (?Q . 16) (?R . 17)
    (?S . 18) (?T . 19) (?U . 20) (?V . 21) (?W . 22) (?X . 23)
    (?Y . 24) (?Z . 25) (?2 . 26) (?3 . 27) (?4 . 28) (?5 . 29)
    (?6 . 30) (?7 . 31)))

(defconst unibyte-base32-hex-alphabet-decoding-alist
  '((?0 . 00) (?1 . 01) (?2 . 02) (?3 . 03) (?4 . 04) (?5 . 05)
    (?6 . 06) (?7 . 07) (?8 . 08) (?9 . 09) (?A . 10) (?B . 11)
    (?C . 12) (?D . 13) (?E . 14) (?F . 15) (?G . 16) (?H . 17)
    (?I . 18) (?J . 19) (?K . 20) (?L . 21) (?M . 22) (?N . 23)
    (?O . 24) (?P . 25) (?Q . 26) (?R . 27) (?S . 28) (?T . 29)
    (?U . 30) (?V . 31)))

(defvar unibyte-base32-alphabet-decoding-vector
  (let ((v (make-vector 123 nil))
	(p unibyte-base32-alphabet-decoding-alist))
    (while p
      (aset v (car (car p)) (cdr (car p)))
      (setq p (cdr p)))
    v))

(defvar unibyte-base32-binary-coding-system 'binary)

(defun unibyte-base32-run-command-on-region (start end output-buffer command
					   &rest arg-list)
  (let ((tempfile nil) status errstring default-process-coding-system
	(coding-system-for-write unibyte-base32-binary-coding-system)
	(coding-system-for-read unibyte-base32-binary-coding-system))
    (unwind-protect
	(progn
	  (setq tempfile (make-temp-name "base32"))
	  (setq status
		(apply 'call-process-region
		       start end command nil
		       (list output-buffer tempfile)
		       nil arg-list))
	  (cond ((equal status 0) t)
		((zerop (save-excursion
			  (set-buffer (find-file-noselect tempfile))
			  (buffer-size)))
		 t)
		(t (save-excursion
		     (set-buffer (find-file-noselect tempfile))
		     (setq errstring (buffer-string))
		     (kill-buffer nil)
		     (cons status errstring)))))
      (ignore-errors
	(delete-file tempfile)))))

(if (string-match "XEmacs" emacs-version)
    (defalias 'unibyte-base32-insert-char 'insert-char)
  (defun unibyte-base32-insert-char (char &optional count ignored buffer)
    (if (or (null buffer) (eq buffer (current-buffer)))
	(insert-char char count)
      (with-current-buffer buffer
	(insert-char char count))))
  (setq unibyte-base32-binary-coding-system 'no-conversion))

(defun unibyte-base32-decode-region (start end)
  (interactive "r")
  (message "Decoding base32...")
  (let ((work-buffer nil)
	(done nil)
	(counter 0)
	(hibits 0)
	(mibits 0)
	(lobits 0)
	(lim 0) inputpos
	(non-data-chars (concat "^=" unibyte-base32-alphabet)))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (generate-new-buffer " *unibyte-base32-work*"))
          (with-current-buffer work-buffer (toggle-enable-multibyte-characters))
	  (buffer-disable-undo work-buffer)
	  (if unibyte-base32-decoder-program
	      (let* ((binary-process-output t) ; any text already has CRLFs
		     (status (apply 'unibyte-base32-run-command-on-region
				    start end work-buffer
				    unibyte-base32-decoder-program
				    unibyte-base32-decoder-switches)))
		(if (not (eq status t))
		    (error "%s" (cdr status))))
	    (goto-char start)
	    (skip-chars-forward non-data-chars end)
	    (while (not done)
	      (setq inputpos (point))
	      (when (> (skip-chars-forward unibyte-base32-alphabet end) 0)
		(setq lim (point))
		(while (< inputpos lim)
		  (setq lobits (lsh lobits 5)
			lobits (logior lobits (aref
					       unibyte-base32-alphabet-decoding-vector
					       (unibyte-base32-char-int (char-after
								 inputpos))))
			mibits (logior (lsh mibits 5)
				       (logand (lsh lobits -16) 31))
			hibits (logior (lsh hibits 5)
				       (logand (lsh mibits -16) 31))
			counter (1+ counter)
			inputpos (1+ inputpos))
		  (when (= counter 8)
		    (unibyte-base32-insert-char (logand (lsh hibits -0) 255)
					1 nil work-buffer)
		    (unibyte-base32-insert-char (logand (lsh mibits -8) 255)
					1 nil work-buffer)
		    (unibyte-base32-insert-char (logand (lsh mibits -0) 255)
					1 nil work-buffer)
		    (unibyte-base32-insert-char (logand (lsh lobits -8) 255)
					1 nil work-buffer)
		    (unibyte-base32-insert-char (logand (lsh lobits -0) 255)
					1 nil work-buffer)
		    (setq lobits 0 mibits 0 hibits 0 counter 0))))
	      (cond ((= (point) end)
		     (if (not (zerop counter))
			 (error
			  "at least %d bits missing at end of base32 encoding"
			  (* (- 8 counter) 5)))
		     (setq done t))
		    ((eq (char-after (point)) ?=)
		     (setq done t)
		     (let ((tmp counter))
		       (while (< tmp 8)
			 (setq lobits (lsh lobits 5)
			       mibits (logior (lsh mibits 5)
					      (logand (lsh lobits -16) 31))
			       hibits (logior (lsh hibits 5)
					      (logand (lsh mibits -16) 31))
			       tmp (1+ tmp))))
		     ;; xxx? warn on bad padding instead of being nice?
		     (when (>= counter 1)
		       (unibyte-base32-insert-char (logand (lsh hibits -0) 255)
					   1 nil work-buffer))
		     (when (>= counter 4)
		       (unibyte-base32-insert-char (logand (lsh mibits -8) 255)
					   1 nil work-buffer))
		     (when (>= counter 5)
		       (unibyte-base32-insert-char (logand (lsh mibits -0) 255)
					   1 nil work-buffer))
		     (when (>= counter 7)
		       (unibyte-base32-insert-char (logand (lsh lobits -8) 255)
					   1 nil work-buffer)))
		    (t (skip-chars-forward non-data-chars end)))))
	  (or (markerp end) (setq end (set-marker (make-marker) end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end))
      (and work-buffer (kill-buffer work-buffer))))
  (message "Decoding base32... done"))

(defun unibyte-base32-encode-region (start end &optional no-line-break)
  (interactive "r")
  (message "Encoding base32...")
  (let ((work-buffer nil)
	(counter 0)
	(cols 0)
	(lobits 0)
	(hibits 0)
	(alphabet unibyte-base32-alphabet)
	inputpos)
    (unwind-protect
	(save-excursion
	  (setq work-buffer (generate-new-buffer " *unibyte-base32-work*"))
          (with-current-buffer work-buffer (toggle-enable-multibyte-characters))
	  (buffer-disable-undo work-buffer)
	  (if unibyte-base32-encoder-program
	      (let ((status (apply 'unibyte-base32-run-command-on-region
				   start end work-buffer
				   unibyte-base32-encoder-program
				   unibyte-base32-encoder-switches)))
		(if (not (eq status t))
		    (error "%s" (cdr status))))
	    (setq inputpos start)
	    (while (< inputpos end)
	      (setq lobits (lsh lobits 8))
	      (setq lobits (logior lobits (unibyte-base32-char-int
					   (char-after inputpos))))
	      (setq hibits (logior (lsh hibits 8)
				   (logand (lsh lobits -20) 255)))
	      (setq counter (1+ counter)
		    inputpos (1+ inputpos))
	      (when (= counter 5)
		(unibyte-base32-insert-char
		 (aref alphabet (logand (lsh hibits -15) 31))
		 1 nil work-buffer)
		(unibyte-base32-insert-char
		 (aref alphabet (logand (lsh hibits -10) 31))
		 1 nil work-buffer)
		(unibyte-base32-insert-char
		 (aref alphabet (logand (lsh hibits -5) 31))
		 1 nil work-buffer)
		(unibyte-base32-insert-char
		 (aref alphabet (logand (lsh hibits -0) 31))
		 1 nil work-buffer)
		(unibyte-base32-insert-char
		 (aref alphabet (logand (lsh lobits -15) 31))
		 1 nil work-buffer)
		(unibyte-base32-insert-char
		 (aref alphabet (logand (lsh lobits -10) 31))
		 1 nil work-buffer)
		(unibyte-base32-insert-char
		 (aref alphabet (logand (lsh lobits -5) 31))
		 1 nil work-buffer)
		(unibyte-base32-insert-char
		 (aref alphabet (logand (lsh lobits -0) 31))
		 1 nil work-buffer)
		(setq cols (+ cols 8))
		(when (and (= cols 72) (not no-line-break))
		  (unibyte-base32-insert-char ?\n 1 nil work-buffer)
		  (setq cols 0))
		(setq lobits 0 hibits 0 counter 0)))
	    ;; write out any remaining bits...
	    (let ((tmp counter))
	      (while (< tmp 5)
		(setq lobits (lsh lobits 8))
		(setq hibits (logior (lsh hibits 8)
				     (logand (lsh lobits -20) 255)))
		(setq tmp (1+ tmp))))
	    (when (>= counter 1)
	      (unibyte-base32-insert-char
	       (aref alphabet (logand (lsh hibits -15) 31))
	       1 nil work-buffer)
	      (unibyte-base32-insert-char
	       (aref alphabet (logand (lsh hibits -10) 31))
	       1 nil work-buffer))
	    (when (>= counter 2)
	      (unibyte-base32-insert-char
	       (aref alphabet (logand (lsh hibits -5) 31))
	       1 nil work-buffer)
	      (unibyte-base32-insert-char
	       (aref alphabet (logand (lsh hibits -0) 31))
	       1 nil work-buffer))
	    (when (>= counter 3)
	      (unibyte-base32-insert-char
	       (aref alphabet (logand (lsh lobits -15) 31))
	       1 nil work-buffer))
	    (when (>= counter 4)
	      (unibyte-base32-insert-char
	       (aref alphabet (logand (lsh lobits -10) 31))
	       1 nil work-buffer)
	      (unibyte-base32-insert-char
	       (aref alphabet (logand (lsh lobits -5) 31))
	       1 nil work-buffer))
	    ;; and appropriate padding
	    (cond ((= counter 1)
		   (unibyte-base32-insert-char ?= 6 nil work-buffer))
		  ((= counter 2)
		   (unibyte-base32-insert-char ?= 4 nil work-buffer))
		  ((= counter 3)
		   (unibyte-base32-insert-char ?= 3 nil work-buffer))
		  ((= counter 4)
		   (unibyte-base32-insert-char ?= 1 nil work-buffer)))
	    (if (and (> cols 0)
		     (not no-line-break))
		(unibyte-base32-insert-char ?\n 1 nil work-buffer)))
	  (or (markerp end) (setq end (set-marker (make-marker) end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end))
      (and work-buffer (kill-buffer work-buffer)))
    (message "Encoding base32... done")))

(defun unibyte-base32-encode (string &optional no-line-break)
  (save-excursion
    (set-buffer (get-buffer-create " *unibyte-base32-encode*"))
    (toggle-enable-multibyte-characters)
    (erase-buffer)
    (insert string)
    (unibyte-base32-encode-region (point-min) (point-max) no-line-break)
    (skip-chars-backward " \t\r\n")
    (delete-region (point-max) (point))
    (prog1
	(buffer-string)
      (kill-buffer (current-buffer)))))

(defun unibyte-base32-decode (string)
  (let ((string (string-to-unibyte string)))
    (save-excursion
      (set-buffer (get-buffer-create " *unibyte-base32-decode*"))
      (toggle-enable-multibyte-characters)
      (erase-buffer)
      (insert string)
      (unibyte-base32-decode-region (point-min) (point-max))
      (goto-char (point-max))
      (skip-chars-backward " \t\r\n")
      (delete-region (point-max) (point))
      (prog1
          (buffer-string)
        (kill-buffer (current-buffer))))))

(fset 'unibyte-base32-decode-string 'unibyte-base32-decode)
(fset 'unibyte-base32-encode-string 'unibyte-base32-encode)

(provide 'unibyte-base32)

;;; base32.el ends here
