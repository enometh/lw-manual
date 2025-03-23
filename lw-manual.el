;;; lw-manual.el -- lookup LispWorks Documentation -*- lexical-binding: t; -*-
;;;
;;;   Touched: Sat Apr 18 19:21:57 2009 +0530 <enometh@meer.net>
;;;   Time-stamp: <2022-05-16 22:11:34 IST>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2020 Madhu.  All Rights Reserved.
;;;
;;; This is code for looking up entries in the LispWorks
;;; documentation.  It is based on fi-manual.el which was is based on
;;; hyperspec.el, which was created by Erik Naggum and others.  Kevin
;;; Layer @ Franz, Inc., got this code from Robert P. Goldman.
;;;
;;; Copright 2006 by SIFT, LLC and Robert P. Goldman.  License terms
;;; as for the original hyperspec.el (GNU GPL).
;;; hyperspec.el --- Browse documentation from the Common Lisp HyperSpec
;;;
;; Copyright 1997 Naggum Software
;;
;; Author: Erik Naggum <erik@naggum.no>
;; Keywords: lisp

;; This file is not part of GNU Emacs

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;;
;;; Code:
;;;
(require 'cl)
(require 'browse-url)
(require 'thingatpt)

(defvar lw:manual-base-url
  "http://www.lispworks.com/documentation/lw71/"
  "Location of the `intro.htm' for file:// urls.

If you installed the LispWorks documentation on your local system, set this
variable to something like
\"file:///usr/local/lib64/LispWorksPersonal/lib/7-1-0-0/manual/online/\"."
)

(defvar lw:manual-history nil
  "History of symbols looked up in the LW manual.")

(defvar lw::manual-symbols nil
  "An obarray of the symbols, their value being the relative locations in the
documentation.  Use `set-lw-manual-symbols' to initialize this before
calling `lw:manual'.
"  )

;;; The key -> href mappings are provided in files named
;;; lw-manual-data-X.el which defines a variable +lw-manual-data-X+
;;; where X is typically version number.

(defun lw:choose-lw-manual-data-set ()
  (symbol-value
   (intern-soft
    (completing-read
     "Choose lw-manual-data set: "
     (or (let ((ret nil))
	   (mapatoms (lambda (x)
		       (when (and (symbolp x) (string-match "^\\+lw-manual-data" (symbol-name x)))
			 (push x ret)))
		     obarray)
	   ret)
	 (error "No lw-manual-data set loaded"))
     nil t nil nil nil))))

(defun lw:make-symbol-table (table-data)
  (let ((table (obarray-make (length table-data))))
    ;; madhu 250321 - emacs 30 introduced the opaque obarray type so
    ;; sequence functions don't work on `obarray' anymore
    (mapc (lambda (entry)
	    (let ((symbol (intern (car entry) table)))
	      (cond
	       ((boundp symbol)
		(push (cadr entry) (symbol-value symbol)))
	       (t (set symbol (cdr entry))))))
	  table-data)
    table))

(defun lw:set-lw-manual-symbols (table-data)
  "Initialize lw:manual-symbols from TABLE."
  (interactive (list (lw:choose-lw-manual-data-set)))
  (setq lw::manual-symbols (lw:make-symbol-table table-data)))

(defun lw::symbol-sans-package (symbol-name)
  "Remove the package prefix from SYMBOL-NAME, if present."
  (cond
   ((string-match ":+" symbol-name)
    (substring symbol-name (match-end 0)))
   (t symbol-name)))

(defun lw:read-symbol-internal (prompt symbol-table history-var)
  "Return the symbol at point as a string with PROMPT.

If `current-prefix-arg' is not nil, the user is prompted for the symbol."
  (let ((symbol-at-point (thing-at-point 'symbol)))
    (when symbol-at-point
      (setq symbol-at-point
	    (downcase
	     (lw::symbol-sans-package symbol-at-point))))
    (if (and symbol-at-point
	     (not current-prefix-arg)
	     (intern-soft symbol-at-point symbol-table))
	symbol-at-point
      (completing-read prompt
		       symbol-table
		       #'boundp
		       t
		       symbol-at-point
		       history-var
		       symbol-at-point))))

(defun lw:lookup-internal (symbol-name symbol-table base-url error-template)
  (cl-maplist (lambda (entry)
		(browse-url (concat base-url (car entry)))
		(when (cdr entry)
		  (sleep-for 0.2)))
	      (let ((symbol (intern-soft (downcase symbol-name)
					 symbol-table)))
		(if (and symbol (boundp symbol))
		    (symbol-value symbol)
		  (error (or error-template)
			 "The symbol `%s' is not documented in the LW manual set"
			 symbol-name)))))

(defun lw:manual (symbol-name)
  "View the documentation for SYMBOL-NAME.

SYMBOL-NAME defaults to the symbol at point. If `current-prefix-arg'
is not nil, the user is prompted for the symbol.

If SYMBOL-NAME has more than one definition, all of them are displayed with
your favorite browser in sequence.  The browser should have a \"back\"
function to view the separate definitions, or it may be that the pages
open in different tabs."
  (interactive (list
		(lw:read-symbol-internal "LW documentation lookup: "
					 lw::manual-symbols
					 'lw:manual-history)))
  (lw:lookup-internal symbol-name
		      lw::manual-symbols
		       lw:manual-base-url
		      "The symbol `%s' is not documented in the LW manual set"))

;;(let ((load-path (cons (file-name-directory load-file-name) load-path)))
;;  (require 'lw-manual-data-7-1-0-0))
;;(lw:set-lw-manual-symbols +lw-manual-data-7-1-0-0+)

(provide 'lw-manual)
;;; lw-manual.el ends here


