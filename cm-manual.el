;;;  -*- lexical-binding: t -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Mar 21 15:05:51 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; Original-Copyright (C) 2006 Heinrich Taube
;;; SPDX-License-Identifier: LLGPL
;;;
;;; cm:lookup - documentation lookup for common music.
;;;
;;; ;madhu 250323 - extracted from "cm.el" under
;;; https://github.com/ormf/cm/tree/master/etc/emacs


(require 'lw-manual)
(require 'cm-manual-data-2.12)

(defvar cm::manual-symbols
  (lw:make-symbol-table +cm-manual-2.12-manual-data+))

(defvar cm:manual-history nil
  "History of symbols looked up in the LW manual.")

;; needs configuration
(defvar *common-music-doc-root*  nil
  "\"http://commonmusic.sf.net/doc/\" or a local url like
\"file:///path-to-cm/doc/\
\"https://github.com/ormf/cm/blob/master/doc/dict/\".")

;;(sly-interactive-eval "(cl:concatenate 'cl:string \"file://\" (cl:namestring (mk::system-relative-pathname :cm \"doc/\")))")

(defun cm:lookup (entry)
  (interactive (list (lw:read-symbol-internal "Lookup CM symbol: "
					      cm::manual-symbols
					      'cm:manual-history)))
  (lw:lookup-internal entry
		      cm::manual-symbols
		      *common-music-doc-root*
		      "The symbol `%s' is not documented in the CM manual set"))

(provide 'cm-manual)
