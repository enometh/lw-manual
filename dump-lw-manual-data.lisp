;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Apr 05 20:20:24 2009 +0530 <enometh@meer.net>
;;;   Bugs-To: enometh@meer.net
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2009-2021 Madhu.  All Rights Reserved.
;;;
;;; dump-lw-manual-data.lisp see DUMP-MANUAL-DATA.
;;;
;;; madhu 200622 - plump and lquery. (require 'lquery) before loading
;;; this file.

(defpackage "MAKE-LW-MANUAL"
  (:use "CL"))
(in-package "MAKE-LW-MANUAL")

(defvar $lw-manual-location
  "/usr/local/Lispworks64/lib/7-1-0-0/manual/online/"
  "Canonical directory namestring of the directory where intro.htm is
located within the shipped documentation")

;; unused. kept around only for debugging
(defvar *all-entries-table* (make-hash-table :test #'equal)
  "TABLE = hash-table of KEY => (KEY . HREF-PATHS)")

(defun snarf-index-page (front-page)
  (let ((doc (plump:parse (truename front-page))))
    (let ((l (lquery:$1 doc "link[rel=index]")))
      (when l
	(plump:attribute l "href")))))

(defun add-to-table (table subdir entry href)
  (let ((path (concatenate 'string subdir href)) l)
    (if (setq l (gethash entry table))
	(pushnew path (cdr l) :test #'string=)
	(setf (gethash entry table) (list entry path)))))

(defun process-plump-p-dom (p)
  "return as values KEY and HREF for a given <P>"
  (assert (equal "P" (plump:tag-name p)))
  (let ((entry (lquery:$1 p "code.Code"))
	(href (lquery:$1 p "em.MyCharTag a.Index")))
    (when (and entry href)
      (values (plump:text entry) (plump:attribute href "href")))))

(defun add-entries (table lw-manual-location index-page)
  (let* ((subdir (directory-namestring index-page))
	 (ret 0)
	 (path (truename (concatenate 'string lw-manual-location index-page)))
	 (main (plump:parse path)))
    (assert (eql (elt subdir (1- (length subdir))) #\/))
    (let ((ps (lquery:$ main "p")))
      (loop for p across ps
	    do (multiple-value-bind (entry href)
		   (process-plump-p-dom p)
		 (when entry
		   (incf ret)
		   (add-to-table table subdir entry href)))))
    #+:lispworks-personal-edition	;don't run out of heap
    (hcl:gc-all)
    ret))

(defun add-hyperspec-entries (table lw-manual-location)
  (with-open-file (stream
		   (truename (concatenate 'string lw-manual-location
					  "CLHS" "/Data/Map_Sym.txt")))
    (loop for sym = (or (read-line stream nil nil) (return ret))
	  for loc = (read-line stream)
	  do (assert (zerop (search "../Body/" loc)))
	     (add-to-table table "CLHS"
			   (string-downcase sym)
			   (subseq loc 2))
	  count 1 into ret)))

(defun extract-version (lw-manual-location)
  (let ((p2 (search "/manual/online/" lw-manual-location)))
    (assert p2)
    (let ((p1 (position #\/ lw-manual-location :end p2 :from-end t)))
      (assert p1)
      (subseq lw-manual-location (1+ p1) p2))))

(defun get-front-pages (lw-manual-location)
  (let* ((intro (plump:parse
		 (truename
		  (concatenate 'string lw-manual-location "intro.htm"))))
	 (li (lquery:$ intro  "UL" "LI" "A")))
    (map 'list (lambda (x)
		 (plump:attribute x "href"))
	 li)))

(defun get-index-pages (front-pages lw-manual-location)
  (loop for front-page in front-pages
	for path = (concatenate 'string lw-manual-location front-page)
	for index-page = (snarf-index-page path)
	when index-page
	  collect (concatenate 'string (directory-namestring front-page)
			       index-page)))

(defun build-table (lw-manual-location &optional (table (make-hash-table :test #'equal)))
  (let* ((front-pages (get-front-pages lw-manual-location))
	 (index-pages (get-index-pages front-pages lw-manual-location)))
    (clrhash table)
    (dolist (x index-pages)
      (add-entries table lw-manual-location x))
    ;; CLHS does not add any entries as the index file isn't in the LW
    ;; format
    (add-hyperspec-entries table lw-manual-location)
    table))

(defun dump-manual-data (&key
			 (lw-manual-location $lw-manual-location)
			 (table
			  (build-table lw-manual-location *all-entries-table*))
			 (version (extract-version lw-manual-location))
			 (defaults *default-pathname-defaults*)
			 (filename (format nil "lw-manual-data-~A.el" version))
			 (varname (format nil "+lw-manual-data-~A+" version)))
  (with-open-file (stream (merge-pathnames filename defaults)
			  :direction :output :if-exists :supersede)
    (format stream "(defconst ~(~A~)~%  '(" varname)
    (loop for key being each hash-key of table
	    using (hash-value val)
	  do (assert (equal key (car val)))
	     (format stream "~S~%    " val))
    (format stream "))~%(provide '~a)~%"
	    (file-namestring (make-pathname
			      :name (pathname-name filename)
			      :version nil
			      :type nil
			      :directory '(:relative)
			      :defaults filename)))
    filename))

#||;;; USAGE
(setq $lw-manual-location "/scratch/opt/lw712/lib/7-1-0-0/manual/online/")

;; dump /tmp/lw-manual-data-7-1-1-0.el
(dump-manual-data :defaults "/tmp/")

(inspect *all-entries*)
;;; JUNK AT EOF
(defvar $front-pages (get-front-pages $lw-manual-location))
(defvar $index-pages (get-index-pages $front-pages $lw-manual-location))
(clrhash *all-entries*)
(map nil (lambda (x)
	   (add-entries *all-entries* $lw-manual-location x))
     (cdr $index-pages))
(add-hyperspec-entries $lw-manual-location *all-entries-table*)
||#
