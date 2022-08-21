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

(defun snarf-index-page-8 (front-page)
  (let ((doc (plump:parse (truename front-page))))
    (let ((i (lquery:$1 doc "img[alt=index]")))
      (when i
	(plump:attribute (plump:parent i) "href")))))

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

;madhu 220821 801
(defun process-plump-p-dom-8 (p)
  "return as values KEY and HREF for a given <P>"
  (assert (equal "P" (plump:tag-name p)))
  (let ((entry (lquery:$1 p "a.Index-Entry-Link")))
    (when entry
      (let ((text (plump:text entry))
	    (href (plump:attribute entry "href")))
	(when (and entry href)
	  (values text href))))))

(defun add-entries (table lw-manual-location index-page)
  (let* ((subdir (directory-namestring index-page))
	 (ret 0)
	 (path (truename (concatenate 'string lw-manual-location index-page)))
	 (main (plump:parse path)))
    (assert (eql (elt subdir (1- (length subdir))) #\/))
    (let ((ps (lquery:$ main "p")))
      (loop for p across ps
	    do (or (multiple-value-bind (entry href)
		       (process-plump-p-dom p)
		     (when entry
		       (incf ret)
		       (add-to-table table subdir entry href)))
		   (multiple-value-bind (entry href)
		       (process-plump-p-dom-8 p)
		     (when entry
		       (incf ret)
		       (add-to-table table subdir entry href)))
		   )))
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
  (let ((p2 (or (search  "/manual/online/" lw-manual-location)
		;madhu 220821 - amd64-linux/lwdoc80-x86-linux.tar.gz
		(search  "/manual/html-l/" lw-manual-location))))
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

(defun guess-redirect (front-page lw-manual-location)
  "If front-page ends with .htm and there is a .html file with the
same basename, use that instead of front-page"
  (let ((path (merge-pathnames front-page lw-manual-location)))
    (assert (probe-file path))
    (cond ((and (equal (pathname-type path) "htm")
		(probe-file (make-pathname :type "html" :defaults path)))
	   (namestring (make-pathname :type "html" :defaults front-page)))
	  (t front-page))))

(defun get-index-pages (front-pages lw-manual-location)
  (loop for front-page in front-pages
	for path = (concatenate 'string lw-manual-location front-page)
	for index-page = (or (snarf-index-page path)
			     (snarf-index-page-8 path))
	when index-page
	  collect (concatenate 'string (directory-namestring front-page)
			       index-page)))

(defun build-table (lw-manual-location &optional (table (make-hash-table :test #'equal)))
  (let* ((front-pages (get-front-pages lw-manual-location))
	 ;;madhu 220821 - some htm pages redirect to html, fake it
	 (front-pages-1 (mapcar (lambda (x) (guess-redirect x lw-manual-location))
				front-pages))
	 (index-pages (get-index-pages front-pages-1 lw-manual-location)))
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
