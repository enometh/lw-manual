# `M-x lw:manual`

Look up LispWorks<sup>&reg;</sup> Documentation and display it using
Emacs' `browse-url` facility.

###  .emacs
First put the .el files somewhere in your load-path and customize your
browse-url. Also install the Lispworks<sup>&reg;</sup> 7.1
Documentation locally. Then

```
	(require 'lw-manual-data-7-1-1-0)
	(require 'lw-manual)
	(setq lw:manual-base-url
		"file:///usr/local/lib64/LispWorksPersonal/lib/7-1-0-0/manual/online/")
	(lw:set-lw-manual-symbols +lw-manual-data-7-1-0-0+)
```

### files
#### `lw-manual.el`
derived from original 1997 hyperspec.el by way of Franz Inc.'s
fi-manual.el.

Use `M-x lw:manual <TAB>` to lookup the documentation for the
symbol-at-point. Use the `C-u` prefix argument to always prompt for a
symbol.

Use `lw:set-lw-manual-symbols` directly to set or change the
documentation set (after loading a new manual-data table)

<small><i>Note </i>The code was cleaned up and tested on
Emacs 28. Support for earlier Emacs versions may have been
inadvertently dropped and may need to be fixed.</small>

#### `lw-manual-data-7-1-1-0.el`
contains the manual-data table for the LispWorks <sup>&reg;</sup>
Documentation that shipped with version 7.1.2.

The tar file`lwdoc71-x86-linux.tar.gz` was downloaded from
[http://www.lispworks.com/download](http://www.lispworks.com/download).
Note that this unpacks to a sub-directory - 

```
  ./lib/7-1-0-0/manual/online/
```

Point the variable `lw:manual-base-url` to whereever the docs were
unpacked locally.

```emacs-lisp
  (setq lw:manual-base-url
	  "file:///usr/local/lib64/LispWorksPersonal/lib/7-1-0-0/manual/online/")
```

Or to lookup the corresponding version of the documentation over HTTP
directly on www.lispworks.com

```emacs-lisp
  (setq lw:manual-base-url "http://www.lispworks.com/documentation/lw71/")
```

<small><i>Note </i> The table for lw71 contains 5321 entries stored in
an Emacs obarray.</small>

#### `dump-lw-manual.lisp`
contains code to generate the elisp manual-data from the LispWorks
<sup>&reg;</sup> documentation. It uses the external libraries
`lQuery` and `Plump` to process HTML.

It was used to generate `lw-manual-data-7-1-1-0.el` -

```lisp
  (in-package "MAKE-LW-MANUAL")
  (setq $lw-manual-location "/usr/local/LispWorks64/lib/7-1-0-0/manual/online/")
  (dump-manual-data :defaults "/tmp/")
```
to create an elisp file `/tmp/lw-manual-data-7-1-0-0.el`.  Note this file
defines an elisp constant variable `+lw-manual-data-7-1-1-0+` which
contains the manual-data table.

It is likely that this code can be used to generate manual-data tables
for other LispWorks versions (at least starting from 6.1)

  <small><i>Note </i> Earlier versions of `dump-lw-manual` used
  `html-parse` and manipulated the sexp DOM.  lQuery seems to be
  cleaner to express the extraction.  Upstream lQuery may need to be
  fixed to make it work with LispWorks and the parser may run out of
  memory on the LispWorks Personal Edition.</small>

### Update [22082 lwdoc80]
#### emacs config
```emacs-lisp
    (require 'lw-manual)
    (require 'lw-manual-data-8-0-0-0)
    (lw:set-lw-manual-symbols +lw-manual-data-8-0-0-0+)
    nil)
;;  (setq lw:manual-base-url "http://www.lispworks.com/documentation/lw80/")
    (setq lw:manual-base-url "file:///opt/lw801/lib/8-0-0-0/manual/html-l/")
```

#### dump table
```lisp
  (require 'plump)
  (require 'lquery)
  (load "dump-lw-manual-data.lisp")
  (in-package "MAKE-LW-MANUAL")
  (setq $lw-manual-location "/opt/lw801/lib/8-0-0-0/manual/html-l/")
  (dump-manual-data :defaults "/tmp/")
```

### See also
- [LispWorks](http://www.lispworks.com/documentation/index.html)
  Documention sets.

- [lQuery](https://shinmera.github.io/lquery/) Dissect and manipulate
  the DOM with jQuery-like commands.

