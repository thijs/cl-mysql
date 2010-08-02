;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-mysql
  (:nicknames #:mysql)
  (:use :cl)
  (:import-from :cl-mysql-asd :*mysql-version*)
  (:export #:connect
           #:disconnect
           #:query
           #:get-last-insert-id)
  (:documentation
   "Common Lisp @em{mysql} binary client protocol implementation.

    @begin[About]{section}

    About cl-mysql

    @end{section}

    @begin[Documentation]{section}

    This documentation was generated by @a[http://www.lichteblau.com/atdoc/doc/]{atdoc},
    the documentation generation system written by David Lichteblau.

    @end{section}
"))
