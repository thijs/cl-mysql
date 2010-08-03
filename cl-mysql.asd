;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-mysql-asd
  (:use :cl :asdf))


(in-package #:cl-mysql-asd)


(defvar *mysql-version* "0.0.1"
  "A string denoting the current version of cl-mysql.")

(export '*mysql-version*)


(defsystem #:cl-mysql
  :name "CL-MYSQL"
  :version #.*mysql-version*
  :maintainer "M.L. Oppermann <M.L.Oppermann@gmail.com>"
  :author "M.L. Oppermann <M.L.Oppermann@gmail.com>"
  :licence "To be determined"
  :description ""
  :long-description "CL-MYSQL is a binary client to mysql"
  :serial t
  :components ((:file "package")
               (:file "constants")
               (:file "cl-mysql"))
  :depends-on (:usocket
               :babel
               :cl-sha1))
