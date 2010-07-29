;;; Access to MySQL via the MySQL Client/Server protocol.
;;; No third-party libraries are needed (i.e. mysqllib), no ffi calls.
;;; Written in Common Lisp (Lispworks 5.1+).
;;; Based on http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol
;;;
;;; Works with MySQL 4.1 or higher.

;;; Copyright (c) 2009-2010, Art Obrezan
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Source code can not be used in projects under GNU General Public Licenses
;;;    and its derivatives (LGPL, etc.)
;;;
;;; THIS SOFTWARE IS PROVIDED BY ART OBREZAN ''AS IS'' AND ANY
;;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL ART OBREZAN BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Usage:
;;;
;;; mysql:connect &key host port user password database => connection
;;;   host - string
;;;   port - number (optional, default is 3306)
;;;   user - string
;;;   password - string
;;;   database - string (optional)
;;; mysql:disconnect connection
;;; mysql:query connection query-strings => list of results
;;; mysql:get-last-insert-id connection  => number 
;;;
;;; Notes:
;;; On connection sets the utf-8 encoding to comunicate with the server.
;;; The query function returns column fields as text.


(in-package "CL-USER")

(defpackage "MYSQL"
  (:add-use-defaults t)
  (:export connect
           disconnect
           query
           get-last-insert-id))

(in-package "MYSQL")
(require "comm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition mysql-error (error)
  ((number  :initarg :number  :initform nil :reader mysql-error-number)
   (message :initarg :message :initform nil :reader mysql-error-message))
  (:report (lambda (condition stream)
             (format stream "MySQL~:[~;~:*(~A)~]: ~a~%"
                     (mysql-error-number condition)
                     (mysql-error-message condition)))))

(defstruct mysqlcon stream host port connection-id insert-id)

(defconstant +max-packet-size+ (* 1024 1024))

(defconstant +latin1-swedish-ci+ 8)
(defconstant +utf8-general-ci+  33)

(defconstant +com-quit+ 1)
(defconstant +com-query+ 3)

(defconstant +capabilities+
  `((:client-long-password     .      1)   ; new more secure passwords
    (:client-found-rows        .      2)   ; found instead of affected rows
    (:client-long-flag         .      4)   ; get all column flags
    (:client-connect-with-db   .      8)   ; one can specify db on connect
    (:client-no-schema         .     16)   ; don't allow database.table.column
    (:client-compress          .     32)   ; can use compression protocol
    (:client-odbc              .     64)   ; odbc client
    (:client-local-files       .    128)   ; can use LOAD DATA LOCAL
    (:client-ignore-space      .    256)   ; ignore spaces before '('
    (:client-protocol-41       .    512)   ; new 4.1 protocol
    (:client-interactive       .   1024)   ; this is an interactive client
    (:client-ssl               .   2048)   ; switch to SSL after handshake
    (:client-ignore-sigpipe    .   4096)   ; ignore sigpipes
    (:client-transactions      .   8192)   ; client knows about transactions
    (:client-reserved          .  16384)   ; old flag for 4.1 protocol
    (:client-secure-connection .  32768)   ; new 4.1 authentication
    (:client-multi-statements  .  65536)   ; enable/disable multi-stmt support
    (:client-multi-results     . 131072))) ; enable/disable multi-results

(defconstant +client-capabilities+ '(:client-long-password
                                     :client-long-flag
                                     :client-protocol-41
                                     :client-secure-connection
                                     :client-ignore-space
                                     :client-transactions))

;;-

(defun connect (&key host (port 3306) user password (database nil))
  (let* ((stream (open-stream host port))
         (packet (read-packet stream)))
    (if (error-packet-p packet)
        (error 'mysql-error :number  (error-number  packet)
                            :message (error-message packet))
      (let* ((server-info (parse-handshake-packet packet))
             (scramble (getf server-info :scramble)))
        (send-authorization-packet stream user password database scramble)
        (let ((packet (read-packet stream))
              (connection nil))
          (cond ((ok-packet-p packet)
                 (setq connection (initialize-connection stream host port server-info)))
                ((error-packet-p packet)
                 (close-stream stream)
                 (error 'mysql-error :number  (error-number  packet)
                                     :message (error-message packet)))
                (t
                 (close-stream stream)
                 (error 'mysql-error :message "Unknown error during connection.")))
          (query connection "set names 'utf8'")
          connection)))))

(defun open-stream (host port)
  (let ((stream (comm:open-tcp-stream host port
                                      :direction :io
                                      :timeout 3
                                      :element-type '(unsigned-byte 8))))
    (unless stream
      (error 'mysql-error :message (format nil "cannot connect to ~a:~a" host port)))
    stream))

(defun close-stream (stream)
  (when stream (close stream)))

(defun send-authorization-packet (stream user password database scramble)
  (write-packet (prepare-auth-packet user password database scramble)
                stream
                :packet-number 1))

(defun initialize-connection (stream host port server-info)
  (make-mysqlcon :stream stream
                 :host host
                 :port port
                 :insert-id nil
                 :connection-id (getf server-info :thread-id)))

;;-

(defun disconnect (connection)
  (send-quit connection)
  (when (close-stream (mysqlcon-stream connection))
    (setf (mysqlcon-stream connection) nil)))

(defun send-quit (connection)
  (write-packet `#(,+com-quit+) (mysqlcon-stream connection)
                :packet-number 0))

;;-

(defun query (connection &rest args)
  (let ((stream (mysqlcon-stream connection))
        (query-string (append-query-strings args)))
    (unless stream
      (error 'mysql-error :message "No database connection"))
    (send-query-string query-string stream)
    (let ((packet (read-packet stream)))
      (cond ((error-packet-p packet)
             (error 'mysql-error :number  (error-number  packet)
                                 :message (error-message packet)))
            ((ok-packet-p packet)
             (update-connection-data connection packet))
            (t
             (parse-data-packets packet stream))))))

(defun send-query-string (string stream)
  (write-packet (concatenate 'vector `#(,+com-query+) (string-to-utf8 string))
                stream :packet-number 0))

(defun update-connection-data (connection packet)
  (let* ((ok (parse-ok-packet packet))
         (insert-id (getf ok :insert-id)))
    (setf (mysqlcon-insert-id connection) insert-id))
  nil)

(defun append-query-strings (strings)
  "Appends query strings into one string; addes the #\Space character to
   each string before appending strings into one"
  (apply #'string-append
         (mapcar #'(lambda (string) (string-append string " ")) strings)))

;;-

(defun get-last-insert-id (connection)
  (mysqlcon-insert-id connection))

;;-

(defun read-packet-header (stream)
  (let ((length 0)
        (number 0))
    (setq length (+ (read-byte stream)
                    (ash (read-byte stream) 8)
                    (ash (read-byte stream) 16)))
    (setq number (read-byte stream))
    (values length number)))

(defun read-packet (stream)
  (multiple-value-bind (packet-length packet-number)
      (read-packet-header stream) ; TODO: return packet-number as a value?
    (declare (ignore packet-number))
    (let ((buffer (make-array packet-length
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)))
      (read-sequence buffer stream)
      buffer)))  

(defun write-packet-header (length packet-number stream)
  (write-byte (logand #xff length) stream)
  (write-byte (logand #xff (ash length -8)) stream)
  (write-byte (logand #xff (ash length -16)) stream)
  (write-byte (logand #xff packet-number) stream))

(defun write-packet (data stream &key packet-number)
  (write-packet-header (length data) packet-number stream)
  (write-sequence data stream)
  (force-output stream))

;;-

(defun prepare-auth-packet (user password database scramble)
  (let ((buf (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
        (client-flags (capabilities-to-number
                       (if database
                           (cons :client-connect-with-db +client-capabilities+)
                         +client-capabilities+)))
        (max-packet-size +max-packet-size+)
        (charset-number  +utf8-general-ci+)
        (scramble-buf (password-to-token password scramble)))
    (put-int32-to-array  client-flags    buf :position 0)
    (put-int32-to-array  max-packet-size buf :position 4)
    (put-int8-to-array   charset-number  buf :position 8)
    (concatenate 'vector
                 buf
                 (string-to-cstring user)
                 (vector (length scramble-buf)) scramble-buf
                 (when database (string-to-cstring database)))))

(defun %to-list (seq) (coerce seq 'list))
(defun %to-vector (seq) (coerce seq 'vector))

(defun password-to-token (password scramble)
  (let* ((pwd (string-to-latin1 password))
         (stage1-hash (sha1:digest pwd))
         (stage2-hash (sha1:digest stage1-hash)))
    (%to-vector
     (mapcar #'logxor
             (%to-list (sha1:digest (concatenate 'vector scramble stage2-hash)))
             (%to-list stage1-hash)))))

;;-
        
(defun parse-handshake-packet (buffer)
  (let* ((protocol-version (aref buffer 0))
         (pos (position 0 buffer)) ;;; end position of a c-line (zero)
         (server-version (latin1-to-string buffer :start 1 :end pos))
         (thread-id (+ (aref buffer (+ pos 1))
                       (ash (aref buffer (+ pos 2)) 8)
                       (ash (aref buffer (+ pos 3)) 16)
                       (ash (aref buffer (+ pos 4)) 24)))
         (server-capabilities (number-to-capabilities
                               (+ (aref buffer (+ pos 14))
                                  (ash (aref buffer (+ pos 15)) 8))))
         (server-language (aref buffer (+ pos 16)))
         (server-status (+ (aref buffer (+ pos 17))
                           (ash (aref buffer (+ pos 18)) 8)))
         (scramble (make-array 20 :element-type '(unsigned-byte 8))))
    (dotimes (i 8)
      (setf (aref scramble i) (aref buffer (+ pos i 5))))
    (dotimes (i 12)
      (setf (aref scramble (+ i 8)) (aref buffer (+ pos i 32))))
    (list :protocol-version protocol-version
          :server-version server-version
          :thread-id thread-id
          :server-capabilities server-capabilities
          :server-language server-language
          :server-status server-status
          :scramble scramble)))

(defun error-packet-p (buffer)
  (= #xFF (aref buffer 0)))

(defun parse-error-packet (buffer)
  (let ((error (+ (aref buffer 1)
                  (ash (aref buffer 2) 8)))
        (sqlstate nil)
        (message nil))
    (if (char/= #\# (code-char (aref buffer 3)))
        (setf message (latin1-to-string buffer :start 3))
      (progn
        (setf sqlstate (latin1-to-string buffer :start 4 :end 8))
        (setf message (latin1-to-string buffer :start 9))))
    (list :error error :sqlstat sqlstate :message message)))

(defun ok-packet-p (buffer)
  (zerop (aref buffer 0)))

(defun parse-ok-packet (buffer)
  (multiple-value-bind (affected-rows len)
      (decode-length-coded-binary buffer 1)
    (multiple-value-bind (insert-id len2)
        (decode-length-coded-binary buffer (+ 1 len))
      (let ((pos (+ 1 len len2)))
        (let ((server-status (+ (aref buffer pos)
                                (ash (aref buffer (+ pos 1)) 8)))
              (warning-count (+ (aref buffer (+ pos 2))
                                (ash (aref buffer (+ pos 3)) 8)))
              (message (when (< (+ pos 4) (length buffer))
                         (utf8-to-string buffer :start (+ pos 4) :end (length buffer)))))
          (list :affected-rows affected-rows
                :insert-id insert-id
                :server-status server-status
                :warning-count warning-count
                :message message))))))

(defun eof-packet-p (buffer)
  (and (= #xFE (aref buffer 0))
       (= 5    (length buffer))))

(defun parse-eof-packet (buffer)
  (let ((warning-count (+ (aref buffer 1)
                          (ash (aref buffer 2) 8)))
        (status (+ (aref buffer 3)
                          (ash (aref buffer 4) 8))))
    (list :warning-count warning-count :status status)))

(defun parse-data-packets (packet stream)
  (let ((num (decode-length-coded-binary packet 0))) ; number of columns
    (dotimes (i num) (read-packet stream))
    (read-packet stream) ;read eof packet
    (let ((list nil))
      (do ((packet (read-packet stream) (read-packet stream)))
          ((eof-packet-p packet))
        (setq list (cons (parse-raw-packet packet) list)))
      (reverse list))))

(defun parse-raw-packet (buffer)
  (let ((buffer-length (length buffer))
        (pos 0)
        (list nil))
    (loop
     (multiple-value-bind (len start) (decode-length-coded-binary buffer pos)
       (if (= len -1) ;column value = NULL
           (progn
             (setq list (cons "NULL" list))
             (setq pos (+ start pos)))
         (progn
           (setq list (cons (utf8-to-string buffer :start (+ start pos)
                                                   :end (+ start pos len)) list))
           (setq pos (+ start pos len))))
       (when (>= pos buffer-length) (return))))
     (reverse list)))

;;-

(defun format-error-message (packet) 
  (let* ((error-list (parse-error-packet packet))
         (number  (getf error-list :error))
         (message (getf error-list :message)))
    (format nil "MySQL(~a): ~a" number message)))

(defun error-message (packet)
  (getf (parse-error-packet packet) :message))

(defun error-number (packet)
  (getf (parse-error-packet packet) :error))

;;-

(defun capabilities-to-number (capabilities)
  (let ((acc 0))
    (dolist (option capabilities)
      (incf acc (cdr (assoc option +capabilities+))))
    acc))

(defun number-to-capabilities (number)
  (remove nil (mapcar #'(lambda (cons)
                          (if (zerop (logand (cdr cons) number))
                              nil
                            (car cons)))
                      +capabilities+)))

(defun string-to-latin1 (str)
  (external-format:encode-lisp-string str :latin-1))

(defun string-to-cstring (str)
  (concatenate 'vector (string-to-latin1 str) #(0)))

(defun string-to-utf8 (str)
  (external-format:encode-lisp-string str :utf-8))

(defun latin1-to-string (buf &key (start 0) (end (length buf)))
  (external-format:decode-external-string buf :latin-1 :start start :end end)) 

(defun utf8-to-string (buf &key (start 0) (end (length buf)))
  (external-format:decode-external-string buf :utf-8 :start start :end end)) 

(defun put-int8-to-array (int array &key position)
  (setf (aref array position) (logand #xFF int)))

(defun put-int32-to-array (int array &key position)
  (setf (aref array position) (logand #xFF int))
  (setf (aref array (+ position 1)) (logand #xFF (ash int -8)))
  (setf (aref array (+ position 2)) (logand #xFF (ash int -16)))
  (setf (aref array (+ position 3)) (logand #xFF (ash int -24))))

(defun decode-length-coded-binary (buffer pos)
  (let ((val (aref buffer pos)))
    (cond ((< val 251) (values val 1))
          ((= val 251) (values -1 1)) ;column value = NULL (only appropriate in a Row Data Packet)
          ((= val 252) (values (%lcb-get-int16 buffer pos) 3))
          ((= val 253) (values (%lcb-get-int24 buffer pos) 4))
          ((= val 254) (values (%lcb-get-int64 buffer pos) 9)))))

(defun %lcb-get-int16 (buffer pos)
  (+ (aref buffer (+ 1 pos))
     (ash (aref buffer (+ 2 pos)) 8)))

(defun %lcb-get-int24 (buffer pos)
  (+ (aref buffer (+ 1 pos))
     (ash (aref buffer (+ 2 pos)) 8)
     (ash (aref buffer (+ 3 pos)) 16)))

(defun %lcb-get-int64 (buffer pos)
  (+ (aref buffer (+ 1 pos))
     (ash (aref buffer (+ 2 pos)) 8)
     (ash (aref buffer (+ 3 pos)) 16)
     (ash (aref buffer (+ 4 pos)) 24)
     (ash (aref buffer (+ 5 pos)) 32)
     (ash (aref buffer (+ 6 pos)) 40)
     (ash (aref buffer (+ 7 pos)) 48)
     (ash (aref buffer (+ 8 pos)) 56)))
