;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; See the LICENSE file for licensing information.

(in-package #:cl-mysql)


(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (and (boundp ',name)
                               (equal (symbol-value ',name) ,value))
                          (symbol-value ',name) ,value)
     ,@(when doc (list doc))))



;; #      Name                Associated client function
;; -      ----                --------------------------

;; 0x00   COM_SLEEP           (none, this is an internal thread state)
;; 0x01   COM_QUIT            mysql_close
;; 0x02   COM_INIT_DB         mysql_select_db
;; 0x03   COM_QUERY           mysql_real_query
;; 0x04   COM_FIELD_LIST      mysql_list_fields
;; 0x05   COM_CREATE_DB       mysql_create_db (deprecated)
;; 0x06   COM_DROP_DB         mysql_drop_db (deprecated)
;; 0x07   COM_REFRESH         mysql_refresh
;; 0x08   COM_SHUTDOWN        mysql_shutdown
;; 0x09   COM_STATISTICS      mysql_stat
;; 0x0a   COM_PROCESS_INFO    mysql_list_processes
;; 0x0b   COM_CONNECT         (none, this is an internal thread state)
;; 0x0c   COM_PROCESS_KILL    mysql_kill
;; 0x0d   COM_DEBUG           mysql_dump_debug_info
;; 0x0e   COM_PING            mysql_ping
;; 0x0f   COM_TIME            (none, this is an internal thread state)
;; 0x10   COM_DELAYED_INSERT  (none, this is an internal thread state)
;; 0x11   COM_CHANGE_USER     mysql_change_user
;; 0x12   COM_BINLOG_DUMP     sent by the slave IO thread to request a binlog
;; 0x13   COM_TABLE_DUMP      LOAD TABLE ... FROM MASTER (deprecated)
;; 0x14   COM_CONNECT_OUT     (none, this is an internal thread state)
;; 0x15   COM_REGISTER_SLAVE  sent by the slave to register with the master (optional)
;; 0x16   COM_STMT_PREPARE    mysql_stmt_prepare
;; 0x17   COM_STMT_EXECUTE    mysql_stmt_execute
;; 0x18   COM_STMT_SEND_LONG_DATA mysql_stmt_send_long_data
;; 0x19   COM_STMT_CLOSE      mysql_stmt_close
;; 0x1a   COM_STMT_RESET      mysql_stmt_reset
;; 0x1b   COM_SET_OPTION      mysql_set_server_option
;; 0x1c   COM_STMT_FETCH      mysql_stmt_fetch



;(define-constant +com-sleep+ #x00)
(define-constant +com-quit+ #x01)
(define-constant +com-init-db+ #x02)
(define-constant +com-query+ #x03)
(define-constant +com-field-list+ #x04)
;(define-constant +com-create-db+ #x05)
;(define-constant +com-drop-db+ #x06)
(define-constant +com-refresh+ #x07)
(define-constant +com-shutdown+ #x08)
(define-constant +com-statistics+ #x09)
(define-constant +com-process-info+ #x0a)
;(define-constant +com-connect+ #x0b)
(define-constant +com-process-kill+ #x0c)
(define-constant +com-debug+ #x0d)
(define-constant +com-ping+ #x0e)
;(define-constant +com-time+ #x0f)
;(define-constant +com-delayed-insert+ #x10)
(define-constant +com-change-user+ #x11)
;(define-constant +com-binlog-dump+ #x12)
;(define-constant +com-table-dump+ #x13)
;(define-constant +com-connect-out+ #x14)
;(define-constant +com-register-slave+ #x15)
(define-constant +com-stmt-prepare+ #x16)
(define-constant +com-stmt-execute+ #x17)
(define-constant +com-stmt-send-long-data+ #x18)
(define-constant +com-stmt-close+ #x19)
(define-constant +com-stmt-reset+ #x1a)
(define-constant +com-set-option+ #x1b)
(define-constant +com-stmt-fetch+ #x1c)


(define-constant +max-packet-size+ (* 1024 1024))

(define-constant +latin1-swedish-ci+ 8)
(define-constant +utf8-general-ci+  33)

(define-constant +capabilities+
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

(define-constant +client-capabilities+ '(:client-long-password
                                         :client-long-flag
                                         :client-protocol-41
                                         :client-secure-connection
                                         :client-ignore-space
                                         :client-transactions))
