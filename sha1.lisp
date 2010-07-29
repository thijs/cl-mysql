;;; Secure hash algorithms - SHA-1
;;; http://csrc.nist.gov/publications/fips/fips180-2/fips180-2.pdf
;;;
;;; Note: implemented for byte messages

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


(in-package "CL-USER")

(defpackage "SHA1"
  (:add-use-defaults t)
  (:export digest))

(in-package "SHA1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro to-32-bits-word (&rest body)
  `(logand #xFFFFFFFF ,@body))

(defun rotl (n shift)
  (logior (to-32-bits-word (ash n shift))
	  (ash n (- shift 32))))

(defun pad-the-message (message)
  (flet ((padding-size (n)
           (let ((x (mod (- 56 (rem n 64)) 64)))
             (if (zerop x) 64 x))))
    (let* ((message-len (length message))
           (message-len-in-bits (* message-len 8))
           (buffer-len (+ message-len 8 (padding-size message-len)))
           (buffer (make-array buffer-len
                               :element-type '(unsigned-byte 8)
                               :initial-element 0)))
      (dotimes (i message-len)
        (setf (aref buffer i) (aref message i)))
      (setf (aref buffer message-len) #b10000000)
      (dotimes (i 8)
        (setf (aref buffer (- buffer-len (1+ i)))
              (logand #xFF (ash message-len-in-bits (* i -8)))))
      buffer)))

(defun prepare-message-block (n data)
  (let ((message-block (make-array 80))
        (offset (* n 64)))
    (dotimes (i 16)
      (setf (aref message-block i) (+ (ash (aref data (+ offset   (* i 4))) 24)
                              (ash (aref data (+ offset 1 (* i 4))) 16)
                              (ash (aref data (+ offset 2 (* i 4))) 8)
                                   (aref data (+ offset 3 (* i 4))))))
    (loop :for i :from 16 :to 79 :do
          (setf (aref message-block i) 
                (to-32-bits-word
                        (rotl (logxor (aref message-block (- i 3))
                                      (aref message-block (- i 8))
                                      (aref message-block (- i 14))
                                      (aref message-block (- i 16))) 1))))
    message-block))

(defun sha1-f (n x y z)
  (cond ((<= 0 n 19)
         (to-32-bits-word (logior (logand x y)
                                  (logand (lognot x) z))))
        ((or (<= 20 n 39) (<= 60 n 79))
         (to-32-bits-word (logxor x y z)))
        ((<= 40 n 59)
         (to-32-bits-word (logior (logand x y)
                                  (logand x z)
                                  (logand y z))))))

(defun sha1-k (n)
  (cond ((<=  0 n 19) #x5A827999)
        ((<= 20 n 39) #x6ED9EBA1)
        ((<= 40 n 59) #x8F1BBCDC)
        ((<= 60 n 79) #xCA62C1D6)))
  
(defun sha1-digest (message)
  (let* ((h0 #x67452301)
         (h1 #xEFCDAB89)
         (h2 #x98BADCFE)
         (h3 #x10325476)
         (h4 #xC3D2E1F0)
         (padded-message (pad-the-message message))
         (n (/ (length padded-message) 64)))
    (dotimes (i n)
      (let ((a h0) (b h1) (c h2) (d h3) (e h4) (temp 0)
            (message-block (prepare-message-block i padded-message)))
        (dotimes (i 80)
          (setq temp (to-32-bits-word (+ (rotl a 5)
                                         (sha1-f i b c d)
                                         e
                                         (sha1-k i)
                                         (aref message-block i))))
          (setq e d)
          (setq d c)
          (setq c (to-32-bits-word (rotl b 30)))
          (setq b a)
          (setq a temp))  
        (setq h0 (to-32-bits-word (+ h0 a)))
        (setq h1 (to-32-bits-word (+ h1 b)))
        (setq h2 (to-32-bits-word (+ h2 c)))
        (setq h3 (to-32-bits-word (+ h3 d)))
        (setq h4 (to-32-bits-word (+ h4 e)))))
      (list h0 h1 h2 h3 h4)))

(defun digest (message &key (format :vector))
  "Make a SHA1 digest from a vector of bytes"
  (case format
    (:list   (sha1-digest message))
    (:vector (let ((list nil))
               (flet ((%to-bytes (x)
                        (push (logand #xFF (ash x -24)) list)
                        (push (logand #xFF (ash x -16)) list)
                        (push (logand #xFF (ash x -8)) list)
                        (push (logand #xFF x) list)))
                 (mapcar #'%to-bytes (sha1-digest message)))
               (coerce (nreverse list) 'vector)))
    (:string (let ((digest (sha1-digest message)))
              (string-downcase (format nil "~8,'0x~8,'0x~8,'0x~8,'0x~8,'0x"
                                       (first  digest)
                                       (second digest)
                                       (third  digest)
                                       (fourth digest)
                                       (fifth  digest)))))))


