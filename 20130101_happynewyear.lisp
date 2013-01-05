;; Copyright (c) 2013, Frederic Nowak
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; http://programmingpraxis.com/2013/01/01/happy-new-year/

(defun tuples (list n)
  (reduce (lambda (first rest)
	    (declare (ignore first))
	    (loop for r in rest
	       append (loop for l in list
			 collecting (cons l r))))
	  (make-sequence 'list n :initial-element (first list))
	  :from-end t
	  :initial-value '(())))

(defun merge-numbers (&rest numbers)
  (parse-integer
   (apply #'concatenate 'string
	  (loop for n in numbers
	     collect (write-to-string n)))))

(defun combine (operators numbers)
  (let ((of (first operators))
	(or (rest operators))
	(nf (first numbers))
	(nr (rest numbers)))
    (cond
      ;; middle: non-nil-operator
      ((and of or nf nr) (list of nf (combine or nr)))
      ;; middle: nil-operator
      ((and (not of) or nf nr) (combine or (cons (merge-numbers nf (first nr)) (rest nr))))
      ;; end: non-nil-operator
      ((and of (not or) nf nr) (list of nf (first nr)))
      ;; end: nil-operator
      ((and (not of) (not or) nf nr) (merge-numbers nf (first nr))))))

(defun countdown (ops nums year)
  (loop
     for tuple in (tuples ops (1- (length numbers)))
     nconc
       (let* ((combination (combine tuple nums))
	      (result (handler-case (eval combination)
			(division-by-zero () nil))))
	 (if (and result
		  (= result year))
	     (list combination)))))

(defun happy-new-year-2013 ()
  (countdown '(nil + - * /)
	     '(10 9 8 7 6 5 4 3 2 1)
	     2013))