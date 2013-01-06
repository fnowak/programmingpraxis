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

;; http://programmingpraxis.com/

(defun sq (x)
  (* x x))

(defun distance (a b)
  (let ((x (- (first a) (first b)))
	(y (- (second a) (second b))))
    (sqrt (+ (sq x ) (sq y )))))

(defun squarep (a b c d)
  (let ((distances (sort (list (distance a b)
			       (distance a c)
			       (distance a d)
			       (distance b c)
			       (distance b d)
			       (distance c d))
			 #'<)))
    (and (apply #'= (subseq distances 0 3))
	 (apply #'= (subseq distances 4 6))
	 (= (round (* 2 (sq (first distances))))
	    (round (sq (fifth distances)))))))

(defparameter squares
  '(((0 0) (0 1) (1 1) (1 0))	 ;; the unit square
    ((0 0) (2 1) (3 -1) (1 -2))	 ;; square not aligned to axis
    ((0 0) (1 1) (0 1) (1 0)))) ;; unit square, in different order

(defparameter non-squares
  '(((0 0) (0 2) (3 2) (3 0))	  ;; rectangle
    ((0 0) (3 4) (8 4) (5 0))	  ;; rhombus
    ((0 0) (0 0) (1 1) (0 0))	  ;; degenerate
    ((0 0) (0 0) (1 0) (0 1))	  ;; degenerate
    ((0 0) (1 0) (0 1) (-1 -1)))) ;; degenerate (Remco)

(defun test ()
  (and (every #'identity (mapcar (lambda (s) (apply #'squarep s)) squares))
       (notany #'identity (mapcar (lambda (ns) (apply #'squarep ns)) non-squares))))