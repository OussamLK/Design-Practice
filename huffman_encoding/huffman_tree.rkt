#lang racket/base
(define (assert exp error-message) (if (not exp) (error error-message) (display "Test OK.\n") ))
(define (make-leaf character frequency) (list 'leaf character frequency))
(define (make-node left-child right-child) (list 'node left-child right-child))
(define (get-frequency node)
    (if (eq? (car node) 'leaf) (caddr node)
        (+ (get-frequency (cadr node)) (get-frequency (caddr node)))))
(define (pop-min elements key) ; key is a function as in Python3
    (let ((smallest (apply min (map key elements)))
          (head (car elements)))
          (if (= (key head) smallest) (cdr elements) (cons head (pop-min (cdr elements) key)))))



;Tests
(let ((l1 (make-leaf 'a 2))
      (l2 (make-leaf 'b 1)))
      (assert (= (get-frequency (make-node l1 l2)) 3) "get-frequency not working properly" )
      (assert (equal? (pop-min '((3 2) (1 2) (10 1) (9 2)) car)
                      '((3 2) (10 1) (9 2))) "pop-min not working properly" ))

