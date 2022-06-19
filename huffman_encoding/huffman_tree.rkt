#lang racket/base
(define (assert exp error-message) (if (not exp) (error error-message) (display "Test OK.\n") ))
(define (make-leaf character frequency) (list 'leaf character frequency))
(define (make-node left-child right-child) (list 'node left-child right-child))
(define (get-frequency node)
    (if (eq? (car node) 'leaf) (caddr node)
        (+ (get-frequency (cadr node)) (get-frequency (caddr node)))))



;tests
(let ((l1 (make-leaf 'a 2))
      (l2 (make-leaf 'b 1)))
      (assert (= (get-frequency (make-node l1 l2)) 3) "get-frequency not working properly" ))

