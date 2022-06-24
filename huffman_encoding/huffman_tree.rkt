#lang racket/base
(define (assert exp error-message) (if (not exp) (error error-message) (display "Test OK.\n") ))
(define (make-leaf character frequency) (list 'leaf character frequency))
(define (make-node left-child right-child) (list 'node left-child right-child))
(define (get-frequency node)
    (if (eq? (car node) 'leaf) (caddr node)
        (+ (get-frequency (cadr node)) (get-frequency (caddr node)))))
(define (min-key elements key)
    (cond ((null? elements) (error "min-key of an empty list is not defined"))
          ((= (length elements) 1) (car elements))
          (else (let ((min-cdr (min-key (cdr elements) key)))
                     (if (< (key (car elements)) (key min-cdr)) (car elements) min-cdr)))))
(define (pop-min elements key) ; key is a function as in Python3, only returns the list without the min
    (let ((smallest (min-key elements key)))
         (if (equal? smallest (car elements)) (cdr elements)
                                              (cons (car elements) (pop-min (cdr elements) key)))))
(define (min-frequ-node nodes)(min-key nodes get-frequency))
(define (pop-min-frequ-node nodes) (pop-min nodes get-frequency) )
(define (build-huffman nodes)
    (cond ((=(length nodes) 1) (car nodes))
          (else (define min1 (min-frequ-node nodes))
                (define min2 (min-frequ-node (pop-min-frequ-node nodes)))
                (define rest (pop-min-frequ-node (pop-min-frequ-node nodes)))
                (build-huffman (cons (make-node min1 min2) rest)))))







;Tests
(let ((l1 (make-leaf 'a 2))
      (l2 (make-leaf 'b 1)))
      (assert (= (get-frequency (make-node l1 l2)) 3) "get-frequency not working properly" )
      (assert (equal? (pop-min '((3 2) (1 2) (10 1) (9 2)) car)
                      '((3 2) (10 1) (9 2))) "pop-min not working properly" ))
      (assert (equal? (min-key '((3 2) (1 2) (10 1) (9 2)) car)
                      '(1 2)) "pop-min not working properly" )
(define (pretty-print l)
    (cond ((null? l) (display "\n"))
        (else (display (car l))
              (display "\n")
              (pretty-print (cdr l)))
    )
)

(define alphabet (list (make-leaf 'a 8)
                     (make-leaf 'b 3)
                     (make-leaf 'c 1)
                     (make-leaf 'd 1)
                     (make-leaf 'e 1)
                     (make-leaf 'f 1)
                     (make-leaf 'g 1)))
(define tree (build-huffman alphabet))
(pretty-print (cddr tree))