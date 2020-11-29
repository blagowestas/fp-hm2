#lang racket

(require racket/trace)

(require rackunit)
(require rackunit/text-ui)
(require rackunit/gui)


(define (is-number? ch)
  (and (char>=? ch #\0) (char<=? ch #\9)))


(define (empty-tree? str index)
  (char=? (string-ref str index) #\*))

(define (get-first-index-after-number str index) 
  (if (is-number? (string-ref str index)) (get-first-index-after-number str (+ 1 index))
      index)
  )
  
(define (get-last-index str index number-of-opened-brackets) ;взима индекса на правилната затваряща скоба
  (cond 
    ((>= index (string-length str)) #f)
    ((char=? (string-ref str index) #\{) (get-last-index str (+ 1 index) (+ 1 number-of-opened-brackets)))
    ((and (char=? (string-ref str index) #\}) (<= number-of-opened-brackets 1)) index)
    ((and (char=? (string-ref str index) #\}) (> number-of-opened-brackets 1)) (get-last-index str (+ 1 index) (- number-of-opened-brackets 1)))
    (else (get-last-index str (+ 1 index) number-of-opened-brackets))
    )
  ) 

(define (remove-whitespace str index)
  (cond
    ((= index (string-length str)) "")
    ((char-whitespace? (string-ref str index)) (remove-whitespace str (+ 1 index)))
    (else (string-append (substring str index (+ 1 index)) (remove-whitespace str (+ 1 index))))
    )
  )

(define (tree? str)
 
  (define (root? str index)
    (is-number? (string-ref str index))
    )

  (define (is-tree? str first-index last-index) ; ПОМИСЛИ КАК ДА ПРЕСКАЧАШ СПЕЙСОВЕТЕ
    (if (and (char=? (string-ref str first-index) #\*) (= first-index last-index))#t
        (and
         (char=? (string-ref str first-index) #\{)
         (root? str (+ 1 first-index))
         (let* (
                (first-index-left-tree (get-first-index-after-number str (+ 1 first-index))) 
                (last-index-left-tree (if (char=? (string-ref str first-index-left-tree) #\*) first-index-left-tree (get-last-index str first-index-left-tree 0)))          
                (first-index-right-tree (if (false? last-index-left-tree) #f (+ 1 last-index-left-tree)))                         
                (last-index-right-tree (cond
                                         ((false? first-index-right-tree) #f)
                                         ((char=? (string-ref str first-index-right-tree) #\*) first-index-right-tree)
                                         (else (get-last-index str first-index-right-tree 0))))        
                )
           (if (or (false? first-index-left-tree) (false? last-index-left-tree) (false? first-index-right-tree) (false? last-index-left-tree))
               #f
               (and (is-tree? str first-index-left-tree last-index-left-tree)
                    (is-tree? str first-index-right-tree last-index-right-tree)
                    )
               )
           )
         (char=? (string-ref str last-index) #\}))
        )
    )


  (define (correct-brackets? str index counter)
    (cond
      ((and (= index (string-length str)) (zero? counter)) #t)
      ((and (= index (string-length str)) (> counter 0)) #f)      
      ((char=? (string-ref str index) #\{) (correct-brackets? str (+ 1 index) (+ 1 counter)))
      ((and (char=? (string-ref str index) #\}) (< counter 1) #f))
      ((and (char=? (string-ref str index) #\}) (> counter 0)) (correct-brackets? str (+ 1 index) (- counter 1)))
      (else (correct-brackets? str (+ 1 index) counter)))
    )



  (if (correct-brackets? str 0 0)
      (let* (
             (str-without-whitespace (remove-whitespace str 0))
             (len (string-length str-without-whitespace))
             )        
        (is-tree? str-without-whitespace 0 (- len 1))
        )
      #f)
  )


;(trace is-tree?)
;(trace correct-brackets?)
;(trace root?)
;(trace get-first-index-after-number)
;(trace get-last-index)



(define (string->tree str)

  (define (get-number str index index2)
    (string->number (substring str index index2)))
  
  (define (make-tree str index) ;string without whitespace
    (cond
      ((= index (string-length str)) '())
      ((empty-tree? str index) '())
      ((is-number? (string-ref str index)) (let ((index2 (get-first-index-after-number str index)))
                                             (list
                                              (get-number str index index2)
                                              (make-tree str index2)
                                              (make-tree str (if (empty-tree? str index2) (+ 1 index2) (+ 1 (get-last-index str index 0)))))))
     
      (else (make-tree str (+ 1 index)))))


  (if (tree? str) (make-tree str 0) #f))


;(trace make-tree)

(define (tree-root tree)
  (car tree))

(define (left-tree tree)
  (cadr tree))

(define (right-tree tree)
  (caddr tree))


(define (ordered? tree)
  (or (empty? tree)
      (let (
            (left-tree (left-tree tree))
            (right-tree (right-tree tree))
            )    
   
        (and
         (or (empty? left-tree) (> (tree-root tree) (tree-root left-tree)))
         (or (empty? right-tree) (< (tree-root tree) (tree-root right-tree)))
         (ordered? left-tree) (ordered? right-tree))
      )
  )
)

(define (height tree)
  (if (empty? tree) 0
      (+ 1 (max
            (height (left-tree tree))
            (height (right-tree tree))))
  )
)


(define (balanced? tree)
  (or (empty? tree)
      (let (
            (left-tree (left-tree tree))
            (right-tree (right-tree tree))
            )
        (and (<= (abs (- (height left-tree) (height right-tree))) 1)
             (balanced? left-tree)
             (balanced? right-tree))
       )
   )
)


;
;     A
;   /   \
;  B     C  
; /     / \  
;D     E   F  
;     /  
;    G  

;balanced: '(a (b (d () ()) ()) (c (e (g () ()) ()) (f () ()))))
;not-balanced '(a (b (d () ()) ()) (c (e (g (t () ()) ()) ()) (f () ()))))
;(balanced? '(a (b (d (f (y () ()) ()) ()) (g () ())) (c (e (g () ()) ()) (f () ())))) - f
;(balanced? '(a (b (d (f (y () ()) ()) (h () ())) (g (d () ()) ())) (c (e (g () ()) ()) (f () ())))) - t
;(balanced? '(a (b (d (f (y () ()) ()) (h () ())) (g (d () ()) ())) (c (e () ()) (f () ())))) - f



;
;(test/gui
; (test-suite
;    "all tests"
;    (test-suite
;     "Testing tree?"
;     (test-case "Empty tree" (check-true (tree? "*")))
;     (test-case "Test with empty children trees and a lot whitespacces" (check-true (tree? " {  10   *   *}    ")))
;     (test-case "Tree with one child" (check-true (tree? "{10 { 5 * * } *}")))
;     (test-case "Tree with two children and no whitespaces" (check-true (tree? "{10{5**}{15**}}")))
;     (test-case "Complex tree" (check-true (tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")))
;     (test-case "Wrong tree 1" (check-false (tree? "{}")))
;     (test-case "Wrong tree 2" (check-false (tree? "{*}")))
;     (test-case "Wrong tree 3" (check-false (tree? "{10{*{{}10")))
;     (test-case "Wrong tree 4" (check-false (tree? "{10*10}")))
;     (test-case "Wrong tree 5" (check-false (tree? "}{")))
;     (test-case "Wrong tree 6" (check-false (tree? "***")))
;    )
;    (test-suite
;     "Testing ordered?"
;     (test-case "Empty tree" (check-true (ordered? '())))
;     (test-case "Simple tree" (check-true (ordered? '(10 () ()))))
;     (test-case "Complex tree" (check-true (ordered? '(15 (5 (0 () ()) (10 () ())) (25 (20 () ()) (30 () ()))))))
;     (test-case "Not-ordered tree 1" (check-false (ordered? '(15 (25 (0 () ()) (10 () ())) ()))))
;     (test-case "Not-ordered tree 2" (check-false (ordered? '(15 (5 (0 () ()) (10 () ())) (10 () ())))))
;     (test-case "Not-ordered tree 3" (check-false (ordered? '(15 (25 () ()) (10 () ())))))
;    )
;     (test-suite
;     "Testing balanced?"
;     (test-case "Empty tree" (check-true (balanced? '())))
;     (test-case "Simple tree" (check-true (balanced? '(10 () ()))))
;     (test-case "Complex tree 1" (check-true (balanced? '(15 (5 (0 () ()) (10 () ())) (25 (20 () ()) (30 () ()))))))
;     (test-case "Complex tree 2" (check-true (balanced? '(1 (2 (3 () ()) ()) (4 (5 (6 () ()) ()) (7 () ()))))))
;     (test-case "Complex tree 2" (check-true (balanced? '(4 (6 (7 (8 (9 () ()) ()) (10 () ())) (11 (12 () ()) ())) (13 (14 (15 () ()) ()) (16 () ()))))))
;     (test-case "Not-ordered tree 1" (check-false (balanced? '(1 (2 (3 () ()) ()) (4 (5 (6 (7 () ()) ()) ()) (8 () ()))))))
;     (test-case "Not-ordered tree 2" (check-false (balanced? '(1 (2 (3 (4 (5 () ()) ()) ()) (6 () ())) (7 (8 (9 () ()) ()) (10 () ()))))))
;     (test-case "Not-ordered tree 3" (check-false (balanced? '(1 (2 (3 (4 (5 () ()) ()) (6 () ())) (7 (8 () ()) ())) (9 (10 () ()) (11 () ()))))))
;    )
;   )
;)



   