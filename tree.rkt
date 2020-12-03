#lang racket
(require racket/trace)
(require racket/stream)
(require racket/include)
(require rackunit)
;(require rackunit/text-ui)
(require rackunit/gui)

(define (is-number? ch)
  (and (char>=? ch #\0) (char<=? ch #\9)))


(define (empty-string-tree? str index)
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


(define (correct-brackets? str index counter)
  (cond
    ((and (= index (string-length str)) (zero? counter)) #t)
    ((and (= index (string-length str)) (> counter 0)) #f)
    ((char=? (string-ref str index) #\{) (correct-brackets? str (+ 1 index) (+ 1 counter)))
    ((and (char=? (string-ref str index) #\}) (< counter 1) #f))
    ((and (char=? (string-ref str index) #\}) (> counter 0)) (correct-brackets? str (+ 1 index) (- counter 1)))
    (else (correct-brackets? str (+ 1 index) counter)))
  )

(define (number-of-children str first-index last-index)

  (define (helper current-index counter)
    (cond
      ((= current-index last-index) counter)
      ((char=? (string-ref str current-index) #\{) (helper (get-last-index str current-index 0) (+ 1 counter))) ; ако срещнем отваряща скоба, това е текущ наследник, прескачаме го и увеличаваме брояча
      ((char=? (string-ref str current-index) #\*) (helper (+ 1 current-index) (+ 1 counter))) ; празно дърво - отново текущ наследник и увеличаваме брояча
      (else (helper (+ 1 current-index) counter))) ; останалите неща, които срещаме са корена и празни разстояния, съответно ги прескачаме
   )

  (helper (+ 1 first-index) 0) ; прескачаме първата скоба, не искаме да я броим
) 

(define (skip-whitespaces str index) ;прескача интервалите
  (if (not (char-whitespace? (string-ref str index))) index
      (skip-whitespaces str (+ 1 index)))
)


(define (root? str index)
    (is-number? (string-ref str index))
    )

  (define (is-tree? str first-index last-index) ;
    (if (and (char=? (string-ref str first-index) #\*) (= first-index last-index)) #t
        (and
         (char=? (string-ref str first-index) #\{)
         (= (number-of-children str first-index last-index) 2)

         (let* ( ;тук проверяваме дали имаме валиден корен и ляво поддърво
                (root-index (skip-whitespaces str (+ 1 first-index)))
                
                (first-index-left-tree (skip-whitespaces str (get-first-index-after-number str root-index)))
                
                (last-index-left-tree (if (empty-string-tree? str first-index-left-tree)
                                          first-index-left-tree
                                          (get-last-index str first-index-left-tree 0)))
                
                )
           (and
            (root? str root-index)
            (is-tree? str first-index-left-tree last-index-left-tree) ;ако имаме валидно ляво поддърво, следващият индекс е на дясното, трябва да го проверим

            (let* (
                   (first-index-right-tree (skip-whitespaces str (+ 1 last-index-left-tree)))
                   (last-index-right-tree (if (empty-string-tree? str first-index-right-tree)
                                              first-index-right-tree
                                              (get-last-index str first-index-right-tree 0)))
                   )
              (is-tree? str first-index-right-tree last-index-right-tree)
            )
           )
         )
        (char=? (string-ref str last-index) #\}))
        )
    )


(define (tree? str)
  (if (not (correct-brackets? str 0 0)) #f
      (is-tree? str 0 (- (string-length str) 1))
  )
)

(trace skip-whitespaces)
(trace is-tree?)
(trace correct-brackets?)
;(trace root?)
(trace get-first-index-after-number)
(trace get-last-index)
(trace number-of-children)

(define (string->tree str)

  (define (get-number str index index2)
    (string->number (substring str index index2)))
  
  (define (make-tree str index) ;string without whitespace, valid tree, no need of end index.
    (cond
      ((= index (string-length str)) '())
      ((empty-string-tree? str index) '())

      ((is-number? (string-ref str index))
       (let (
             (index2 (get-first-index-after-number str index))
             )
         (list
          (get-number str index index2)
          (make-tree str index2)
          (make-tree str (if (empty-string-tree? str index2) (+ 1 index2) (+ 1 (get-last-index str index2 0)))))))
     
      (else (make-tree str (+ 1 index)))
      )
    )
  
  (if (tree? str)
      (make-tree (remove-whitespace str 0) 0)
      #f)
  )

(define empty-tree '())

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



(define (tree->stream tree order)

  (define (make-stream-inorder tree)
    (if(empty? tree) empty-stream
       (stream-append (make-stream-inorder (left-tree tree)) (stream (tree-root tree)) (make-stream-inorder (right-tree tree))))
    )

  (define (make-stream-preorder tree)
    (if(empty? tree) empty-stream
       (stream-append (stream (tree-root tree)) (make-stream-preorder (left-tree tree)) (make-stream-preorder (right-tree tree))))
    )

  (define (make-stream-postorder tree)
    (if(empty? tree) empty-stream
       (stream-append (make-stream-postorder (left-tree tree)) (make-stream-postorder (right-tree tree)) (stream (tree-root tree)) ))
    )

  (cond
    ((string=? order "inorder") (make-stream-inorder tree)) ; ЛКД
    ((string=? order "postorder") (make-stream-postorder tree)) ; ЛДК
    ((string=? order "preorder") (make-stream-preorder tree)) ; КЛД
    (else "Wrong order."))
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






(test/gui
 (test-suite
    "all tests"
    (test-suite
     "Testing tree?"
     (test-case "Empty tree" (check-true (tree? "*")))
     (test-case "Tree with empty children trees and a lot whitespacces" (check-true (tree? "{  10   *   *}")))
     (test-case "Correct tree with one child" (check-true (tree? "{10 { 5 * * } *}")))
     (test-case "Tree with two children and no whitespaces" (check-true (tree? "{10{5**}{15**}}")))
     (test-case "Complex tree" (check-true (tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")))
     (test-case "Tree with more than 2 children" (check-false (tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}} {1 * *}}")))
     (test-case "Tree with only one child." (check-false (tree? "{5 {22 {2 * *} *}}")))
     (test-case "Tree with more roots" (check-false (tree? "{1 2 3 * * }")))
     (test-case "Wrong tree 1" (check-false (tree? "{}")))
     (test-case "Wrong tree 2" (check-false (tree? "{*}")))
     (test-case "Wrong tree 3" (check-false (tree? "{10{*{{}10")))
     (test-case "Wrong tree 4" (check-false (tree? "{10*10}")))
     (test-case "Wrong tree 5" (check-false (tree? "}{")))
     (test-case "Wrong tree 6" (check-false (tree? "***")))
    )
    (test-suite
     "Testing ordered?"
     (test-case "Empty tree" (check-true (ordered? '())))
     (test-case "Simple tree" (check-true (ordered? '(10 () ()))))
     (test-case "Complex tree" (check-true (ordered? '(15 (5 (0 () ()) (10 () ())) (25 (20 () ()) (30 () ()))))))
     (test-case "Not-ordered tree 1" (check-false (ordered? '(15 (25 (0 () ()) (10 () ())) ()))))
     (test-case "Not-ordered tree 2" (check-false (ordered? '(15 (5 (0 () ()) (10 () ())) (10 () ())))))
     (test-case "Not-ordered tree 3" (check-false (ordered? '(15 (25 () ()) (10 () ())))))
    )
     (test-suite
     "Testing balanced?"
     (test-case "Empty tree" (check-true (balanced? '())))
     (test-case "Simple tree" (check-true (balanced? '(10 () ()))))
     (test-case "Complex tree 1" (check-true (balanced? '(15 (5 (0 () ()) (10 () ())) (25 (20 () ()) (30 () ()))))))
     (test-case "Complex tree 2" (check-true (balanced? '(1 (2 (3 () ()) ()) (4 (5 (6 () ()) ()) (7 () ()))))))
     (test-case "Complex tree 3" (check-true (balanced? '(4 (6 (7 (8 (9 () ()) ()) (10 () ())) (11 (12 () ()) ())) (13 (14 (15 () ()) ()) (16 () ()))))))
     (test-case "Not-ordered tree 1" (check-false (balanced? '(1 (2 (3 () ()) ()) (4 (5 (6 (7 () ()) ()) ()) (8 () ()))))))
     (test-case "Not-ordered tree 2" (check-false (balanced? '(1 (2 (3 (4 (5 () ()) ()) ()) (6 () ())) (7 (8 (9 () ()) ()) (10 () ()))))))
     (test-case "Not-ordered tree 3" (check-false (balanced? '(1 (2 (3 (4 (5 () ()) ()) (6 () ())) (7 (8 () ()) ())) (9 (10 () ()) (11 () ()))))))
    )
     (test-suite
     "Testing tree->stream?"
     (test-case "Empty tree, inorder" (check-equal? (tree->stream empty-tree "inorder") empty-stream))
     (test-case "Empty tree, postorder" (check-equal? (tree->stream empty-tree "postorder") empty-stream))
     (test-case "Empty tree, preorder" (check-equal? (tree->stream empty-tree "preorder") empty-stream))
     (test-case "Simple tree, inorder" (check-equal? (stream->list (tree->stream '(10 () ()) "inorder")) '(10)))
     (test-case "Simple tree, postorder" (check-equal? (stream->list (tree->stream '(10 () ()) "postorder")) '(10)))
     (test-case "Simple tree, preorder" (check-equal? (stream->list (tree->stream '(10 () ()) "preorder")) '(10)))
     (test-case "BST, inorder" (check-equal? (stream->list (tree->stream '(15 (5 (0 () ()) (10 () ())) (25 (20 () ()) (30 () ()))) "inorder")) '(0 5 10 15 20 25 30)))
     (test-case "BST, postorder" (check-equal? (stream->list (tree->stream '(15 (5 (0 () ()) (10 () ())) (25 (20 () ()) (30 () ()))) "postorder")) '(0 10 5 20 30 25 15)))
     (test-case "BST, preorder" (check-equal? (stream->list (tree->stream '(15 (5 (0 () ()) (10 () ())) (25 (20 () ()) (30 () ()))) "preorder")) '(15 5 0 10 25 20 30)))
    )
   )
)