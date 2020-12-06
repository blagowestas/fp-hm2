#lang racket
(require racket/trace)
(require racket/stream)
(provide (all-defined-out))


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
 
(define (skip-whitespaces str index) 
  (if (or (= index (string-length str)) (not (char-whitespace? (string-ref str index)))) index
      (skip-whitespaces str (+ 1 index)))
  )

(define (string-tree-root? str index)
  (is-number? (string-ref str index))
  )


(define (tree? str)

  (define (check-subtrees str index subs)
    (cond
      ((= index (string-length str)) #t)
      ((and (char=? (string-ref str index) #\*) (null? subs))   (or (= (+ 1 index) (string-length str)) (= (skip-whitespaces str (+ 1 index)) (string-length str))));empty tree
    
      ((and (not (char=? (string-ref str index) #\*)) (not (char=? (string-ref str index) #\{)) (not (char=? (string-ref str index) #\}))) (check-subtrees str (+ 1 index) subs)); skip other chars

      ((char=? (string-ref str index) #\{) (check-subtrees str (+ 1 index) (cons 0 subs)))
    
      ((char=? (string-ref str index) #\*) (check-subtrees str (+ 1 index) (cons (+ 1 (car subs)) (cdr subs))))
      ((and (char=? (string-ref str index) #\}) (= 2 (car subs)) (not (null? (cdr subs)))) (check-subtrees str (+ 1 index) (cons (+ 1 (cadr subs)) (cddr subs))))
      ((and (char=? (string-ref str index) #\}) (= 2 (car subs)) (null? (cdr subs))) #t) ; last bracket 
      (else #f)))


  (define (correct-brackets? str index counter) 
    (cond
      ((and (= index (string-length str)) (zero? counter)) #t)
      ((and (= index (string-length str)) (> counter 0)) #f)
      ((char=? (string-ref str index) #\{) (correct-brackets? str (+ 1 index) (+ 1 counter)))
      ((and (char=? (string-ref str index) #\}) (< counter 1)) #f)
      ((and (char=? (string-ref str index) #\}) (> counter 0)) (correct-brackets? str (+ 1 index) (- counter 1)))
      (else (correct-brackets? str (+ 1 index) counter))))


  (define (check-tree str index should-be child) ;
    (cond
      ((= index (string-length str)) #t)
      ((char-whitespace? (string-ref str index)) (check-tree str (+ 1 index) should-be child))

      ((and (char=? (string-ref str index) #\*)  (equal? should-be 'beg) (equal? child 'main)) ;проверка за празното дърво
       (or (= (+ 1 index) (string-length str)) (= (skip-whitespaces str (+ 1 index)) (string-length str))))  

      ((and (is-number? (string-ref str index)) (equal? should-be 'number)) (check-tree str (get-first-index-after-number str index) 'beg 'left)) ;beg = { or *

      ((and (char=? (string-ref str index) #\{) (equal? should-be 'beg)) (check-tree str (+ 1 index) 'number child))

      ((and (char=? (string-ref str index) #\*) (equal? should-be 'beg) (equal? child 'left)) (check-tree str (+ 1 index) 'beg 'right))
      
      ((and (char=? (string-ref str index) #\*) (equal? should-be 'beg) (equal? child 'right)) (check-tree str (+ 1 index) 'end 'right)) 
      
      ((and (char=? (string-ref str index) #\}) (equal? should-be 'end) (equal? child 'left)) (check-tree str (+ 1 index) 'beg 'right))
      
      ((and (char=? (string-ref str index) #\}) (equal? should-be 'end) (equal? child 'right)) (or (= (skip-whitespaces str index) (string-length str))
                                                                                                   (check-tree str (+ 1 index) 'end 'right)
                                                                                                   (check-tree str (+ 1 index) 'beg 'right)))
      
      (else #f))
    )


  
  (if (and (correct-brackets? str 0 0) (check-subtrees str 0 '()))
      (check-tree str 0 'beg 'main)
      #f)
)
    

(define (string->tree str)

  (define (get-number str index index2)
    (string->number (substring str index index2)))
  
  (define (make-tree str index) ;валидно дърво, подаваме го без интервали
    (cond
      ((= index (string-length str)) '())
      ((empty-string-tree? str index) '())

      ((string-tree-root? str index)
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

(define (empty? tree)
  (equal? tree empty-tree)) 

(define (tree-root tree)
  (car tree))

(define (left-tree tree)
  (cadr tree))

(define (right-tree tree)
  (caddr tree))

(define (is-leaf? tree)
  (if (empty? tree) #f
      (and (empty? (left-tree tree)) (empty? (right-tree tree)))))

(define (is-valid? tree)
  (or
   (empty? tree)
   (and
    (list? tree)
    (number? (tree-root tree))
    (= (length tree) 3)
    (is-valid? (left-tree tree))
    (is-valid? (right-tree tree))))
)

(define (ordered? tree)
  (if (not (is-valid? tree))
      "Invalid tree"
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
)


(define (height tree)
  (if (empty? tree) 0
      (+ 1 (max
            (height (left-tree tree))
            (height (right-tree tree))))
      )
  )


(define (balanced? tree)
  (if (not (is-valid? tree))
      "Invalid tree"
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
)


(define (tree->string tree)
  (if (not (is-valid? tree))
      "Invalid tree"
      (if (empty? tree) "*"
          (string-append
           "{"
           (number->string (tree-root tree))
           " "
           (tree->string (left-tree tree))
           " "
           (tree->string (right-tree tree))
           "}"
          )
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
  (if (not (is-valid? tree))
      "Invalid tree"
      (cond
        ((string=? order "inorder") (make-stream-inorder tree)) ; ЛКД
        ((string=? order "postorder") (make-stream-postorder tree)) ; ЛДК
        ((string=? order "preorder") (make-stream-preorder tree)) ; КЛД
        (else "Wrong order."))
      )
  )


(define (level-elements n tree)
  (cond
    ((empty? tree) '())
    ((= n 1) (list (tree-root tree)))
    (else (append (level-elements (- n 1) (left-tree tree)) (level-elements (- n 1) (right-tree tree)))))
)


(define (get-level-list n tree) ;подаваме височината на дървото, която съвпада с броя на нивата
  (cond
    ((zero? n) '())
    (else (append (list (level-elements n tree)) (get-level-list (- n 1) tree))))
)


                   
(define (width tree)
  (foldr
   (lambda (current result) (max (length current) result))
   0
   (get-level-list (height tree) tree))
)


(define (get-n-chars n ch)
  (define (helper n result)
    (if (< n 1) result
      (helper (- n 1) (string-append ch result))))
  (helper n ""))


(define (get-level tree elem)
  
  (define (helper current-level current-tree)
    (cond
      ((or (> current-level (height tree)) (empty? current-tree)) #f)
      ((= (tree-root current-tree) elem) current-level)
      (else (or (helper (+ 1 current-level) (left-tree current-tree)) (helper (+ 1 current-level) (right-tree current-tree))))))

  (helper 1 tree))



(define (number-of-digits number)
  (define (helper current-number counter)
    (if (zero? current-number) counter
        (helper (quotient current-number 10) (+ 1 counter))))

     (if (zero? number) 1 (helper number 0)))

(define (print-left tree height-right number-of-spaces) 
  (cond
    ((empty? tree) "")
    ((is-leaf? tree) (string-append "\n" (get-n-chars (- number-of-spaces 1) " ") "|\n" (get-n-chars (- number-of-spaces 1) " ") (number->string (tree-root tree))))
    (else   (string-append
       "\n"
       (get-n-chars number-of-spaces " ")
       "|\n"
       (get-n-chars number-of-spaces " ")
       (number->string (tree-root tree))
       (print-right (right-tree tree) (width (left-tree tree))  number-of-spaces)
       (print-left (left-tree tree) (height (right-tree tree))  number-of-spaces))))
)

  
(define (print-right tree width-left number-of-spaces)
  (cond
    ((empty? tree) "")
    ((is-leaf? tree) (string-append "--" (number->string (tree-root tree))))
    (else
     (string-append 
      (get-n-chars (+ 1 width-left) "--")
      (number->string (tree-root tree))
      (print-right (right-tree tree) (width (left-tree tree)) (+ (* 2 (+ 1 width-left)) number-of-spaces (number-of-digits (tree-root tree))))
      (print-left (left-tree tree) (height (right-tree tree)) (+ (* 2 (+ 1 width-left)) number-of-spaces (number-of-digits (tree-root tree)))))))
    )
 

(define (visualize tree) 
 
  (cond
    ((not (is-valid? tree)) "Invalid tree")
    ((empty? tree) "")
    (else
     (string-append
      (number->string (tree-root tree))
      (print-right (right-tree tree) (width (left-tree tree)) (number-of-digits (tree-root tree))) 
      (print-left (left-tree tree) (height (right-tree tree)) 0)
      )))
 )


;(trace visualize)
;(trace print-left)
;(trace print-right)
;(trace get-n-chars)

;дървото от условието
;(10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))
;
;10-----5------1--3
;|      |         |
;|      |         111 
;|      22--6
;|      |
;|      2
;15--7
;|
;2
;
;      10
;   /      \
;  15       5  
; / \      /  \  
;2   7   22    1
;       /  \    \
;      2    6    3
;               /
;              111
;
;
;BST: '(15 (5 (0 () ()) (10 () ())) (25 (20 () ()) (30 () ())))


