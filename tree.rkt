#lang racket
(require racket/trace)
(require racket/stream)

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
 
(define (skip-whitespaces str index) ;прескача интервалите
  (if (not (char-whitespace? (string-ref str index))) index
      (skip-whitespaces str (+ 1 index)))
  )

(define (string-tree-root? str index)
  (is-number? (string-ref str index))
  )



(define (tree? str)

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

  (define (is-tree? str first-index last-index) ;
    (if (and (char=? (string-ref str first-index) #\*) (= first-index last-index)) #t
        (and
         (char=? (string-ref str first-index) #\{)
         (= (number-of-children str first-index last-index) 2)

         (let* (  ;проверка за валидни корен, ляво и дясно поддърво
                (root-index (skip-whitespaces str (+ 1 first-index)))
                
                (first-index-left-tree (skip-whitespaces str (get-first-index-after-number str root-index)))
                
                (last-index-left-tree (if (empty-string-tree? str first-index-left-tree) ;ако е празно дървото, първият индекс съвпада с последния; иначе търсим правилната затваряща скоба
                                          first-index-left-tree
                                          (get-last-index str first-index-left-tree 0)))
                
                )
           (and ;тук проверяваме дали имаме валиден корен и ляво поддърво
            (string-tree-root? str root-index)
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

  (if (correct-brackets? str 0 0) (is-tree? str 0 (- (string-length str) 1))
      #f
  )
)


(define (string->tree str)

  (define (get-number str index index2)
    (string->number (substring str index index2)))
  
  (define (make-tree str index) ;валидно дърво, няма нужда от последен индекс, подаваме го без интервали
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