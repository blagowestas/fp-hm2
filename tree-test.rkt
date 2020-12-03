#lang racket

(require racket/include)
(require rackunit)
(require rackunit/gui)

(require "tree.rkt")


(test/gui
 (test-suite
    "all tests"
    (test-suite
     "Testing 'tree?'"
     (test-case "Empty tree" (check-true (tree? "*")))
     (test-case "Tree with empty children trees and a lot whitespacces" (check-true (tree? "{  10   *   *}")))
     (test-case "Correct tree with one child" (check-true (tree? "{10 { 5 * * } *}")))
     (test-case "Tree with two children and no whitespaces" (check-true (tree? "{10{5**}{15**}}")))
     (test-case "Complex tree" (check-true (tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")))
     (test-case "Tree with more than 2 children" (check-false (tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}} {1 * *}}")))
     (test-case "Tree with only one child." (check-false (tree? "{5 {22 {2 * *} *}}")))
     (test-case "Tree with more roots" (check-false (tree? "{1 2 3 * * }")))
     (test-case "Missing root" (check-false (tree? "{ * {1 * *}}")))
     (test-case "Wrong tree 1" (check-false (tree? "{}")))
     (test-case "Wrong tree 2" (check-false (tree? "{*}")))
     (test-case "Wrong tree 3" (check-false (tree? "{10{*{{}10")))
     (test-case "Wrong tree 4" (check-false (tree? "{10*10}")))
     (test-case "Wrong tree 5" (check-false (tree? "}{")))
     (test-case "Wrong tree 6" (check-false (tree? "***")))
    )
    (test-suite
     "Testing 'ordered?'"
     (test-case "Empty tree" (check-true (ordered? '())))
     (test-case "Simple tree" (check-true (ordered? '(10 () ()))))
     (test-case "Complex tree" (check-true (ordered? '(15 (5 (0 () ()) (10 () ())) (25 (20 () ()) (30 () ()))))))
     (test-case "Not-ordered tree 1" (check-false (ordered? '(15 (25 (0 () ()) (10 () ())) ()))))
     (test-case "Not-ordered tree 2" (check-false (ordered? '(15 (5 (0 () ()) (10 () ())) (10 () ())))))
     (test-case "Not-ordered tree 3" (check-false (ordered? '(15 (25 () ()) (10 () ())))))
     (test-case "Invalid tree" (check-equal? (ordered? '(1 (2 (3 () ())) (4 (5 (6 () ()) ()) (7 () ())))) "Invalid tree"))
    )
     (test-suite
     "Testing 'balanced?'"
     (test-case "Empty tree" (check-true (balanced? '())))
     (test-case "Simple tree" (check-true (balanced? '(10 () ()))))
     (test-case "Complex tree 1" (check-true (balanced? '(15 (5 (0 () ()) (10 () ())) (25 (20 () ()) (30 () ()))))))
     (test-case "Complex tree 2" (check-true (balanced? '(1 (2 (3 () ()) ()) (4 (5 (6 () ()) ()) (7 () ()))))))
     (test-case "Complex tree 3" (check-true (balanced? '(4 (6 (7 (8 (9 () ()) ()) (10 () ())) (11 (12 () ()) ())) (13 (14 (15 () ()) ()) (16 () ()))))))
     (test-case "Not-ordered tree 1" (check-false (balanced? '(1 (2 (3 () ()) ()) (4 (5 (6 (7 () ()) ()) ()) (8 () ()))))))
     (test-case "Not-ordered tree 2" (check-false (balanced? '(1 (2 (3 (4 (5 () ()) ()) ()) (6 () ())) (7 (8 (9 () ()) ()) (10 () ()))))))
     (test-case "Not-ordered tree 3" (check-false (balanced? '(1 (2 (3 (4 (5 () ()) ()) (6 () ())) (7 (8 () ()) ())) (9 (10 () ()) (11 () ()))))))
     (test-case "Invalid tree" (check-equal? (balanced? '(1 (2 (3 () ())) (4 (5 (6 () ()) ()) (7 () ())))) "Invalid tree"))
    )
     (test-suite
     "Testing 'tree->stream?'"
     (test-case "Empty tree, inorder" (check-equal? (tree->stream empty-tree "inorder") empty-stream))
     (test-case "Empty tree, postorder" (check-equal? (tree->stream empty-tree "postorder") empty-stream))
     (test-case "Empty tree, preorder" (check-equal? (tree->stream empty-tree "preorder") empty-stream))
     (test-case "Simple tree, inorder" (check-equal? (stream->list (tree->stream '(10 () ()) "inorder")) '(10)))
     (test-case "Simple tree, postorder" (check-equal? (stream->list (tree->stream '(10 () ()) "postorder")) '(10)))
     (test-case "Simple tree, preorder" (check-equal? (stream->list (tree->stream '(10 () ()) "preorder")) '(10)))
     (test-case "BST, inorder" (check-equal? (stream->list (tree->stream '(15 (5 (0 () ()) (10 () ())) (25 (20 () ()) (30 () ()))) "inorder")) '(0 5 10 15 20 25 30)))
     (test-case "BST, postorder" (check-equal? (stream->list (tree->stream '(15 (5 (0 () ()) (10 () ())) (25 (20 () ()) (30 () ()))) "postorder")) '(0 10 5 20 30 25 15)))
     (test-case "BST, preorder" (check-equal? (stream->list (tree->stream '(15 (5 (0 () ()) (10 () ())) (25 (20 () ()) (30 () ()))) "preorder")) '(15 5 0 10 25 20 30)))
     (test-case "Invalid tree" (check-equal? (tree->stream '(1 (2 (3 () ())) (4 (5 (6 () ()) ()) (7 () ()))) "inorder") "Invalid tree"))
    )
     (test-suite
     "Testing 'tree->string?'"
     (test-case "Empty tree" (check-equal? (tree->string '()) "*"))
     (test-case "Simple tree" (check-equal? (tree->string '(10 () ())) "{10 * *}")) 
     (test-case "BST" (check-equal? (tree->string '(15 (5 (0 () ()) (10 () ())) (25 (20 () ()) (30 () ())))) "{15 {5 {0 * *} {10 * *}} {25 {20 * *} {30 * *}}}"))
     (test-case "Complex tree 1" (check-equal? (tree->string '(1 (2 (3 () ()) ()) (4 (5 (6 () ()) ()) (7 () ())))) "{1 {2 {3 * *} *} {4 {5 {6 * *} *} {7 * *}}}"))
     (test-case "Complex tree 2" (check-equal?
                                  (string->tree (tree->string '(4 (6 (7 (8 (9 () ()) ()) (10 () ())) (11 (12 () ()) ())) (13 (14 (15 () ()) ()) (16 () ())))))
                                  '(4 (6 (7 (8 (9 () ()) ()) (10 () ())) (11 (12 () ()) ())) (13 (14 (15 () ()) ()) (16 () ())))))
     (test-case "Invalid tree" (check-equal? (tree->string '(1 (2 (3 () ())) (4 (5 (6 () ()) ()) (7 () ())))) "Invalid tree")) 
     )
   )
)