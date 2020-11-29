#lang racket

(require racket/include)
(require rackunit)
(require rackunit/text-ui)
(require rackunit/gui)

(include "tree.rkt")
(include "hm1_group_b.rkt")


(test/gui
 (test-suite
    "all tests"
    (test-suite
     "Testing tree?"
     (test-case "Empty tree" (check-true (tree? "*")))
     (test-case "Test with empty children trees and a lot whitespacces" (check-true (tree? " {  10   *   *}    ")))
     (test-case "Tree with one child" (check-true (tree? "{10 { 5 * * } *}")))
     (test-case "Tree with two children and no whitespaces" (check-true (tree? "{10{5**}{15**}}")))
     (test-case "Complex tree" (check-true (tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")))
     (test-case "Wrong tree 1" (check-false (tree? "{}")))
     (test-case "Wrong tree 2" (check-false (tree? "{*}")))
     (test-case "Wrong tree 3" (check-false (tree? "{10{*{{}10")))
     (test-case "Wrong tree 4" (check-false (tree? "{10*10}")))
     (test-case "Wrong tree 5" (check-false (tree? "}{")))
     (test-case "Wrong tree 6" (check-false (tree? "***")))
    )
   )
)
     