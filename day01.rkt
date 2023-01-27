#lang racket

(require racket/file)

(define (split-by lst x)
  (foldr (lambda (elm next)
           (if (eqv? elm x)
               (cons empty next)
               (cons (cons elm (first next)) (rest next))))
         (list empty)
         lst))

(define (max-in lst)
  (foldl max (first lst) (rest lst)))

(define *input*
  (split-by (map string->number (file->lines "inputs/day01.txt"))
            #f))
(define *input-elves*
  (map (lambda (elf) (apply + elf)) *input*))

(define (calc-part1)
  (max-in *input-elves*))

(define (calc-part2)
  (let* [(lst1 *input-elves*)
         (e1 (max-in lst1))
         (lst2 (remove e1 lst1))
         (e2 (max-in lst2))
         (lst3 (remove e2 lst2))
         (e3 (max-in lst3))]
    (+ e1 e2 e3)))

;; Uncomment to calcualte immediately on load
; (define *part1* (calc-part1))
; (define *part2* (calc-part2))
