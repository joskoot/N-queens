#lang racket/base
(provide queens)
(require (only-in 2htdp/image square rectangle star overlay text/font))
(require (only-in racket set-member? set-union set remove-duplicates count))

#|――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

N-queens

Find all solutions of placing N queens on a NxN chess board, such that no queen attacks another one.
The rows of a chess board are called 'ranks', the columns are called 'files'. A chess player numbers
ranks from 1 up to and including N. The files are identified by the letters 'a', 'b', 'c', etc.
In the present program both files and ranks are identified by numbers 0 up to but not including N.
A solution has exactly one queen in every file and in every rank. Therefore a solution can be written
as a list (rank[0] .. rank[N-1]) where rank[i] indicates which rank of file i contains its queen.
Solutions are found by placing queens in non attacked ranks of successive files. (rank[0].. ank[K-1])
with K<N-1 is a partial solution. Given a list of partial solutions, a new list of partial solutions
is made with one more file. For each old partial solution every rank of the next file is tried.
In the program new ranks are consed to partial solutions, which means that the solutions are reversed,
but this is irrelevant, for the list of full solutions includes the reverse of each solution, reversal
corresponding to reflection in the center line parallel to files. For N<4 the solutions are trivial
and are returned immediately. For N≥4 we have the following nested loops:

1. extend-solutions: Start with all partial solutions of one file: ((0) (1) .. (N-1)). In every cycle
   the list of partial solutions of f files is replaced by a new list of partial solutions of f+1
   files. When f reaches N, the partial solutions are the full solutions.

2. partial-solution-loop: For each old partial solution find all partial solutions with one file
   added. Given a partial solution (rank[f] .. rank[0]) new partial solutions are made by consing a
   rank such as to obtain (rank[f+1] .. rank[0]).

3. rank-loop: For each old partial solution try every rank in the next file.

4. safe? and attack?: Within rank-loop each rank must be tested not being attacked. This involves a
   short loop cycling over the length of the old partial solution.

Loop 2 may have many cycles, even many more than the final number of full solutions. The other loops
have N or less cycles. After all solutions have been found, the list of solutions is divided in
sublists of symmetrically equivalent solutions. Such a sublist is an equivalence-class.

No speeding up strategies are used. There are two of them:

1. By placing queens only in the lower half of the first file and after all solutions have been found
   adding the reflections of all solutions in the center line parallel to ranks.

2. Also by placing queens in the first file or first few files and for each such partial solution,
   parallely computing the full solutions that can be generated from them.

It is possible that a partial solution already attacks all ranks in one or more of the remaining
files and therefore can be rejected immediately. This is done in extend-solutions-with-look-ahead,
which is used when argument look-ahead is true. This can reduce the number of cycles in the partial-
solution-loop. However, I guess that checking for each partial solution that there is a free rank in
all files yet to come does not increase speed. I checked for N=12, 13, 14 and 15 and found the maximum
number of partial solutions to be halved but execution time doubled. 

WARNING
The number of solutions grows very fast with increasing N, even when counting symmetrically equivalent
solutions once only. Symmetrical equivalence is an equivalence relation defining equivalence classes.

――――――――――――――――――――
 N solutions classes
―――――――――――――――――――― 
 0         1       1
 1         1       1
 2         0       0
 3         0       0
 4         2       1
 5        10       2
 6         4       1
 7        40       6
 8        92      12
 9       352      46
10       724      92
11      2680     341
12     14200    1787
13     73712    9233
14    365596   45752
15   2279184  285053
――――――――――――――――――――

See https://oeis.org/A000170 and https://oeis.org/A002562 for more values.

――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――|#

(define (queens N (show-solutions? #t) (whole-classes? #t) (show-boards? #t) (look-ahead? #f))

 (define (find-all-solutions)
  (case N
   ((0) '(()))
   ((1) '((0)))
   ((2 3) '())
   (else
    ; Start with all N partial solutions of one file, id est ((0) (1) (2) (N-1)).
    ((if look-ahead? extend-solutions-with-look-ahead extend-solutions) 1 (build-list N list)))))

;(define n 0)

 (define (extend-solutions file old-partial-solutions)
; (set! n (max n (length old-partial-solutions)))
  (cond
   ; File loop. With file=N we have all full solutions and we can return them.
   ((= file N) old-partial-solutions)
   ; Otherwise we have to find all partial solutions with one more file.
   ; New solutions are accumulated in list new-partial-solutions.
   ; At the end of a cycle new-partial-solutions become old-partial-solutions.
   ; Variable new-partial-solutions is used consequently to gather new solutions.
   ; It passed to and returned from each nested loop.
   (else
    (let partial-solution-loop
     ((old-partial-solutions old-partial-solutions) (new-partial-solutions '()))
     (cond
      ; After all old partial solutions have been done, return to the file-loop.
      ((null? old-partial-solutions) (extend-solutions (add1 file) new-partial-solutions))
      ; Otherwise find new partial solutions extending the old partial solution with one file.
      (else
       (let
        ((old-partial-solution (car old-partial-solutions))
         (old-partial-solutions (cdr old-partial-solutions)))
        ; Add new partial solutions 
        (let rank-loop ((rank 0) (new-partial-solutions new-partial-solutions))
         (cond
          ; After all ranks have been tried, return to partial-solution-loop.
          ((= rank N) (partial-solution-loop old-partial-solutions new-partial-solutions))
          ; Try next rank in the file to be added.
          ((safe? rank file old-partial-solution (sub1 file))
           ; Continue rank-loop with a new partial solution added.
           (rank-loop (add1 rank) (cons (cons rank old-partial-solution) new-partial-solutions)))
          (else ; Continue rank-loop without adding a new partial solution.
           (rank-loop (add1 rank) new-partial-solutions)))))))))))

 (define (extend-solutions-with-look-ahead file old-partial-solutions)
; (set! n (max n (length old-partial-solutions)))
  (cond
   ; File loop. With file=N we have all full solutions and we can return them.
   ((= file N) old-partial-solutions)
   ; Otherwise we have to find all partial solutions with one more file.
   ; New solutions are accumulated in list new-partial-solutions.
   ; At the end of a cycle new-partial-solutions become old-partial-solutions.
   ; Variable new-partial-solutions is used consequently to gather new solutions.
   ; It passed to and returned from each nested loop.
   (else
    (let partial-solution-loop
     ((old-partial-solutions old-partial-solutions) (new-partial-solutions '()))
     (cond
      ; After all old partial solutions have been done, return to the file-loop.
      ((null? old-partial-solutions)
       (extend-solutions-with-look-ahead (add1 file) new-partial-solutions))
      ; Otherwise find new partial solutions extending the old partial solution with one file.
      (else
       (let ((old-partial-solution (car old-partial-solutions)))
        ; Add new partial solutions 
        (let rank-loop ((rank 0) (new-partial-solutions new-partial-solutions))
         (cond
          ; After all ranks have been tried, return to partial-solution-loop.
          ((= rank N) (partial-solution-loop (cdr old-partial-solutions) new-partial-solutions))
          ; Try next rank in the file to be added.
          ((safe? rank file old-partial-solution (sub1 file))
           (define new-solution (cons rank old-partial-solution))
           (cond
            ((has-future? (add1 file) new-solution)
             ; Continue rank-loop with a new partial solution added.
             (rank-loop (add1 rank) (cons new-solution new-partial-solutions)))
             ; Continue rank-loop without adding a new partial solution.
            (else (rank-loop (add1 rank) new-partial-solutions))))
          (else ; Continue rank-loop without adding a new partial solution.
           (rank-loop (add1 rank) new-partial-solutions)))))))))))

 (define (has-future? file solution) ; Check that each remaining file has at least one free rank.
  (for/and ((f (in-range (add1 file) N)))
   (for/or ((rank range-N)) (safe? rank f solution file))))

 (define (safe? new-rank new-file old-solution file-of-first-element-of-old-solution)
  (cond
   ((null? old-solution))
   ((attacks? new-file new-rank file-of-first-element-of-old-solution (car old-solution)) #f)
   (else (safe? new-rank new-file (cdr old-solution) (sub1 file-of-first-element-of-old-solution)))))

 (define (attacks? new-file new-rank old-file old-rank)
  (or ; No need to check new-file ≠ old-file, for always new-file > old-file.
   (= new-rank old-rank)
   (= (+ new-file new-rank) (+ old-file old-rank))
   (= (- new-file new-rank) (- old-file old-rank))))
 
 (define (make-classes)
  ; Take all symmetrical solutions of a solution. They form a class. Variable done contains a set of
  ; all solutions already in a registered class. It protects against duplicate classes and also
  ; avoids repeated compution of classes from solutions already registered in a class.
  (define setequal set)
  (define classes
   ; Important: use setequal such that set-member? correctly recognizes solutions already registered.
   ; This is required because find-symmetrically-equivalent-solutions returns a list of solutions
   ; not necessarily eqv? (let alone eq?) to one of the list of all solutions.
   (for/fold ((done (setequal)) (classes '()) #:result classes)
    ((solution (in-list all-solutions)) #:when (not (set-member? done solution)))
    (define symmetrically-equivalent-solutions (find-symmetrically-equivalent-solutions solution))
    (values
     (set-union done (apply setequal symmetrically-equivalent-solutions))
     (cons symmetrically-equivalent-solutions classes))))
  ; Sorting is not necessary, but takes little time.
  (sort
   (for/list ((class (in-list classes)))
    (sort class solution<?))
    class<?))
 
 (define (class<? class1 class2)
  (or (< (length class1) (length class2))
   (and (= (length class1) (length class2)) (solution<? (car class1) (car class2)))))
 
 (define (solution<? solution-1 solution-2)
  (cond
   ((null? solution-1) #f)
   ((< (car solution-1) (car solution-2)))
   ((and (= (car solution-1) (car solution-2)) (solution<? (cdr solution-1) (cdr solution-2))))
   (else #f)))

 (define (find-symmetrically-equivalent-solutions solution)
  (define (apply-symmetry-operation symmetry-operation) (symmetry-operation solution))
  ; When a solution has symmetries, not all symmetry operations produce distinct solutions.
  (remove-duplicates (cons solution (map apply-symmetry-operation symmetry-operations))))

 (define (find-nr-of-classes-of-given-size)
  ; Use the fact that the size of a class is either 1, 2, 4 or 8.
  (let loop ((class-size '(1 2 4 8)) #;(done 0))
   (cond
    ((null? class-size) #;(unless (= done (apply + (map length classes))) (error "check fails")) '())
    (else
     (define k (car class-size))
     (define n (count (λ (class) (= (length class) k)) classes))
     (if (zero? n)
      (loop (cdr class-size) #;done)
      (cons (cons n k) (loop (cdr class-size) #;(+ (* n k) done))))))))
 
 (define (make-symmetry-operations)
  (define w (make-vector N))
  (define (reverse-file x) (- N x 1))
  (define Sv reverse)                      ; Reflection in vertical center line.
  (define (Sh x) (map reverse-file x))     ; Reflection in horizontal center line.
  (define (Sd1 x)                          ; Reflection in diagonal (0 0) to (N-1 N-1).
   (define v (list->vector x))
   (for ((file range-N)) (vector-set! w (vector-ref v file) file))
   (vector->list w))
  (define R   (compose Sv Sd1))            ; Clockwise rotation about 90°.
  (define R2  (compose Sh Sv))             ; Clockwise rotation about 180°.
  (define R3  (compose Sh Sd1))            ; Clockwise rotation about 270°.
  (define Sd2 (compose R2 Sd1))            ; Reflection in diagonal (0 N-1) to (N-1 0).
  (list #;identity R R2 R3 Sh Sv Sd1 Sd2)) ; We don't need the identity.

 (define (find-symmetries class-size)
  (case class-size ; The symmetry operations that produce equal solutions within the class.
   ; The list of symmetries is fully defined by the number of elements in the class.
   ((1) '(E R R2 R3 Sv Sh Sd1 Sd2))
   ((2) '(E R R2 R3))
   ((4) '(E R2))
   ((8) '(E))
   (else (error "wrong-class-size"))))
 
 (define (find-generators class-size)
  (case class-size ; The symmetry operations that produce distinct solutions within the class.
   ; The list is fully defined by the number of elements in the class.
   ((1) '(E))
   ((2) '(E Sv=Sh=Sd1=Sd2))
   ((4) '(E R=R3 Sv=Sh Sd1=Sd2))
   ((8) '(E R R2 R3 Sv Sh Sd1 Sd2))
   (else (error "wrong-class-size"))))
 
 (define (print-results)
  (printf " ~n ~n")
  (print-border)
  (printf "N : ~a~n" N)
  (printf "Number of solutions : ~a~n" nr-of-solutions)
  (printf "Number of classes : ~a~n" nr-of-classes)
  (printf "Class sizes: ~s~n" nr-of-classes-of-given-size)
; (printf "max nr of partial-solutions: ~s~n" n)
  (when show-solutions?
   (for ((class (in-list classes)) (n (in-naturals 1)))
    (define class-size (length class))
    (printf " ~nClass ~s~n~
             Symmetries: ~s~n~
             Generators: ~s~n~
             Nr of symmetrically equivalent solutions: ~s~n"
     n (find-symmetries class-size) (find-generators class-size) class-size)
    (cond
     (show-boards?
      (newline)
      (print-board (car class))
      (when whole-classes? (for ((sol (in-list (cdr class)))) (print-board sol))))
     (else
      (printf "~s~n" (car class))
      (when whole-classes? (for ((sol (in-list (cdr class)))) (printf "~s~n" sol)))))))
  (print-border))

 (define (print-board solution)
  (define rank-vector (list->vector solution))
  (for ((n range-N))
   (display
    (overlay
     (text/font (format "~s" (vector-ref rank-vector n)) 17 'black #f 'roman 'normal 'normal #f)
     white-field))
    (display space))
  (cond
   ((zero? N) (printf "0 by 0 board~n"))
   (else
    (newline)
    (for ((n range-N))
     (for ((i range-N))
      (let ((field (if (even? (+ n i N)) yellow-field cyan-field)))
       (display (if (= (vector-ref rank-vector i) (- N n 1)) (overlay queen field) field)))
      (display space))
      (newline))))
   (newline))

 (define border (make-string 75 #\―))
 (define (print-border) (displayln border))
 (define range-N (in-range N))
 (define symmetry-operations (make-symmetry-operations))
 (define all-solutions (find-all-solutions))
 (define classes (make-classes))
 (define nr-of-solutions (length all-solutions))
 (define nr-of-classes (length classes))
 (define nr-of-classes-of-given-size (find-nr-of-classes-of-given-size))
 (define cyan-field   (square 20 "solid" "cyan"))
 (define yellow-field (square 20 "solid" "yellow"))
 (define white-field  (square 20 "solid" "white"))
 (define space   (rectangle 3 20 "solid" "white"))
 (define queen          (star 13 "solid" "black"))
  
 (print-results)
 (list N nr-of-solutions nr-of-classes nr-of-classes-of-given-size #;n))

;----------------------------------------------------------------------------------------------------

;(for ((N (in-range 16))) (time (queens N #f)))
