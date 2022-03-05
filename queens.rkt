#lang racket/base

(provide queens)

(require (only-in 2htdp/image square rectangle star overlay text/font))
(require (only-in racket set-member? set-union set remove-duplicates count))

#|

N-queens

Find all solutions of placing N queens on a NxN board, such that no queen attacks another queen.
Files and ranks will be identified by numbers 0 up to but not including N.
A solution has exactly one queen in every file (and in every rank).
Therefore a solution can be written as a list (rank[N-1] .. rank[0])
where rank[i] indicates which rank of file i contains its queen.
Solutions are made by placing queens in non attacked ranks of successive files.
(rank[K] .. rank[0]) with K<N-1 is a partial solution.
Given a list of partial solutions, a new list of partial solutions is made.
For each old partial solution every rank of the next file is tried.
For N<4 the solutions are returned immediately.
For N≥4 we have the following nested loops:

  1. file-loop
     Start with all partial solutions of one file: ((0) (1) .. (N-1)).
     In every cycle the list of partial solutions of f files
     is replaced by the list of partial solutions of f+1 files.
     When f reaches N, the partial solutions are the full solutions.

  2. partial-solution-loop
     For each known partial solution find all partial solutions with one file added.

  3. rank-loop
     For each known partial solution try every rank in the next file.

  4. safe? and attack?
     Within rank-loop the rank must be tested not being attacked.
     This involves a short loop cycling over the length the known partial solution.

Loop 2 may have many cycles.
The other loops have N or less cycles.
After all solutions have been found,
the list of solutions is divided in sublists of symmetrically equivalent solutions.
A sublist of symmetrically equivalent solutions is an equivalence-class.

No speeding up strategies are used. There are two of them:

  1. By placing queens only in the lower half of the first file
     and after all solutions have been found
     adding the reflections of all solutions in the horizontal center line
     (regarding ranks horizontal, and files vertical).

  2. Also by placing queens in the first file or first few files
     and for each such partial solution,
     parallely computing the full solutions that can be generated from them.

For each partial solution keeping record of attacked fields in files yet to be added
does not increase speed and would require much more memory.

WARNING
The number of solutions grows very fast with increasing N:

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

|#

(define (queens N (show-solutions? #t) (whole-classes? #t) (show-boards? #t))
 
 (define (generate-solutions)
  (case N
   ((0) '(()))
   ((1) '((0)))
   ((2 3) '())
   (else
    (let file-loop ((file 1) (known-partial-solutions (build-list N list)))
    (cond
     ((= file N) known-partial-solutions) ; With file=N we have all full solutions. Exit file-loop.
     (else
      (let partial-solution-loop
       ((known-partial-solutions known-partial-solutions) (new-partial-solutions '()))
       (cond
        ((null? known-partial-solutions) ; Exit solution-loop and continue file-loop.
         (file-loop (add1 file) new-partial-solutions))
        (else
         (let ((known-partial-solution (car known-partial-solutions)))
          (let rank-loop ((rank 0) (new-partial-solutions new-partial-solutions))
           (cond
            ((= rank N) ; Exit rank-loop and continue solution-loop.
             (partial-solution-loop (cdr known-partial-solutions) new-partial-solutions))
            ((safe? rank file known-partial-solution (sub1 file))
             ; Continue rank-loop with a new partial solution added.
             (rank-loop (add1 rank) (cons (cons rank known-partial-solution) new-partial-solutions)))
            (else ; Continue rank-loop without adding a new partial solution.
             (rank-loop (add1 rank) new-partial-solutions))))))))))))))

 (define (safe? rank file solution file-of-first-element-of-solution)
  (cond
   ((null? solution))
   ((attacks? file rank file-of-first-element-of-solution (car solution)) #f)
   (else (safe? rank file (cdr solution) (sub1 file-of-first-element-of-solution)))))

 (define (attacks? file-a rank-a file-b rank-b)
  (or ; No need to check file-a ≠ file-b, for always file-a > file-b.
   (= rank-a rank-b)
   (= (+ file-a rank-a) (+ file-b rank-b))
   (= (- file-a rank-a) (- file-b rank-b))))
 
 (define (make-classes)
  (define classes
   (for/fold ((done (set)) (classes '()) #:result classes)
    ((solution (in-list all-solutions)) #:when (not (set-member? done solution)))
    (define symmetrically-equivalent-solutions (find-symmetrically-equivalent-solutions solution))
    (values
     (set-union done (apply set symmetrically-equivalent-solutions))
     (cons symmetrically-equivalent-solutions classes))))
  (sort
   (for/list ((class (in-list classes)))
    (sort class solution<?))
    class<?))
 
 (define (find-symmetrically-equivalent-solutions solution)
  (define (apply-symmetry-operation symmetry-operation) (symmetry-operation solution))
  (remove-duplicates (cons solution (map apply-symmetry-operation symmetry-operations))))

 (define (find-symmetries class-size)
  (case class-size
   ((1) '(E R R2 R3 Sv Sh Sd1 Sd2))
   ((2) '(E R R2 R3))
   ((4) '(E R2))
   ((8) '(E))
   (else (error "wrong-c-size"))))
 
 (define (find-generators class-size)
  (case class-size
   ((1) '(E))
   ((2) '(E Sv=Sh=Sd1=Sd2))
   ((4) '(E R=R3 Sv=Sh Sd1=Sd2))
   ((8) '(E R R2 R3 Sv Sh Sd1 Sd2))
   (else (error "wrong-c-size"))))
 
 (define (make-symmetry-operations)
  (define w (make-vector N))
  (define (reverse-file x) (- N x 1))
  (define Sh reverse)
  (define (Sv x) (map reverse-file x))
  (define (Sd1 x)
   (define v (list->vector x))
   (for ((file (in-range N))) (vector-set! w (vector-ref v file) file))
   (vector->list w))
  (define R   (compose Sv Sd1))
  (define R2  (compose Sh Sv))
  (define R3  (compose Sh Sd1))
  (define Sd2 (compose R2 Sd1))
  (list #;identity R R2 R3 Sh Sv Sd1 Sd2)) ; We don't need the identity.

 (define symmetry-operations (make-symmetry-operations))

 (define (class<? class1 class2)
  (or (< (length class1) (length class2))
   (and (= (length class1) (length class2)) (solution<? (car class1) (car class2)))))
 
 (define (solution<? solution-1 solution-2)
  (cond
   ((null? solution-1) #f)
   ((< (car solution-1) (car solution-2)))
   ((and (= (car solution-1) (car solution-2)) (solution<? (cdr solution-1) (cdr solution-2))))
   (else #f)))

 (define all-solutions (generate-solutions))
 (define classes (make-classes))
 (define nr-of-solutions (length all-solutions))
 (define nr-of-classes (length classes))
 
 (define (print-board solution)
  (define rank-vector (list->vector solution))
  (define n (vector-length rank-vector))
  (define cyan-field   (square 20 "solid" "cyan"))
  (define yellow-field (square 20 "solid" "yellow"))
  (define white-field  (square 20 "solid" "white"))
  (define space   (rectangle 3 20 "solid" "white"))
  (define queen          (star 13 "solid" "black"))
  (for ((k (in-range 0 n)))
   (display
    (overlay
     (text/font
      (format "~s" (vector-ref rank-vector k))
      17
      'black
      #f
      'roman
      'normal
      'normal
      #f)
     white-field))
    (display space))
  (cond
   ((zero? n) (printf "0 by 0 board~n"))
   (else
    (newline)
    (for ((k (in-range 0 n)))
     (for ((i (in-range 0 n)))
      (let ((field (if (even? (+ k i n)) yellow-field cyan-field)))
       (display (if (= (vector-ref rank-vector i) (- n k 1)) (overlay queen field) field)))
      (display space))
      (newline))))
   (newline))

 (define (print-results)
  (printf "~n~n")
  (print-line)
  (printf "N : ~a~n" N)
  (printf "Number of solutions : ~a~n" nr-of-solutions)
  (printf "Number of classes : ~a~n" nr-of-classes)
  (printf "Class sizes: ~s~n" (nr-of-classes-of-given-size))
  (when show-solutions?
   (for ((class (in-list classes)) (n (in-naturals 1)))
    (define nr (length class))
    (printf "~nClass ~s~n~
             Symmetries: ~s~n~
             Generators: ~s~n~
             Nr of symmetrically equivalent solutions: ~s~n"
     n (find-symmetries nr) (find-generators nr) nr)
    (cond
     (show-boards?
      (newline)
      (print-board (car class))
      (when whole-classes? (for ((sol (in-list (cdr class)))) (print-board sol))))
     (else
      (printf "~s~n" (car class))
      (when whole-classes? (for ((sol (in-list (cdr class)))) (printf "~s~n" sol)))))))
  (print-line))

 (define line (make-string 75 #\―))
 (define (print-line) (displayln line))

 (define (nr-of-classes-of-given-size)
  (let loop ((class-size '(1 2 4 8)) (done 0))
   (cond
    ((null? class-size) (unless (= done (apply + (map length classes))) (error "check fails")) '())
    (else
     (define k (car class-size))
     (define n (count (λ (class) (= (length class) k)) classes))
     (if (zero? n)
      (loop (cdr class-size) done)
      (cons (cons n k) (loop (cdr class-size) (+ (* n k) done))))))))
 
 (print-results)
 (list N nr-of-solutions nr-of-classes (nr-of-classes-of-given-size)))

(for/list ((N (in-range 16))) (time (queens N #f)))
