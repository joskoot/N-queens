#lang racket

#|――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

Count all solutions of placing N queens on a NxN chess board such that no queen attacks another one.

Number ranks and files from 0 up to but not including N. Every file and every rank can have one queen
only. Define a partial solution for K<N as a list (rank[K-1] .. rank[0]) indicating that file i has
its queen in rank[i] and such that no queen attacks another queen. Having such a partial solution,
count the number of full solutions it can be extended to. A solution is extended by consing a non
attacked rank to the partial solution. It may happen that there is no such rank. In that case the
partial solution yields count 0. If there are one or more free ranks, count for each extended solution
and add the counts. A partial solution with K=N is a full solution and counts as 1. 

The computation can easily be parallelized because counting the number of full solutions can be done
independently for each partial solution and for each rank it is extended with. In the program below
counting is parallelized by means of futures for the first nr-of-files-to-be-parallelized files, which
for N>3 is set to (ceiling (inexact->exact (/ (log (processor-count)) (log N)))). This avoids further
parallelization when no more free processors are available. Running much more futures than processors
available may slow down the computation and may require more memory than necessary. The program does
not require much memory, though, for it does not need to memorize many partial solutions. The number
of partial solutions in memory never exceeds N times the number of futures. Factor N stems from the
recursion progressing through subsequent files.

By Jacob J. A. Koot

――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――|#

(module N-queens-count-with-futures racket

 (provide N-queens-count)

 (define (N-queens-count N)
  (define nr-of-files-to-be-parallelized
   (and (> N 3)
    (ceiling (inexact->exact (/ (log (processor-count)) (log N))))))
  (define (N-queens-count partial-solution current-nr-of-files)
   (cond
    ((= current-nr-of-files N) 1)
    ((< current-nr-of-files nr-of-files-to-be-parallelized)
     (define futures
      (for/list ((rank (in-range N)) #:when (safe? rank current-nr-of-files partial-solution))
       (future (λ () (add-rank rank partial-solution current-nr-of-files)))))
     (apply + (map touch futures)))
    (else
     (apply +
      (map
       (λ (rank)
        (if (safe? rank current-nr-of-files partial-solution)
         (add-rank rank partial-solution current-nr-of-files)
         0))
       (range N))))))
  (define (add-rank rank partial-solution current-nr-of-files)
   (N-queens-count (cons rank partial-solution) (add1 current-nr-of-files)))
  (case N
   ((0 1) 1)
   ((2 3) 0)
   (else (N-queens-count '() 0))))

 (define (safe? rank current-nr-of-files partial-solution)
  (not
   (for/or ((file (in-range (sub1 current-nr-of-files) -1 -1)) (sol-rank (in-list partial-solution)))
    (attacks? rank current-nr-of-files sol-rank file))))

 (define (attacks? rank1 file1 rank2 file2)
  (or
   (= rank1 rank2)
   (= (+ rank1 file1) (+ rank2 file2))
   (= (- rank1 file1) (- rank2 file2)))))

#|――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

Install fmt/fmt from https://github.com/joskoot/fmt.git.

The number of solutions is computed for N from 0 up to and including max-N. Shown are:

   First line  : number of processors available.
   Second line : #t if furures are enabled.
   A table showing:

      N               : board size
      count           : number of solutions
      cpu             : cpu time in ms
      real            : real time in ms
      ratio           : count[N]/count[N-1] (na for N=0 or count[N-1]=0)
      parallelization : cpu/real (na real=0)

It may happen that some futures have terminated while others still are running. The still running ones
could initiate futures in order to use the free processors, but this is not implemented. 

|#

(require 'N-queens-count-with-futures fmt/fmt)
(require (only-in racket/gui/base get-text-from-user))

(processor-count)
(futures-enabled?)
;(define max-N 17)
((fmt 'cur "R2'N'2XR8'count'2XR8'cpu'2XR7'real'2XR5'ratio'2XL'parallelization'/"))

(define max-N
 (string->number
  (get-text-from-user "N-queens"
   (string-append
    "Up to which N (included) do you want to compute the number of solutions?\n"
    "Enter an exact non-negative integer.\n"
    "Be aware that a large N may take much time."))))

(unless (exact-nonnegative-integer? max-N)
 (error 'dialog "exact-nonnegative-integer expected, but given: ~s" max-N))

(for/fold ((prev-count 0) #:result (void)) ((N (in-range (add1 max-N))))
 (define-values (wrapped-count cpu real gc)
  (time-apply N-queens-count (list N)))
 (define current-count (car wrapped-count))
 ((fmt 'cur "I2,2XI8,2XI8,2XI7,2XQ(SR5W)(SF5.2)2XQ(SR3W)(SF3.1)/")
  N
  current-count
  cpu
  real
  (zero? prev-count) (if (zero? prev-count) 'na (/ current-count prev-count))
  (zero? real) (if (zero? real) 'na (/ cpu real)))
 current-count)

