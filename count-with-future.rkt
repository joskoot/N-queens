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
for N>3 is set to (ceiling (inexact->exact (/ (log (processor-count)) (log N)))). This avoids
parallelization when no more free processors are available. Running much more futures than processors
available may slow down the computation and may require more memory than necessary. The program does
not require much memory, for it does not need to memorize many partial solutions. The number of
partial solutions in memory never exceeds N times the number of futures. Factor N stems from the
recursion over files.

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
       (future (λ () (N-queens-count (cons rank partial-solution) (add1 current-nr-of-files))))))
     (apply + (for/list ((f (in-list futures))) (touch f))))
    (else
     (apply +
      (map
       (λ (rank)
        (if (safe? rank current-nr-of-files partial-solution)
         (N-queens-count (cons rank partial-solution) (add1 current-nr-of-files))
         0))
       (range N))))))
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

;―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

(require 'N-queens-count-with-futures)
(define-syntax-rule (timer expr)
 (let*
  ((starting-time (current-inexact-milliseconds))
   (value expr))
  (printf "~s~n" (- (current-inexact-milliseconds) starting-time))
  value))
(processor-count)
(futures-enabled?)
(for ((N (in-range 16))) (printf "~s ~s~n" N (time (N-queens-count N))))

