#lang racket

(provide queens)
(require (only-in 2htdp/image square rectangle star overlay text/font))

(define (queens N (show-solutions? #t) (whole-classes? #t) (show-boards? #t))
 
 (define (generate-solutions)
  (let file-loop ((file 0) (occupied-files '()) (old-solutions '(())))
   (cond
    ((= file N) old-solutions)
    ((null? old-solutions) '())
    (else
     (let solution-loop ((old-solutions old-solutions) (new-solutions '()))
      (cond
       ((null? old-solutions) (file-loop (add1 file) (cons file occupied-files) new-solutions))
       (else
        (let ((old-solution (car old-solutions)) (old-solutions (cdr old-solutions)))
         (let rank-loop ((rank 0) (new-solutions new-solutions))
          (cond
           ((= rank N) (solution-loop old-solutions new-solutions))
           ((for/or ((old-rank (in-list old-solution)) (old-file (in-list occupied-files)))
             (attacks? old-file old-rank file rank))
            (rank-loop (add1 rank) new-solutions))
           (else (rank-loop (add1 rank) (cons (cons rank old-solution) new-solutions)))))))))))))

 (define (attacks? file-a rank-a file-b rank-b)
  (or
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
   ((2) '(E Sv=Sh=Sd2=Sd2))
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
