#lang racket

(provide queens)
(require (only-in 2htdp/image square rectangle star overlay text/font))

; The N-queens puzzle
;
; Both files and ranks are identified by the numbers 0 up to but not including N.
; A chess player identifies files by the letters of the alphabet.
; Natural numbers are easier when given a file f we want the next file.
; An encoding in letters is possible, of course, but would slow down the program.
; Keeping record of all attacked fields when placing a queen on the board
; does not accelerate the program and requires much more memory.
; Hence, no such recording is made.

(define (queens N (show-solutions? #t) (whole-classes? #t) (show-boards? #t))
 
 ; A (partial) solution is a list of ranks in reversed order of files.
 ; Hence, the last element of the list is the rank in file 0 (= file a).
 ; Whether the first or the last element of a partial solution corresponds to file 0 is important
 ; in procedure safe? and attacks?. It is a choise, but once made we must stick to it.
 
 (define (generate-solutions partial-N) ; Place queens in files 0 up to but not including partial-N.
  (cond                                 ; With partial-N = N we have a full solution.
   ((zero? partial-N) '(())) ; We want a list of solutions, each solution being a list. Hence (()).
   (else ; Loop for each partial-solution of partial-N files.
    (let loop1
     ((old-partial-solutions (generate-solutions (sub1 partial-N)))
      (new-partial-solutions '()))
     (cond
      ((null? old-partial-solutions) new-partial-solutions)
      (else
       (let
        ((old-partial-solution (car old-partial-solutions))
         (old-partial-solutions (cdr old-partial-solutions)))
        ; Nested loop for each rank in file partial-N.
        (let loop2 ((rank 0) (new-partial-solutions new-partial-solutions))
         (cond
          ((>= rank N) (loop1 old-partial-solutions new-partial-solutions))
          (else
           (cond
            ((safe? old-partial-solution rank partial-N) ; Does nested loop over files.
             (loop2 (add1 rank) (cons (cons rank old-partial-solution) new-partial-solutions)))
            (else (loop2 (add1 rank) new-partial-solutions)))))))))))))

 (define (safe? partial-solution new-rank new-file)
  (let loop ((old-file (sub1 new-file)) (list-of-ranks partial-solution))
   ; Start with old-file = new-file = partial-N, because the choise is made
   ; to interpret a solution as a list of ranks in reversed order of files.
   (or (null? list-of-ranks)
    (let ((old-rank (car list-of-ranks)))
     (and
      (not (attacks? old-file old-rank new-file new-rank))
      (loop (sub1 old-file) (cdr list-of-ranks)))))))
 
 (define (attacks? file-a rank-a file-b rank-b)
  (or
   (= rank-a rank-b)
   (= file-a file-b)
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

 (define all-solutions (generate-solutions N))
 (define classes (make-classes))
 (define nr-of-solutions (length all-solutions))
 (define nr-of-classes (length classes))
 
 (define (print-board lst)
  (define rank-vector (list->vector lst))
  (define n (vector-length rank-vector))
  (define cyan-field (square 20 "solid" "cyan"))
  (define yellow-field (square 20 "solid" "yellow"))
  (define white-field (square 20 "solid" "white"))
  (define space (rectangle 3 20 "solid" "white"))
  (define queen (star 13 "solid" "black"))
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
  (printf line)
  (printf "N : ~a~n" N)
  (printf "Number of solutions : ~a~n" nr-of-solutions)
  (printf "Number of classes : ~a~n" nr-of-classes)
  (printf "Class sizes: ~s~n" (nr-of-classes-of-given-size))
  (when show-solutions?
   (for ((class (in-list classes)) (n (in-naturals 1)))
    (define nr (length class))
    (printf "~nClass ~s, symmetries ~s, nr of symmetrically equivalent solutions: ~s~n"
     n (find-symmetries nr) nr)
    (cond
     (show-boards?
      (newline)
      (print-board (car class))
      (when whole-classes? (for ((sol (in-list (cdr class)))) (print-board sol))))
     (else
      (printf "~s~n" (car class))
      (when whole-classes? (for ((sol (in-list (cdr class)))) (printf "~s~n" sol)))))))
  (printf line))

 (define line (string-append (make-string 75 #\―) "~n"))

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

