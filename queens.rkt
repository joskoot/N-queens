#lang racket

(provide queens)
(require (only-in 2htdp/image square rectangle star overlay text/font))

(define (queens N (show-sols? #t) (whole-classes? #t) (show-boards? #t))
 ; A (partial) solution is a list of ranks in reversed order of files.
 ; Hence, the last element of the list is the rank in file 0 (= file a).
 ; 'sol' is short for solution and 'part' is short for partial.
 ; Every partial solution is accompanied by a set of free ranks (in a pair).
 ; Without the set of free ranks we call it a pure sol.
 ; Not every rank in such a set is free, for the set does not account for diagonal attacks.
 ; It accounts for horizontal attacks only, making the set applicable to all remaining files.

 (define pure-sol car)
 (define free-ranks-of-sol cdr)
 (define make-sol cons)
 (define set-of-all-ranks (apply seteq (range N)))
 
 (define (generate-solutions part-N)
  (cond
   ((zero? part-N) (list (make-sol '() set-of-all-ranks)))
   (else ; loop for each part-sol of part-N files.
    (let loop1 ((old-part-sols (generate-solutions (sub1 part-N))) (new-part-sols '()))
     (cond
      ((null? old-part-sols) new-part-sols)
      (else
       (let
        ((old-part-sol (car old-part-sols))
         (old-part-sols (cdr old-part-sols)))
        ; nested loop for each rank in file part-N.
        (let loop2 ((rank 0) (new-part-sols new-part-sols))
         (cond
          ((>= rank N) (loop1 old-part-sols new-part-sols))
          (else
           (cond
            ((safe? old-part-sol rank part-N) ; Nested loop too! over files of the old part-sol.
             ; Notice that in a part-sol ranks are listed in reversed order of files.
             (loop2 (add1 rank)
              (make-sol
               (cons (cons rank (pure-sol old-part-sol))
                (set-remove (free-ranks-of-sol old-part-sol) rank))
               new-part-sols)))
            (else (loop2 (add1 rank) new-part-sols)))))))))))))

 (define (safe? part-sol new-rank new-file)
  (and
   (set-member? (free-ranks-of-sol part-sol) new-rank)
   (let loop ((old-file (sub1 new-file)) (sol-of-sol (pure-sol part-sol)))
    ; Start with old-file = new-file = part-N,
    ; because a part-sol lists ranks in reversed order of files.
    (or (null? sol-of-sol)
     (let ((old-rank (car sol-of-sol)))
      (and
       (not (attacks? old-file old-rank new-file new-rank))
       (loop (sub1 old-file) (cdr sol-of-sol))))))))
 
 (define (attacks? file-a rank-a file-b rank-b)
  (or
 ;  (= rank-a rank-b)
   (= file-a file-b)
   (= (+ file-a rank-a) (+ file-b rank-b))
   (= (- file-a rank-a) (- file-b rank-b))))
 
 (define (make-classes all-sols)
  (define hash (make-hash))
  (for ((sol (in-list all-sols)))
   (define classes (find-symmetrically-distinct-sols sol))
   (let loop ((class classes))
    (cond
     ((null? class) (hash-set! hash (car classes) (apply set classes)))
     ((hash-ref hash (car class) #f))
     (else (loop (cdr class))))))
  (sort
   (for/list ((class (in-hash-values hash)))
    (sort (set->list class) sol<?))
    class<?))
 
 (define (find-symmetrically-distinct-sols sol)
  (define (apply-symmetry-operation symmetry-operation) (symmetry-operation sol))
  (remove-duplicates (cons sol (map apply-symmetry-operation symmetry-operations))))

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
  (list #;identity R R2 R3 Sh Sv Sd1 Sd2))

 (define symmetry-operations (make-symmetry-operations))

 (define (class<? class1 class2)
  (or (< (length class1) (length class2))
   (and (= (length class1) (length class2)) (sol<? (car class1) (car class2)))))
 
 (define (sol<? sol1 sol2)
  (cond
   ((null? sol1) #f)
   ((< (car sol1) (car sol2)))
   ((and (= (car sol1) (car sol2)) (sol<? (cdr sol1) (cdr sol2))))
   (else #f)))

 (define all-sols (map pure-sol (generate-solutions N)))
 (define classes (make-classes all-sols))
 (define nr-of-sols (length all-sols))
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
  (printf "Number of solutions : ~a~n" nr-of-sols)
  (printf "Number of classes : ~a~n" nr-of-classes)
  (printf "Class sizes: ~s~n" (map-length classes))
  (when show-sols?
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

 (define (map-length classes)
  (let loop ((class-size '(1 2 4 8)) (done 0))
   (cond
    ((null? class-size) (unless (= done (apply + (map length classes))) (error "check fails")) '())
    (else
     (define k (car class-size))
     (define n (count (λ (class) (= (length class) k)) classes))
     (if (zero? n)
      (loop (cdr class-size) done)
      (cons (string->symbol (format "~s*~s" n k)) (loop (cdr class-size) (+ (* n k) done))))))))
 
 (print-results)
 (list N nr-of-sols nr-of-classes (map-length classes)))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

#;(if
 (equal?
  (for/list ((N (in-range 0 16))) (time (queens N #f 'na 'na)))
 '(( 0       1      1 (1*1              ))
   ( 1       1      1 (1*1              ))
   ( 2       0      0 (                 ))
   ( 3       0      0 (                 ))
   ( 4       2      1 (1*2              ))
   ( 5      10      2 (1*2           1*8))
   ( 6       4      1 (     1*4         ))
   ( 7      40      6 (     2*4      4*8))
   ( 8      92     12 (     1*4     11*8))
   ( 9     352     46 (     4*4     42*8))
   (10     724     92 (     3*4     89*8))
   (11    2680    341 (    12*4    329*8))
   (12   14200   1787 (4*2 18*4   1765*8))
   (13   73712   9233 (4*2 32*4   9197*8))
   (14  365596  45752 (   105*4  45647*8))
   (15 2279184 285053 (   310*4 284743*8))))
 (printf "~nAll is well.~n")
 (error "Test fails"))

