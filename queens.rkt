#lang racket

(provide queens)

#|════════════════════════════════════════════════════════════════════════════════════════════════════

procedure: (queens N [show-solutions? whole-classes? show-boards?])
           -> (list/c natural? natural? natural? (listof n*s))

   N               : natural?
   show-solutions? : any/c = #t
   whole-classes?  : any/c = #t
   show-boards?    : any/c = #t

Computes all solutions of the N-queens problem on an NxN board and shows the number of solutions and
the number of symmetrically distinct solutions.

If optional argument show-solutions? is #f, no solutions are shown and the other two optional
arguments are ignored. If show-solutions? is not #f, and arguments whole-classes? is #f,
one solution of every class of symmetrically equivalent solutions is shown.
If both show-solutions? and whole-classes? have true value, all solutions are shown.
If show-boards? is not #f, solutions are shown as pictures, otherwise they are shown as lists.

A rank is a row of the board. A file is a column of the board.
Ranks are identified by the numbers 0 up to but not including N.
Usually files are named 'a', 'b', 'c', etc.
However, it is easier to identify the files by the numbers 0 up to but not including N.
As far as applicable, solutions are sorted according to the order of their class of symmetrical
equivalence. Within each class the solutions are sorted according to their representation.
In every solution every rank and every file contains exactly one queen.
Therefore a solution can be written as:

   ([rank 0] .. [rank N-1])

the i-th element showing which rank in file i contains the queen.
Reversal of this list corresponds to reflection in the centerline parallel to the files.
The list can also be read as:

   ([file 0] .. [file N-1])

the i-th element showing which file in rank i contains the queen.
The difference between these two interpretations corresponds to
a reflection in the diagonal from field (0 0) to field (N-1 N-1).
Reversal of this list corresponds to reflection in the centerline parellel to the ranks.

For N=0 and N=1 there is one solution with all symmetries of a square.
For N=2 and N=3 there are no solutions.
For N≥4 there always is more than one solution and the following list is returned:

   (N nr-of-solutions nr-of-classes (n*s ...))

where s is 2, 4 or 8 and n*s indicates that there are n classes of size s.
A class is a set of distinct but symmetrically equivalent solutions.
(+ n ...) always equals the number of all classes.
(+ (* n s) ...) always is the total number of solutions.
The returned data are printed too.

――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

A square has 8 symmetries. Consider the square:

   3 ―――― 2
   │      │
   │      │
   0 ―――― 1

The symmetries are:

       v →  0  1  2  3
            ――――――――――
     E(v)   0  1  2  3   identity
     R(v)   1  2  3  0   clockwise rotation over 90°
    R2(v)   2  3  0  1   clockwise rotation over 180° (for the square the same as inversion)
    R3(v)   3  0  1  2   clockwise rotation over 270°
    Sv(v)   1  0  3  2   reflection in vertical centerline
    Sh(v)   3  2  1  0   reflection in horizontal centerline
   Sd1(v)   0  3  2  1   reflection in diagonal 0-2
   Sd2(v)   2  1  0  3   reflection in diagonal 1-3
            ――――――――――

The symmetries form a group, named C4v in the nomenclature of Schoenflies, 4.m in the nomenclature of
Shubnikov and 4mm in the International nomenclature.
The table of composition is (modulo permutation of the rows and/or the colums)

   E   R   R2  R3  Sh  Sd1 Sv  Sd2
   R   R2  R3  E   Sd1 Sv  Sd2 Sh
   R2  R3  E   R   Sv  Sd2 Sh  Sd1
   R3  E   R   R2  Sd2 Sh  Sd1 Sv
   Sh  Sd2 Sv  Sd1 E   R3  R2  R
   Sd1 Sh  Sd2 Sv  R   E   R3  R2
   Sv  Sd1 Sh  Sd2 R2  R   E   R3
   Sd2 Sv  Sd1 Sh  R3  R2  R   E

Each element is the composition of the element in the left column with that in the upper row,
the left column and upper row included (because E·s=s for every element s).
Notice that the composition is not abelean. For example R·Sd1=Sv and Sd1·R=Sh.
The group can be formed from a base of two elements only. The minimal bases are:

   {R  Sh }
   {R  Sv }
   {R  Sd1}
   {R  Sd2}
   {R3 Sh }
   {R3 Sv }
   {R3 Sd1}
   {R3 Sd2}
   {Sh Sd1}
   {Sh Sd2}
   {Sv Sd1}
   {Sv Sd2}

In the program a base of three elements is used: Sv, Sh and Sd1.
This is done because Sv and Sh are fast functions and R2=Sh·Sv.
All other elements are made by composition with Sd1, which is slower.

   R   = Sv·Sd1
   R2  = Sh·Sv
   R3  = Sh·Sd1
   Sd2 = R2·Sd1 = Sh·Sv·Sd1

For N≥4 a reflection always produces a distinct solution, but not every reflection necessarily yields
a distinct solution. For example, applied to a solution that is invariant under rotation through 90°,
every reflection yields the same solution. Id est, If R(s)=s, then Sv(s)=Sh(s)=Sd1(s)=Sd2(s).

For N=0 there is one solution with all symmetries (no queen at all on an empty board)
For N=1 there is one solution with all symmetries (one queen on the single field of the board)
For N=2 there is no solution.
For N=3 there is no solution.

For N≥4 there always is more than one solution. A solution s1 is symmetrically equivalent with
solution s2 if s2 can be obtained from s1 by applying a symmetry operation to s1.
This defines an equivalence relation.
A solution for N≥4 cannot have a reflection symmetry, because every rank, everyfile and
every diagonal can contain one queen only. Remain the following three possibilities:

   Solution s has symmetry R.
   This implies E(s)=R(s)=R2(s)=R3(s).
   Hence a rotation does not alter the solution.
   There are 2 equivalent but distinct solutions,
   which can be obtained one from the other by an arbitrary reflection,
   but with R(s)=E(s) we have Sv(s)=Sh(s)=Sd1(s)=Sd2(s)
   meaning that every reflection produces the same solution.
   
   Solution s has symmetries E and R2 only.
   There are 4 equivalent but distinct solutions,
   which can be obtained one from another by R-odd,
   a reflection or an R-odd combined with a reflection,
   where R-odd is R or R3.
   With R2(s)=E(s), we also have R3(s)=R(s), Sh(s)=Sv(s) and Sd1(s)=Sd2(s).

   Solution s has symmetry E only.
   In common language no symmetry at all.
   There are 8 equivalent but distinct solutions.
   Given a solution s, every symmetry operation p yields a distinct solution p(s),
   E(s) being equal to s, of course.

Hence, for N≥4, a class of equivalent solutions consists of 2, 4 or 8 distinct solutions
and the total number of solutions is even.

   ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
                           nr of classes of size (zeros not shown)
   ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
    N  solutions  classes  1  2    4       8
   ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
    0         1        1   1
    1         1        1   1
    2         0        0 
    3         0        0 
    4         2        1      1
    5        10        2      1            1
    6         4        1           1
    7        40        6           2       4
    8        92       12           1      11
    9       352       46           4      42
   10       724       92           3      89
   11      2680      341          12     329
   12     14200     1787      4   18    1765
   13     73712     9233      4   32    9197
   14    365596    45752         105   45647
   15   2279184   285053         310  284743
   ―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

Notice the peculiar small number of solutions for N=6, all in the same class and with symmetry R2.
See https://oeis.org/A000170 and https://oeis.org/A002562 for more values.

════════════════════════════════════════════════════════════════════════════════════════════════════|#

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
   (let ((sol-of-sol (pure-sol part-sol)))
    (let loop ((old-file (sub1 new-file)) (sol-of-sol sol-of-sol))
     ; Start with old-file = new-file = part-N,
     ; because a part-sol lists ranks in reversed order of files.
     (or (null? sol-of-sol)
      (let ((old-rank (car sol-of-sol)))
       (and
        (not (attacks? old-file old-rank new-file new-rank))
        (loop (sub1 old-file) (cdr sol-of-sol)))))))))
 
 (define (attacks? file-a rank-a file-b rank-b)
  (or
   (= rank-a rank-b)
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
  (for/list ((N (in-range 0 15))) (time (queens N #f 'na 'na)))
 '(( 0      1     1 (1*1             ))
   ( 1      1     1 (1*1             ))
   ( 2      0     0 (                ))
   ( 3      0     0 (                ))
   ( 4      2     1 (1*2             ))
   ( 5     10     2 (1*2          1*8))
   ( 6      4     1 (     1*4        ))
   ( 7     40     6 (     2*4     4*8))
   ( 8     92    12 (     1*4    11*8))
   ( 9    352    46 (     4*4    42*8))
   (10    724    92 (     3*4    89*8))
   (11   2680   341 (    12*4   329*8))
   (12  14200  1787 (4*2 18*4  1765*8))
   (13  73712  9233 (4*2 32*4  9197*8))
   (14 365596 45752 (   105*4 45647*8))))
 (printf "~nAll is well.~n")
 (error "Test fails"))

