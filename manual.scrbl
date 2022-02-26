#lang scribble/manual

@(require racket "queens.rkt" "scribble-utensils.rkt"
  (for-label "queens.rkt" racket)
  (for-template "queens.rkt" racket)
  (for-syntax racket))

@title[#:version ""]{N-queens}
@author{Jacob J. A. Koot}
@(Defmodule)

@section{Procedure N-queens}

@defproc[
(queens
 (N natural?)
 (show-solutions? any/c #t)
 (whole-classes? any/c #t)
 (show-boards? any/c #t))
 (list/c natural? natural? natural? (listof n*s))]{

Computes all solutions of the @nbr[N]-queens problem on an @nbr[N]×@nbr[N] board and
shows the number of solutions and the number of symmetrically distinct solutions.

If optional argument @racket[show-solutions?] is @nbr[#f],
no solutions are shown and the other two optional arguments are ignored.
If @racket[show-solutions?] is not @nbr[#f], and argument @racket[whole-classes?] is @nbr[#f],
one solution of every class of symmetrically equivalent solutions is printed.
If both @racket[show-solutions?] and @racket[whole-classes?] have true value,
all solutions are printed.
If @racket[show-boards?] is not @nbr[#f],
solutions are printed as pictures, otherwise they are printed as lists.

A rank is a row of the board. A file a column.
Chess players number the ranks from 1 up to and including @nbr[N] and files by the letters
'a', 'b', 'c', etc. 
However, when programming it is easier to identify ranks and files by the numbers
@nbr[0] up to but not including @nbr[N].
In every solution every rank and every file contains exactly one queen.
Therefore a solution can be written as:

@inset{@tt{(rank@↓{0} .. rank@↓{@nbr[N]-1})}}

the i-th element showing which rank in file i contains the queen.
Reversal of this list corresponds to reflection in the centerline parallel to the files.
The list can also be read as:

@inset{@tt{(file@↓{0} .. file@↓{@nbr[N]-1})}}

the i-th element showing which file in rank i contains the queen.
The difference between these two interpretations corresponds to
a reflection in the diagonal from field @tt{(0 0)} to field @tt{(@nbr[N]-1 @nbr[N]-1)}.
Reversal of this list of files corresponds to reflection in the centerline parellel to the ranks.

Solutions are sorted according to the order of their class of symmetrical
equivalence. Within each such class they are sorted according to their representation
as a list of ranks or a list of files.

For @nbr[N]=@nbr[0] and @nbr[N]=@nbr[1] there is one solution with all symmetries of a square.
For @nbr[N]=@nbr[2] and @nbr[N]=@nbr[3] there are no solutions.
For @nbr[N]≥@nbr[4] there always is more than one solution and the following list is returned:

@inset{@tt{(@nbr[N] nr-of-solutions nr-of-classes (n*s ...))}}

where @tt{s} is @nbr[2], @nbr[4] or @nbr[8] and @tt{n*s} indicates
that there are @tt{n} classes of size @tt{s}.
A class is a set of distinct but symmetrically equivalent solutions.
@nbr[(+ n ...)] always equals the number of all classes.
@nbr[(+ (* n s) ...)] always is the total number of solutions.
The returned data are printed too.

@section{Symmetries}

A square has 8 symmetries. Consider the square:

@inset{@tt{
   3 ―――― 2@(lb)
   │@(hspace 6)│@(lb)
   │@(hspace 6)│@(lb)
   0 ―――― 1}}

The symmetries are:

@inset{@Tabular[
(("v →" "0" "1" "2" "3" " ")
 ("       E(v)" "0" "1" "2" "3" "identity")
 ("       R(v)" "1" "2" "3" "0" "clockwise rotation over 90°")
 ((list " R"@↑{2}"(v)") "2" "3" "0" "1" "clockwise rotation over 180°")
 ((list " R"@↑{3}"(v)") "3" "0" "1" "2" "clockwise rotation over 270°")
 ((list " S"@↓{v}"(v)") "1" "0" "3" "2" "reflection in vertical centerline")
 ((list " S"@↓{h}"(v)") "3" "2" "1" "0" "reflection in horizontal centerline")
 ((list "S"@↓{d1}"(v)") "0" "3" "2" "1" "reflection in diagonal 0-2")
 ((list "S"@↓{d2}"(v)") "2" "1" "0" "3" "reflection in diagonal 1-3"))
 #:sep (hspace 2)
 #:row-properties '((top-border bottom-border) ()()()()()()() 'bottom-border)
 #:column-properties '(right center center center left)]}

For a square R@↑{2} is the same as inversion
(placing every vertex to opposit position with respect to the center of the square).
The symmetries form a group, named @italic{C}@↓{4v} in the nomenclature of Schönflies,
4.m in the nomenclature of Shubnikov and 4mm in the International nomenclature.
The table of composition, modulo the order of rows and columns, is:

@inset{@Tabular[
(("E""R"(list"R"@↑{2})(list"R"@↑{3})(list"S"@↓{h})(list"S"@↓{d1})(list"S"@↓{v})(list"S"@↓{d2}))
 ("R"(list"R"@↑{2})(list"R"@↑{3})"E"(list"S"@↓{d1})(list"S"@↓{v})(list"S"@↓{d2})(list"S"@↓{h}))
 ((list"R"@↑{2})(list"R"@↑{3})"E""R"(list"S"@↓{v})(list"S"@↓{d2})(list"S"@↓{h})(list"S"@↓{d1}))
 ((list"R"@↑{3})"E""R"(list"R"@↑{2})(list"S"@↓{d2})(list"S"@↓{h})(list"S"@↓{d1})(list"S"@↓{v}))
 ((list"S"@↓{h})(list"S"@↓{d2})(list"S"@↓{v})(list"S"@↓{d1})"E"(list"R"@↑{3})(list"R"@↑{2})"R")
 ((list"S"@↓{d1})(list"S"@↓{h})(list"S"@↓{d2})(list"S"@↓{v})"R""E"(list"R"@↑{3})(list"R"@↑{2}))
 ((list"S"@↓{v})(list"S"@↓{d1})(list"S"@↓{h})(list"S"@↓{d2})(list"R"@↑{2})"R""E"(list"R"@↑{3}))
 ((list"S"@↓{d2})(list"S"@↓{v})(list"S"@↓{d1})(list"S"@↓{h})(list"R"@↑{3})(list"R"@↑{2})"R""E"))
 #:sep (hspace 2)
 #:row-properties '(top-border () () () () () () bottom-border)]}

Each element is the composition of the element in the left column with that in the top row,
the left column and the top row included. Because the top-left corner has identity E and for
every element P we have E•P=P=P•E, the left column and top row can be read both as labels and
as part of the table proper.
Notice that the composition is not abelean. For example R•S@↓{d1}=S@↓{v} and S@↓{d1}•R=S@↓{h}.
The group can be formed from a base of two elements only. There are 12 minimal bases:

@inset{@Tabular[
((      "{R"       (list "S"@↓{h} "}"))
 (      "{R"       (list "S"@↓{v} "}"))
 (      "{R"       (list "S"@↓{d1}"}"))
 (      "{R"       (list "S"@↓{d2}"}"))
 ((list "{R"@↑{3}) (list "S"@↓{h} "}"))
 ((list "{R"@↑{3}) (list "S"@↓{v} "}"))
 ((list "{R"@↑{3}) (list "S"@↓{d1}"}"))
 ((list "{R"@↑{3}) (list "S"@↓{d2}"}"))
 ((list "{S"@↓{h}) (list "S"@↓{d1}"}"))
 ((list "{S"@↓{h}) (list "S"@↓{d2}"}"))
 ((list "{S"@↓{v}) (list "S"@↓{d1}"}"))
 ((list "{S"@↓{v}) (list "S"@↓{d2}"}"))) #:sep (hspace 2)]}

In the program a base of three elements is used: S@↓{v}, S@↓{h} and S@↓{d1}.
This is done because S@↓{v} and S@↓{h} are fast functions.
All other elements, E excepted, are made by composition with S@↓{d1}, which is slower.

@inset{@Tabular[
(("E" "" "trivial" 'cont 'cont)
 ("R" "=" (list "S"@↓{v}"•S"@↓{d1}) "" "")
 ((list "R"@↑{2}) "=" (list "S"@↓{h}"•S"@↓{v}) "" "")
 ((list "R"@↑{3}) "=" (list "S"@↓{h}"•S"@↓{d1}) "" "")
 ((list "S"@↓{h}) "" "used as base element" 'cont 'cont) 
 ((list "S"@↓{v}) "" "used as base element" 'cont 'cont)
 ((list "S"@↓{d1}) "" "used as base element" 'cont 'cont)
 ((list "S"@↓{d2}) "=" (list "R" @↑{2}"•S"@↓{d1}) "=" (list "S"@↓{h}"•S"@↓{v}"•S"@↓{d1})))
 #:sep (hspace 1)]}

For @nbr[N]≥@nbr[4] a reflection always produces a distinct solution,
but not every reflection necessarily yields a distinct solution.
For example, applied to a solution that is invariant under rotation through 90°,
every reflection yields the same solution.
Id est, if R(s)=s, then S@↓{v}(s)=S@↓{h}(s)=S@↓{d1}(s)=S@↓{d2}(s).

For @nbr[N]=@nbr[0] there is 1 solution with all symmetries
(no queen at all on an empty board)@(lb)
For @nbr[N]=@nbr[1] there is 1 solution with all symmetries
(1 queen on the single field of the board)@(lb)
For @nbr[N]=@nbr[2] there is no solution.@(lb)
For @nbr[N]=@nbr[3] there is no solution.

For @nbr[N]≥@nbr[4] there always is more than one solution.
A solution s@↓{1} is symmetrically equivalent with solution s@↓{2} if s@↓{2}
can be obtained from s@↓{1} by applying a symmetry operation to s@↓{1}.
This defines an equivalence relation.
A solution for @nbr[N]≥@nbr[4] cannot have a reflection symmetry, because every rank, everyfile and
every diagonal can contain one queen only. Remain the following three possibilities:

@inset{   Solution s has symmetry R.
   This implies E(s)=R(s)=R@↑{2}(s)=R@↑{3}(s).
   Hence a rotation does not alter the solution.
   There are 2 equivalent but distinct solutions,
   which can be obtained one from the other by an arbitrary reflection,
   but with R(s)=E(s) we have S@↓{v}(s)=S@↓{h}(s)=S@↓{d1}(s)=S@↓{d2}(s)
   meaning that every reflection produces the same solution.}

@inset{
   Solution s has symmetries E and R@↑{2} only.
   There are 4 equivalent but distinct solutions,
   which can be obtained one from another by R-odd,
   a reflection or an R-odd combined with a reflection,
   where R-odd is R or R@↑{3}.
   With R@↑{2}(s)=E(s), we also have R@↑{3}(s)=R(s), S@↓{h}(s)=S@↓{v}(s) and S@↓{d1}(s)=S@↓{d2}(s).}

@inset{
   Solution s has symmetry E only.
   In common language no symmetry at all.
   There are 8 equivalent but distinct solutions.
   Given a solution s, every symmetry operation P yields a distinct solution P(s),
   E(s) being equal to s, of course.}

Hence, for @nbr[N]≥@nbr[4], a class of equivalent solutions consists of 2, 4 or 8 distinct solutions
and the total number of solutions is even.

@inset{@Tabular[
((" " 'cont 'cont "nr of classes of size " 'cont 'cont 'cont)
 (@nbr[N] "solutions" "classes" "1" "2" "  4" "       8")
 ("0"         "1"       "1" "1" " " "   " "        ")
 ("1"         "1"       "1" "1" " " "   " "        ")
 ("2"         "0"       "0" " " " " "   " "        ") 
 ("3"         "0"       "0" " " " " "   " "        ")
 ("4"         "2"       "1" " " "1" "   " "        ")
 ("5"        "10"       "2" " " "1" "   " "       1")
 ("6"         "4"       "1" " " " " "  1" "        ")
 ("7"        "40"       "6" " " " " "  2" "       4")
 ("8"        "92"      "12" " " " " "  1" "      11")
 ("9"       "352"      "46" " " " " "  4" "      42")
 ("10"      "724"      "92" " " " " "  3" "      89")
 ("11"     "2680"     "341" " " " " " 12" "     329")
 ("12"    "14200"    "1787" " " "4" " 18" "    1765")
 ("13"    "73712"    "9233" " " "4" " 32" "    9197")
 ("14"   "365596"   "45752" " " " " "105" "   45647")
 ("15"  "2279184"  "285053" " " " " "310" "  284743"))
#:column-properties '(right right right right right right right)
#:row-properties
'((top-border bottom-border) 'bottom-border ()()()()()()()()()()()()()()() 'bottom-border)
#:sep (hspace 2)]}

Notice the peculiar small number of solutions for @nbr[N]=@nbr[6],
all in the same class and with symmetry R@↑{2}.
See @url{https://oeis.org/A000170} and @url{https://oeis.org/A002562} for more values.}

@section{Examples}
@Interaction[
(queens 8 #t #f #f)]

@(image "picture-example.gif")