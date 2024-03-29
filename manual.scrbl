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
 (show-boards? any/c #t)
 (look-ahead? any/c #f))
 (list/c natural? natural? natural? (listof (cons/c natural? natural?)))]{

Computes all solutions of the @nbr[N]-queens problem on an @nbr[N]×@nbr[N] board and
shows the number of solutions and the number of symmetrically distinct solutions.
The result should be read as:
@inset{
@tt{(@nbr[N] @italic{nr-of-solutions} @italic{nr-of-classes} ((@italic{n} . @italic{s}) ...))}}
where @italic{s} is the size of a non-empty class of symmetrically equivalent solutions
and @italic{n} the number of classes of this size.
A class size is 1, 2, 4 or 8 and a size never is mentioned more than once.
Class size 1 is a special case for @nbr[N]=@nbr[0] and @nbr[N]=@nbr[1].
There are no solutions for @nbr[N]=@nbr[2] and @nbr[N]=@nbr[3].
For @nbr[N]≥@nbr[4] all class sizes are even and hence the total number of solutions is even as well.
@inset{@Tabular[
((@tt{(+ @italic{n} ...)} "=" @tt{@italic{nr-of-classes}})
 (@tt{(+ (* @italic{n} @italic{s}) ...)} "=" @tt{@italic{nr-of-solutions}}))
 #:sep (hspace 1) #:column-properties '(right 'left 'left)]}

If optional argument @racket[show-solutions?] is @nbr[#f],
no solutions are shown and the other two optional arguments are ignored.
If @racket[show-solutions?] is not @nbr[#f], and argument @racket[whole-classes?] is @nbr[#f],
one solution of every class of symmetrically equivalent solutions is printed.
If both @racket[show-solutions?] and @racket[whole-classes?] have true value,
all solutions are printed.
@nb{If @racket[show-boards?]} is @nbr[#f],
solutions are printed as lists.
When running with
@hyperlink["https://docs.racket-lang.org/drracket/index.html"]{DrRacket}
 and argument @racket[show-boards?] is not @nbr[#f],
solutions are printed as pictures in the interactions window.
A true value for @racket[show-boards?] makes no sense for output to any other location.

If argument @nbr[look-ahead?] is true, every partial solution is checked to leave at least one rank
free in every remaining files. This reduces memory but increases time. If the argument is @nbr[#f],
partial solutions are rejected when the next file has no free rank, without inspecting the remaining
files.

A rank is a row of the board. A file a column.
Chess players number the ranks from 1 up to and including @nbr[N] and files by the letters
'a', 'b', 'c', etc. 
However, when programming it is easier to identify ranks and files by the numbers
@nbr[0] up to but not including @nbr[N].
In a solution, every rank and every file contains exactly one queen.
Therefore a solution can be written as a list of ranks:

@inset{@tt{(rank@↓{0} .. rank@↓{@nbr[N]-1})}}

the i-th element showing which rank in file i contains the queen.
Reversal of this list corresponds to reflection in the centerline parallel to the files.
The list can also be read as:

@inset{@tt{(file@↓{0} .. file@↓{@nbr[N]-1})}}

the i-th element showing which file in rank i contains the queen.
The difference between these two interpretations corresponds to
a reflection in the diagonal from field @tt{(0 0)} to field @tt{(@nbr[N]-1 @nbr[N]-1)}.
Reversal of a list of files corresponds to reflection in the centerline parellel to the ranks.

Solutions are sorted according to the order of their class of symmetrical
equivalence. Within each such class they are sorted according to their representation
as a list of ranks or a list of files.

For @nbr[N]=@nbr[0] and @nbr[N]=@nbr[1] there is one solution with all symmetries of a square.
For @nbr[N]=@nbr[2] and @nbr[N]=@nbr[3] there are no solutions.
For @nbr[N]≥@nbr[4] there always is more than one solution and the following list is returned:

@inset{@tt{(@nbr[N] @italic{nr-of-solutions} @italic{nr-of-classes} ((@italic{n} . @italic{s}) ...))}}

where @tt{@italic{s}} is @nbr[2], @nbr[4] or @nbr[8] and @tt{(@italic{n} . @italic{s})} indicates
that there are @tt{@italic{n}} classes of size @tt{@italic{s}}.
A class is a set of distinct but symmetrically equivalent solutions.
@tt{(+ @italic{n} ...)} always equals the number of all classes.
@tt{(+ (* @italic{n} @italic{s}) ...)} always is the total number of solutions.
The returned data are printed too.

@section{Symmetries}

A square has 8 symmetries. Consider the square:

@inset{@image["square.gif" #:scale 0.75]}

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
The only two symmetries that commute with all symmetries are E and R@↑{2}; their
columns list the symmetries in the same order as their rows.
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

For @nbr[N]≥@nbr[4], given a solution s and a reflection S, we always have
S(s)≠s, but distinct reflections not necessarily yield distinct solutions.
For example, applied to a solution that is invariant under rotation through 90°,
every reflection yields the same solution, for R(s)=s
implies S@↓{v}(s)=S@↓{h}(s)=S@↓{d1}(s)=S@↓{d2}(s).

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

@itemize[
@item{   Solution s has symmetry R.
   This implies E(s)=R(s)=R@↑{2}(s)=R@↑{3}(s).
   Hence a rotation does not alter the solution.
   There are 2 equivalent but distinct solutions,
   which can be obtained one from the other by an arbitrary reflection,
   but with R(s)=E(s) we have S@↓{v}(s)=S@↓{h}(s)=S@↓{d1}(s)=S@↓{d2}(s)
   meaning that every reflection produces the same solution.}

@item{
   Solution s has symmetries E and R@↑{2} only.
   There are 4 equivalent but distinct solutions,
   which can be obtained one from another by R-odd,
   a reflection or an R-odd combined with a reflection,
   where R-odd is R or R@↑{3}.
   With R@↑{2}(s)=E(s), we also have R@↑{3}(s)=R(s), S@↓{h}(s)=S@↓{v}(s) and S@↓{d1}(s)=S@↓{d2}(s).}

@item{
   Solution s has symmetry E only.
   In common language no symmetry at all.
   There are 8 equivalent but distinct solutions.
   Given a solution s, every symmetry operation P yields a distinct solution P(s),
   E(s) being equal to s, of course.}]

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

@(image "picture-example.gif" #:scale 1.2)

@section{Count}
@(Defmodule2)
Opens a dialog asking for an exact non-negative number N-max.
Shows a table of the number of solutions for N from 0 up to and including max-N.
Uses futures for speeding up. The table shows:

@inset{
@Tabular[
(("N"               "board size")
 ("count"           "number of solutions")
 ("cpu"             "cpu time in ms")
 ("real"            "real time in ms")
 ("ratio"           "count[N]/count[N−1] (na for N=0 or count[N−1]=0)")
 ("parallelization" "cpu/real (na when real=0))")) #:sep (list (hspace 1) ":" (hspace 1))]}

The program is low on memory because it does not need to memorize solutions.
It may take much time for large N, though.
It uses as many or slightly more futures than processors available.
Check @nbr[processor-count] and @nbr[futures-enabled?].
@note{The parallelization with futures does not work with Racket BC. Use Racket CS.}
Example with 8 processors:

@inset{
@tt{C:\>racket  -e "(require N-queens/count-with-futures)"}
 
@(image "dialog.gif")

Output:

@Tabular[
(
 (@tt{N}     @tt{count}       @tt{cpu}     @tt{real}  @tt{ratio}  @tt{parallelization})
 (@tt{0}         @tt{1}         @tt{0}        @tt{0}     @tt{na}   @tt{na})
 (@tt{1}         @tt{1}         @tt{0}        @tt{0}   @tt{1.00}   @tt{na})
 (@tt{2}         @tt{0}         @tt{0}        @tt{0}   @tt{0.00}   @tt{na})
 (@tt{3}         @tt{0}         @tt{0}        @tt{0}     @tt{na}   @tt{na})
 (@tt{4}         @tt{2}         @tt{0}        @tt{0}     @tt{na}   @tt{na})
 (@tt{5}        @tt{10}         @tt{0}        @tt{0}   @tt{5.00}   @tt{na})
 (@tt{6}         @tt{4}         @tt{0}        @tt{0}   @tt{0.40}   @tt{na})
 (@tt{7}        @tt{40}         @tt{0}        @tt{1}  @tt{10.00}  @tt{0.0})
 (@tt{8}        @tt{92}         @tt{0}        @tt{0}   @tt{2.30}   @tt{na})
 (@tt{9}       @tt{352}        @tt{62}       @tt{38}   @tt{3.83}  @tt{1.6})
(@tt{10}       @tt{724}         @tt{0}        @tt{8}   @tt{2.06}  @tt{0.0})
(@tt{11}      @tt{2680}       @tt{390}      @tt{130}   @tt{3.70}  @tt{3.0})
(@tt{12}     @tt{14200}      @tt{1203}      @tt{259}   @tt{5.30}  @tt{4.6})
(@tt{13}     @tt{73712}      @tt{7343}     @tt{1681}   @tt{5.19}  @tt{4.4})
(@tt{14}    @tt{365596}     @tt{47140}    @tt{10544}   @tt{4.96}  @tt{4.5})
(@tt{15}   @tt{2279184}    @tt{317437}    @tt{51854}   @tt{6.23}  @tt{6.1}))
#:sep (hspace 2)
#:column-properties '(right right right right right left)]}
