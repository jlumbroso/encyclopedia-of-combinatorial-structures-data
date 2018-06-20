# Copyright (C) 1991--2013 by INRIA.
#
# This file is part of Algolib.
#
# Algolib is free software: you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# Algolib is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with Algolib.  If not, see
# <http://www.gnu.org/licenses/>.

macro(divisors=numtheory[divisors]);
EISnb:=proc(k)
    if k<10 then cat("EIS A00000",k)
    elif k<100 then cat("EIS A0000",k)
    elif k<1000 then cat("EIS A000",k)
    elif k<10000 then cat("EIS A00",k)
    else cat("EIS A0",k)
    fi
end:

#---------------------------------------------------------------------
# Alcohols [Alcohols or UNlabelled Non Plane Ternary Trees]
inserttable(maintable," Alcohols or Unlabelled Non Plane Ternary Trees",[A,{A=Union(Z,Prod(Z,Set(A,card=3))),Z=Atom},unlabelled],
"Alcohols or unlabelled  non plane ternary Trees",EISnb(598));

#sloane436

#---------------------------------------------------------------------
# Necklaces of m colours (m=2,3,4,5)
necklace:=proc(m) local i;
        [N,{N=Cycle(Union(seq(c||i,i=1..m))),seq(c||i=Atom,i=1..m)},unlabelled];
end;
inserttable(maintable,"Necklaces of 2 colours",necklace(2),
cat("Necklaces of 2 colours;",
" binary irreducible polynomials whose degree divides n"),
cat(EISnb(31),"; http://sue.csc.uvic.ca/~cos/inf/neck/NecklaceInfo.html"));
#sloane203

inserttable(maintable,"Necklaces of 3 colours",necklace(3),
"Necklaces of 3 colours",EISnb(1867));
#sloane1008

inserttable(maintable,"Necklaces of 4 colours",necklace(4),
"Necklaces of 4 colours",EISnb(1868));
#sloane1370

inserttable(maintable,"Necklaces of 5 colours",necklace(5),
"Necklaces of 5 colours",EISnb(1869));
#sloane1582

#-------------------------------------------------------------------------

#Words of n letters in a two letters alphabet
inserttable(maintable,"Words", [S, {S = Sequence(Union(a,b)), a = Atom, b = Atom},
unlabelled],
"Words of n letters in a two letters alphabet" ,EISnb(79));

#Words of n letters in a three letters alphabet
inserttable(maintable,"Words", [S, {S = Sequence(Union(a,b,c)), a = Atom, b = Atom,c=Atom},
unlabelled],
"Words of n letters in a three letters alphabet" ,
EISnb(244));

#Words of n letters in a four letters alphabet
inserttable(maintable,"Words", [S, {S = Sequence(Union(a,b,c,d)), a = Atom, b
= Atom,c=Atom, d=Atom}, unlabelled],
"Words of n letters in a four letters alphabet" ,
EISnb(302));

#------------------------------------------------------------------------
# Words with at most "m" consecutive b-letters
word1:=proc(m) [L,{L=Prod(X,Sequence(Prod(a,X))),X=Sequence(b,card<=m),
        a=Atom,b=Atom},unlabelled] end;

# Fibonacci numbers for m=1
inserttable(maintable,"Words with at most 1 consecutive b-letters",word1(1),
cat("Words with at most 1 consecutive b-letters; Fibonacci numbers;",
" integer F(n) such that F(n)=F(n-1)+F(n-2), F(0)=F(1)=1"),
cat(" Fibonacci; ",EISnb(45),"; http://www.ee.surrey.ac.uk/Personal/R.Knott/Fibonacci/fib.html"));
#sloane256

# Tribonacci numbers for m=2
inserttable(maintable,"Words with at most 2 consecutive b-letters",word1(2),
"Words with at most 2 consecutive b-letters; Tribonacci numbers",
EISnb(73));
#sloane406

# Tetranacci numbers for m=3
inserttable(maintable,"Words with at most 3 consecutive b-letters",word1(3),
"Words with at most 3 consecutive b-letters; Tetranacci numbers",
EISnb(78));
#sloane423

# Pentanacci numbers for m=4
inserttable(maintable,"Words with at most 4 consecutive b-letters",word1(4),
"Words with at most 4 consecutive b-letters; Pentanacci numbers",
EISnb(1591));
#sloane429

# Hexanacci numbers for m=5
inserttable(maintable,"Words with at most 5 consecutive b-letters",word1(5),
"Words with at most 5 consecutive b-letters; Hexanacci numbers",
EISnb(1592));
#sloane431

#------------------------------------------------------------------------
# Words with at least "m" consecutive b-letters
word2:=proc(m) [L,{L=Prod(X,Sequence(Prod(a,X))),X=Sequence(b,card>=m),
        a=Atom,b=Atom},unlabelled]; end;

inserttable(maintable,"Words with at least 2 consecutive b-letters ",word2(2),
"Words with at least 2 consecutive b-letters",EISnb(930));
#sloane207

#------------------------------------------------------------------------
# Set partitions
# with all blocks of size >=m
partitionswithlargeblocks:=proc(m) [B,{B=Set(Set(Z,card>=m))},labelled] end;

# m=1, Bell numbers
inserttable(maintable,"Set partitions",partitionswithlargeblocks(1),
"Set partitions with all blocks of size >=1; Bell numbers",
cat("Bell; ",EISnb(110)));
#sloane585

# m=2, expansion of exp(exp(x)-1-x)
inserttable(maintable,"Set partitions",partitionswithlargeblocks(2),
"Set partitions with all blocks of size >=2; expansion of exp(exp(x)-1-x) ",
EISnb(296));
#sloane1387

# with all blocks of size <=m
partitionswithsmallblocks:=proc(m)
local j;
[B,{B=Set(Union(seq(Set(Z,card=j),j=1..m)))},labelled];
 end;

# m=2, involution numbers a(n)=a(n-1)+(n-1)a(n-2)
inserttable(maintable,"Set partitions",partitionswithsmallblocks(2),
cat("Set partitions with all blocks of size <=2;",
" self-conjugate permutations on n letters;",
" involution numbers a(n)=a(n-1)+(n-1)a(n-2)"),
EISnb(85));
#sloane469

# m=3, the partition function G(n,3)
inserttable(maintable,"Set partitions",partitionswithsmallblocks(3),
cat("Set partitions with all blocks of size <=3;",
" the partition function G(n,3) "),EISnb(1680));
#sloane579

# m=4, the partition function G(n,4)
inserttable(maintable,"Set partitions",partitionswithsmallblocks(4),
cat("Set partitions with all blocks of size <=4;",
" the partition function G(n,4) "),EISnb(1681));
#sloane584

#------------------------------------------------------------------------
# Permutations
#a:=[A,{A=Set(Cycle(Z))},labelled];
# Factorial numbers
inserttable(maintable,"Permutations", [d,{d=Set(Cycle(Z))},labelled],
"Permutations of n elements",
cat("Euler, 1741; Stirling, 1730; ",EISnb(142)));

# Cycles >=m
cycles_set2:=proc(m) [A,{A=Set(Cycle(Z,card>=m))},labelled]; end;

# m=2, derangements [subfactorial or rencontre numbers]
inserttable(maintable,"derangements",cycles_set2(2),
cat("Cycles >=2, permutations of n element with no fixed points;",
" derangements (subfactorial or rencontre numbers)"),
EISnb(166));

# cycles<=m
cycles_set3:=proc(m) [t,{t=Set(Cycle(Z,m>=card))},labelled];end;

#m=2, involution
inserttable(maintable,"involutions",cycles_set3(2),
"involution of n elements",
EISnb(85) );

#involution without fixed points
inserttable(maintable,"involutions",[t,{t=Set(Cycle(Z,card=2))},labelled],
cat("involution without fixed points of n elements",
" even numbers are double factorials"),
cat(" even number; ",EISnb(1147)));

perm:=proc(m) local i;
[t,{t=Set(Union(seq(Cycle(Z,card=i),i=divisors(m))))},labelled]; end;

inserttable(maintable,"Cycles set", perm(3),
cat("permutations Sigma such that Sigma^3=Id; ",
"degree n permutations of order dividing 3"),
EISnb(1470) );

inserttable(maintable,"Cycles set", perm(4),
cat("permutations Sigma such that Sigma^4=Id; ",
"degree n permutations of order dividing 4"),
EISnb(1472) );

inserttable(maintable,"Cycles set", perm(5),
cat("permutations Sigma such that Sigma^5=Id; ",
"degree n permutations of order dividing 5"),EISnb(52501) );

permwfp:=proc(m) local i;
[t,{t=Set(Union(seq(Cycle(Z,card=i),i=op(divisors(m)
minus{1}))))},labelled]; end;

inserttable(maintable,"Cycles set without fixed point", permwfp(3),
cat("permutations Sigma without fixed point such that Sigma^3=Id"),EISnb(52502));

inserttable(maintable,"Cycles set without fixed point", permwfp(4),
cat("permutations Sigma without fixed point such that Sigma^4=Id"),EISnb(52503));

inserttable(maintable,"Cycles set without fixed point", permwfp(5),
cat("permutations Sigma without fixed point such that Sigma^5=Id"),EISnb(52504));

# At least two cycles [Differences of factorial numbers]
cycles_set1:=proc(m) [A,{A=Set(Cycle(Z),card>=m)},labelled]; end;
inserttable(maintable,"Cycles Set",cycles_set1(2) ,
"Permutation with at least two cycles",EISnb(1563));
#sloane1436

# Exactly m cycles
cycles_set2:=proc(m) [A,{A=Set(Cycle(Z),card=m)},labelled]; end;

# m=2 [Stirling numbers of first kind]
inserttable(maintable,"Cycles Set",cycles_set2(2) ,
cat("Permutation with exactly two cycles;",
" Stirling numbers of first kind"),EISnb(254));
#sloane1165

# m=3 [Stirling numbers of first kind]
inserttable(maintable,"Cycles Set",cycles_set2(3) ,
"Permutation with exactly 3 cycles; Stirling numbers of first kind",
EISnb(399));
#sloane1762

# m=4 [Stirling numbers of first kind]
inserttable(maintable,"Cycles Set",cycles_set2(4) ,
cat("Permutation with exactly 4 cycles;",
" Stirling numbers of first kind"),EISnb(454));
#sloane2022



#-------------------------------------------------------------------------
# Cycles of cycles [coefficients of iterated exponentials?]
inserttable(maintable," Cycles of cycles",[A,{A=Cycle(Cycle(Z))},labelled],
" Cycles of cycles",
EISnb(3713)); #MS
#sloane710

#--------------------------------------------------------------------------
# Arrangements, partial permutations [Permutations of N things]
inserttable(maintable,"Arrangements",[N,{N=Prod(Sequence(Z),Set(Z))},labelled],
"Arrangements; partial permutations (Permutations of n thing)",
EISnb(522));
#sloane589

#--------------------------------------------------------------------------
# Functional graphs
inserttable(maintable,"Functional graphs",
[F,{F=Set(K), K=Cycle(Tree), Tree=Prod(Z,Set(Tree))},labelled],
"Functional graphs",EISnb(312));
#sloane1469

# Connected FG's [Connected graphs by points]
inserttable(maintable,"Functional graphs",
[F,{F=Set(K,card=1), K=Cycle(Tree), Tree=Prod(Z,Set(Tree))},labelled],
" Connected Functional graphs; Connected graphs by points",
EISnb(1865)) ;
#sloane1232

inserttable(maintable,"Labelled 3-constrained functional graphs", [k, {k =
Set(Cycle(Prod(Z,Set(g,card = 2)))), g = Union(Z,Prod(Z,Set(g,card
=3)))}, labelled],
"Labelled 3-constrained functional graphs" ,EISnb(52505));

# Maps of f^2=f [Trees of height at most 1]
inserttable(maintable,"Trees",
# [F,{F=Set(K), K=Cycle(Tree,card=1), Tree=Prod(Z,Set(Z, card>0))},labelled],
[F,{F=Set(Tree), Tree=Prod(Z,Set(Z, card>0))},labelled],
"Maps of f^2=f;Trees of height at most 1",EISnb(52506)) ;
#sloane1148

#---------------------------------------------------------------------
# Mapping patterns [Functional digraphs]
###inserttable(maintable,"Mapping patterns",
###[F,{F=Set(K), K=Cycle(T), T=Prod(Z,Set(T,card>0))},unlabelled],
###"Mapping patterns, Functional digraphs","FAIL");
#sloane1069

# Connected mapping patterns
#[Connected graphs with at most one cycle]
###inserttable(maintable,"Mapping patterns",
###[F,{F=Set(K,card=1), K=Cycle(T), T=Prod(Z,Set(T))},unlabelled],
###cat("Connected mapping patterns,",
###" Connected graphs with at most one cycle"),"FAIL");
#-> completes Sloane's sequence, sloane455

#-----------------------------------------------------------------------
# Sets of segments [Forest of greatest height]
inserttable(maintable,"Sets of segments",
[R,{R=Set(Prod(Z,Sequence(Z)))},labelled],
"Sets of segments; Forest of greatest height; Acyclic injective partial maps",EISnb(262));
#sloane1190

#------------------------------------------------------------------------
# Ordered set partitions, surjections [Preferential arrangements]
inserttable(maintable," Ordered set partitions, Balls and Urns",
[R,{R=Sequence(Set(Z,card>=1))},labelled],
cat(" Ordered set partitions; surjections (Preferential arrangements);",
"Ways to place balls into urns: distinguishable balls and distinguishable urns"),
EISnb(670)); ## MS
#sloane1191


#------------------------------------------------------------------------
# Brackettings
# Schroeder's problem [Dissections of a polygon or parenthesizing a product]
inserttable(maintable,"Schroeder's problem",
[S,{S=Union(Z,Sequence(S,card>=2))},unlabelled],
cat("Schroeder's problem. ",
"Dissections of a polygon or parenthesizing a product"),
EISnb(1003));
#sloane1163

# Non plane rooted binary trees [Wedderburn-Etherington numbers]
inserttable(maintable,"Non plane rooted binary trees",
[S,{S=Union(Z,Set(S,card=2))},unlabelled],
cat("unlabelled binary rooted trees with n endpoints inequivalent ",
"under reflections in vertical axes.",
" (Wedderburn-Etherington numbers)"),EISnb(1190));
#sloane298

# Non plane trees with degree >=2 [series-reduced planted trees]
# -> !  completes Sloane's sequence
inserttable(maintable,"Non plane trees ",
[S,{S=Union(Z,Set(S,card>=2))},unlabelled],
cat("Unlabelled non plane trees with degree >=2 ",
"(series-reduced planted trees)"),
EISnb(669));
#sloane558

#Unlabelled non plane binary trees
inserttable(maintable,"Unlabelled Non Plane Binary Trees",
[g,{g=Union(Z,Prod(Z,Set(g,card=2)))},unlabelled],
 "Unlabelled Non Plane Binary Trees",EISnb(1190));

#see first entry in the table  MS
#Unlabelled non plane ternary trees
#inserttable(maintable,"Unabelled Non Plane Ternary Trees",
#[g,{g=Union(Z,Prod(Z,Set(g,card=3)))},unlabelled],
# "Unlabelled Non Plane Ternary Trees","FAIL");

#Labelled non plane binary trees
inserttable(maintable,"Labelled Non Plane Binary Trees",
[g,{g=Union(Z,Prod(Z,Set(g,card=2)))},labelled],
 "Labelled Non Plane Binary Trees",EISnb(36770));

#labelled non plane ternary trees
inserttable(maintable,"Labelled Non Plane Ternary Trees",
[g,{g=Union(Z,Prod(Z,Set(g,card=3)))},labelled],
 "Labelled Non Plane Ternary Trees",EISnb(36771));

# General plane trees [Catalan numbers or binomial coefficients C(2N,N)/(N+1)]
inserttable(maintable,"General plane trees",
[S,{S=Prod(Z,Sequence(S))},unlabelled],
cat("Unlabelled general rooted plane trees;",
" Catalan numbers"),
cat(EISnb(108),"; http://mathworld.wolfram.com/CatalanNumber.html;",
"http://forum.swarthmore.edu/advanced/robertd/catalan.html"));
#sloane577

#Labelled General Plane Trees
inserttable(maintable,"Labelled Plane General Trees",
[c, {c = Prod(Z,Sequence(c))}, labelled],
"Labelled General Plane Trees of n elements", EISnb(808));

# Unary-binary trees, Motzkin numbers [Generalized ballot numbers]
inserttable(maintable,"Unary-binary trees",
[S,{S=Prod(Z,Sequence(S,card<=2))},unlabelled],
cat("Unlabelled unary-binary trees; Motzkin numbers",
" (Generalized ballot numbers)"),
cat("Motzkin; ",EISnb(1006)));
#sloane456
inserttable(maintable,"Labelled Plane Binary Trees", [b, {b =
Union(Z,Prod(b,b))}, labelled],
" Binary trees of n leaves (external nodes)", EISnb(808));

inserttable(maintable,"Unlabelled Plane Binary Trees",[b, {b =
 Union(Z,Prod(b,b))}, unlabelled],
 " Unlabelled binary trees of n leaves (external nodes)",
cat(EISnb(108),"; Catalan numbers;",
"http://mathworld.wolfram.com/CatalanNumber.html;",
"http://forum.swarthmore.edu/advanced/robertd/catalan.html"));

inserttable(maintable,"Noy's counting of non-crossing trees",
[T,{T=Prod(Z,F),F=Sequence(B), B=Prod(F,Z,F)},unlabelled],
cat("Noy's counting of non-crossing trees ",
"number of trees that can be built on the n vertices on the n-gon,",
" assuming that no edges of the tree intersect"),
cat("Noy; ",EISnb(1764)));

inserttable(maintable,"Labelled Plane Binary Trees", [b, {b =
 Union(Z,Prod(Z,b,b))}, labelled], cat("Binary Trees of n elements",
" (external nodes or internal nodes)"), EISnb(52510));

inserttable(maintable,"Unlabelled Plane Binary Trees",[b, {b =
Union(Z,Prod(Z,b,b))}, unlabelled],
cat("Unlabelled Binary Trees of n elements",
" (external nodes or internal nodes) with two branches"),
cat("Odd terms are the Catalan numbers; ",EISnb(108)));

inserttable(maintable,"rooted identity trees",
[S,{S=Prod(Z,PowerSet(S))},unlabelled],
"rooted identity trees",EISnb(4111));

#-------------------------------------------------------------------------
# Trees
# Rooted, non plane, unlabelled [Rooted unlabelled trees]
inserttable(maintable,"Trees",
[T,{T=Prod(Z,Set(T))},unlabelled],
"Rooted, non plane, unlabelled Trees",EISnb(81));
#sloane454

# Labelled trees of bounded height
trees1:=proc(m) local i;
        [T0,{seq(T||i=Prod(Z,Set(T||(i+1))),i=0..m-1),T||m=Z},labelled];   end;

# Labelled trees of height =2
inserttable(maintable,"Trees", trees1(2),
"Labelled trees of height 2",EISnb(52512));
# completes Sloane's sequence, sloane1764

# Labelled trees of height = 3
inserttable(maintable,"Trees", trees1(3),
"Labelled trees of height 3",EISnb(52513));
# completes Sloane's sequence, sloane2241

# Labelled trees of height = 4
inserttable(maintable,"Trees", trees1(4),
"Labelled trees of height 4",EISnb(52514));
# completes Sloane's sequence, sloane2335

# Unlabelled trees of bounded height
trees2:=proc(m) local i;
        [T0,{seq(T||i=Prod(Z,Set(T||(i+1))),i=0..m-1),T||m=Z},unlabelled];   end;

# Unlabelled trees of height 2
inserttable(maintable,"Trees", trees2(2),
"Unlabelled trees of height 2",EISnb(41));
# [completes Sloane's sequence], sloane379

# Unlabelled trees of height 3
inserttable(maintable,"Trees", trees2(3),
"Unlabelled trees of height 3",EISnb(1383));
#sloane1097

# Unlabelled trees of height 4
inserttable(maintable,"Trees", trees2(4),
"Unlabelled trees of height 4",EISnb(1384));
#sloane1408

#------------------------------------------------------------
# Injective partial maps (Borwein et al) <=increasing subsequences in perms>
# HIS[708]
inserttable(maintable,"maps",
[IPM,{IPM=Set(Union(Cycle(Z),Sequence(Z,card>=1)))},labelled],
cat("Injective partial maps (Borwein et al),",
" (increasing subsequences in perms)"),EISnb(2720));

# Strictly increasing partial maps (Borwein et al)
# = Bell numbers
# HIS[585]
inserttable(maintable,"maps",
[InPM,{InPM=Set(Set(Z,card>=1))},labelled],
"Strictly increasing partial maps (Borwein et al); Bell numbers",
cat("Borwein et al; ",EISnb(110)));

# Increasing (nonstrict) injective partial maps
# HIS[653], values of Bell polynomials
inserttable(maintable," maps",
[InPM2,{InPM2=Set(Union(Set(Z,card>=1),Set(Z,card>=1)))},labelled],
"Increasing (nonstrict) injective partial maps",EISnb(1861));

# Acyclic injective partial maps
# HIS[1190] = Forests of greatest height
## MS
#inserttable(maintable,"maps",
#[X,{X=Set(Sequence(Z,card>=1))},labelled],
#" Acyclic injective partial maps",EISnb(262));
#----------------------------------------------------------------
# Partial maps, (n+1)^n, HIS[771]
inserttable(maintable,"maps",
[PartialMap, {PartialMap=Set(Connected),
Connected=Union(Cycle(Tree),Prod(Undefined,Tree)),
Tree=Prod(Z,Set(Tree)), Undefined=Epsilon}, labelled],
"Partial maps; (n+1)^n",EISnb(169));

#--------------------------------------------------------------------------
#Sequences (n1,...,nk) such as n1+n2+...+nk=n with ni and k positive integers
inserttable(maintable,"Integer composition", [C, {a = Atom, C =
Sequence(Sequence(a,1 <= card))}, unlabelled],
cat("integer composition of size n:",
" Sequences (n1,...,nk) such that n1+n2+...+nk=n,",
" with ni and k positive integers"),EISnb(79));

#--------------------------------------------------------------------------
#Ways of making change for n cents using coins of 1, 2, 5, 10 cents

#inserttable(maintable,"coins of 1, 2, 5, 10 cents",
#[m,{m=Prod(Sequence(a),Sequence(b),Sequence(c),Sequence(d)),
#a=Atom,b=Prod(a,a), c=Prod(a,a,a,a,a), d=Prod(a,a,a,a,a,a,a,a,a,a)},
#unlabelled],
#cat ("Ways of making change for n cents using coins",
#" of 1, 2, 5, 10 cents"),EISnb(8));

#--------------------------------------------------------------------------
#Hierarchies:

inserttable(maintable,"Hierarchies",[h,{h=Union(Z,Set(h,card>=2))},labelled],
"Hierarchies; Schroeder's fourth problem",EISnb(311));

inserttable(maintable,"3-balanced hierarchies",
[l,{l=Set(Set(Set(Z,card>=1),card>=1))},labelled],
"3-balanced hierarchies",EISnb(258));

#--------------------------------------------------------------------------
#Euler's counting of triangulations
inserttable(maintable,"Euler's counting of triangulations",
[T,{T=Union(Z,Prod(U,Z,T),Prod(T,Z,U),Prod(T,Z,T)),U=Epsilon},unlabelled],
cat("Euler's counting of triangulations: number of ways of cutting",
" up a convex(n+2)-gon into n triangles by means of (n-1) ",
"non-intersecting diagonals; Catalan numbers"),
cat("Euler, 1753; ",EISnb(108)));

#--------------------------------------------------------------------------
#Balls and Urns

inserttable(maintable,"Balls and Urns",
[S,{S=Sequence(U),U=Sequence(Z,card>=1)},unlabelled],
cat("Ways to place balls into urns: indistinguishable balls ",
"and distinguishable urns"),
EISnb(79));

##MS
#inserttable(maintable,"Balls and Urns",
#[S,{S=Sequence(U),U=Set(Z,card>=1)},labelled],
#cat("Ways to place balls into urns: distinguishable balls ",
#"and distinguishable urns"),
#EISnb(670));

inserttable(maintable,"Balls and Urns",
[S,{S=Set(U),U=Set(Z,card>=1)},labelled],
cat("Ways to place balls into urns: distinguishable balls ",
"and indistinguishable urns"),
cat("Bell numbers; ",EISnb(110)));

inserttable(maintable,"Balls and Urns",
[S,{S=Set(U),U=Sequence(Z,card>=1)},unlabelled],
cat("Ways to place balls into urns: indistinguishable balls ",
"and indistinguishable urns"),
EISnb(41));

#-------------------------------------------------------------------
#Stanley's children rounds

inserttable(maintable,"Stanley's children rounds",
[R,{R=Set(Prod(Z,Cycle(Z)))},labelled],
"Stanley's children rounds",
cat("Stanley; ",EISnb(7113)));

#------------------------------------------------------------------
inserttable(maintable,"Ways of parenthezing a product",
[p,{p=Union(Prod(Z,Z),Prod(Z,p),Prod(p,Z),Prod(p,p))},unlabelled],
cat("Number of ways of completely parenthesizing a product of n letters;",
" Catalan numbers"),
cat(EISnb(108),"; http://mathworld.wolfram.com/CatalanNumber.html;",
"http://forum.swarthmore.edu/advanced/robertd/catalan.html"));


inserttable(maintable,"FAIL",[A,{A = Prod(B,B), B = Set(Z,1 <=
card)},labelled],
"FAIL",
EISnb(918));

## INUTILE1??? voir LE BON1
#inserttable(maintable,"FAIL",[A,{A = Sequence(B,2 <= card), B =
#Union(A,Z)},labelled],
#"FAIL",EISnb(32037));

inserttable(maintable,"FAIL",[A,{A = Set(B), B = Prod(A,Z)},labelled],"FAIL",
EISnb(272));

inserttable(maintable,"FAIL",[A,{B = Union(A,Z), A = Cycle(B,2 <=
card)},unlabelled],
"FAIL",EISnb(32203));

inserttable(maintable,"FAIL",[A,{B = Prod(A,Z), A = Sequence(B)},labelled],
"FAIL",EISnb(1761));

#Pairs of sets of cardinality at least 2
inserttable(maintable,"Pairs of sets",[A,{A =
Prod(B,B), B = Set(Z,2 <= card)},labelled],
"Pairs of sets of cardinality at least 2",EISnb(52515));

#Pairs of sets of cardinality at least 3
inserttable(maintable,"Pairs of sets",[A,{A = Prod(B,B), B = Set(Z,3 <= card)},labelled],
"Pairs of sets of cardinality at least 3",EISnb(52516));

#Pairs of cycles of cardinality at least 1
inserttable(maintable,"Pairs of cycles",[A,{A = Prod(B,B), B = Cycle(Z)},labelled],
"Pairs of cycles of cardinality at least 1",EISnb(52517));

#Pairs of cycles of cardinality at least 2
inserttable(maintable,"Pairs of cycles",[A,{A = Prod(B,B), B = Cycle(Z,2 <=
card)},labelled],
"Pairs of cycles of cardinality at least 2",EISnb(52518));

#Pairs of cycles of cardinality at least 3
inserttable(maintable,"Pairs of cycles",[A,{A = Prod(B,B), B = Cycle(Z,3 <=
card)},labelled],
"Pairs of cycles of cardinality at least 3",EISnb(52519));

#Pairs of sequences of cardinality at least 1
inserttable(maintable,"Pairs of sequences",[A,{A = Prod(B,B), B = Sequence(Z,1
<= card)},labelled],
"Pairs of sequences of cardinality at least 1","FAIL");

#Pairs of sequences of cardinality at least 2
inserttable(maintable,"Pairs of sequences",[A,{A = Prod(B,B), B = Sequence(Z,2
<= card)},labelled],
"Pairs of sequences of cardinality at least 2",EISnb(52520));

#Pairs of sequences of cardinality at least 3
inserttable(maintable,"Pairs of sequences",[A,{A = Prod(B,B), B = Sequence(Z,3
<= card)},labelled],
"Pairs of sequences of cardinality at least 3",EISnb(52521));


#Labelled mobile with cycles of length at least 2
inserttable(maintable,"Labelled mobile",[A,{A = Union(B,Z), B = Cycle(A,2 <=
card)},labelled],
"Labelled mobile with cycles of length at least 2",EISnb(32188));

#Labelled mobile with cycles of length at least 3
inserttable(maintable,"Labelled mobile",[A,{B = Cycle(A,3 <= card), A =
Union(B,Z)},labelled],
"Labelled mobile with cycles of length at least 3",EISnb(52522));

#Unlabelled mobile with cycles of length at least 3
inserttable(maintable,"Unlabelled mobile",[A,{B = Cycle(A,3 <= card), A =
Union(B,Z)},unlabelled],
"Unlabelled mobile with cycles of length at least 3",EISnb(52523));

## LE BON1
#Planar labelled trees with no unary node
inserttable(maintable,"Planar trees",[A,{A = Union(B,Z), B = Sequence(A,2 <= card)},labelled],
"Planar labelled trees with no unary node",EISnb(32037));

#Planar unlabelled trees with neither unary nor binary nodes
inserttable(maintable,"Planar trees",[A,{A = Union(B,Z), B = Sequence(A,3 <= card)},unlabelled],
"Planar unlabelled trees with neither unary nor binary nodes",EISnb(46736));

#Planar labelled trees with neither unary nor binary nodes
inserttable(maintable,"Planar trees",[A,{A = Union(B,Z), B = Sequence(A,3 <= card)},labelled],
"Planar labelled trees with neither unary nor binary nodes",EISnb(52524));


#Non-planar unlabelled trees with neither unary nor binary nodes
inserttable(maintable,"Non-planar trees",[A,{B = Union(A,Z), A = Set(B,3 <= card)},unlabelled],
"Non-planar unlabelled trees with neither unary nor binary nodes",EISnb(52525));

#Non-planar labelled trees with neither unary nor binary nodes
inserttable(maintable,"Non-planar trees",[A,{B = Union(A,Z), A = Set(B,3 <= card)},labelled],
"Non-planar labelled trees with neither unary nor binary nodes",EISnb(52526));

#Centered labelled non-empty cycles
inserttable(maintable,"Centered cycles",[A,{B = Cycle(Z), A = Prod(B,Z)},labelled],
"Centered labelled cycles",EISnb(1048));


inserttable(maintable,"FAIL",[A,{B = Union(Z,C), C = Sequence(Z,3 <= card),
 A = Sequence(B,1 <=
card)},unlabelled],
"FAIL",EISnb(5251));

#ECSv1 100
#inserttable(maintable,"FAIL",[A,{A = Cycle(B), B = Sequence(Z,1 <= card)},unlabelled],
#"FAIL",EISnb(8965));


inserttable(maintable,"FAIL",[A,{A = Cycle(B), B = Set(Z,1 <= card)},labelled], "FAIL",EISnb(629));

################## ECS 100 #############################
inserttable(maintable,"FAIL",[A,{A = Set(B,1 <= card), B = Sequence(Z,2 <= card)},unlabelled],
"FAIL",EISnb(2865));

inserttable(maintable,"FAIL",[A,{B = Cycle(Z), C = Set(B,1 <= card), A = Union(B,C)},labelled],
"FAIL",EISnb(1048));

inserttable(maintable,"FAIL",[A,{A = Set(Set(Z,3 <= card))},labelled],
"FAIL",EISnb(6505));

inserttable(maintable,"FAIL",[A,{A = Prod(B,B),B=Prod(Z,Set(Z))},labelled],
"FAIL",EISnb(1815));

#-----------

inserttable(maintable,"FAIL",[A,{A = Set(C), C = Union(B,Z), B = Cycle(Z,3 <= card)},labelled],
"FAIL",EISnb(266));

inserttable(maintable,"FAIL",[A,{A = Prod(B,C), B = Sequence(Z,1 <= card), C = Set(B,card = 2)},unlabelled],
"FAIL",EISnb(2620));

inserttable(maintable,"FAIL",[A,{A = Union(Z,B), B = Cycle(A,card = 2)},labelled],
"FAIL",EISnb(1147));

inserttable(maintable,"FAIL",[A,{A = Cycle(B), B = Union(Z,Sequence(Z,card>0))},unlabelled],
"FAIL",EISnb(5594));# (verifier la reference ?)


inserttable(maintable,"FAIL",[A,{B = Sequence(Z,card>0), A = PowerSet(B)},unlabelled],
"FAIL",EISnb(9));

inserttable(maintable,"FAIL",[A,{A = Cycle(B), C = Sequence(B), B = Prod(C,Z)},labelled],
"FAIL",EISnb(6963));

inserttable(maintable,"FAIL",[A,{A = Sequence(C), B = Set(Z), C = Prod(B,Z)},labelled],
"FAIL",EISnb(6153));

inserttable(maintable,"FAIL",[A,{B = Union(C,Z), C = Sequence(Z,card>0), A = Cycle(B)},unlabelled],
"FAIL",EISnb(5594)); #(verifier la reference)

inserttable(maintable,"FAIL",[A,{A = Prod(B,B), C = Sequence(Z,card>0), B = Sequence(C)},unlabelled],
"FAIL",EISnb(3416));

inserttable(maintable,"FAIL",[A,{A = Set(B), C = Sequence(Z,card>0), B = Union(C,Z)},unlabelled],
"FAIL",EISnb(70));

inserttable(maintable,"FAIL",[A,{B = Sequence(C), A = Union(C,B), C = Sequence(Z,card>0)},unlabelled],
"FAIL",EISnb(51));

inserttable(maintable,"FAIL",[A,{C = Set(B), A = Prod(C,Z), B = Cycle(A)},labelled],
"FAIL",EISnb(1813));

inserttable(maintable,"FAIL",[A,{B = Set(Z), C = Cycle(Z), A = Prod(C,B)},labelled],
"FAIL",EISnb(2104));

inserttable(maintable,"FAIL",[A,{C = Prod(B,Z), A = Set(C), B = Set(Z)},labelled],
"FAIL",EISnb(248));

inserttable(maintable,"FAIL",[A,{C = Set(B), A = Cycle(B), B = Prod(C,Z)},unlabelled],
"FAIL",EISnb(2861));

inserttable(maintable,"FAIL",[A,{B = Union(Epsilon,Z), C = Prod(B,Z), A = Cycle(C)},unlabelled],
"FAIL",EISnb(358));

inserttable(maintable,"FAIL",[A,{B = Prod(C,Z), C = Union(Epsilon,Z), A = Set(B)},unlabelled],
"FAIL",EISnb(8619));

inserttable(maintable,"FAIL",[A,{A = Sequence(C), C = Prod(B,Z), B = Sequence(Z)},labelled],
"FAIL",EISnb(2866));

inserttable(maintable,"FAIL",[A,{A = Prod(C,B), C = Cycle(Z), B = Sequence(C)},labelled],
"FAIL",EISnb(7840));

inserttable(maintable,"FAIL",[A,{B = Set(C), A = Sequence(C), C = Prod(B,Z)},unlabelled],
"FAIL",EISnb(107));
#ECSv1 126
#inserttable(maintable,"FAIL",[A,{A = Cycle(B), B = Prod(C,Z), C = Sequence(Z)},unlabelled],
#"FAIL",EISnb(8965));

# was:
# inserttable(maintable,"FAIL",[A,{B = Sequence(A), C = Cycle(Z), A = Prod(C,B)},unlabelled],"FAIL",EISnb(7317));
inserttable(maintable,"FAIL",[A,{B = Sequence(A), C = Sequence(Z,card>=1), A = Prod(C,B)},unlabelled],"FAIL",EISnb(7317));

inserttable(maintable,"FAIL",[A,{A = Cycle(B), C = Set(Z), B = Prod(C,Z)},labelled],
"FAIL",EISnb(9444));

inserttable(maintable,"FAIL",[A,{B = Set(C), A = Prod(C,B), C = Sequence(Z,card>0)},unlabelled],
"FAIL",EISnb(70));

inserttable(maintable,"FAIL",[A,{C = Sequence(Z,card>0), B = Union(C,Z), A = Sequence(B)},unlabelled],
"FAIL",EISnb(1519)); #(verifier la recurrence)

## MS
#inserttable(maintable,"FAIL",[A,{C = Sequence(Z), B = Prod(C,Z), A = PowerSet(B)},unlabelled],
#"FAIL",EISnb(9));

inserttable(maintable,"FAIL",[A,{A = Set(B), C = Cycle(Z), B = Prod(C,C)},labelled],
"FAIL",EISnb(9199));

inserttable(maintable,"FAIL",[A,{C = Sequence(Z,card>0), B = Set(C), A = Prod(B,B)},unlabelled],
"FAIL",EISnb(712));

inserttable(maintable,"FAIL",[A,{C = Sequence(Z,card>0), B = Cycle(C), A = Union(C,B)},unlabelled],
"FAIL",EISnb(31));

inserttable(maintable,"FAIL",[A,{B = Sequence(C), A = Set(C), C = Prod(B,Z)},labelled],
"FAIL",EISnb(1517));# (il faudrait demander une preuve a Philippe)


#---------------------------------


inserttable(maintable,"FAIL",[A,{A = Sequence(Z)},unlabelled],
"FAIL",EISnb(12));#

inserttable(maintable,"FAIL",[A,{A=Prod(Sequence(Z),Sequence(Z))},unlabelled],
"FAIL",EISnb(27));#

inserttable(maintable,"FAIL",[A,{A=Prod(Z,Z)},unlabelled],
"FAIL",EISnb(38));#

inserttable(maintable,"FAIL",[A,{A = Sequence(E), C = Sequence(B,1 <= card), E = Prod(C,Z), B = Sequence(Z ,1 <= card)},unlabelled],
"FAIL",EISnb(129));# (verifier la recurrence)

inserttable(maintable,"FAIL",[A,{A = Sequence(Union(Z,Z),1 <= card)},labelled],
"FAIL",EISnb(165));# (verifier la forme close)

inserttable(maintable,"FAIL",[A,{B = Sequence(Z), E = Sequence(Z,1 <= card), A = Set(C,1 <= card), C = Prod(B,E)},unlabelled],
"FAIL",EISnb(219));#

inserttable(maintable,"FAIL",[A,{A=Prod(Set(Z,card>=1),Set(Z))},labelled],
"FAIL",EISnb(225));#

inserttable(maintable,"FAIL",[A,{E = Prod(Z,B), C = Sequence(E,1 <= card), B = Sequence(E), A = Sequence(C)},labelled],
"FAIL",EISnb(407));# (verifier la forme close)

#ECSv1 143
#inserttable(maintable,"FAIL",[A,{C = Cycle(B,2 <= card), A = Union(C,B), B = Set(Z,1 <= card)},labelled],
#"FAIL",EISnb(629));#

inserttable(maintable,"FAIL",[A,{B = Cycle(Z), A = Union(C,B), C = Sequence(B,card = 2)},labelled],
"FAIL",EISnb(776));# (verifier la forme close)

inserttable(maintable,"FAIL",[A,{E = Prod(B,C), A = Set(E), B = Sequence(Z), C = Sequence(Z,1 <= card)},unlabelled],
"FAIL",EISnb(990));#

inserttable(maintable,"FAIL",[A,{B = Sequence(E,1 <= card), A = Sequence(B,1 <= card), C = Sequence(Z,1 <= card), E = Prod(C,Z)},unlabelled],
"FAIL",EISnb(1045));# (verifier la recurrence

inserttable(maintable,"FAIL",[A,{A = Sequence(E,1 <= card), E = Prod(B,C), B = Sequence(Z,1 <= card), C = Union(Epsilon,Z)},unlabelled],
"FAIL",EISnb(1333));# (verifier la recurrence)

inserttable(maintable,"FAIL",[A,{B = Prod(E,Z), E = Set(B), A = Set(C), C = Cycle(B)},unlabelled],
"FAIL",EISnb(1372));#

inserttable(maintable,"FAIL",[A,{E = Prod(Z,B), C = Sequence(E,1 <= card), B = Sequence(E), A = Sequence(C)},unlabelled],
"FAIL",EISnb(1700));# (verifier la forme close)

inserttable(maintable,"FAIL",[A,{A = Prod(C,E), E = Sequence(B,1 <= card), C = Sequence(B), B = Sequence(Z,1 <= card)},unlabelled],
"FAIL",EISnb(1792));#(verifier la forme close)

inserttable(maintable,"FAIL",[A,{A = Sequence(E), E = Prod(B,C), B = Sequence(Z), C = Sequence(Z,1 <= card)},unlabelled],
"FAIL",EISnb(1906));# (verifier la recurrence)

inserttable(maintable,"FAIL",[A,{A = Set(C,1 <= card), C = Set(B,1 <= card), B = Prod(Z,E), E = Sequence(Z)},unlabelled],
"FAIL",EISnb(1970));# (verifier la reference)

inserttable(maintable,"FAIL",[A,{C = Set(Z), B = Set(Z,1 <= card), E = Sequence(B,1 <= card), A = Prod(C,E)},labelled],
"FAIL",EISnb(2050));#

inserttable(maintable,"FAIL",[A,{B = Sequence(Z), A = Prod(B,C), C = Set(Z,1 <= card)},labelled],
"FAIL",EISnb(2627));#

## MS
#inserttable(maintable,"FAIL",[A,{B = Cycle(Z,2 <= card), C = Union(Z,B), A = Cycle(C)},labelled],
#"FAIL",EISnb(3713));# (verifier la fg)

inserttable(maintable,"FAIL",[A,{C = Sequence(E,1 <= card), A = Prod(B,C), E = Sequence(Z,1 <= card), B = Sequence(Z)},unlabelled],
"FAIL",EISnb(3945));#(verifier la fonction generatrice)

inserttable(maintable,"FAIL",[A,{A = Prod(B,C), C = Set(Z,1 <= card), B = Set(C)},labelled],
"FAIL",EISnb(5493));

inserttable(maintable,"FAIL",[A,{C = Sequence(Z,1 <= card), E = Prod(A,B), A =
Sequence(E), B = Sequence(C,1 <= card)},unlabelled],
"FAIL",EISnb(5572));# (verifier la reference)

inserttable(maintable,"FAIL",[A,{A = Prod(B,B), B = Sequence(C), C = Set(Z,1 <= card)},labelled],
"FAIL",EISnb(5649));#

inserttable(maintable,"FAIL",[A,{B = Sequence(Z,1 <= card), C = Union(E,Z), A = Sequence(C,1 <= card), E = Sequence(B,1 <= card)},unlabelled],
"FAIL",EISnb(6012));# (verifier la recurrence)

inserttable(maintable,"FAIL",[A,{A = Prod(Z,B), C = Sequence(Z,1 <= card), B = Set(C)},labelled],
"FAIL",EISnb(6152));#

inserttable(maintable,"FAIL",[A,{A = Sequence(C), C = Union(Z,B), B = Set(Z,1 <= card)},labelled],
"FAIL",EISnb(6155));#

inserttable(maintable,"FAIL",[A,{B = Sequence(F,1 <= card), A = Sequence(C), E = Sequence(Z,1 <= card), C = Prod(Z,B), F = Sequence(E,1 <= card)},unlabelled],
"FAIL",EISnb(6190));# (verifier la recurrence)

inserttable(maintable,"FAIL",[A,{B = Prod(Z,C), A = Sequence(B,1 <= card), C = Sequence(A)},unlabelled],
"FAIL",EISnb(6318));# (verifier la reference)

inserttable(maintable,"FAIL",[A,{C = Set(A,card = 2), B = Set(Z,1 <= card), A = Union(C,B)},labelled],
"FAIL",EISnb(6677));#

inserttable(maintable,"FAIL",[A,{C = Sequence(Z,1 <= card), A = Prod(B,Z), B = Set(E), E = Cycle(C)},unlabelled],
"FAIL",EISnb(6951));# (verifier la reference)

inserttable(maintable,"FAIL",[A,{A = Set(B,1 <= card), B = Cycle(E), E = Union(F,C), F = Sequence(Z,card>=1), C = Sequence(Z,1 <= card)},unlabelled],
"FAIL",EISnb(6952));# (verifier la reference)

inserttable(maintable,"FAIL",[A,{C = Sequence(E), E = Sequence(B,1 <= card), A = Prod(B,C), B = Sequence(Z,1 <= card)},unlabelled],
"FAIL",EISnb(7051));# (verifier la forme close)

inserttable(maintable,"FAIL",[A,{F = Sequence(C,1 <= card), A = Sequence(F), B = Sequence(E), E = Sequence(Z,1 <= card), C = Prod(Z,B)},unlabelled],
"FAIL",EISnb(7052));# (verifier la fonction generatrice)

#ECSv1 169
# inserttable(maintable,"Sequence of 2's",[A,{A=Union(Sequence(Z),Sequence(Z))},unlabelled],
# "FAIL",EISnb(7395));#

inserttable(maintable,"FAIL",[A,{B = Sequence(Z,1 <= card), C = Set(Z), A = Prod(B,C)},labelled],
"FAIL",EISnb(7526));#

inserttable(maintable,"FAIL",[A,{C = Prod(Z,E), B = Set(C,1 <= card), A = Set(B,1 <= card), E = Set(Z)},labelled],
"FAIL",EISnb(7550));# (verifier la reference)

inserttable(maintable,"FAIL",[A,{A = Set(B), B = Set(C,1 <= card), C = Prod(Z,A)},unlabelled],
"FAIL",EISnb(7563));# (verifier la reference)

inserttable(maintable,"FAIL",[A,{F = Sequence(E,1 <= card), E = Sequence(C,1 <= card), B = Sequence(F), A = Prod(E,B), C = Sequence(Z,1 <= card)},unlabelled],
"FAIL",EISnb(7582));#(verifier la forme close)

inserttable(maintable,"FAIL",[A,{E = Sequence(Z,1 <= card), A = Union(B,C), C = Sequence(B,1 <= card), B = Sequence(E,1 <= card)},unlabelled],
"FAIL",EISnb(7689));#

inserttable(maintable,"FAIL",[A,{A = Sequence(C), B = Sequence(Z,1 <= card), E = Sequence(Z,card>=1), C = Union(B,E)},unlabelled],
"FAIL",EISnb(8776));# (verifier la forme close)

inserttable(maintable,"FAIL",[A,{B = Sequence(Z,1 <= card), F = Union(C,E), E = Sequence(B,1 <= card), A = Sequence(F), C = Sequence(B,1 <= card)},unlabelled],
"FAIL",EISnb(9117));# (comprendre pourquoi)

inserttable(maintable,"FAIL",[A,{B = Cycle(E), A = Prod(B,C), E = Cycle(Z), C = Set(Z)},labelled],
"FAIL",EISnb(9318));# (verifier la fonction generatrice)

inserttable(maintable,"FAIL",[A,{E = Prod(B,C), A = Cycle(E), B = Set(Z), C = Cycle(Z)},labelled],
"FAIL",EISnb(9324));# (verifier la fonction generatrice)

change:=proc(l::list)
local i;
    "Denumerant",
   [A,subs(Prod(Z)=Z,{A=Prod(seq(Sequence(Prod(Z$i)),i=l))}),unlabelled],
   cat("number of ways to make n cents with coins of", seq(op([" ",i]),i=l),
   " cents")
end:
inserttable(maintable,change([1,2,5,10]),EISnb(8));
inserttable(maintable,change([1,5,10,25]),EISnb(1299));
inserttable(maintable,change([1,5,10,25,50]),EISnb(1300));
inserttable(maintable,change([1,2,5,10,25]),EISnb(1301));
inserttable(maintable,change([1,2,5,10,25,50]),EISnb(1302));
inserttable(maintable,change([1,5,10,20,50,100]),EISnb(1306));
inserttable(maintable,change([1,2,4,10,20,40,100]),EISnb(1310));
inserttable(maintable,change([1,2,5,10,50,100]),EISnb(1312));
inserttable(maintable,change([1,2,5,10,20,50]),EISnb(1313));
inserttable(maintable,change([2,5,5,10,20,50]),EISnb(1314));
inserttable(maintable,change([2,5,10,20,50]),EISnb(1319));
inserttable(maintable,change([5,10,20,50,100]),EISnb(1343));
inserttable(maintable,change([1,2,4,10]),EISnb(1362));
inserttable(maintable,change([1,2,4,12,24,48,96,120]),EISnb(1364));
inserttable(maintable,change([1,1,5]),EISnb(8732));
inserttable(maintable,change([1,1,6]),EISnb(8724));
inserttable(maintable,change([1,1,7]),EISnb(8725));
inserttable(maintable,change([1,1,8]),EISnb(8726));
inserttable(maintable,change([1,1,9]),EISnb(8727));
inserttable(maintable,change([1,1,10]),EISnb(8728));
inserttable(maintable,change([1,1,11]),EISnb(8729));
inserttable(maintable,change([1,1,12]),EISnb(8730));
inserttable(maintable,change([1,1,2,3]),EISnb(601));
inserttable(maintable,change([1,1,2,4]),EISnb(8804));
inserttable(maintable,change([1,1,2,5]),EISnb(1304));
inserttable(maintable,change([1,1,2,3,4]),EISnb(2621));
################## ECS 200 #############################
inserttable(maintable,change([1,1,2,4,10,20]),EISnb(1307));
inserttable(maintable,change([1,1,2,5,10,20]),EISnb(1305));
inserttable(maintable,change([1,1,2,5,10,20]),EISnb(1311));
inserttable(maintable,change([1,1,1,2]),EISnb(2623));
inserttable(maintable,change([1,1,1,2,2]),EISnb(2624));
inserttable(maintable,change([1,1,1,2,2,3]),EISnb(2625));
inserttable(maintable,change([1,1,1,2,2,3,4]),EISnb(2626));
inserttable(maintable,change([1,1,3]),EISnb(1840));
inserttable(maintable,change([1,1,4]),EISnb(1972));
inserttable(maintable,change([1,2]),EISnb(8619));
inserttable(maintable,change([1,3]),EISnb(8620));
inserttable(maintable,change([1,4]),EISnb(8621));
inserttable(maintable,change([2,3]),EISnb(8615));
inserttable(maintable,change([2,5]),EISnb(8616));
inserttable(maintable,change([2,7]),EISnb(8617));
inserttable(maintable,change([2,9]),EISnb(8618));
inserttable(maintable,change([3,4]),EISnb(8679));
inserttable(maintable,change([3,5]),EISnb(8676));
inserttable(maintable,change([1,3,5]),EISnb(8672));
inserttable(maintable,change([1,3,9]),EISnb(8649));
inserttable(maintable,change([1,4,16]),EISnb(8652));
inserttable(maintable,change([1,5,25]),EISnb(8648));
inserttable(maintable,change([2,2,3]),EISnb(8731));
inserttable(maintable,change([2,2,5]),EISnb(8720));
inserttable(maintable,change([2,2,7]),EISnb(8721));
inserttable(maintable,change([2,2,9]),EISnb(8722));
inserttable(maintable,change([2,2,11]),EISnb(8723));
inserttable(maintable,change([2,3,7]),EISnb(8671));
inserttable(maintable,change([3,4,5]),EISnb(8680));
inserttable(maintable,change([4,5,6]),EISnb(8682));
inserttable(maintable,change([3,5,7]),EISnb(8677));
inserttable(maintable,change([4,6,7]),EISnb(8622));
inserttable(maintable,change([1,2,2,3]),EISnb(8763));
inserttable(maintable,change([1,2,4,8]),EISnb(8643));
inserttable(maintable,change([1,3,5,7]),EISnb(8673));
inserttable(maintable,change([1,3,9,27]),EISnb(8650));
inserttable(maintable,change([2,6,8,12]),EISnb(8670));
inserttable(maintable,change([3,4,5,6]),EISnb(8681));
inserttable(maintable,change([3,5,7,9]),EISnb(8678));
inserttable(maintable,change([4,8,12,20]),EISnb(8669));
inserttable(maintable,change([2,12,20,30]),EISnb(8668));
inserttable(maintable,change([12,18,24,30]),EISnb(8667));
inserttable(maintable,change([1,4,6,12]),EISnb(8719));
inserttable(maintable,change([1,2,4,8,16]),EISnb(8644));
inserttable(maintable,change([1,3,5,7,9]),EISnb(8674));
inserttable(maintable,change([1,2,4,8,16,32]),EISnb(8645));
inserttable(maintable,change([1,3,5,7,9,11]),EISnb(8675));
inserttable(maintable,change([1,4,6,10,12,18]),EISnb(8666));
inserttable(maintable,change([2,5,6,8,9,12]),EISnb(8584));
inserttable(maintable,change([6,12,18,24,30,42]),EISnb(8581));
inserttable(maintable,change([2,6,8,10,12,14,18]),EISnb(8583));
inserttable(maintable,change([1,1,2,6,12,24,48,60]),EISnb(1365));
inserttable(maintable,change([2,8,12,14,18,20,24,30]),EISnb(8582));

binomialnk:=proc(k, ind)
   inserttable(maintable,"Binomial coefficients",
    [A,{A=Prod(B$k),B=Sequence(Z)},unlabelled],
    cat("Arrangements of n unlabelled objects in ",k," rows"),
    EISnb(ind))
end:

binomialnk(3,217);
binomailnk(4,292);
binomialnk(5,332);
binomialnk(6,389);
binomialnk(7,579);
binomialnk(8,580);
binomialnk(9,581);
binomialnk(10,582);
binomialnk(11,1287);
binomialnk(12,1288);

factoverk:=proc(k,ind)
   inserttable(maintable,"Arrangements",
    [A,{A=Prod(B$k),B=Sequence(Z)},unlabelled],
    cat("Arrangements of n labelled objects in ",k," rows"),
    EISnb(ind))
end:

factoverk(3,1710);
factoverk(4,1715);
factoverk(5,1720);
factoverk(6,1725);
factoverk(7,1730);

powersofk:=proc(k,ind)
   inserttable(maintable,cat("Powers of ",k),
    [A,{A=Prod(B$k),B=Set(Z)},labelled],
    cat("Arrangement of n distinguishable objects in ",k,"distinguishable urns")
    ,EISnb(ind))
end:

powersofk(2,79);
powersofk(3,244);
powersofk(4,302);
powersofk(5,351);
powersofk(6,400);
powersofk(7,420);
powersofk(8,1018);
powersofk(9,1019);
powersofk(11,1020);
powersofk(12,1021);
powersofk(13,1022);
powersofk(14,1023);
powersofk(15,1024);
powersofk(16,1025);
powersofk(17,1026);
powersofk(18,1027);
powersofk(19,1029);

dissect:=proc(k,ind)
    inserttable(maintable,"Trees",
    [A,{A=Sequence(B), B=Prod(Z,A$k)},unlabelled],
    cat("Planar trees with arity ",k),EISnb(ind))
end:

dissect(1,108);
dissect(2,1764);
dissect(3,2293);
dissect(4,2294);
dissect(5,2295);
dissect(6,2296);
dissect(7,7556);

iterexp:=proc(k,ind)
local i;
    inserttable(maintable,"Hierarchies",
    [A[0],{A[0]=Set(A[1],card>=1),seq(A[i]=Set(A[i+1],card>=1),i=1..k-1),A[k]=Z},
        labelled],
    cat("Hierarchies of height ",k),EISnb(ind))
end:

iterexp(2,110);
iterexp(3,258);
iterexp(4,307);
iterexp(5,357);
iterexp(6,405);
iterexp(7,1669);

mobile1:=proc(k,ind)
local i;
    inserttable(maintable,"Mobiles",
    [A[0],{A[0]=Cycle(A[1]),seq(A[i]=Cycle(A[i+1]),i=1..k-1),A[k]=Z},labelled],
    cat("Mobiles of height ",k),EISnb(ind))
end:

mobile1(1,142);
mobile1(2,3713);
mobile1(3,268);
mobile1(4,310);
mobile1(5,359);
mobile1(6,406);
mobile1(7,1765);

coordseq:=proc(k,ind)
   inserttable(maintable,"FAIL",
    [A,subs(Union(Z)=Z,{A = Prod(Union(Epsilon,Z),Sequence(Union(Z$k)))}),
        unlabelled],
    "FAIL",
    EISnb(ind))
end:

#ECSv1 308    coordseq(1,7395);
coordseq(2,3945);
coordseq(3,3946);
coordseq(4,3947);
coordseq(5,3948);
coordseq(6,3949);
coordseq(7,3950);
coordseq(8,3951);
coordseq(9,3952);
coordseq(10,3953);
coordseq(11,3954);

multk:=proc(k,ind)
    inserttable(maintable,cat("Multiples of ",k),
    [A,{A=Prod(Union(Z$k),Sequence(Z),Sequence(Z))},unlabelled],
    cat("Multiples of ",k),EISnb(ind))
end:

multk(2,5843);
multk(3,8585);
multk(4,8586);
multk(5,8587);
multk(6,8588);
multk(7,8589);
multk(8,8590);
multk(9,8591);
multk(10,8592);
multk(11,8593);
multk(12,8594);
multk(13,8595);
multk(14,8596);
multk(15,8597);
multk(16,8598);
multk(17,8599);
multk(18,8600);
multk(19,8601);
multk(20,8602);
multk(21,8603);
multk(22,8604);
multk(23,8605);
multk(24,8606);
multk(25,8607);

poldeg2:=proc(k,ind)
    inserttable(maintable,"k-gonal numbers",
   [A,{A=Union(B,E$k),E=Prod(Z,B),B=Prod(C,C,C),C=Sequence(Z)},unlabelled],
    cat(" ",k,"-gonal numbers"),
    EISnb(ind))
end:

poldeg2(1,290);
poldeg2(2,326);
poldeg2(3,384);
poldeg2(4,566);
poldeg2(5,567);
poldeg2(6,1106);
poldeg2(7,1107);

limpart:=proc(k,ind)
local i;
    inserttable(maintable,"Stirling numbers of the second kind",
   [A,{A=Prod(Sequence(Z),seq(Sequence(Union(Z$i)),i=2..k))},unlabelled],
   cat("Partitions of a set of size n into ",k," non-empty subsets"),
    EISnb(ind))
end:

limpart(2,225);
limpart(3,392);
limpart(4,453);
limpart(5,481);
limpart(6,770);
limpart(7,771);

limpart2:=proc(k,ind)
local i;
    inserttable(maintable,"Integer partition",
        [A,{A = Set(Sequence(Z,card>=1),card<=k)},unlabelled],
        cat("Partition of n in at most ",k," parts"),
        EISnb(ind))
end:

limpart2(2,8619);
limpart2(3,1399);
limpart2(4,1400);
limpart2(5,1401);
limpart2(6,1402);
limpart2(7,8636);
limpart2(8,8637);
limpart2(9,8638);
limpart2(10,8639);
limpart2(11,8640);
limpart2(12,8641);

sumpow:=proc(k,ind)
local i;
    inserttable(maintable,"Lists",
        [A,subs(Union(Z)=Z,{A=Union(seq(Sequence(Union(Z$i)),i=1..k))}),
            unlabelled],
        "Sum of nth powers",
        EISnb(ind))
end:

sumpow(2,51);
sumpow(3,1550);
sumpow(4,1551);
sumpow(5,1552);
sumpow(6,1553);
sumpow(7,1554);
sumpow(8,1555);
sumpow(9,1556);
sumpow(10,1557);

sumpow2:=proc(k,ind)
    inserttable(maintable,"Sums of powers",
        [A,{A = Prod(Sequence(Z),Sequence(Union(Z$k)))},unlabelled],
        "FAIL",EISnb(ind))
end:

sumpow2(2,225);
sumpow2(3,3462);
sumpow2(4,2450);
sumpow2(5,3463);
sumpow2(6,3464);

fibolike:=proc(k,ind)
    inserttable(maintable,"Fibonacci like recurrence",
    [A,{A = Sequence(Prod(Z,Sequence(Prod(Z$k))))},unlabelled],
    "FAIL",EISnb(ind))
end:

fibolike(3,930);
fibolike(4,3269);
fibolike(5,3520);
fibolike(6,5708);
fibolike(7,5709);
fibolike(8,5710);
fibolike(9,5711);


inserttable(maintable,"FAIL",
    [A,{A = Union(Prod(Z,Sequence(Prod(Z,Z))),Sequence(Z))},unlabelled],
    "FAIL",
EISnb( 34));

inserttable(maintable,"FAIL",
    [A,{C = Prod(B,Z), E = Union(A,B), A = Prod(C,E), B = Union(C,Z)},unlabelled],
    "FAIL",
EISnb( 71));

inserttable(maintable,"FAIL",
    [A,{B = Set(C), A = Prod(C,B), C = Prod(Z,B)},unlabelled],
    "FAIL",
EISnb( 106));

inserttable(maintable,"FAIL",
    [A,{A = Union(Sequence(Z),Prod(Z,Sequence(Z),Sequence(Z),Sequence(Z)))},unlabelled],
    "FAIL",
EISnb( 124));

inserttable(maintable,"FAIL",
    [A,{B = Set(A), A = Prod(Z,B,B)},unlabelled],
    "FAIL",
EISnb( 151));

inserttable(maintable,"FAIL",
    [A,{A = Prod(Sequence(Union(Z,Z)),Sequence(Z),Sequence(Z))},unlabelled],
    "FAIL",
EISnb( 295));

inserttable(maintable,"FAIL",
    [A,{A = Prod(Sequence(Union(Z,Z,Z)),Sequence(Z),Sequence(Z))},unlabelled],
    "FAIL",
EISnb( 340));

inserttable(maintable,"FAIL",
    [A,{B = Set(C), C = Sequence(Z,card>=1), A = Prod(C,B,B)},unlabelled],
    "FAIL",
EISnb( 713));

inserttable(maintable,"FAIL",
    [A,{A = Prod(B,B,B), C = Sequence(Z,1 <= card), B = Set(C)},unlabelled],
    "FAIL",
EISnb( 716));

inserttable(maintable,"FAIL",
    [A,{A = Prod(Sequence(Z),Sequence(Z),Union(Z,Sequence(Z)))},labelled],
    "FAIL",
EISnb( 780));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Prod(Z,Union(Z,Prod(Z,Z))))},unlabelled],
    "FAIL",
EISnb( 931));

inserttable(maintable,"FAIL",
    [A,{A = Prod(Sequence(Union(Z,Z)),Sequence(Prod(Z,Z)))},unlabelled],
    "FAIL",
EISnb( 975));

inserttable(maintable,"FAIL",
    [A,{A = Union(B,C,Z), C = Prod(A,B), B = Prod(A,A)},unlabelled],
    "FAIL",
EISnb( 1002));

inserttable(maintable,"FAIL",
    [A,{B = Prod(A,C), A = Union(Z,C,B), C = Prod(A,A,Z)},unlabelled],
    "FAIL",
EISnb( 1005)); # verifier la reference

inserttable(maintable,"FAIL",
    [A,{B = Set(Z), C = Set(Z,1 <= card), A = Prod(C,B,B)},labelled],
    "FAIL",
EISnb( 1047));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Union(Z,Z,Z,Z,Prod(Z,Z)))},unlabelled],
    "FAIL",
EISnb( 1076));

inserttable(maintable,"FAIL",
    [A,{A = Prod(C,C,B), C = Sequence(Z), B = Sequence(Z,1 <= card)},labelled],
    "FAIL",
EISnb( 1286 )); # (verifier la forme close)

inserttable(maintable,"FAIL",
    [A,{A = Prod(C,B,B), B = Sequence(Z), C = Set(Z)},labelled],
    "FAIL",
EISnb( 1339));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Prod(Z,Sequence(Z),Union(Z,Prod(Z,Z))))},unlabelled],
    "FAIL",
EISnb( 1590));

inserttable(maintable,"FAIL",
    [A,{A = Union(Sequence(Prod(Z,Z,Sequence(Z))),Sequence(Z))},unlabelled],
    "FAIL",
EISnb( 1611));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Union(Z,Prod(Union(Z,Z,Z,Z),Sequence(Z))))},unlabelled],
    "FAIL",
EISnb( 1653));

inserttable(maintable,"FAIL",
    [A,{A = Set(B,1 <= card), B = Prod(C,Z), C = Union(Z,A)},unlabelled],
    "FAIL",
EISnb( 1678 )); # verifier la reference

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Prod(Union(Z,Prod(Z,Z,Z,Z)),Z))},unlabelled],
    "FAIL",
EISnb( 1687));

inserttable(maintable,"FAIL",
    [A,{B = Set(C), C = Cycle(Z), A = Prod(C,B,B)},labelled],
    "FAIL",
EISnb( 1705));

inserttable(maintable,"FAIL",
    [A,{B = Prod(Z,A,A), A = Sequence(B)},labelled],
    "FAIL",
EISnb( 1763 )); # verifier la forme close

inserttable(maintable,"FAIL",
    [A,{A = Prod(Z,B,B), B = Set(Z)},labelled],
    "FAIL",
EISnb( 1787));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Union(Z,Prod(Union(Z,Z),Sequence(Z))))},unlabelled],
    "FAIL",
EISnb( 1835));

inserttable(maintable,"FAIL",
    [A,{B = Set(Z), A = Prod(Z,Z,B)},labelled],
    "FAIL",
EISnb( 2378));

inserttable(maintable,"FAIL",
    [A,{B = Prod(C,C), C = Union(B,Z), A = Union(B,C,Z)},unlabelled],
    "leaves in binary trees or pairs of binary trees",
EISnb( 2420));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(B), B = Prod(C,C,Z), C = Union(Epsilon,Z)},unlabelled],
    "FAIL",
EISnb( 2478 )); # verifier la recurrence

inserttable(maintable,"FAIL",
    [A,{A = Prod(Sequence(Prod(Z,Z)),Sequence(Z),Sequence(Z),Sequence(Z))},unlabelled],
    "FAIL",
EISnb( 2623));

inserttable(maintable,"FAIL",
    [A,{A = Prod(Z,B,B,B,B), B = Set(Z)},labelled],
    "FAIL",
EISnb( 2697));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(B), B = Sequence(C,1 <= card), C = Prod(Z,A,A)},unlabelled],
    "FAIL",
EISnb( 3168 )); # verifier la forme close

inserttable(maintable,"FAIL",
    [A,{A = Sequence(B,1 <= card), C = Sequence(A), B = Prod(C,C,Z)},unlabelled],
    "FAIL",
EISnb( 3169));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Union(Z,Prod(Z,Z,Union(Z,Z))))},unlabelled],
    "FAIL",
EISnb( 3229));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Union(Z,Prod(Z,Union(Z,Sequence(Z)),Sequence(Z))))},unlabelled],
    "FAIL",
EISnb( 3480));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Prod(Sequence(Union(Z,Z)),Union(Prod(Z,Z),Z)))},unlabelled],
    "FAIL",
EISnb( 3688 )); # verifier la reference

inserttable(maintable,"FAIL",
    [A,{A = Prod(Union(Z,Sequence(Prod(Z,Z))),Sequence(Z),Sequence(Z))},unlabelled],
    "FAIL",
EISnb( 4116 )); # en verifier plus

inserttable(maintable,"FAIL",
    [A,{C = Sequence(B,1 <= card), A = Union(C,Z), B = Prod(Z,A)},unlabelled],
    "FAIL",
EISnb( 4148 )); # verifier la forme close

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Prod(Sequence(Prod(Union(Z,Z,Z),Sequence(Z))),Z))},unlabelled],
    "FAIL",
EISnb( 4253));

inserttable(maintable,"FAIL",
    [A,{A = Union(Z,C,B), C = Prod(Z,B), B = Prod(A,A,A)},unlabelled],
    "FAIL",
EISnb( 5043 )); # verifier la recurrence

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Union(Z,Prod(Z$4,Sequence(Z))))},unlabelled],
    "FAIL",
EISnb( 5252));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Union(Z,Prod(Z$5,Sequence(Z))))},unlabelled],
    "FAIL",
EISnb( 5253));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Prod(Z,Z,Z,Sequence(Z),Sequence(Z)))},unlabelled],
    "FAIL",
EISnb( 5314));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Union(Prod(Z,Z),Z,Z,Z,Z,Z,Z))},unlabelled],
    "FAIL",
EISnb( 5668));

inserttable(maintable,"FAIL",
    [A,{B = Prod(Z,A,A), A = Set(B)},unlabelled],
    "FAIL",
EISnb( 5750));

inserttable(maintable,"FAIL",
    [A,{B = PowerSet(A), A = Prod(C,Z), C = Prod(B,B)},unlabelled],
    "FAIL",
EISnb( 5753));

inserttable(maintable,"FAIL",
    [A,{B = PowerSet(C), C = Prod(A,B), A = Prod(Z,B)},unlabelled],
    "FAIL",
EISnb( 5754));

inserttable(maintable,"FAIL",
    [A,{A = Union(C,B), C = Prod(Z,B), B = Sequence(C)},unlabelled],
    "FAIL",
EISnb( 5807));

inserttable(maintable,"FAIL",
    [A,{A = Prod(Z,B,B), B = Sequence(A)},unlabelled],
    "FAIL",
EISnb( 6013));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Prod(Sequence(Prod(Z,Z)),Prod(Z,Z,Sequence(Z))))},unlabelled],
    "FAIL",
EISnb( 6053));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Prod(Z,Sequence(Prod(Z,Z)),Sequence(Z)))},unlabelled],
    "FAIL",
EISnb( 6054));

inserttable(maintable,"FAIL",
    [A,{A = Prod(C,B), C = Union(Z,B), B = Set(Z)},labelled],
    "FAIL",
EISnb( 6127));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Union(Z,Prod(Z,Union(Z,Z,Z))))},unlabelled],
    "FAIL",
EISnb( 6130));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Union(Z,Prod(Z,Union(Z,Z,Z,Z))))},unlabelled],
    "FAIL",
EISnb( 6131));

inserttable(maintable,"FAIL",
    [A,{A = Prod(B,B,B,Z), B = Sequence(A)},unlabelled],
    "FAIL",
EISnb( 6632));

inserttable(maintable,"FAIL",
    [A,{B = Set(A), A = Prod(B,B,B,Z)},unlabelled],
    "FAIL",
EISnb( 6964));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Prod(Z,Sequence(Union(Z,Z)),Sequence(Z)))},unlabelled],
    "FAIL",
EISnb( 7070));

inserttable(maintable,"FAIL",
    [A,{A = Union(Z,Prod(Z,Union(Z,Prod(A,A))))},unlabelled],
    "FAIL",
EISnb( 7477));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Union(Z,Prod(Z,Union(Z,Z)),Union(Z,Z)))},unlabelled],
    "FAIL",
EISnb( 7482));

inserttable(maintable,"FAIL",
    [A,{E = Union(B,C), B = Union(A,C), C = Prod(A,E), A = Union(C,Z)},unlabelled],
    "FAIL",
EISnb( 7564));

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Union(Prod(Z,Z,Union(Z,Z),Sequence(Z)),Z))},unlabelled],
    "FAIL",
EISnb( 7909 )); # verifier plus de nombres et la reference

inserttable(maintable,"FAIL",
    [A,{A = Sequence(Prod(Z,Union(Z,Z,Prod(Z,Z))))},unlabelled],
    "FAIL",
EISnb( 8346));

inserttable(maintable,"FAIL",
    [A,{C = Sequence(Z,1 <= card), B = Prod(C,Z,Z), A = Set(B)},unlabelled],
    "FAIL",
EISnb( 8483));

inserttable(maintable,"FAIL",
    [A,{A = Prod(Union(Z,Sequence(Z)),Sequence(Prod(Z,Z,Z)))},unlabelled],
    "FAIL",
EISnb( 8611));

inserttable(maintable,"FAIL",
    [A,{A = Set(C), C = Prod(Z,Z,B), B = Union(Z,Epsilon)},unlabelled],
    "FAIL",
EISnb( 8615));

inserttable(maintable,"FAIL",
    [A,{A = Prod(Z,Sequence(Prod(Z,Z,Z)),Sequence(Z))},unlabelled],
    "FAIL",
EISnb( 8620));

inserttable(maintable,"FAIL",
    [A,{B = Union(Epsilon,Z), C = Prod(Z,B,B), A = Set(C)},unlabelled],
    "FAIL",
EISnb( 8763));

### Rational functions that were not in the EIS

## former 457
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Z,Union(Z,Sequence(Z)))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Union(Z,Sequence(Z))))}, unlabelled],"FAIL",EISnb(6356));
## former 459
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Union(Z,Z,Sequence(Z)))}, unlabelled],"FAIL","FAIL");
## former 460
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Sequence(Prod(Z,Sequence(Z))))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Union(Z,Z,Prod(Z,Z,Z)))}, unlabelled],"FAIL",EISnb(8998));
# inserttable(maintable,"A simple regular expression",[A,
#   {A = Union(Prod(Z,Z),Sequence(Union(Z,Z)))}, unlabelled],"FAIL","FAIL");
## former 463
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Union(Z,Sequence(Z),Sequence(Z)))}, unlabelled],"FAIL","FAIL");
## former 464
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Union(Z,Sequence(Union(Z,Z))))}, unlabelled],"FAIL","FAIL");
## former 465
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Z,Sequence(Prod(Z,Sequence(Z))))}, unlabelled],"FAIL","FAIL");
## former 466
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Union(Z,Sequence(Z)),Sequence(Z))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Z,Union(Z,Sequence(Z))))}, unlabelled],"FAIL",EISnb(52527));
inserttable(maintable,"A simple regular expression",[A,
   {A = Union(Sequence(Z),Sequence(Union(Z,Z,Z)))}, unlabelled],"FAIL",EISnb(34472));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Union(Z,Z,Sequence(Z))))}, unlabelled],"FAIL",EISnb(52528));
## former 470
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Union(Z,Z,Z,Sequence(Z)))}, unlabelled],"FAIL","FAIL");
## fformer 471
#inserttable(maintable,"A simple regular expression",[A,
#   {A = Prod(Z,Sequence(Prod(Z,Z,Sequence(Z))))}, unlabelled],"FAIL","FAIL");
## former 472
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Union(Z,Sequence(Z)),Union(Z,Z))}, unlabelled],"FAIL","FAIL");
## former 473
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Z,Union(Z,Z,Sequence(Z)))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Sequence(Z),Union(Z,Z,Z)))}, unlabelled],"FAIL",EISnb(2001));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Sequence(Z),Union(Z,Z)))}, unlabelled],"FAIL",EISnb(14113));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Union(Z,Z,Z,Z),Sequence(Z)))}, unlabelled],"FAIL",EISnb(5054));
# inserttable(maintable,"A simple regular expression",[A,
#    {A = Union(Sequence(Union(Z,Z)),Prod(Z,Z,Z))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Sequence(Z),Sequence(Z),Sequence(Z)))}, unlabelled],"FAIL",EISnb(52529));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Union(Z,Z),Sequence(Z),Sequence(Z)))}, unlabelled],"FAIL",EISnb(52530));
inserttable(maintable,"A simple regular expression",[A,
   {A = Union(Sequence(Union(Z,Z)),Sequence(Prod(Z,Z)))}, unlabelled],"FAIL",EISnb(52531));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Z,Z,Union(Z,Sequence(Z))))}, unlabelled],"FAIL",EISnb(52532));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Union(Z,Z,Z),Sequence(Z)))}, unlabelled],"FAIL",EISnb(52533));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Union(Z,Prod(Z,Sequence(Prod(Z,Z)))))}, unlabelled],"FAIL",EISnb(52534));
## former 484
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Z,Union(Z,Z,Z,Sequence(Z)))}, unlabelled],"FAIL","FAIL");
## former 485
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Union(Z,Z),Union(Z,Z,Sequence(Z)))}, unlabelled],"FAIL","FAIL");
## fformer 486
#inserttable(maintable,"A simple regular expression",[A,
#   {A = Prod(Z,Union(Z,Sequence(Union(Z,Z,Z))))}, unlabelled],"FAIL","FAIL");

###ffformer 480 ???
#inserttable(maintable,"A simple regular expression",[A,
#   {A = Prod(Z,Sequence(Prod(Z,Sequence(Z),Sequence(Z))))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Union(Z,Sequence(Prod(Z,Z)))))}, unlabelled],"FAIL",EISnb(52535));
inserttable(maintable,"A simple regular expression",[A,
   {A = Union(Sequence(Prod(Z,Z)),Sequence(Z),Sequence(Z))}, unlabelled],"FAIL",EISnb(10693));
## former 490
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Union(Prod(Z,Z,Union(Z,Z)),Sequence(Z))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Union(Z,Prod(Z,Union(Z,Sequence(Z)))))}, unlabelled],"FAIL",EISnb(52536));
###ffformer 484 ???
#inserttable(maintable,"A simple regular expression",[A,
#   {A = Prod(Z,Sequence(Prod(Union(Z,Z),Sequence(Z))))}, unlabelled],"FAIL","FAIL");
## fformer 493
#inserttable(maintable,"A simple regular expression",[A,
#   {A = Prod(Z,Sequence(Prod(Z,Sequence(Union(Z,Z)))))}, unlabelled],"FAIL","FAIL");
## fformer 494
#inserttable(maintable,"A simple regular expression",[A,
#   {A = Prod(Z,Z,Sequence(Prod(Z,Z,Sequence(Z))))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Z,Union(Z,Z),Sequence(Z)))}, unlabelled],"FAIL",EISnb(52537));
## fformer 496
#inserttable(maintable,"A simple regular expression",[A,
#   {A = Sequence(Prod(Z,Z,Z,Sequence(Prod(Z,Z))))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Union(Z,Z,Z,Sequence(Z))))}, unlabelled],"FAIL",EISnb(52538));
inserttable(maintable,"A simple regular expression",[A,
   {A = Union(Sequence(Union(Z,Z,Z,Z)),Sequence(Z))}, unlabelled],"FAIL",EISnb(52539));
inserttable(maintable,"A simple regular expression",[A,
   {A = Prod(Union(Z,Sequence(Z)),Sequence(Z),Sequence(Z))}, unlabelled],"FAIL",EISnb(46231));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Union(Prod(Z,Z),Sequence(Z))))}, unlabelled],"FAIL",EISnb(52540));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Union(Z,Sequence(Z),Sequence(Z))))}, unlabelled],"FAIL",EISnb(30186));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Union(Z,Z,Z,Prod(Z,Sequence(Z))))}, unlabelled],"FAIL",EISnb(18902));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Union(Z,Z,Z,Prod(Z,Z,Z)))}, unlabelled],"FAIL",EISnb(52541));
## former 504
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Union(Z,Sequence(Prod(Z,Sequence(Z)))))}, unlabelled],"FAIL","FAIL");
## former 505
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Union(Prod(Z,Z),Sequence(Z)),Sequence(Z))}, unlabelled],"FAIL","FAIL");
## former 506
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Union(Z,Z),Union(Z,Sequence(Z)))}, unlabelled],"FAIL","FAIL");
## former 507
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Union(Z,Prod(Sequence(Z),Sequence(Z))))}, unlabelled],"FAIL","FAIL");
## former 508
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Z,Z,Union(Z,Z,Sequence(Z)))}, unlabelled],"FAIL","FAIL");
## fformer 509
#inserttable(maintable,"A simple regular expression",[A,
#   {A = Prod(Z,Sequence(Prod(Z,Z,Z,Sequence(Z))))}, unlabelled],"FAIL","FAIL");
## fformer 510
#inserttable(maintable,"A simple regular expression",[A,
#   {A = Prod(Z,Sequence(Prod(Z,Union(Z,Sequence(Z)))))}, unlabelled],"FAIL","FAIL");
## fformer 511
#inserttable(maintable,"A simple regular expression",[A,
#   {A = Sequence(Prod(Z,Z,Sequence(Prod(Z,Sequence(Z)))))}, unlabelled],"FAIL","FAIL");
## fformer 512
#inserttable(maintable,"A simple regular expression",[A,
#   {A = Sequence(Prod(Z,Z,Sequence(Prod(Z,Z,Z))))}, unlabelled],"FAIL","FAIL");

inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Union(Z,Z,Prod(Z,Union(Z,Z))))}, unlabelled],"FAIL",EISnb(2605));
## former 514
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Z,Union(Z,Sequence(Z),Sequence(Z)))}, unlabelled],"FAIL","FAIL");
## former 515
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Union(Z,Prod(Z,Z),Sequence(Z)))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Union(Z,Z),Sequence(Prod(Z,Z))))}, unlabelled],"FAIL",EISnb(52542));
## former 517
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Z,Union(Z,Sequence(Union(Z,Z))))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Union(Z,Z),Union(Z,Sequence(Z))))}, unlabelled],"FAIL",EISnb(52543));
## former 519
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Z,Z,Sequence(Prod(Z,Sequence(Z))))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Union(Z,Z,Prod(Z,Z,Z,Z)))}, unlabelled],"FAIL",EISnb(8999));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Union(Z,Prod(Z,Sequence(Z),Sequence(Z))))}, unlabelled],"FAIL",EISnb(52544));
## former 522
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Z,Union(Z,Sequence(Z)),Sequence(Z))}, unlabelled],"FAIL","FAIL");
## former 523
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Union(Z,Z,Sequence(Union(Z,Z))))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Union(Z,Sequence(Z)),Sequence(Z)))}, unlabelled],"FAIL",EISnb(52545));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Z,Union(Z,Z,Sequence(Z))))}, unlabelled],"FAIL",EISnb(52546));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Union(Z,Prod(Z,Sequence(Z)))))}, unlabelled],"FAIL",EISnb(52547));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Z,Union(Z,Prod(Z,Z))))}, unlabelled],"FAIL",EISnb(17817));
## former 528
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Union(Prod(Z,Z,Z),Sequence(Z),Sequence(Z))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Union(Sequence(Union(Z,Z)),Sequence(Z),Sequence(Z))}, unlabelled],"FAIL",EISnb(52548));
## former 530
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Union(Prod(Z,Z),Prod(Sequence(Z),Sequence(Z)))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Prod(Sequence(Union(Z,Z)),Union(Z,Sequence(Z)))}, unlabelled],"FAIL",EISnb(52549));
inserttable(maintable,"A simple regular expression",[A,
   {A = Sequence(Prod(Z,Union(Z,Sequence(Union(Z,Z)))))}, unlabelled],"FAIL",EISnb(52550));
# inserttable(maintable,"A simple regular expression",[A,
#    {A = Prod(Union(Z,Z,Z),Union(Z,Sequence(Z)))}, unlabelled],"FAIL","FAIL");
## former 534
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Union(Z,Z,Z,Z,Sequence(Z)))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Prod(Sequence(Prod(Z,Union(Z,Z))),Sequence(Z))}, unlabelled],"FAIL",EISnb(52551));
# inserttable(maintable,"A simple regular expression",[A,
#    {A = Union(Prod(Z,Z),Sequence(Union(Z,Z,Z)))}, unlabelled],"FAIL","FAIL");
## former 537
##inserttable(maintable,"A simple regular expression",[A,
##   {A = Prod(Z,Union(Z,Z,Sequence(Z)),Sequence(Z))}, unlabelled],"FAIL","FAIL");
###ffformer 515 ???
#inserttable(maintable,"A simple regular expression",[A,
#   {A = Sequence(Prod(Z,Z,Z,Sequence(Union(Z,Z))))}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression",[A,
   {A = Union(Sequence(Prod(Z,Union(Z,Z))),Sequence(Z))}, unlabelled],"FAIL",EISnb(52552));
# inserttable(maintable,"A simple regular expression",[A,
#    {A = Prod(Union(Z,Sequence(Z)),Union(Z,Sequence(Z)))}, unlabelled],"FAIL","FAIL");


inserttable(maintable,"A simple regular expression in a labelled universe",
  [A, {A = Union(Sequence(Z),Sequence(Z))}, labelled],"FAIL",EISnb(52553));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Z,Z))}, labelled],"FAIL",EISnb(32031));
## former 543
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Z,Z,Z))}, labelled],"FAIL",EISnb(47053));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Sequence(Z)))}, labelled],"FAIL",EISnb(52554));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Prod(Z,Z)))}, labelled],"FAIL",EISnb(5442));
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Union(Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
#ECSv1 506
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Sequence(Z),Sequence(Z))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Sequence(Union(Z,Z))))}, labelled],"FAIL",EISnb(34001));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Z,Prod(Z,Z)))}, labelled],"FAIL",EISnb(52555));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Prod(Z,Z,Z)))}, labelled],"FAIL",EISnb(52556));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Sequence(Union(Z,Z)))}, labelled],"FAIL",EISnb(29767));
## former 553
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Z),Sequence(Z))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Z,Sequence(Z)))}, labelled],"FAIL",EISnb(52557));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Sequence(Prod(Z,Z)))}, labelled],"FAIL",EISnb(52558));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Union(Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52559));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Z),Sequence(Z),Sequence(Z))}, labelled],"FAIL",EISnb(52560));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Z),Sequence(Union(Z,Z)))}, labelled],"FAIL",EISnb(52561));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Z,Z,Z,Z))}, labelled],"FAIL",EISnb(47054));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Union(Z,Z),Sequence(Z)))}, labelled],"FAIL",EISnb(52563));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Sequence(Prod(Z,Sequence(Z))))}, labelled],"FAIL",EISnb(14297));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Z),Prod(Z,Z,Z))}, labelled],"FAIL",EISnb(52565));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Z),Sequence(Prod(Z,Z)))}, labelled],"FAIL",EISnb(52566));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Sequence(Z),Sequence(Z)))}, labelled],"FAIL",EISnb(52567));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Prod(Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52568));
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Sequence(Union(Z,Z)))}, labelled],"FAIL","FAIL");
## former 567
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Sequence(Prod(Z,Z))))}, labelled],"FAIL",EISnb(5443));
## former 569
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Z,Sequence(Z),Union(Z,Z))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Prod(Z,Z,Z)),Sequence(Z))}, labelled],"FAIL",EISnb(52569));
#ECSv1 525
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Z,Union(Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Sequence(Union(Z,Z,Z,Z)))}, labelled],"FAIL",EISnb(34177));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Z,Sequence(Z),Sequence(Z))}, labelled],"FAIL",EISnb(52571));
## former 574
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Sequence(Z),Union(Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
## former 575
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Sequence(Z),Sequence(Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Union(Z,Z,Sequence(Z)),Sequence(Z))}, labelled],"FAIL",EISnb(52572));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Z),Sequence(Union(Z,Z,Z)))}, labelled],"FAIL",EISnb(52573));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Sequence(Prod(Z,Sequence(Z)))))}, labelled],"FAIL",EISnb(52574));
## former 579
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Union(Prod(Z,Z,Sequence(Z)),Sequence(Z))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Union(Z,Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52575));

## former 581
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Prod(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
## fformer 582
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Z),Union(Z,Z,Z))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Prod(Z,Z),Sequence(Union(Z,Z)))}, labelled],"FAIL",EISnb(52576));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Sequence(Union(Z,Z,Z)))}, labelled],"FAIL",EISnb(52577));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Z,Sequence(Union(Z,Z)))}, labelled],"FAIL",EISnb(51578));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Union(Z,Z,Z,Z),Sequence(Z))}, labelled],"FAIL",EISnb(52578));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Prod(Z,Z,Z)),Sequence(Z))}, labelled],"FAIL",EISnb(52579));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Sequence(Union(Z,Z))))}, labelled],"FAIL",EISnb(52580));
## former 589
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Union(Sequence(Z),Sequence(Z)),Sequence(Z))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Z,Z,Sequence(Z)))}, labelled],"FAIL",EISnb(52581));
## former 591
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Sequence(Z),Union(Z,Z))}, labelled],"FAIL",EISnb(52582)); #ECSv1 540
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Sequence(Union(Z,Z,Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Sequence(Prod(Z,Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52583));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Z,Z,Z,Z,Z))}, labelled],"FAIL",EISnb(47058));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Prod(Z,Sequence(Z))),Sequence(Z))}, labelled],"FAIL",EISnb(52584));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Prod(Z,Union(Z,Z))))}, labelled],"FAIL",EISnb(52585));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Sequence(Prod(Z,Z,Z))))}, labelled],"FAIL",EISnb(52586));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Sequence(Prod(Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52587));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Union(Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52588));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Sequence(Z),Sequence(Union(Z,Z)))}, labelled],"FAIL",EISnb(52589));
## former 602
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Union(Z,Z),Union(Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Z,Prod(Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52590));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Sequence(Z),Sequence(Prod(Z,Z)))}, labelled],"FAIL",EISnb(52591));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Sequence(Z),Union(Z,Z,Z)))}, labelled],"FAIL",EISnb(52592));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Prod(Z,Z,Z,Z)))}, labelled],"FAIL",EISnb(52593));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Union(Z,Sequence(Union(Z,Z))))}, labelled],"FAIL",EISnb(52594));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Z,Z,Prod(Z,Z)))}, labelled],"FAIL",EISnb(52595));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Z),Prod(Z,Z,Z,Z))}, labelled],"FAIL",EISnb(52596));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Union(Z,Prod(Z,Z))))}, labelled],"FAIL",EISnb(52597));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Sequence(Z),Union(Z,Z)))}, labelled],"FAIL",EISnb(52598));
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Union(Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Z,Prod(Z,Z,Z)))}, labelled],"FAIL",EISnb(52599));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Prod(Z,Z)),Sequence(Union(Z,Z)))}, labelled],"FAIL",EISnb(52600));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Sequence(Z),Union(Z,Z)))}, labelled],"FAIL",EISnb(52601));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Sequence(Prod(Z,Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52602));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Sequence(Z),Sequence(Z),Sequence(Z)))}, labelled],"FAIL",EISnb(52603));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Union(Sequence(Z),Prod(Z,Z))))}, labelled],"FAIL",EISnb(52604));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Sequence(Prod(Z,Z,Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52605));
#ECSv1 566
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Z),Prod(Z,Z,Union(Z,Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Sequence(Z),Sequence(Z),Union(Z,Z)))}, labelled],"FAIL",EISnb(52606));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Sequence(Prod(Z,Z,Z))))}, labelled],"FAIL",EISnb(52607));
## former 623
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Union(Sequence(Z),Prod(Z,Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Union(Z,Z),Union(Z,Z))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Sequence(Z),Union(Z,Prod(Z,Z))))}, labelled],"FAIL",EISnb(52608));
## former 626
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Z,Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Sequence(Z),Sequence(Z),Union(Z,Z))}, labelled],"FAIL",EISnb(52609));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Prod(Z,Z,Union(Z,Z))))}, labelled],"FAIL",EISnb(52610));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Z,Prod(Z,Union(Z,Z))))}, labelled],"FAIL",EISnb(52611));
#ECSv1 574
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Prod(Z,Sequence(Union(Z,Z)))))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Union(Sequence(Z),Sequence(Prod(Z,Z))))}, labelled],"FAIL",EISnb(52612));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Sequence(Prod(Z,Sequence(Z)))))}, labelled],"FAIL",EISnb(52613));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Sequence(Prod(Z,Z,Z,Z)))}, labelled],"FAIL",EISnb(52614));
#ECSv1 578
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Union(Z,Sequence(Prod(Z,Sequence(Z)))))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Z,Union(Z,Z,Sequence(Z)))}, labelled],"FAIL",EISnb(52615));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Z),Sequence(Z),Sequence(Prod(Z,Z)))}, labelled],"FAIL",EISnb(52616));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Union(Z,Sequence(Z)),Sequence(Union(Z,Z)))}, labelled],"FAIL",EISnb(52617));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Sequence(Z),Sequence(Prod(Z,Z)))}, labelled],"FAIL",EISnb(52618));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Sequence(Z),Union(Z,Z,Z))}, labelled],"FAIL",EISnb(52619));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Union(Z,Sequence(Union(Z,Z))))}, labelled],"FAIL",EISnb(32184));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Sequence(Z),Union(Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52620));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Z),Sequence(Prod(Z,Z,Z,Z)))}, labelled],"FAIL",EISnb(52621));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Union(Z,Z),Sequence(Prod(Z,Z))))}, labelled],"FAIL",EISnb(52622));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Sequence(Prod(Z,Sequence(Z),Sequence(Z))))}, labelled],"FAIL",EISnb(52623));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Prod(Sequence(Z),Sequence(Z)),Prod(Z,Z))}, labelled],"FAIL",EISnb(52624));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Z,Sequence(Z),Sequence(Z)))}, labelled],"FAIL",EISnb(52625));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Z),Sequence(Z),Sequence(Union(Z,Z)))}, labelled],"FAIL",EISnb(52626));
#ECSv1 592
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Union(Sequence(Z),Prod(Z,Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Z,Z,Z,Sequence(Z)))}, labelled],"FAIL",EISnb(52627));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Z),Sequence(Z),Prod(Z,Z,Z))}, labelled],"FAIL",EISnb(52628));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Z,Z,Prod(Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52629));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Z,Z,Z,Prod(Z,Z)))}, labelled],"FAIL",EISnb(52630));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Sequence(Union(Z,Z,Prod(Z,Z))))}, labelled],"FAIL",EISnb(52631));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Prod(Z,Z,Z,Z,Z)))}, labelled],"FAIL",EISnb(52632));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Sequence(Z),Union(Z,Sequence(Z)))}, labelled],"FAIL",EISnb(52633));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Prod(Z,Union(Z,Z))),Sequence(Z))}, labelled],"FAIL",EISnb(52634));


inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Sequence(Union(Z,Z,Z))))}, labelled],"FAIL",EISnb(52635));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Prod(Z,Union(Z,Z))),Sequence(Z))}, labelled],"FAIL",EISnb(52636));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Union(Z,Sequence(Z)),Union(Z,Z,Z))}, labelled],"FAIL",EISnb(52637));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Union(Z,Sequence(Union(Z,Z))))}, labelled],"FAIL",EISnb(52638));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Z,Sequence(Union(Z,Z))))}, labelled],"FAIL",EISnb(52639));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Sequence(Prod(Z,Union(Z,Sequence(Z)))))}, labelled],"FAIL",EISnb(52640));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Union(Z,Sequence(Z),Sequence(Z))))}, labelled],"FAIL",EISnb(52641));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Union(Z,Sequence(Z),Sequence(Z)))}, labelled],"FAIL",EISnb(52642));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Union(Z,Sequence(Z)),Union(Z,Sequence(Z)))}, labelled],"FAIL",EISnb(52643));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Union(Z,Z,Z,Sequence(Z)))}, labelled],"FAIL",EISnb(52644));
#ECSv1 611
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Union(Z,Sequence(Z),Prod(Z,Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Union(Z,Z),Union(Z,Sequence(Z)))}, labelled],"FAIL",EISnb(52645));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Sequence(Union(Z,Prod(Z,Z))))}, labelled],"FAIL",EISnb(52646));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Prod(Z,Z)),Sequence(Union(Z,Z)))}, labelled],"FAIL",EISnb(52647));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Sequence(Union(Z,Z,Z,Z))))}, labelled],"FAIL",EISnb(34325));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Union(Z,Z,Z,Z,Z))}, labelled],"FAIL",EISnb(52648));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Union(Z,Sequence(Z),Sequence(Z)))}, labelled],"FAIL",EISnb(52649));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Sequence(Z),Sequence(Union(Z,Z)))}, labelled],"FAIL",EISnb(52650));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Z,Union(Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52651));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Z,Z,Sequence(Union(Z,Z)))}, labelled],"FAIL",EISnb(52652));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Sequence(Prod(Z,Union(Z,Z)))))}, labelled],"FAIL",EISnb(52653));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Sequence(Union(Z,Z,Z,Z)))}, labelled],"FAIL",EISnb(52654));
## former 679
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Union(Z,Z),Union(Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Union(Z,Prod(Sequence(Z),Sequence(Z))))}, labelled],"FAIL",EISnb(52655));
## fformer 681
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Z),Union(Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Union(Z,Z,Sequence(Union(Z,Z))))}, labelled],"FAIL",EISnb(52656));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Sequence(Z),Sequence(Prod(Z,Z)))}, labelled],"FAIL",EISnb(52657));
## former 684
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Sequence(Z),Union(Z,Prod(Z,Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Sequence(Z),Sequence(Prod(Z,Z))))}, labelled],"FAIL",EISnb(52658));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Sequence(Z),Sequence(Union(Z,Z))))}, labelled],"FAIL",EISnb(52659));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Z),Sequence(Union(Z,Prod(Z,Z))))}, labelled],"FAIL",EISnb(52660));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Z),Sequence(Prod(Z,Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52661));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Prod(Z,Sequence(Prod(Z,Z)))))}, labelled],"FAIL",EISnb(52662));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Z,Z,Union(Z,Sequence(Z)))}, labelled],"FAIL",EISnb(52663));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Union(Z,Z,Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52664));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Sequence(Z),Sequence(Union(Z,Z)))}, labelled],"FAIL",EISnb(52665));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Prod(Z,Union(Z,Z,Z))))}, labelled],"FAIL",EISnb(52666));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Z,Z,Z,Z,Z,Z))}, labelled],"FAIL",EISnb(51188));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Z,Prod(Z,Z,Z,Z)))}, labelled],"FAIL",EISnb(52667));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Z,Z,Prod(Z,Z,Z)))}, labelled],"FAIL",EISnb(52668));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Union(Z,Sequence(Union(Z,Z)))))}, labelled],"FAIL",EISnb(52669));
###
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Sequence(Union(Z,Z,Z,Z)))}, labelled],"FAIL",EISnb(52670));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Z,Sequence(Prod(Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52671));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Union(Z,Prod(Z,Sequence(Z)))))}, labelled],"FAIL",EISnb(52672));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Sequence(Z),Union(Z,Z,Z))}, labelled],"FAIL",EISnb(52673));
#ECSv1 643
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Z,Z,Sequence(Z),Sequence(Z))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Union(Z,Z),Union(Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52674));
#ECSv1 645
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Union(Z,Z,Z)),Prod(Z,Z))}, labelled],"FAIL","FAIL");
## former 705
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Z,Prod(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Sequence(Z),Union(Z,Z,Z,Z)))}, labelled],"FAIL",EISnb(52675));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Union(Z,Z)),Union(Z,Z,Z))}, labelled],"FAIL",EISnb(52676));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Prod(Sequence(Z),Union(Z,Z))))}, labelled],"FAIL",EISnb(52677));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Z,Sequence(Union(Z,Z,Z)))}, labelled],"FAIL",EISnb(52678));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Z,Sequence(Prod(Z,Z))))}, labelled],"FAIL",EISnb(52679));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Prod(Z,Sequence(Union(Z,Z)))))}, labelled],"FAIL",EISnb(52680));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Union(Z,Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52681));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Sequence(Z),Union(Z,Z,Z)))}, labelled],"FAIL",EISnb(52682));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Z,Sequence(Z),Union(Z,Z))}, labelled],"FAIL",EISnb(52683));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Union(Z,Z,Prod(Z,Z))))}, labelled],"FAIL",EISnb(52684));
#ECSv1 656
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Union(Z,Prod(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Union(Z,Sequence(Prod(Z,Z)))))}, labelled],"FAIL",EISnb(52685));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Z,Union(Z,Z,Z,Sequence(Z)))}, labelled],"FAIL",EISnb(52686));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Union(Z,Sequence(Prod(Z,Z))))}, labelled],"FAIL",EISnb(52687));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Sequence(Z),Sequence(Prod(Z,Z,Z)))}, labelled],"FAIL",EISnb(52688));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Union(Z,Sequence(Z)),Sequence(Prod(Z,Z)))}, labelled],"FAIL",EISnb(52689));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Union(Z,Sequence(Union(Z,Z,Z))))}, labelled],"FAIL",EISnb(52690));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Prod(Z,Z,Z,Sequence(Z))))}, labelled],"FAIL",EISnb(52691));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Sequence(Prod(Z,Z,Z,Z))))}, labelled],"FAIL",EISnb(52692));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Sequence(Z),Union(Sequence(Z),Prod(Z,Z)))}, labelled],"FAIL",EISnb(1048));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Prod(Z,Union(Z,Sequence(Z)))))}, labelled],"FAIL",EISnb(52693));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Union(Z,Z)),Prod(Z,Z,Z))}, labelled],"FAIL",EISnb(52694));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Z),Sequence(Union(Z,Z,Z,Z)))}, labelled],"FAIL",EISnb(52695));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Union(Z,Prod(Z,Sequence(Z),Sequence(Z))))}, labelled],"FAIL",EISnb(52696));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Z,Z,Union(Z,Prod(Z,Z))))}, labelled],"FAIL",EISnb(52697));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Sequence(Prod(Union(Z,Z),Sequence(Union(Z,Z))))}, labelled],"FAIL",EISnb(51711));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Sequence(Z),Sequence(Union(Z,Z,Z)))}, labelled],"FAIL",EISnb(52698));
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Union(Sequence(Z),Prod(Z,Z,Z,Z,Z))}, labelled],"FAIL",EISnb(52699));
## fformer 734
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Union(Z,Z,Prod(Z,Z)))}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple regular expression in a labelled universe",
   [A, {A = Prod(Z,Sequence(Prod(Sequence(Z),Union(Z,Z))))}, labelled],"FAIL",EISnb(52700));

### Rational functions viewed as egf
## former 736
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Union(Sequence(Z),Sequence(Z))}, labelled],"FAIL","FAIL");
#ECSv1 675
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Z,Z))}, labelled],"FAIL","FAIL");
## former 738
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Sequence(Z)))}, labelled],"FAIL","FAIL");

#ECSv1 676 -> 678
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Z,Z,Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Prod(Z,Z)))}, labelled],"FAIL","FAIL");


#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Union(Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
###ffformer 702 ????
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Sequence(Z),Sequence(Z))}, labelled],"FAIL","FAIL");

#ECSv1 679 -> 681
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Sequence(Union(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Z,Prod(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Prod(Z,Z,Z)))}, labelled],"FAIL","FAIL");

## fformer 747
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Sequence(Union(Z,Z)))}, labelled],"FAIL","FAIL");
## former 748
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Z),Sequence(Z))}, labelled],"FAIL","FAIL");

#ECSv1 682 -> 684
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Sequence(Prod(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Union(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");

## fformer 752
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Z),Sequence(Z),Sequence(Z))}, labelled],"FAIL","FAIL");

#ECSv1 685 -> 687
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Z),Sequence(Union(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Z,Z,Z,Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Union(Z,Z),Sequence(Z)))}, labelled],"FAIL","FAIL");

#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Prod(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
### fff 715
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Z),Prod(Z,Z,Z))}, labelled],"FAIL","FAIL");

#ECSv1 688 -> 691
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Z),Sequence(Prod(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
# [A, {A = Sequence(Prod(Z,Sequence(Z),Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Prod(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Sequence(Union(Z,Z)))}, labelled],"FAIL","FAIL");

## former 762
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");

#ECSv1 692 -> 699  (sans compter les deja enleves)
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Sequence(Prod(Z,Z))))}, labelled],"FAIL","FAIL");
## former 764
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Z,Sequence(Z),Union(Z,Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Prod(Z,Z,Z)),Sequence(Z))}, labelled],"FAIL","FAIL");
### fff 722
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Z,Union(Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Union(Z,Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Z,Sequence(Z),Sequence(Z))}, labelled],"FAIL","FAIL");
## former 769
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Sequence(Z),Union(Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
## former 770
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Sequence(Z),Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Union(Z,Z,Sequence(Z)),Sequence(Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Z),Sequence(Union(Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Sequence(Prod(Z,Sequence(Z)))))}, labelled],"FAIL","FAIL");
## former 774
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Union(Prod(Z,Z,Sequence(Z)),Sequence(Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Union(Z,Z,Sequence(Z))))}, labelled],"FAIL","FAIL");


## former 776
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Prod(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
## fformer 777
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Z),Union(Z,Z,Z))}, labelled],"FAIL","FAIL");

#ECSv1 700 -> 706  (sans compter les deja enleves)
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Prod(Z,Z),Sequence(Union(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Sequence(Union(Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Z,Sequence(Union(Z,Z)))}, labelled],"FAIL","FAIL");
### fff 733
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Union(Z,Z,Z,Z),Sequence(Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Prod(Z,Z,Z)),Sequence(Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Sequence(Union(Z,Z))))}, labelled],"FAIL","FAIL");
## former 784
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Union(Sequence(Z),Sequence(Z)),Sequence(Z))}, labelled],"FAIL","FAIL");
###
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
## former 786
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Sequence(Z),Union(Z,Z))}, labelled],"FAIL","FAIL");


#ECSv1 707 -> 715
#inserttable(maintable,"A simple regular expression in a labelled universe",
#  [A, {A = Prod(Z,Z,Sequence(Union(Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Prod(Z,Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Z,Z,Z,Z,Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Prod(Z,Sequence(Z))),Sequence(Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Prod(Z,Union(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Sequence(Prod(Z,Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Sequence(Prod(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Union(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Z),Sequence(Union(Z,Z)))}, labelled],"FAIL","FAIL");


## former 797
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Union(Z,Z),Union(Z,Sequence(Z)))}, labelled],"FAIL","FAIL");

#ECSv1 716 -> 723
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Z,Prod(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Z),Sequence(Prod(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Sequence(Z),Union(Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Prod(Z,Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Union(Z,Sequence(Union(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Z,Z,Prod(Z,Z)))}, labelled],"FAIL","FAIL");
### fff 753
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Union(Sequence(Z),Prod(Z,Z,Z,Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Union(Z,Prod(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Sequence(Z),Union(Z,Z)))}, labelled],"FAIL","FAIL");

##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Z,Union(Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");

#ECSv1 724 -> 732
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Z,Prod(Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Prod(Z,Z)),Sequence(Union(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Sequence(Z),Union(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Sequence(Prod(Z,Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Sequence(Z),Sequence(Z),Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Union(Sequence(Z),Prod(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Prod(Z,Z,Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
### fff 764
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Z),Prod(Z,Z,Union(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Sequence(Z),Sequence(Z),Union(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Sequence(Prod(Z,Z,Z))))}, labelled],"FAIL","FAIL");

## former 818
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Union(Sequence(Z),Prod(Z,Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Union(Z,Z),Union(Z,Z))}, labelled],"FAIL","FAIL");

#ECSv1 733 -> 736
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Sequence(Z),Union(Z,Prod(Z,Z))))}, labelled],"FAIL","FAIL");
## former 821
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Z,Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Z),Sequence(Z),Union(Z,Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Prod(Z,Z,Union(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Z,Prod(Z,Union(Z,Z))))}, labelled],"FAIL","FAIL");

##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Sequence(Prod(Z,Sequence(Union(Z,Z)))))}, labelled],"FAIL","FAIL");

#ECSv1 737 -> 739
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Union(Sequence(Z),Sequence(Prod(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Sequence(Prod(Z,Sequence(Z)))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Sequence(Prod(Z,Z,Z,Z)))}, labelled],"FAIL","FAIL");

##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Sequence(Prod(Z,Sequence(Z)))))}, labelled],"FAIL","FAIL");
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Z,Z,Union(Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");

#ECSv1 740 -> 750
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Z),Sequence(Z),Sequence(Prod(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Union(Z,Sequence(Z)),Sequence(Union(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Sequence(Z),Sequence(Prod(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Sequence(Z),Union(Z,Z,Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Union(Z,Sequence(Union(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Sequence(Z),Union(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Z),Sequence(Prod(Z,Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Union(Z,Z),Sequence(Prod(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Prod(Z,Sequence(Z),Sequence(Z))))}, labelled],"FAIL","FAIL");
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Union(Prod(Sequence(Z),Sequence(Z)),Prod(Z,Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Z,Sequence(Z),Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Z),Sequence(Z),Sequence(Union(Z,Z)))}, labelled],"FAIL","FAIL");

###fff 790
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Union(Sequence(Z),Prod(Z,Z)))}, labelled],"FAIL","FAIL");

#ECSv1 751 -> 763
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Z,Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Union(Sequence(Z),Sequence(Z),Prod(Z,Z,Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Z,Z,Prod(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Z,Z,Z,Prod(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Union(Z,Z,Prod(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Prod(Z,Z,Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Sequence(Z),Union(Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Prod(Z,Union(Z,Z))),Sequence(Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Sequence(Union(Z,Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Prod(Z,Union(Z,Z))),Sequence(Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Union(Z,Sequence(Z)),Union(Z,Z,Z))}, labelled],"FAIL","FAIL");
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Z,Union(Z,Sequence(Union(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Z,Sequence(Union(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Prod(Z,Union(Z,Sequence(Z)))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Union(Z,Sequence(Z),Sequence(Z))))}, labelled],"FAIL","FAIL");

###ffformer 806
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Union(Z,Sequence(Z),Sequence(Z)))}, labelled],"FAIL","FAIL");

#ECSv1 764 -> 775
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Union(Z,Sequence(Z)),Union(Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Union(Z,Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Sequence(Z),Prod(Z,Z)))}, labelled],"FAIL","FAIL");
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Z),Union(Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Sequence(Union(Z,Prod(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Prod(Z,Z)),Sequence(Union(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Sequence(Union(Z,Z,Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Union(Z,Z,Z,Z,Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Union(Z,Sequence(Z),Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Sequence(Z),Sequence(Union(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Z,Union(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Z,Z,Sequence(Union(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Sequence(Prod(Z,Union(Z,Z)))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Sequence(Union(Z,Z,Z,Z)))}, labelled],"FAIL","FAIL");

## former 874
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Union(Z,Z),Union(Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");

#ECSv1 776 -> 778
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Union(Z,Prod(Sequence(Z),Sequence(Z))))}, labelled],"FAIL","FAIL");
## fformer 876
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Sequence(Z),Union(Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Union(Z,Z,Sequence(Union(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Sequence(Z),Sequence(Prod(Z,Z)))}, labelled],"FAIL","FAIL");

## former 879
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Sequence(Z),Union(Z,Prod(Z,Z)))}, labelled],"FAIL","FAIL");

#ECSv1 779 -> 794
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Sequence(Z),Sequence(Prod(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Sequence(Z),Sequence(Union(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Z),Sequence(Union(Z,Prod(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Z),Sequence(Prod(Z,Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Prod(Z,Sequence(Prod(Z,Z)))))}, labelled],"FAIL","FAIL");
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Z,Z,Z,Union(Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Union(Z,Z,Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Sequence(Z),Sequence(Union(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Prod(Z,Union(Z,Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Z,Z,Z,Z,Z,Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Z,Prod(Z,Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Z,Z,Prod(Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Union(Z,Sequence(Union(Z,Z)))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Sequence(Union(Z,Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Z,Sequence(Prod(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Union(Z,Prod(Z,Sequence(Z)))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Sequence(Z),Union(Z,Z,Z))}, labelled],"FAIL","FAIL");

### ffformer 842
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Z,Z,Sequence(Z),Sequence(Z))}, labelled],"FAIL","FAIL");

#ECSv1 795 -> 804
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Union(Z,Z),Union(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Union(Z,Z,Z)),Prod(Z,Z))}, labelled],"FAIL","FAIL");
## former 900
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Union(Z,Z,Prod(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Sequence(Z),Union(Z,Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Union(Z,Z)),Union(Z,Z,Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Prod(Sequence(Z),Union(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Z,Sequence(Union(Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Z,Sequence(Prod(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Prod(Z,Sequence(Union(Z,Z)))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Union(Z,Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Sequence(Z),Union(Z,Z,Z)))}, labelled],"FAIL","FAIL");


### ffformer 853
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Z,Z,Sequence(Z),Union(Z,Z))}, labelled],"FAIL","FAIL");

#ECSv1 805 -> 812
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Union(Z,Z,Prod(Z,Z))))}, labelled],"FAIL","FAIL");
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Z,Union(Z,Prod(Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Union(Z,Sequence(Prod(Z,Z)))))}, labelled],"FAIL","FAIL");
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Z,Z,Union(Z,Z,Z,Sequence(Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Union(Z,Sequence(Prod(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Z),Sequence(Prod(Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Union(Z,Sequence(Z)),Sequence(Prod(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Union(Z,Sequence(Union(Z,Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Prod(Z,Z,Z,Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Sequence(Prod(Z,Z,Z,Z))))}, labelled],"FAIL","FAIL");


## fformer 920
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Sequence(Z),Union(Sequence(Z),Prod(Z,Z)))}, labelled],"FAIL","FAIL");

#ECSv1 813 -> 820
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Prod(Z,Union(Z,Sequence(Z)))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Union(Z,Z)),Prod(Z,Z,Z))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Union(Sequence(Z),Sequence(Union(Z,Z,Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Union(Z,Prod(Z,Sequence(Z),Sequence(Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Z,Z,Union(Z,Prod(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Sequence(Prod(Union(Z,Z),Sequence(Union(Z,Z))))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Z),Sequence(Union(Z,Z,Z)))}, labelled],"FAIL","FAIL");
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Union(Sequence(Z),Prod(Z,Z,Z,Z,Z))}, labelled],"FAIL","FAIL");
### fformer 929
##inserttable(maintable,"A simple regular expression in a labelled universe",
##   [A, {A = Prod(Sequence(Z),Union(Z,Z,Prod(Z,Z)))}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple regular expression in a labelled universe",
#   [A, {A = Prod(Z,Sequence(Prod(Sequence(Z),Union(Z,Z))))}, labelled],"FAIL","FAIL");

#### Algebraic generating functions

inserttable(maintable,"A simple context-free grammar",
   [A, {C = Prod(A,A), B = Union(C,Z), A = Union(B,C)}, unlabelled],"FAIL",EISnb(52701));
## fformer 932
#inserttable(maintable,"A simple context-free grammar",
#   [A, {B = Union(A,Z), C = Union(B,Z), A = Prod(C,C)}, unlabelled],"FAIL","FAIL");
### fff 879
inserttable(maintable,"A simple context-free grammar",
   [A, {C = Union(A,Z), B = Union(A,C), A = Prod(B,B)}, unlabelled],"FAIL",EISnb(3645));
## fformer 934
#inserttable(maintable,"A simple context-free grammar",
#   [A, {C = Union(A,Z), A = Union(B,Z), B = Prod(C,C)}, unlabelled],"FAIL","FAIL");
### fff 879
#inserttable(maintable,"A simple context-free grammar",
#   [A, {A = Union(B,C), C = Union(B,Z), B = Prod(C,C)}, unlabelled],"FAIL","FAIL");
#inserttable(maintable,"A simple context-free grammar",
#   [A, {C = Union(B,Z), A = Union(C,Z), B = Prod(A,C)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple context-free grammar",
   [A, {C = Prod(A,A), B = Union(C,Z), A = Union(B,Z)}, unlabelled],"square root singularity",EISnb(25225));
inserttable(maintable,"A simple context-free grammar",
   [A, {B = Prod(C,Z), A = Prod(B,B), C = Union(A,B,Z)}, unlabelled],"FAIL",EISnb(52702));
inserttable(maintable,"A simple context-free grammar",
   [A, {C = Prod(A,A), B = Prod(Z,Z), A = Union(B,C,Z)}, unlabelled],"FAIL",EISnb(25227));
#inserttable(maintable,"A simple context-free grammar",
#   [A, {B = Union(A,Z), A = Prod(C,C), C = Union(A,B,Z)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple context-free grammar",
   [A, {A = Prod(B,C), C = Prod(B,B), B = Union(A,C,Z)}, unlabelled],"FAIL",EISnb(52703));
#inserttable(maintable,"A simple context-free grammar",
#   [A, {C = Prod(B,B), A = Prod(C,Z), B = Union(A,C,Z)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple context-free grammar",
   [A, {C = Prod(Z,Z), A = Prod(B,B), B = Union(A,C,Z)}, unlabelled],"FAIL",EISnb(25227));
inserttable(maintable,"A simple context-free grammar",
   [A, {A = Union(B,Z), B = Prod(C,C), C = Union(A,B,Z)}, unlabelled],"FAIL",EISnb(52704));
###fff 889
#inserttable(maintable,"A simple context-free grammar",
#   [A, {C = Union(A,Z), A = Prod(B,C), B = Union(A,C,Z)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple context-free grammar",
   [A, {C = Prod(A,A), B = Prod(C,C), A = Union(B,C,Z)}, unlabelled],"FAIL",EISnb(49140));
inserttable(maintable,"A simple context-free grammar",
   [A, {A = Prod(B,B), C = Prod(A,Z), B = Union(A,C,Z)}, unlabelled],"FAIL",EISnb(52705));
#inserttable(maintable,"A simple context-free grammar",
#   [A, {C = Prod(A,A), A = Prod(B,Z), B = Union(A,C,Z)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple context-free grammar",
   [A, {A = Prod(C,C), B = Prod(A,C), C = Union(A,B,Z)}, unlabelled],"FAIL",EISnb(52706));
inserttable(maintable,"A simple context-free grammar",
   [A, {C = Union(B,Z), A = Union(B,C,Z), B = Prod(A,A)}, unlabelled],"FAIL",EISnb(52707));
inserttable(maintable,"A simple context-free grammar",
   [A, {C = Prod(A,A), A = Prod(B,B), B = Union(A,C,Z)}, unlabelled],"FAIL",EISnb(52708));
inserttable(maintable,"A simple context-free grammar",
   [A, {A = Union(B,C,Z), C = Prod(B,Z), B = Prod(A,A)}, unlabelled],"FAIL",EISnb(52709));
inserttable(maintable,"A simple context-free grammar",
   [A, {C = Prod(B,B), A = Prod(C,C), B = Union(A,C,Z)}, unlabelled],"FAIL",EISnb(52710));
inserttable(maintable,"A simple context-free grammar",
   [A, {B = Prod(C,C), A = Union(B,C,Z), C = Prod(A,Z)}, unlabelled],"FAIL",EISnb(23431));

### Algebraic functions viewed as egf

inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {A = Prod(B,Z), C = Union(B,Z), B = Prod(C,C)}, labelled],"FAIL",EISnb(52711));
## fformer 956
#inserttable(maintable,"A simple context-free grammar in a labelled universe",
#   [A, {B = Union(A,Z), A = Union(Z,C), C = Prod(B,B)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {B = Prod(C,C), A = Union(B,Z), C = Union(B,A)}, labelled],"FAIL",EISnb(52712));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Union(B,Z), A = Union(Z,C), B = Prod(A,A)}, labelled],"square root singularity",EISnb(52713));
#inserttable(maintable,"A simple context-free grammar in a labelled universe",
#   [A, {B = Union(A,Z), C = Union(B,A), A = Prod(C,C)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {B = Union(Z,C), C = Prod(A,A), A = Union(B,C)}, labelled],"FAIL",EISnb(52714));
#inserttable(maintable,"A simple context-free grammar in a labelled universe",
#   [A, {C = Union(B,Z), B = Union(A,Z), A = Prod(B,C)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Prod(B,B), B = Union(Z,C), A = Prod(C,C)}, labelled],"FAIL",EISnb(52715));
## fformer 963
#inserttable(maintable,"A simple context-free grammar in a labelled universe",
#   [A, {C = Union(B,Z), B = Union(A,Z), A = Prod(C,C)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Union(B,Z), A = Union(Z,C), B = Prod(A,C)}, labelled],"FAIL",EISnb(52716));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Union(B,Z), B = Prod(C,C), A = Prod(Z,C)}, labelled],"FAIL",EISnb(52717));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Prod(B,B), B = Union(Z,C), A = Union(B,C)}, labelled],"FAIL",EISnb(52718));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Prod(B,B), B = Union(Z,C), A = Prod(B,C)}, labelled],"FAIL",EISnb(52719));
## fformer 968
#inserttable(maintable,"A simple context-free grammar in a labelled universe",
#   [A, {B = Union(A,C), C = Union(A,Z), A = Prod(B,C)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Prod(B,B), B = Union(Z,C), A = Prod(C,C,C)}, labelled],"FAIL",EISnb(52720));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Union(B,Z), B = Prod(C,C), A = Prod(B,B,Z)}, labelled],"FAIL",EISnb(52721));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Union(B,Z), B = Prod(C,C), A = Prod(B,B,C)}, labelled],"FAIL",EISnb(52722));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {B = Prod(A,A), A = Prod(Z,C), C = Union(B,A,Z)}, labelled],"FAIL",EISnb(52723));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {A = Prod(B,B), B = Prod(Z,C), C = Union(B,A,Z)}, labelled],"FAIL",EISnb(52724));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {B = Prod(C,C), A = Prod(B,B), C = Union(B,A,Z)}, labelled],"FAIL",EISnb(52725));
### fff 919
#inserttable(maintable,"A simple context-free grammar in a labelled universe",
#   [A, {C = Union(A,Z), A = Prod(B,C), B = Union(A,Z,C)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Prod(A,A), B = Prod(Z,C), A = Union(B,Z,C)}, labelled],"FAIL",EISnb(52726));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {B = Prod(A,A), C = Prod(Z,Z), A = Union(B,Z,C)}, labelled],"FAIL",EISnb(52727));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Prod(A,Z), A = Prod(B,B), B = Union(A,Z,C)}, labelled],"FAIL",EISnb(52728));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {A = Prod(C,C), B = Prod(A,C), C = Union(B,A,Z)}, labelled],"FAIL",EISnb(52729));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Prod(B,B), A = Prod(B,C), B = Union(A,Z,C)}, labelled],"FAIL",EISnb(52730));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Prod(B,Z), A = Prod(B,C), B = Union(A,Z,C)}, labelled],"FAIL",EISnb(52731));
#inserttable(maintable,"A simple context-free grammar in a labelled universe",
#   [A, {B = Union(A,Z), A = Prod(C,C), C = Union(B,A,Z)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Prod(B,B), B = Union(Z,C), A = Prod(Z,Z,C)}, labelled],"FAIL",EISnb(52732));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Prod(B,B), B = Union(Z,C), A = Prod(B,Z,Z)}, labelled],"FAIL",EISnb(52733));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {B = Prod(C,C), A = Union(B,Z), C = Union(B,A,Z)}, labelled],"FAIL",EISnb(52734));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {A = Prod(B,Z), C = Prod(B,A), B = Union(A,Z,C)}, labelled],"FAIL",EISnb(52735));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {A = Prod(C,C), B = Prod(Z,C), C = Union(B,A,Z)}, labelled],"FAIL",EISnb(52736));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {B = Union(Z,C), C = Prod(A,A), A = Union(B,Z,C)}, labelled],"FAIL",EISnb(52737));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Prod(A,A), A = Prod(B,B), B = Union(A,Z,C)}, labelled],"FAIL",EISnb(52738));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {A = Prod(C,C), B = Prod(Z,Z), C = Union(B,A,Z)}, labelled],"FAIL",EISnb(52739));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Prod(B,B), B = Prod(A,A), A = Union(B,Z,C)}, labelled],"FAIL",EISnb(52740));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {C = Prod(B,B), B = Union(Z,C), A = Prod(B,Z,C)}, labelled],"FAIL",EISnb(52741));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {B = Prod(C,C), A = Prod(Z,C), C = Union(B,A,Z)}, labelled],"FAIL",EISnb(52742));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {B = Prod(C,C), C = Prod(A,Z), A = Union(B,Z,C)}, labelled],"FAIL",EISnb(52743));
inserttable(maintable,"A simple context-free grammar in a labelled universe",
   [A, {A = Prod(B,Z), B = Prod(C,C), C = Union(B,A,Z)}, labelled],"FAIL",EISnb(52744));

#### Non algebraic generating functions
## fformer 996
#inserttable(maintable,"Pairs of cycles",
#    [A, {A = Prod(B,B), B = Cycle(Z)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(Z,B,B), B = Cycle(Z)}, labelled],"FAIL",EISnb(52745));
### fff 942
#inserttable(maintable,"A simple grammar",
#    [A, {A = PowerSet(B), B = Prod(Z,A,A)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(Z,B,B), B = Set(A)}, labelled],"FAIL",EISnb(52746));
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(Z,Z,B), B = Cycle(Z)}, labelled],"FAIL",EISnb(52747));
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(B,B,B), B = Cycle(Z)}, labelled],"FAIL",EISnb(52748));
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(Z,B,B), B = Set(Z,1 <= card)}, labelled],"FAIL",EISnb(52749));
inserttable(maintable,"A simple grammar",
    [A, {A = Set(B), B = Prod(Z,A,A)}, labelled],"FAIL",EISnb(52750));
inserttable(maintable,"A simple grammar",
    [A, {A = Set(B), B = Prod(A,A,A,Z)}, unlabelled],"FAIL",EISnb(52751));
inserttable(maintable,"A simple grammar",
    [A, {A = Set(B), B = Prod(A,A,A,Z)}, labelled],"FAIL",EISnb(52752));
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(B,B,B,B)}, labelled],"FAIL",EISnb(52753));
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(B,B,Z,Z)}, labelled],"FAIL",EISnb(52754));
inserttable(maintable,"A simple grammar",
    [A, {A = PowerSet(B), B = Prod(A,A,A,Z)}, unlabelled],"FAIL",EISnb(52755));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(A), A = Prod(B,B,B,Z)}, labelled],"FAIL",EISnb(27471));
inserttable(maintable,"A simple grammar",
    [A, {B = PowerSet(A), A = Prod(B,B,B,Z)}, unlabelled],"FAIL",EISnb(52756));
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(B,B,B,Z)}, labelled],"FAIL",EISnb(52757));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z), A = Prod(B,B,B,Z)}, labelled],"FAIL",EISnb(52758));
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(B,Z,Z,Z)}, labelled],"FAIL",EISnb(52759));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(B,B,Z,Z)}, labelled],"FAIL",EISnb(52760));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(B,B,B,Z)}, labelled],"FAIL",EISnb(52761));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z), A = Prod(Z,Z,Z,Z,B)}, labelled],"FAIL",EISnb(52762));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(A), A = Prod(Z,B,B,B,B)}, unlabelled],"FAIL",EISnb(52763));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(A), A = Prod(Z,B,B,B,B)}, labelled],"FAIL",EISnb(52764));
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(Z,Z,B,B,B)}, labelled],"FAIL",EISnb(52765));
##  1020
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(Z,Z,Z,B,B)}, labelled],"FAIL",EISnb(52766));
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(B,B,B,B,B)}, labelled],"FAIL",EISnb(52767));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(Z,Z,Z,Z,B)}, labelled],"FAIL",EISnb(52768));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(Z,Z,Z,B,B)}, labelled],"FAIL",EISnb(52769));
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(Z,B,B,B,B)}, labelled],"FAIL",EISnb(52770));
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(Z,Z,Z,B,B), B = Set(Z)}, labelled],"FAIL",EISnb(52771));
inserttable(maintable,"A simple grammar",
    [A, {B = PowerSet(A), A = Prod(Z,B,B,B,B)}, unlabelled],"FAIL",EISnb(52772));
inserttable(maintable,"A simple grammar",
    [A, {A = Set(B), B = Prod(Z,A,A,A,A)}, unlabelled],"FAIL",EISnb(52773));
inserttable(maintable,"A simple grammar",
    [A, {A = Set(B), B = Prod(Z,A,A,A,A)}, labelled],"FAIL",EISnb(52774));
inserttable(maintable,"A simple grammar",
    [A, {A = PowerSet(B), B = Prod(Z,A,A,A,A)}, unlabelled],"FAIL",EISnb(52775));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(Z,B,B,B,B)}, labelled],"FAIL",EISnb(52776));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(Z,Z,B,B,B)}, labelled],"FAIL",EISnb(52777));
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(Z,Z,Z,Z,B)}, labelled],"FAIL",EISnb(52778));
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(B,B,B,B,B,B)}, labelled],"FAIL",EISnb(52779));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z), A = Prod(Z,Z,B,B,B,B)}, labelled],"FAIL",EISnb(52780));
inserttable(maintable,"A simple grammar",
    [A, {A = Set(B), B = Prod(Z,A,A,A,A,A)}, unlabelled],"FAIL",EISnb(52781));
inserttable(maintable,"A simple grammar",
    [A, {A = Set(B), B = Prod(Z,A,A,A,A,A)}, labelled],"FAIL",EISnb(52782));
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(Z,B,B,B,B,B)}, labelled],"FAIL",EISnb(52783));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(Z,Z,Z,B,B,B)}, labelled],"FAIL",EISnb(52784));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(Z,B,B,B,B,B)}, labelled],"FAIL",EISnb(52785));
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(Z,Z,Z,B,B,B)}, labelled],"FAIL",EISnb(52786));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z), A = Prod(Z,Z,Z,Z,Z,B)}, labelled],"FAIL",EISnb(52787));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(A), A = Prod(Z,B,B,B,B,B)}, unlabelled],"FAIL",EISnb(52788));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(A), A = Prod(Z,B,B,B,B,B)}, labelled],"FAIL",EISnb(53464));
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(Z,Z,B,B,B,B)}, labelled],"FAIL",EISnb(52790));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z), A = Prod(Z,Z,Z,B,B,B)}, labelled],"FAIL",EISnb(52791));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(Z,Z,B,B,B,B)}, labelled],"FAIL",EISnb(52792));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(Z,Z,Z,Z,B,B)}, labelled],"FAIL",EISnb(52793));
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(Z,Z,Z,Z,Z,B)}, labelled],"FAIL",EISnb(52794));
inserttable(maintable,"A simple grammar",
    [A, {A = Sequence(B), B = Prod(Z,A,A,A,A,A)}, labelled],"FAIL",EISnb(52795));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z), A = Prod(Z,Z,Z,Z,B,B)}, labelled],"FAIL",EISnb(52796));
inserttable(maintable,"A simple grammar",
    [A, {B = PowerSet(A), A = Prod(Z,B,B,B,B,B)}, unlabelled],"FAIL",EISnb(52797));
inserttable(maintable,"A simple grammar",
    [A, {A = PowerSet(B), B = Prod(Z,A,A,A,A,A)}, unlabelled],"FAIL",EISnb(52798));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z), A = Prod(Z,B,B,B,B,B)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(Z,Z,Z,Z,B,B)}, labelled],"FAIL",EISnb(52799));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(Z,Z,Z,Z,Z,B)}, labelled],"FAIL",EISnb(52800));
### 1000
#inserttable(maintable,"A simple grammar",
#    [A, {A = Prod(B,B), C = Sequence(Z,1 <= card), B = PowerSet(C)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(B,B), C = Cycle(Z), B = Sequence(C)}, labelled],"labelled pairs of Sequences of Cycles",EISnb(52801));
## fformer 1058
#inserttable(maintable,"A simple grammar",
#    [A, {B = Prod(A,Z), C = Cycle(B), A = Sequence(C)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {B = Prod(A,Z), C = Cycle(B), A = Sequence(C)}, labelled],"FAIL",EISnb(52802));
inserttable(maintable,"A simple grammar",
    [A, {A = Cycle(B), C = Sequence(B), B = Prod(C,Z)}, unlabelled],"FAIL",EISnb(3239));
inserttable(maintable,"A simple grammar",
    [A, {C = Cycle(Z), B = Prod(C,A), A = Sequence(B)}, labelled],"FAIL",EISnb(52803));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z), C = Sequence(Z), A = Union(C,B)}, labelled],"FAIL",EISnb(38507));
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(Z,card>=1), A = Cycle(B), B = Prod(C,Z)}, unlabelled],"FAIL",EISnb(32190));
inserttable(maintable,"A simple grammar",
    [A, {C = Cycle(Z), A = Cycle(B), B = Prod(C,Z)}, labelled],"Cycles of rooted Cycles",EISnb(52804));
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(B), B = Prod(C,Z), A = PowerSet(B)}, unlabelled],"FAIL",EISnb(52805));
inserttable(maintable,"A simple grammar",
    [A, {B = PowerSet(C), A = Sequence(C), C = Prod(B,Z)}, unlabelled],"FAIL",EISnb(52806));
#inserttable(maintable,"A simple grammar",
#    [A, {B = Prod(C,Z), C = PowerSet(B), A = Set(B)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {B = Sequence(Z,card>=1), A = Prod(C,B), C = Set(A)}, unlabelled],"FAIL",EISnb(36249));
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Prod(C,B), C = Set(A)}, labelled],"FAIL",EISnb(52807));
inserttable(maintable,"A simple grammar",
    [A, {C = Cycle(Z), A = Cycle(B), B = Union(C,Z)}, labelled],"FAIL",EISnb(52808));
#inserttable(maintable,"A simple grammar",
#    [A, {C = Sequence(Z,card>=1), B = Prod(C,Z), A = PowerSet(B)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Cycle(Z), B = Cycle(C), A = Prod(B,Z)}, labelled],"Cycles of Cycles",EISnb(52809));
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(Z,card>=1), A = Union(C,B), B = Set(C)}, unlabelled],"FAIL",EISnb(52810));
inserttable(maintable,"A simple grammar",
    [A, {C = Set(B), A = Prod(C,Z), B = Cycle(A)}, unlabelled],"FAIL",EISnb(50383));
inserttable(maintable,"A simple grammar",
    [A, {B = PowerSet(C), C = Sequence(Z,card>=1), A = Prod(C,B)}, unlabelled],"FAIL",EISnb(36469));
inserttable(maintable,"A simple grammar",
    [A, {A = Sequence(C), B = Cycle(Z), C = Prod(B,B)}, labelled],"Sequences of pairs of Cycles",EISnb(52811));
inserttable(maintable,"A simple grammar",
    [A, {B = Sequence(Z,card>=1), C = Prod(B,B), A = PowerSet(C)}, unlabelled],"PowerSet of pairs of Sequences",EISnb(52812));
## fformer 1078
#inserttable(maintable,"A simple grammar",
#    [A, {B = Prod(A,Z), C = Cycle(B), A = Set(C)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Cycle(B), B = Prod(C,Z), C = Sequence(Z)}, labelled],"FAIL",EISnb(29767));
#inserttable(maintable,"A simple grammar",
#    [A, {C = Sequence(Z,card>=1), B = Prod(C,A), A = Set(B)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Cycle(Z), B = Prod(C,A), A = Set(B)}, labelled],"FAIL",EISnb(52813));
inserttable(maintable,"A simple grammar",
    [A, {B = PowerSet(C), C = Cycle(A), A = Prod(B,Z)}, unlabelled],"FAIL",EISnb(52814));
#inserttable(maintable,"A simple grammar",
#    [A, {C = Cycle(Z), B = Sequence(A), A = Prod(C,B)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Cycle(B), B = Prod(C,Z), C = Sequence(A)}, unlabelled],"FAIL",EISnb(52815));
#inserttable(maintable,"A simple grammar",
#    [A, {A = Cycle(B), B = Prod(C,Z), C = Sequence(A)}, labelled],"FAIL","FAIL");
##
#ECSv1 951
#inserttable(maintable,"A simple grammar",
#    [A, {C = Sequence(Z,card>=1), B = Sequence(C), A = Prod(B,Z)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Union(B,Z), B = Sequence(Z,card>=1), A = PowerSet(C)}, unlabelled],"FAIL",EISnb(52816));
#inserttable(maintable,"A simple grammar",
#    [A, {C = Prod(B,A), B = Sequence(Z,card>=1), A = PowerSet(C)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {B = PowerSet(C), C = Prod(B,Z), A = Cycle(C)}, unlabelled],"FAIL",EISnb(52817));
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(B), A = Prod(C,Z), B = Cycle(A)}, unlabelled],"FAIL",EISnb(52818));
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(B), A = Prod(C,Z), B = Cycle(A)}, labelled],"FAIL",EISnb(52819));
#inserttable(maintable,"A simple grammar",
#    [A, {A = Cycle(B), B = Prod(C,Z), C = Set(A)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Cycle(Z), A = Sequence(B), B = Union(C,Z)}, labelled],"FAIL",EISnb(52820));
inserttable(maintable,"A simple grammar",
    [A, {C = Cycle(B), A = Prod(C,C), B = Sequence(Z,card>=1)}, unlabelled],"pairs of Cycles of Sequences",EISnb(52821));
inserttable(maintable,"A simple grammar",
    [A, {C = Cycle(B), A = Prod(C,C), B = Cycle(Z)}, labelled],"pairs of Cycles of Cycles",EISnb(52822));
## fformer 1096
#inserttable(maintable,"A simple grammar",
#    [A, {B = Prod(A,Z), C = Cycle(B), A = PowerSet(C)}, unlabelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {C = Sequence(Z,card>=1), B = Prod(C,C), A = Set(B)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Cycle(C), B = Sequence(Z,card>=1), C = Prod(B,B)}, unlabelled],"Cycles of pairs of Sequences",EISnb(52823));
inserttable(maintable,"A simple grammar",
    [A, {A = Cycle(C), B = Cycle(Z), C = Prod(B,B)}, labelled],"Cycles of pairs of Cycles",EISnb(52824));
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(Z,card>=1), A = Prod(C,B), B = Cycle(C)}, unlabelled],"FAIL",EISnb(52825));
inserttable(maintable,"A simple grammar",
    [A, {C = Cycle(Z), A = Prod(C,B), B = Cycle(C)}, labelled],"Product of Cycles and Cycles of Cycles",EISnb(52826));
inserttable(maintable,"A simple grammar",
    [A, {C = Prod(B,Z), B = Set(C), A = PowerSet(C)}, unlabelled],"FAIL",EISnb(52827));
inserttable(maintable,"A simple grammar",
    [A, {C = Cycle(Z), A = Union(C,B), B = Cycle(C)}, labelled],"Union of Cycles and Cycles of Cycles",EISnb(52828));
inserttable(maintable,"A simple grammar",
    [A, {C = PowerSet(A), B = Sequence(Z,card>=1), A = Prod(C,B)}, unlabelled],"FAIL",EISnb(52829));
inserttable(maintable,"A simple grammar",
    [A, {C = Cycle(Z), B = Prod(C,Z), A = Sequence(B)}, labelled],"Sequences of rooted Cycles",EISnb(52830));
inserttable(maintable,"A simple grammar",
    [A, {A = Cycle(B), B = Prod(C,Z), C = PowerSet(A)}, unlabelled],"FAIL",EISnb(52831));
#inserttable(maintable,"A simple grammar",
#    [A, {B = Sequence(C), C = Prod(B,Z), A = Set(C)}, unlabelled],"FAIL","FAIL");
### fff 1052
#inserttable(maintable,"A simple grammar",
#    [A, {C = Sequence(Z,1 <= card), A = Cycle(B), B = Prod(Z,C)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(Z,1 <= card), A = Cycle(B), B = Prod(Z,C)},
labelled],"FAIL",EISnb(52832));
inserttable(maintable,"A simple grammar",
    [A, {A = PowerSet(C), C = Prod(Z,B), B = Sequence(Z,1 <= card)}, unlabelled],"FAIL",EISnb(25147));
#inserttable(maintable,"A simple grammar",
#    [A, {C = Prod(B,B), B = Sequence(Z,1 <= card), A = Cycle(C)}, unlabelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {A = Prod(C,C), B = Sequence(Z,1 <= card), C = Cycle(B)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(C,C), B = Set(Z,1 <= card), C = Cycle(B)}, labelled],"FAIL",EISnb(52833));
#inserttable(maintable,"A simple grammar",
#    [A, {A = PowerSet(B), B = Prod(Z,C), C = Set(B)}, unlabelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {C = Cycle(B), A = Prod(Z,C), B = Cycle(Z)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Set(B,1 <= card), B = Sequence(Z,1 <= card), A = Prod(B,C)}, unlabelled],"FAIL",EISnb(26905));
inserttable(maintable,"A simple grammar",
    [A, {C = Set(B,1 <= card), B = Set(Z,1 <= card), A = Prod(B,C)}, labelled],"FAIL",EISnb(52834));
#inserttable(maintable,"A simple grammar",
#    [A, {C = PowerSet(B), A = Cycle(B), B = Prod(Z,C)}, unlabelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {B = PowerSet(C), C = Prod(Z,B), A = Sequence(C)}, unlabelled],"FAIL","FAIL");
## fformer 1120
#inserttable(maintable,"A simple grammar",
#    [A, {B = Set(Z,1 <= card), A = Union(B,C), C = Sequence(Z)}, labelled],"FAIL","FAIL");
## fformer ?????
#inserttable(maintable,"A simple grammar",
#    [A, {B = Sequence(Z,1 <= card), C = Set(B), A = Union(Z,C)}, unlabelled],"FAIL","FAIL");
## fformer 1121
#inserttable(maintable,"A simple grammar",
#    [A, {B = Set(Z,1 <= card), C = Set(B), A = Union(Z,C)}, labelled],"FAIL","FAIL");
### ffformer 1067
#inserttable(maintable,"A simple grammar",
#    [A, {B = Sequence(Z,1 <= card), C = Set(B), A = Union(B,C)}, unlabelled],"FAIL","FAIL");
## fformer 1122
#inserttable(maintable,"A simple grammar",
#    [A, {B = Set(Z,1 <= card), C = Set(B), A = Union(B,C)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {B = Cycle(Z), A = Union(B,C), C = Sequence(B)},
labelled],"FAIL",EISnb(52835));
inserttable(maintable,"A simple grammar",
    [A, {C = PowerSet(A), B = Prod(Z,C), A = Sequence(B,1 <= card)}, unlabelled],"FAIL",EISnb(52836));
#inserttable(maintable,"A simple grammar",
#    [A, {A = Prod(C,C), C = Sequence(B), B = Cycle(Z)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(B,B), C = Sequence(Z,1 <= card), B = Set(C,1 <= card)}, unlabelled],"FAIL",EISnb(52837));
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(B,B), C = Sequence(Z,1 <= card), B = Set(C,1 <= card)}, labelled],"FAIL",EISnb(52838));
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(Z,1 <= card), B = PowerSet(C), A = Union(B,C)}, unlabelled],"FAIL",EISnb(52839));
## fformer 1131
#inserttable(maintable,"A simple grammar",
#    [A, {B = Cycle(Z), A = Prod(B,C), C = Cycle(Z)}, labelled],"FAIL","FAIL");

inserttable(maintable,"A simple grammar",
    [A, {B = Sequence(Z,1 <= card), C = Cycle(B), A = Prod(Z,C)},
labelled],"FAIL",EISnb(52840));
inserttable(maintable,"A simple grammar",
    [A, {B = Prod(C,C), A = Sequence(B), C = Set(Z,1 <= card)}, labelled],"FAIL",EISnb(52841));
#inserttable(maintable,"A simple grammar",
#    [A, {A = PowerSet(B), C = Sequence(Z,0 < card), B = Union(Z,C)}, unlabelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {C = Prod(Z,B), A = Cycle(C), B = Sequence(A)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Prod(Z,B), A = Cycle(C), B = Sequence(A)}, labelled],"FAIL",EISnb(52842));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(C,1 <= card), C = Prod(Z,A), A = Set(B)}, labelled],"FAIL",EISnb(30019));
#inserttable(maintable,"A simple grammar",
#    [A, {C = Union(Z,B), A = Sequence(C,1 <= card), B = Cycle(Z)},
#labelled],"FAIL","FAIL");
## fformer 1139
#inserttable(maintable,"A simple grammar",
#    [A, {A = Sequence(B), C = Prod(Z,A), B = Cycle(C)},
#unlabelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {A = Sequence(B), C = Prod(Z,A), B = Cycle(C)},
#labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {B = PowerSet(C), C = Prod(Z,B), A = Set(C)}, unlabelled],"FAIL",EISnb(52843));
inserttable(maintable,"A simple grammar",
    [A, {B = Sequence(Z,1 <= card), C = Union(Z,B), A = Set(C)}, labelled],"FAIL",EISnb(52844));
## fformer 1144 ????
inserttable(maintable,"A simple grammar",
    [A, {B = Sequence(Z,1 <= card), A = Set(C,1 <= card), C = Prod(Z,B)}, labelled],"FAIL",EISnb(52845));
#ECSv1 985
#inserttable(maintable,"A simple grammar",
#    [A, {C = Sequence(Z,1 <= card), B = Cycle(C), A = Union(Z,B)},
#unlabelled],"FAIL","FAIL");
## fformer 1145
#inserttable(maintable,"A simple grammar",
#    [A, {C = Cycle(Z), B = Cycle(C), A = Union(Z,B)}, labelled],"FAIL","FAIL");
### fff 1090
#inserttable(maintable,"A simple grammar",
#    [A, {B = Sequence(Z,1 <= card), C = Cycle(B), A = Prod(B,C)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), C = Cycle(B), A = Prod(B,C)}, labelled],"FAIL",EISnb(52846));
#inserttable(maintable,"A simple grammar",
#    [A, {A = Prod(C,C), C = Cycle(B), B = Cycle(Z)}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {A = Prod(Z,C), B = Cycle(Z), C = Sequence(B,1 <= card)},
#labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Prod(B,B), B = Sequence(Z,1 <= card), A = Set(C)}, unlabelled],"FAIL",EISnb(52847));
inserttable(maintable,"A simple grammar",
    [A, {B = Prod(Z,C), A = Sequence(B), C = Set(Z,1 <= card)}, labelled],"FAIL",EISnb(52848));
#inserttable(maintable,"A simple grammar",
#    [A, {A = Cycle(B), B = Prod(Z,C), C = Sequence(B)},
#unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Union(B,C), B = Cycle(Z), C = Cycle(Z)}, labelled],"FAIL",EISnb(52849));
inserttable(maintable,"A simple grammar",
    [A, {B = Sequence(Z,1 <= card), C = Cycle(B), A = Union(B,C)},
labelled],"FAIL",EISnb(52850));
## fformer 1155
#inserttable(maintable,"A simple grammar",
#    [A, {B = Sequence(Z,1 <= card), C = Cycle(B), A = Union(Z,C)},
#labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(B,C), B = Cycle(Z), C = Sequence(A)},
labelled],"FAIL",EISnb(52851));
#inserttable(maintable,"A simple grammar",
#    [A, {C = Cycle(Z), B = Union(Z,C), A = Cycle(B)}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {B = Prod(Z,C), A = Sequence(B,1 <= card), C = Cycle(Z)},
#labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(Z,1 <= card), A = Prod(B,C), B = Set(C)}, labelled],"FAIL",EISnb(52852));
inserttable(maintable,"A simple grammar",
    [A, {C = Prod(Z,B), A = Cycle(C), B = Set(A)}, unlabelled],"FAIL",EISnb(52853));
#ECSv1 994
#inserttable(maintable,"A simple grammar",
#    [A, {C = PowerSet(B), A = Union(Z,C), B = Sequence(Z,0 < card)}, unlabelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {C = Prod(B,B), A = Cycle(C), B = Cycle(Z)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Prod(Z,B), A = Set(C), B = Sequence(C)},
unlabelled],"FAIL",EISnb(52854));
#inserttable(maintable,"A simple grammar",
#    [A, {A = Cycle(B), B = Prod(Z,C), C = Cycle(Z)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Prod(B,A), B = Sequence(Z,1 <= card), A = Set(C)}, unlabelled],"FAIL",EISnb(52855));
#inserttable(maintable,"A simple grammar",
#    [A, {C = Prod(B,A), B = Cycle(Z), A = Set(C)}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {C = Prod(B,B), A = Sequence(C,1 <= card), B = Cycle(Z)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Union(B,C), C = Set(Z,1 <= card), B = Sequence(C)}, labelled],"FAIL",EISnb(52856));
#inserttable(maintable,"A simple grammar",
#    [A, {C = Set(B), A = Prod(Z,C), B = Sequence(A,1 <= card)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Set(B), A = Prod(Z,C), B = Sequence(A,1 <= card)}, labelled],"FAIL",EISnb(52857));
inserttable(maintable,"A simple grammar",
    [A, {C = Prod(Z,B), B = Set(Z,1 <= card), A = Cycle(C)}, labelled],"FAIL",EISnb(52858));
## fformer 1172
#inserttable(maintable,"A simple grammar",
#    [A, {C = PowerSet(B), A = Prod(Z,C), B = Cycle(A)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {B = Prod(C,C), C = Set(Z,1 <= card), A = Set(B)}, labelled],"FAIL",EISnb(52859));
#ECSv1 1001
#inserttable(maintable,"A simple grammar",
#    [A, {C = Sequence(Z,0 < card), B = Sequence(C), A = Prod(Z,B)},
#unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Cycle(Z), B = Sequence(C), A = Prod(Z,B)},
labelled],"rooted Sequences of Cycles",EISnb(52860));
inserttable(maintable,"A simple grammar",
    [A, {B = Sequence(Z,1 <= card), C = Cycle(B), A = Prod(B,C)},
labelled],"FAIL",EISnb(52861));
inserttable(maintable,"A simple grammar",
    [A, {C = Set(Z,1 <= card), B = Cycle(C), A = Prod(Z,B)}, labelled],"FAIL",EISnb(52862));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(B,C), C = Cycle(Z)}, labelled],"FAIL",EISnb(52863));
#inserttable(maintable,"A simple grammar",
#    [A, {A = Prod(B,C), C = Cycle(Z), B = Cycle(C)}, labelled],"FAIL","FAIL");
### fff 1124
#inserttable(maintable,"A simple grammar",
#    [A, {C = PowerSet(B), B = Sequence(Z,1 <= card), A = Prod(B,C)}, unlabelled],"FAIL","FAIL");
## fformer 1181
#inserttable(maintable,"A simple grammar",
#    [A, {B = Sequence(Z,1 <= card), C = Set(B,1 <= card), A = Union(Z,C)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(B,C), C = Cycle(Z), B = Sequence(C,1 <= card)},
labelled],"FAIL",EISnb(52864));
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(B,B), C = Cycle(Z), B = Sequence(C,1 <= card)},
labelled],"FAIL",EISnb(52865));
#inserttable(maintable,"A simple grammar",
#    [A, {A = Prod(B,C), B = Sequence(Z,0 < card), C = Set(A)}, unlabelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {A = Prod(B,C), B = Cycle(Z), C = Set(A)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Union(B,C), B = Cycle(Z), C = Set(Z)}, labelled],"FAIL",EISnb(38507));

#ECSv1 1009
#inserttable(maintable,"A simple grammar",
#    [A, {C = Sequence(Z,1 <= card), A = Union(Z,B), B = Sequence(C,1 <= card)},unlabelled],"FAIL","FAIL");
## fformer 1188
#inserttable(maintable,"A simple grammar",
#    [A, {C = Set(Z,1 <= card), A = Union(Z,B), B = Sequence(C,1 <= card)}, labelled],"FAIL","FAIL");
#ECSv1 1010
#inserttable(maintable,"A simple grammar",
#    [A, {B = Prod(C,A), A = Sequence(B), C = Cycle(Z)},
#labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {B = Sequence(Z,1 <= card), C = Set(B), A = Union(B,C)}, labelled],"FAIL",EISnb(52866));
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(Z,1 <= card), B = Set(Z,1 <= card), A = Prod(B,C)}, labelled],"FAIL",EISnb(38156));
inserttable(maintable,"A simple grammar",
    [A, {C = Union(Z,B), B = Set(Z,1 <= card), A = Cycle(C)}, labelled],"FAIL",EISnb(32182));
inserttable(maintable,"A simple grammar",
    [A, {C = Prod(B,B), B = Sequence(Z,1 <= card), A = Cycle(C)},
labelled],"FAIL",EISnb(52867));
#inserttable(maintable,"A simple grammar",
#    [A, {C = PowerSet(A), B = Prod(Z,C), A = Cycle(B)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(Z,1 <= card), B = Prod(C,A), A = Set(B)}, labelled],"FAIL",EISnb(52868));
#inserttable(maintable,"A simple grammar",
#    [A, {C = Prod(Z,A), A = Set(B), B = Cycle(C)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Set(C,1 <= card), C = Prod(Z,B), B = PowerSet(A)}, unlabelled],"FAIL",EISnb(52869));
inserttable(maintable,"A simple grammar",
    [A, {B = Prod(C,A), A = PowerSet(B), C = Sequence(Z,1 <= card)}, unlabelled],"FAIL",EISnb(52870));
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(Z,1 <= card), A = Prod(B,C), B = Set(A)}, labelled],"FAIL",EISnb(52871));
#inserttable(maintable,"A simple grammar",
#    [A, {B = Set(C,1 <= card), C = Set(Z,1 <= card), A = Prod(Z,B)}, labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {B = Prod(C,C), A = PowerSet(B), C = Sequence(Z,1 <= card)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(B,1 <= card), A = Set(C), B = Prod(Z,A)},
unlabelled],"FAIL",EISnb(52872));
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(B,1 <= card), A = Set(C), B = Prod(Z,A)}, labelled],"FAIL",EISnb(52873));
inserttable(maintable,"A simple grammar",
    [A, {C = Sequence(Z,1 <= card), A = Prod(B,C), B = Set(C,1 <= card)}, labelled],"FAIL",EISnb(52874));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(B,C), C = Sequence(B,1 <= card)}, labelled],"FAIL",EISnb(52875));
#ECSv1 1023
#inserttable(maintable,"A simple grammar",
#    [A, {C = Set(Z,1 <= card), B = Cycle(C), A = Union(Z,B)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(C,C), B = Set(Z,1 <= card), C = Sequence(B,1 <= card)}, labelled],"FAIL",EISnb(52876));
## fformer 1208
#inserttable(maintable,"A simple grammar",
#    [A, {A = PowerSet(C), C = Cycle(B), B = Prod(Z,A)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), C = Cycle(B), A = Union(B,C)}, labelled],"FAIL",EISnb(52877));
inserttable(maintable,"A simple grammar",
    [A, {B = Sequence(Z,1 <= card), C = Union(Z,B), A = Cycle(C)},
labelled],"FAIL",EISnb(52878));
inserttable(maintable,"A simple grammar",
    [A, {A = PowerSet(B), C = Prod(Z,A), B = Sequence(C,1 <= card)}, unlabelled],"FAIL",EISnb(52879));
inserttable(maintable,"A simple grammar",
    [A, {C = Prod(B,A), B = Set(Z,1 <= card), A = Set(C)}, labelled],"FAIL",EISnb(52880));
#inserttable(maintable,"A simple grammar",
#    [A, {C = PowerSet(A), B = Sequence(Z,1 <= card), A = Prod(B,C)}, unlabelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {B = PowerSet(C), A = Prod(Z,B), C = Sequence(A,1 <= card)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(C,C), C = PowerSet(B), B = Sequence(Z,1 <= card)}, unlabelled],"FAIL",EISnb(22567));
### ffformer 1160 ?????
#inserttable(maintable,"A simple grammar",
#    [A, {C = Prod(Z,B), B = Set(Z,1 <= card), A = Set(C)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {B = Sequence(Z,1 <= card), A = Prod(B,C), C = Cycle(Z)},
labelled],"FAIL",EISnb(52881));
## fformer 1218
#inserttable(maintable,"A simple grammar",
#    [A, {B = Set(C), A = Prod(Z,B), C = Cycle(A)}, unlabelled],"FAIL","FAIL");
### fff 1163
#inserttable(maintable,"A simple grammar",
#    [A, {A = Prod(Z,C), C = Sequence(B), B = Set(A,1 <= card)},
#unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(Z,C), C = Sequence(B), B = Set(A,1 <= card)}, labelled],"FAIL",EISnb(53492));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(Z,C), C = Sequence(B)}, labelled],"rooted ordered set partitions",EISnb(52882));
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(B,B), C = Sequence(Z,1 <= card), B = Cycle(C)},
labelled],"FAIL",EISnb(52883));
inserttable(maintable,"A simple grammar",
    [A, {B = Prod(Z,C), A = Sequence(B,1 <= card), C = Set(A)},
unlabelled],"FAIL",EISnb(52884));
inserttable(maintable,"A simple grammar",
    [A, {B = Prod(Z,C), A = Sequence(B,1 <= card), C = Set(A)}, labelled],"FAIL",EISnb(52885));
#inserttable(maintable,"A simple grammar",
#    [A, {A = Union(B,C), C = Cycle(Z), B = Cycle(C)}, labelled],"FAIL","FAIL");
## fformer 1226
#inserttable(maintable,"A simple grammar",
#    [A, {C = Prod(Z,B), A = Cycle(C), B = Sequence(Z)},
#labelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {B = Cycle(Z), A = Union(Z,C), C = Sequence(B,1 <= card)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(B,C), C = Set(Z,1 <= card), B = Sequence(A)}, labelled],"FAIL",EISnb(52886));
#inserttable(maintable,"A simple grammar",
#    [A, {B = PowerSet(C), A = Prod(Z,B), C = Set(A,1 <= card)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Prod(B,B), B = Sequence(Z,1 <= card), A = Set(C)}, labelled],"FAIL",EISnb(52887));
inserttable(maintable,"A simple grammar",
    [A, {B = Set(Z,1 <= card), A = Prod(B,C), C = Set(A)}, labelled],"FAIL",EISnb(48802));
inserttable(maintable,"A simple grammar",
    [A, {A = Set(C,1 <= card), C = Prod(Z,B), B = Set(A)}, unlabelled],"FAIL",EISnb(35052));

inserttable(maintable,"A simple grammar",
    [A, {A = Set(C,1 <= card), C = Prod(Z,B), B = Set(A)}, labelled],"FAIL",EISnb(52888));
inserttable(maintable,"A simple grammar",
    [A, {C = Set(B), A = Prod(Z,C), B = Set(A,1 <= card)}, labelled],"FAIL",EISnb(35051));
#inserttable(maintable,"A simple grammar",
#    [A, {A = PowerSet(B), B = Prod(Z,C), C = Sequence(B)}, unlabelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {C = Set(Z,1 <= card), B = Set(C), A = Prod(Z,B)}, labelled],"rooted set partitions",EISnb(52889));
## fformer 1237
#inserttable(maintable,"A simple grammar",
#    [A, {A = Prod(Z,C), C = Sequence(B), B = Cycle(A)},
#unlabelled],"FAIL","FAIL");
#inserttable(maintable,"A simple grammar",
#    [A, {A = Prod(Z,C), C = Sequence(B), B = Cycle(A)},
#labelled],"FAIL","FAIL");
### fff 1183
#inserttable(maintable,"A simple grammar",
#    [A, {B = Set(Z,1 <= card), A = Prod(Z,C), C = Sequence(B,1 <= card)}, labelled],"FAIL","FAIL");
inserttable(maintable,"A simple grammar",
    [A, {A = PowerSet(B), B = Set(C,1 <= card), C = Prod(Z,A)}, unlabelled],"FAIL",EISnb(52890));
inserttable(maintable,"A simple grammar",
    [A, {A = Set(C,1 <= card), C = Prod(Z,B), B = Sequence(A)},
unlabelled],"FAIL",EISnb(52891));
inserttable(maintable,"A simple grammar",
    [A, {A = Set(C,1 <= card), C = Prod(Z,B), B = Sequence(A)}, labelled],"FAIL",EISnb(52892));
inserttable(maintable,"A simple grammar",
    [A, {C = Set(B,1 <= card), A = Sequence(C), B = Prod(Z,A)},
unlabelled],"FAIL",EISnb(52893));
inserttable(maintable,"A simple grammar",
    [A, {C = Set(B,1 <= card), A = Sequence(C), B = Prod(Z,A)}, labelled],"FAIL",EISnb(52894));
inserttable(maintable,"A simple grammar",
    [A, {B = Prod(C,A), A = Sequence(B), C = Set(Z,1 <= card)}, labelled],"FAIL",EISnb(52895));
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(C,C), C = Set(B,1 <= card), B = Set(Z,1 <= card)}, labelled],"FAIL",EISnb(52896));
inserttable(maintable,"A simple grammar",
    [A, {A = Prod(B,B), C = Sequence(Z,1 <= card), B = Set(C)}, labelled],"FAIL",EISnb(52897));
inserttable(maintable,"A simple grammar",
    [A, {A = Union(Sequence(Z),Sequence(Z),Set(Z))}, labelled],"FAIL",EISnb(52898));


# #rajout 6/8/99
#inserttable(maintable, "Odd numbers",
#[S, {S=Union(C,Prod(Z,C)), C = Prod(B,B), B=Sequence(Z)}, unlabelled],
#"Odd numbers", EISnb(5408));

#rajouts 29/11/99
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Union(Sequence(Union(Z,Z)),Z,Z),Z))},unlabelled],"FAIL", EISnb(52899));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Union(Z,Z,Z),Sequence(Z),Z,Z))},unlabelled],"FAIL", EISnb(52900));
# inserttable(maintable,"A simple regular expression", [A,
#     {A = Prod(Union(Z,Z),Union(Sequence(Z),Prod(Z,Z)))},unlabelled],"FAIL", "FAIL");
inserttable(maintable,"A simple regular expression", [A,
    {A = Prod(Sequence(Z),Sequence(Union(Z,Prod(Z,Z,Z))))},unlabelled],"FAIL",EISnb(20745));
inserttable(maintable,"A simple regular expression", [A,
    {A = Union(Sequence(Z),Sequence(Z),Sequence(Prod(Z,Z,Z)))},unlabelled],"FAIL", EISnb(52901));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Prod(Sequence(Z),Z),Z,Z,Z,Z))},unlabelled],"FAIL", EISnb(18903));
inserttable(maintable,"A simple regular expression", [A,
    {A = Prod(Sequence(Z),Sequence(Prod(Union(Z,Z),Z,Z)))},unlabelled],"FAIL", EISnb(225));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Z,Prod(Sequence(Prod(Z,Z,Z)),Z)))},unlabelled],"FAIL", EISnb(52903));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Z,Union(Sequence(Z),Z,Z,Z,Z)))},unlabelled],"FAIL", EISnb(52904));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Union(Z,Z,Z),Sequence(Union(Z,Z))))},unlabelled],"FAIL", EISnb(5053));
inserttable(maintable,"A simple regular expression", [A,
    {A = Prod(Sequence(Z),Sequence(Z),Union(Sequence(Z),Z,Z))},unlabelled],"FAIL", EISnb(52905));
# inserttable(maintable,"A simple regular expression", [A,
#     {A = Union(Sequence(Union(Z,Z,Z)),Prod(Z,Z,Z))},unlabelled],"FAIL", "FAIL");
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Union(Z,Z,Z),Sequence(Prod(Z,Z))))},unlabelled],"FAIL", EISnb(52906));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Sequence(Z),Sequence(Z),Z,Z,Z,Z))},unlabelled],"FAIL", EISnb(24490));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Union(Z,Z),Union(Z,Prod(Z,Z))))},unlabelled],"FAIL", EISnb(52907));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Sequence(Z),Sequence(Prod(Z,Z,Z)),Z))},unlabelled],"FAIL", EISnb(52908));
inserttable(maintable,"A simple regular expression", [A,
    {A = Prod(Union(Sequence(Z),Z),Sequence(Union(Z,Z,Z)))},unlabelled],"FAIL", EISnb(52909));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Sequence(Prod(Z,Z,Z)),Union(Z,Z)))},unlabelled],"FAIL", EISnb(52910));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Z,Prod(Union(Sequence(Z),Z,Z),Z)))},unlabelled],"FAIL", EISnb(52911));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Prod(Union(Z,Z),Z,Z),Z,Z))},unlabelled],"FAIL", EISnb(52912));
# inserttable(maintable,"A simple regular expression", [A,
#     {A = Union(Sequence(Union(Z,Z)),Prod(Z,Z,Z,Z))},unlabelled],"FAIL", "FAIL");
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Prod(Sequence(Union(Z,Z)),Z),Z,Z))},unlabelled],"FAIL", EISnb(7583));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Prod(Sequence(Z),Union(Z,Z)),Z,Z))},unlabelled],"FAIL", EISnb(52913));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Sequence(Z),Sequence(Z),Sequence(Z),Z,Z))},unlabelled],"FAIL", EISnb(10912));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Union(Sequence(Z),Z,Z),Z,Z,Z))},unlabelled],"FAIL", EISnb(52914));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Z,Z,Union(Sequence(Z),Z,Z,Z)))},unlabelled],"FAIL", EISnb(52915));
inserttable(maintable,"Double Fibonacci numbers", [A,
    {A = Prod(Union(Z,Z),Sequence(Union(Z,Prod(Z,Z))))},unlabelled],"FAIL", EISnb(6355));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Z,Z,Union(Prod(Sequence(Z),Z),Z)))},unlabelled],"FAIL", EISnb(52916));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Z,Z,Z,Prod(Z,Z,Z,Z)))},unlabelled],"FAIL", EISnb(52917));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Z,Z,Z,Z,Z,Prod(Z,Z)))},unlabelled],"FAIL", EISnb(52918));
inserttable(maintable,"A simple regular expression", [A,
    {A = Union(Sequence(Prod(Sequence(Z),Union(Z,Z))),Sequence(Z))},unlabelled],"FAIL", EISnb(52919));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Union(Z,Prod(Z,Z,Z)),Z,Z))},unlabelled],"FAIL", EISnb(52920));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Z,Prod(Sequence(Z),Sequence(Z),Z,Z)))},unlabelled],"FAIL", EISnb(34943));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Z,Z,Prod(Sequence(Z),Z,Z,Z)))},unlabelled],"FAIL", EISnb(52921));
inserttable(maintable,"A simple regular expression", [A,
    {A = Prod(Sequence(Prod(Z,Z)),Sequence(Union(Z,Z,Z)))},unlabelled],"FAIL", EISnb(33113));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Z,Z,Union(Z,Z,Prod(Z,Z))))},unlabelled],"FAIL", EISnb(52922));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Sequence(Z),Z,Union(Z,Z,Z,Z)))},unlabelled],"FAIL", EISnb(52923));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Sequence(Z),Union(Z,Z,Prod(Z,Z))))},unlabelled],"FAIL", EISnb(52924));
inserttable(maintable,"A simple regular expression", [A,
    {A = Union(Sequence(Z),Sequence(Prod(Sequence(Z),Sequence(Z),Z)))},unlabelled],"FAIL", EISnb(52925));
inserttable(maintable,"A simple regular expression", [A,
    {A = Union(Sequence(Z),Sequence(Union(Z,Prod(Z,Z,Z))))},unlabelled],"FAIL", EISnb(20711));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Z,Union(Z,Sequence(Union(Z,Z,Z)))))},unlabelled],"FAIL", EISnb(52926));
# inserttable(maintable,"A simple regular expression", [A,
#     {A = Union(Sequence(Z),Sequence(Z),Z,Prod(Z,Z,Z))},unlabelled],"FAIL", "FAIL");
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Z,Z,Z,Z,Prod(Z,Z,Z)))},unlabelled],"FAIL", EISnb(52927));
inserttable(maintable,"A simple regular expression", [A,
    {A = Union(Sequence(Prod(Z,Z)),Prod(Sequence(Z),Sequence(Z)))},unlabelled],"FAIL", EISnb(52928));
inserttable(maintable,"A simple regular expression", [A,
    {A = Union(Sequence(Prod(Z,Z)),Sequence(Union(Z,Z,Z)))},unlabelled],"FAIL", EISnb(52929));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Union(Sequence(Z),Z),Union(Z,Z),Z))},unlabelled],"FAIL", EISnb(52930));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Z,Union(Z,Z,Z,Prod(Z,Z))))},unlabelled],"FAIL", EISnb(52931));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Union(Sequence(Z),Prod(Z,Z,Z)),Z))},unlabelled],"FAIL", EISnb(52932));
inserttable(maintable,"A simple regular expression", [A,
    {A = Union(Sequence(Z),Sequence(Union(Prod(Sequence(Z),Z),Z)))},unlabelled],"FAIL", EISnb(32908));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Z,Union(Z,Z,Sequence(Prod(Z,Z)))))},unlabelled],"FAIL", EISnb(52933));
inserttable(maintable,"A simple regular expression", [A,
    {A = Union(Sequence(Union(Z,Z)),Prod(Sequence(Z),Sequence(Z)))},unlabelled],"FAIL", EISnb(5126));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Sequence(Z),Union(Z,Z,Z,Z,Z)))},unlabelled],"FAIL", EISnb(52934));
inserttable(maintable,"A simple regular expression", [A,
    {A = Union(Sequence(Prod(Z,Z,Z)),Sequence(Union(Z,Z)))},unlabelled],"FAIL", EISnb(52935));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Sequence(Union(Z,Z)),Union(Z,Z),Z))},unlabelled],"FAIL", EISnb(28860));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Union(Sequence(Z),Sequence(Union(Z,Z))),Z))},unlabelled],"FAIL", EISnb(52936));
inserttable(maintable,"A simple regular expression", [A,
    {A = Prod(Sequence(Prod(Z,Z,Z)),Sequence(Union(Z,Z)))},unlabelled],"FAIL", EISnb(33138));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Z,Z,Prod(Union(Z,Z,Z),Z)))},unlabelled],"FAIL", EISnb(14983));
inserttable(maintable,"A simple regular expression", [A,
    {A = Union(Sequence(Z),Sequence(Union(Z,Z,Prod(Z,Z))))},unlabelled],"FAIL", EISnb(52937));
inserttable(maintable,"A simple regular expression", [A,
    {A = Prod(Union(Sequence(Z),Z,Z),Sequence(Prod(Z,Z)))},unlabelled],"FAIL", EISnb(52938));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Z,Z,Prod(Z,Sequence(Prod(Z,Z)))))},unlabelled],"FAIL", EISnb(52939));
inserttable(maintable,"A simple regular expression", [A,
    {A = Prod(Sequence(Union(Z,Z)),Union(Sequence(Z),Z,Z))},unlabelled],"FAIL", EISnb(52940));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Z,Z,Prod(Union(Sequence(Z),Z),Z)))},unlabelled],"FAIL", EISnb(52941));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Union(Z,Prod(Union(Z,Z),Z,Z,Z)))},unlabelled],"FAIL", EISnb(52942));
inserttable(maintable,"A simple regular expression", [A,
    {A = Sequence(Prod(Z,Z,Union(Z,Sequence(Prod(Z,Z)))))},unlabelled],"FAIL",EISnb(52943));

anplusb:=proc(a,b,nb)
    "Arithmetic sequence",
        [A,{A=Union(Prod(Union(Z$a),Sequence(Z),Sequence(Z)),
                Prod(Union(Z$b),Sequence(Z)))},unlabelled],
                cat(" ",a," n + ",b),EISnb(nb)
end:

anplus:=proc(a,ini)
local i;
    for i to a-1 do
        inserttable(maintable,anplusb(a,i,ini-12+12*i))
    od
end:

anplus(2,5408);
anplus(3,16777);
anplus(4,16813);
anplus(5,16861);
anplus(6,16921);
anplus(7,16993);
anplus(8,17077);
anplus(9,17173);
anplus(10,17281);
anplus(11,17401);
anplus(12,17533);

inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Union(Sequence(Union(Z,Z)),Sequence(Z)),Sequence(Z))},unlabelled],"FAIL",EISnb(52944));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Union(Sequence(Prod(Z,Z)),Sequence(Z)),Sequence(Z))},unlabelled],"FAIL",EISnb(7494));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Sequence(Prod(Sequence(Z),Z)),Union(Sequence(Z),Z))},unlabelled],"FAIL",EISnb(20714));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Sequence(Prod(Z,Z)),Sequence(Z)),Z))},unlabelled],"FAIL",EISnb(52945));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Prod(Sequence(Z),Sequence(Z)),Z),Z))},unlabelled],"FAIL",EISnb(52946));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Prod(Union(Z,Z),Z),Z),Z))},unlabelled],"FAIL",EISnb(52947));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Sequence(Prod(Sequence(Z),Z)),Z),Z))},unlabelled],"FAIL",EISnb(52948));
inserttable(maintable,"A simple regular expression",[A,
    {A = Union(Sequence(Prod(Union(Sequence(Z),Z),Z)),Sequence(Z))},unlabelled],"FAIL",EISnb(52949));
inserttable(maintable,"A simple regular expression",[A,
    {A = Union(Sequence(Prod(Sequence(Z),Z)),Sequence(Prod(Z,Z)))},unlabelled],"FAIL",EISnb(52950));

all_nb:=proc(k,ind)
    inserttable(maintable,"Constant sequence",
        [A,{A=Prod(Union(Z$k),Sequence(Z))},unlabelled],cat("All ",k,"'s"),
        EISnb(ind))
end:

all_nb(2,7395);
all_nb(3,10701);
all_nb(4,10709);
all_nb(5,10716);
all_nb(6,10722);
all_nb(7,10727);
all_nb(8,10731);
all_nb(9,10734);

inserttable(maintable,"A simple regular expression",[A,
   {A = Prod(Union(Prod(Sequence(Z),Sequence(Z)),Sequence(Z)),Sequence(Z))},
   unlabelled],"FAIL",EISnb(96));
inserttable(maintable,"A simple regular expression",[A,
   {A = Prod(Sequence(Union(Prod(Z,Z),Z)),Union(Sequence(Z),Z))},
   unlabelled],"FAIL",EISnb(1595));
inserttable(maintable,"A simple regular expression",[A,
   {A = Prod(Sequence(Union(Prod(Z,Z),Z)),Sequence(Union(Z,Z)))},
   unlabelled],"FAIL",EISnb(8466));

inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Union(Sequence(Union(Z,Z)),Z),Sequence(Union(Z,Z)))},
    unlabelled],"FAIL",EISnb(52951));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Sequence(Prod(Union(Z,Z),Z)),Sequence(Union(Z,Z)))},
    unlabelled],"FAIL",EISnb(32085));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Sequence(Union(Prod(Z,Z),Z)),Sequence(Prod(Z,Z)))},
    unlabelled],"FAIL",EISnb(52952));
inserttable(maintable,"A simple regular expression",[A,
    {A = Union(Sequence(Union(Prod(Union(Z,Z),Z),Z)),Sequence(Z))},
    unlabelled],"FAIL",EISnb(52953));
inserttable(maintable,"A simple regular expression",[A,
    {A = Union(Sequence(Prod(Union(Prod(Z,Z),Z),Z)),Sequence(Z))},
    unlabelled],"FAIL",EISnb(52954));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Sequence(Prod(Union(Z,Z),Z)),Union(Sequence(Z),Z))},
    unlabelled],"FAIL",EISnb(52955));
inserttable(maintable,"A simple regular expression",[A,
    {A = Union(Sequence(Union(Prod(Z,Z),Z)),Sequence(Union(Z,Z)))},
    unlabelled],"FAIL",EISnb(52956));
inserttable(maintable,"A simple regular expression",[A,
    {A = Union(Sequence(Prod(Union(Z,Z),Z)),Sequence(Union(Z,Z)))},
    unlabelled],"FAIL",EISnb(52957));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Prod(Z,Z),Sequence(Z)),Union(Z,Z)))},
    unlabelled],"FAIL",EISnb(52958));
inserttable(maintable,"A simple regular expression",[A,
    {A = Union(Sequence(Union(Prod(Z,Z),Z)),Sequence(Prod(Z,Z)))},
    unlabelled],"FAIL",EISnb(52959));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Sequence(Union(Prod(Z,Z),Z)),Z),Z))},
    unlabelled],"FAIL",EISnb(52960));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Union(Prod(Sequence(Union(Z,Z,Z)),Z),Z))},
    unlabelled],"FAIL",EISnb(52961));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Sequence(Prod(Union(Z,Z),Z)),Z),Z))},
    unlabelled],"FAIL",EISnb(52962));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Union(Prod(Sequence(Union(Prod(Z,Z),Z)),Z),Z))},
    unlabelled],"FAIL",EISnb(52963));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Prod(Sequence(Z),Z),Z,Z),Z))},
    unlabelled],"FAIL",EISnb(52964));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Z,Z,Sequence(Z)),Union(Z,Z)))},
    unlabelled],"FAIL",EISnb(52965));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Sequence(Union(Z,Z,Z)),Union(Z,Z)))},
    unlabelled],"FAIL",EISnb(20699));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Prod(Sequence(Z),Z),Z),Union(Z,Z)))},
    unlabelled],"FAIL",EISnb(52966));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Prod(Z,Z),Z,Sequence(Z)),Z))},
    unlabelled],"FAIL",EISnb(52967));
inserttable(maintable,"A simple regular expression",[A,
    {A = Union(Sequence(Prod(Sequence(Z),Z)),Prod(Sequence(Z),Sequence(Z)))},
    unlabelled],"FAIL",EISnb(52968));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Prod(Union(Sequence(Z),Z),Z),Z),Z))},
    unlabelled],"FAIL",EISnb(52969));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Prod(Sequence(Union(Z,Z)),Z),Z),Z))},
    unlabelled],"FAIL",EISnb(52970));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Prod(Union(Z,Z),Z),Sequence(Z)),Z))},
    unlabelled],"FAIL",EISnb(52971));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Sequence(Prod(Z,Z,Z)),Z),Z))},
    unlabelled],"FAIL",EISnb(52972));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Prod(Union(Z,Z),Sequence(Z)),Z),Z))},
    unlabelled],"FAIL",EISnb(52973));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Sequence(Union(Z,Z)),Z),Z,Z))},
    unlabelled],"FAIL",EISnb(52974));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Sequence(Prod(Sequence(Z),Z)),Sequence(Z)),Z))},
    unlabelled],"FAIL",EISnb(52975));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Sequence(Union(Z,Z)),Prod(Z,Z)),Z))},
    unlabelled],"FAIL",EISnb(52976));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Sequence(Prod(Z,Z)),Prod(Z,Z)),Z))},
    unlabelled],"FAIL",EISnb(52977));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Sequence(Union(Z,Z)),Z),Union(Z,Z)))},
    unlabelled],"FAIL",EISnb(52978));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Sequence(Prod(Z,Z)),Z),Union(Z,Z)))},
    unlabelled],"FAIL",EISnb(52979));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Prod(Union(Z,Z),Z),Z),Sequence(Z)))},
    unlabelled],"FAIL",EISnb(26150));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Prod(Z,Z,Z),Z),Sequence(Z)))},
    unlabelled],"FAIL",EISnb(52980));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Z,Z,Z),Union(Sequence(Z),Z)))},
    unlabelled],"FAIL",EISnb(52981));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Prod(Z,Z),Z),Union(Sequence(Z),Z)))},
    unlabelled],"FAIL",EISnb(52982));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Union(Prod(Sequence(Prod(Z,Z)),Union(Z,Z)),Z))},
    unlabelled],"FAIL",EISnb(28495));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Union(Prod(Sequence(Prod(Z,Z)),Z,Z),Z))},
    unlabelled],"FAIL",EISnb(52983));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Union(Prod(Sequence(Union(Z,Z)),Union(Z,Z)),Z))},
    unlabelled],"FAIL",EISnb(52984));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Union(Prod(Union(Prod(Z,Z),Sequence(Z)),Z),Z))},
    unlabelled],"FAIL",EISnb(52985));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Union(Prod(Union(Sequence(Union(Z,Z)),Z),Z),Z))},
    unlabelled],"FAIL",EISnb(52986));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Union(Prod(Sequence(Prod(Union(Z,Z),Z)),Z),Z))},
    unlabelled],"FAIL",EISnb(52987));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Union(Prod(Union(Sequence(Prod(Z,Z)),Z),Z),Z))},
    unlabelled],"FAIL",EISnb(52988));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Union(Prod(Z,Z),Sequence(Z)),Z,Z))},
    unlabelled],"FAIL",EISnb(52989));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Union(Prod(Union(Sequence(Z),Z),Union(Z,Z)),Z))},
    unlabelled],"FAIL",EISnb(52990));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Sequence(Union(Z,Z,Prod(Z,Z))),Sequence(Z))},
    unlabelled],"FAIL",EISnb(48739));
inserttable(maintable,"A simple regular expression",[A,
    {A = Sequence(Prod(Sequence(Union(Prod(Z,Z),Z)),Union(Z,Z)))},
    unlabelled],"FAIL",EISnb(52991));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Union(Sequence(Prod(Union(Z,Z),Z)),Z),Sequence(Z))},
    unlabelled],"FAIL",EISnb(16116));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Sequence(Prod(Union(Z,Z),Union(Z,Z))),Sequence(Z))},
    unlabelled],"FAIL",EISnb(52992));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Sequence(Prod(Union(Z,Z,Z),Z)),Sequence(Z))},
    unlabelled],"FAIL",EISnb(52993));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Sequence(Prod(Union(Prod(Z,Z),Z),Z)),Sequence(Z))},
    unlabelled],"FAIL",EISnb(23434));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Sequence(Prod(Union(Sequence(Z),Z),Z)),Union(Z,Z))},
    unlabelled],"FAIL",EISnb(52994));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Sequence(Union(Prod(Sequence(Z),Z),Z)),Union(Z,Z))},
    unlabelled],"FAIL",EISnb(52995));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Union(Sequence(Prod(Z,Z)),Sequence(Z)),Union(Z,Z))},
    unlabelled],"FAIL",EISnb(10694));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Union(Prod(Z,Z),Sequence(Z)),Sequence(Union(Z,Z)))},
    unlabelled],"FAIL",EISnb(52996));
inserttable(maintable,"A simple regular expression",[A,
    {A = Prod(Union(Sequence(Prod(Z,Z)),Z),Sequence(Union(Z,Z)))},
    unlabelled],"FAIL",EISnb(52997));
