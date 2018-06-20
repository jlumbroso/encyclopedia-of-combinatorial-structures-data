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

# -*-maple-*-
##################################################
#			     HS
#
#   Apply Harris-Scoenfeld formula to HS-admissible functions.
# We need a global variable _saddlepoint, because the 
# expansion in n never converges.
#
# Ref. Harris, Bernard; Schoenfeld, Lowell: Asymptotic expansions for 
# the coefficients of analytic functions. Illinois J. Math. 12 1968, 264--277.
#
##################################################
# modified BS jan 95	suppressed an extra _saddlepoint^(-n)
## modified BS mar 03 	added comments and simplified code and changed 
##			the notation so that it's closer to the reference.
##			And fix a bug (the result was wrong !).

`equivalent/saddlepoint/HarrisSchoenfeld`:=proc(fct,p,var,sing)
local B,gamma,Bb,u,beta,i,j,k,l,res,somme,s,q,m,prod,z,A,n, pt, T, Gamma, F, U;
    z:=var;
    A:=normal(diff(fct,z)/fct);
    for k to 2*p+2 do
	B[k]:=A*z^k/k!;
	A:=diff(A,z)
    od;
    Bb:=z*diff(B[1],z)/2;	  # B(z)
if assigned(_EnvSolveSaddlePoint) and _EnvSolveSaddlePoint=false then
    u:=[]
else
    u:=[solve(B[1]=n+1,z)];
fi;
    # It is always printed in saddle.
    `equivalent/saddlepoint/saddle`(u,sing,fct,var,false,n);
    if sing<>infinity then pt:=sing*(1-1/_Xasy) else pt:=_Xasy fi;
    beta:=Bb;#`dev/print`(`dev/dev`(subs(z=pt,Bb),p,p),_Xasy,infinity);
    for j to 2*p-2 do
	gamma[j]:=#`dev/dev`(subs(z=pt,
	    -(B[j+2]+(-1)^j/(j+2)*B[1])/beta#),max(1,p-j),max(1,p-j))
    od;
    gamma:=series((1+U-add(gamma[j]*T^j,j=1..2*p-2))^(-1/2),U,p);
    res:=1;
    for k to p-1 do
	F:=k!*coeff(series(coeff(gamma,U,k),T,2*k+1),T,2*k); # F_k
	res:=`dev/add`(res,`dev/dev`(subs(z=pt,F/beta^k),p,p))
    od;
    `dev/prd`(`dev/add`(res,`dev/O`(`dev/pow`(`dev/dev`(subs(z=pt,beta),1,1),
	-p,1))),`dev/dev`(subs(z=pt,fct/2/sqrt(Pi)/beta^(1/2)),p,p))
end:

#savelib( `equivalent/saddlepoint/HarrisSchoenfeld`);
