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
##################################################
# modified BS jan 95	suppressed an extra _saddlepoint^(-n)

`equivalent/saddlepoint/HarrisSchoenfeld`:=proc(fct,p,var,sing)
local b,g,c,u,d,i,j,k,l,res,somme,s,q,m,prod,x,aux,n;
    x:=var;
    k[0]:=0;
    aux:=normal(diff(fct,x)/fct);
    for i from 1 to 2*p+2 do
	b[i]:=aux*x^i/i!;
#	aux:=normal(diff(aux,x))
	aux:=diff(aux,x)
    od;
    c:=x*diff(b[1],x)/2;
if assigned(_EnvSolveSaddlePoint) and _EnvSolveSaddlePoint=false then
    u:=[]
else
    u:=[solve(b[1]=n+1,x)];
fi;
    `equivalent/saddlepoint/saddle`(u,sing,fct,var,false,n);# It is always printed in saddle.
    if sing<>infinity then
	d:=`dev/print`(`dev/dev`(subs(x=sing*(1-1/_Xasy),c),p,p),_Xasy,infinity);
	for i to max(2,p) do
	    g[i]:=`dev/dev`(subs(x=sing*(1-1/_Xasy),
		-(b[i+2]+(-1)^i/(i+2)*b[1])/d),max(1,p-i),max(1,p-i))
	od
    else
	d:=`dev/print`(`dev/dev`(subs(x=_Xasy,c),p,p),_Xasy,infinity);
	for i to max(2,p) do
	    g[i]:=`dev/dev`(subs(x=_Xasy,-(b[i+2]+(-1)^i/(i+2)*b[1])/d)
	    ,max(1,p-i),max(1,p-i))
	od
    fi;
    res:=1;
    for i to max(1,trunc(p/2)) do
	somme:=0;
	for j to 2*i do
	    k[1]:=2*i-j+1;
	    s:=0;
	    for l from 2 to j do k[l]:=1 od;
	    m:=1;
	    while k[1]>0 and m>0 do
		prod:=1;
		for q to j do prod:=`dev/prd`(prod,g[k[q]]) od;
		s:=`dev/add`(s,prod);
		m:=j-1;
		while k[m]=1 do m:=m-1 od;
		k[m]:=k[m]-1;
		k[m+1]:=k[j]+1;
		k[j]:=1
	    od;
	    somme:=`dev/add`(somme,`dev/multbyreal`(s,GAMMA(j+i+1/2)/j!))
	od;
	res:=`dev/add`(res,`dev/multbyreal`(
	    `dev/prd`(somme,`dev/pow`(d,-i,max(1,p-i))),(-1)^i/sqrt(Pi)))
    od;
    if sing<>infinity then
	`dev/prd`(`dev/add`(res,`dev/O`(`dev/pow`(d,-i,1))),
	    `dev/dev`(subs(x=sing*(1-1/_Xasy),fct/2/sqrt(Pi)/d^(1/2)),p,p))
    else
	`dev/prd`(`dev/add`(res,`dev/O`(`dev/pow`(d,-i,1))),
	    `dev/dev`(subs(x=_Xasy,fct/2/sqrt(Pi)/d^(1/2)),p,p))
    fi
end:

#savelib( `equivalent/saddlepoint/HarrisSchoenfeld`);
