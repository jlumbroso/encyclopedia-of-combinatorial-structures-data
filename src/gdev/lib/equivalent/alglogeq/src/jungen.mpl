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
##
##    Title:	jungen
##    Created:	June 87
##    Author:	Bruno Salvy
##	<salvy@rully>
##
## Description: Uses Jungen's theorem to translate an algebrico-logarithmic
## expression into the expansion of its coefficients.
## The expression is given as a monomial in X[1] and X[2], where
## X[1] stands for (1-x) and X[2] means 1/ln(1/(1-x)).
##
##    Modified:	    Fri Nov 10 20:49:33 1989
##    Author:	Bruno Salvy
##    Modification: completely rewritten, Jungen's "algorithm" is replaced by
##	    something much more efficient taken from
##	 Flajolet & Odlyzko 1990 SIAM journal on discrete math, 3-1.
##
##
##    Modified:	    Fri Oct 19 19:44:14 1990
##    Author:	Bruno Salvy
##    Modification: optimized the number of terms used in purjungen,
##	 and a bug fix (the result was wrong by a factor of k.)
##
## Modified BS July 98. It was a big mistake to use limit which calls expand.

`equivalent/alglogeq/jungen`:=proc (dev,p)
local c, res, i, maxl, a, j, g, k, neg, pos, l, m, toadd, toadd2, q, dim, co, kk, nbterms;
    pos:=[];
    neg:=[];
    for i from 3 by 2 to nops(dev) do
	if not type(op(i,dev),nonnegint) then neg:=[op(neg),i]
	elif type(op(i-1,dev),list) then pos:=[op(pos),i]
	fi
    od;
    res:=0;
    if neg<>[] then
	maxl:=max(0,seq(-i[3],i=select(type,[seq(op(i-1,dev),i=neg)],list)));
	if type(p/(maxl+1),integer) then
	    k:=p/(maxl+1)-1
	else
	    k:=trunc(evalf(p/(maxl+1)))
	fi;
	c:=`equivalent/alglogeq/purjungen`(maxl,k,0);
	a:=array(0..maxl,0..k,sparse);
	for i from 0 to maxl do
	    g:=(-1)^i*(op(2*i+1,c))*i!;
	    for j from 0 to k do
		co:=coeff(g,_X,j);
		a[i,j]:=[2,seq(op([coeff(co,_Y,i-kk),kk-i]),kk=0..i)]
	    od
	od;
	for i in neg do
	    if not type(op(i-1,dev),list) then
		if not type(op(i,dev),integer) or -op(i,dev)>k then
		   toadd:=[1,seq(op([subs(_AL=op(i,dev),op(2,a[0,j]))*
		    op(i-1,dev),j+op(i,dev)+1]),j=0..k),1,k+op(i,dev)+2]
		else
		   toadd:=[1,seq(op([subs(_AL=op(i,dev),op(2,a[0,j]))*
		    op(i-1,dev),j+op(i,dev)+1]),j=0..k),0,infinity]
		fi;
	    else
		toadd:=[1];
		for j from 0 to k do
		    toadd2:=`dev/multbyreal`([op(subs(_AL=op(i,dev),
		      a[-op(3,op(i-1,dev)),j])),0,infinity],op(2,op(i-1,dev)));
		    for q from 2 to iquo(nops(op(i-1,dev)),2) do
			toadd2:=`dev/add`(toadd2,`dev/multbyreal`([op(subs(_AL=
			    op(i,dev),a[-op(2*q+1,op(i-1,dev)),j])),0,infinity],
				op(2*q,op(i-1,dev)))) od;
		    toadd:=[op(toadd),[op(1..nops(toadd2)-2,toadd2)],
			j+op(i,dev)+1]
		od;
		if toadd<>[1] then
		    toadd:=[op(toadd),[2,1,-k-1],k+op(i,dev)+2] fi
	    fi;
	    if toadd<>[1] then
		res:=`dev/add`(res,`dev/clean`(toadd))
	    fi
	od
    fi;
    if pos<>[] then # terms of the form ln(1/(1-z))^k*(1-z)^i, i in N
	# special case for 1/n
	if nops(dev)=3 and dev[3]=0 and dev[2][3]=-1 then
	    RETURN([1,dev[2][2],1,0,infinity]) fi;
	nbterms:=0;
	for dim to nops(pos) while nbterms<p+2 do
	    if type(op(op(dim,pos)-1,dev),list) then
		nbterms:=nbterms-op(3,op(op(dim,pos)-1,dev)) fi od;
	if nbterms>=p+2 or dim>nops(pos) then dim:=dim-1 fi;
	pos:=pos[1..dim];
	maxl:=max(0,seq(-i[3],i=select(type,[seq(op(i-1,dev),i=pos)],list)));
	dim:=2+p+op(op(dim,pos),dev);
##	dim:=dim+op(op(1,pos),dev);
#	dim:=add(-i[3],i=select(type,[seq(op(i-1,dev),i=pos)],list))+op(1,pos);
#	if maxl<>1 then dim:=p+2 else dim:=p+2+op(1,pos) fi;
	c:=subs(_AL=-1,`equivalent/alglogeq/purjungen`(maxl,dim,l));
	a:=array(0..maxl,0..dim,sparse);
	for i from 0 to maxl do
	    g:=(-1)^i*op(2*i+1,c)*i!;
	    for j from 0 to dim do
		co:=coeff(g,_X,j);
		a[i,j]:=[2,seq(op([coeff(co,_Y,i-kk),kk-i]),kk=0..i)]
	    od
	od; # after this loop, a[i,j] is something like 
	    # [n^(-j)][z^(n-l)](1-z)^(-1)*log(1/(1-z))^i
	for i in pos do
	    toadd:=[1];
	    for j from 0 to dim do
		toadd2:=`dev/multbyreal`(a[-op(3,op(i-1,dev)),j],
		    op(2,op(i-1,dev)));
		for q from 2 to iquo(nops(op(i-1,dev)),2) do
		    toadd2:=`dev/add`(toadd2,`dev/multbyreal`(
			a[-op(2*q+1,op(i-1,dev)),j],op(2*q,op(i-1,dev)))) od;
		toadd:=[op(toadd),toadd2,j]
	    od;
	    if toadd<>[1] then
		m:=op(i,dev);
		toadd2:=`dev/clean`(subs(l=0,toadd));
		for j from 1 to m+1 do
		    toadd2:=`dev/add`(toadd2,`dev/multbyreal`(`dev/clean`(
			subs(l=j,toadd)),(-1)^j*binomial(m+1,j)))
		od;
		res:=`dev/add`(res,toadd2);
	    fi
	od
    fi;
    # cases where the expansion is exact
#    if type(res,list) and nops(res)<2*p+3 then
#	for i from 3 by 2 to nops(dev) while
#	    type(op(i,dev),integer) and not type(op(i-1,dev),list) or 
#	    (type(op(i,dev),nonnegint) and type({seq(op(2*j+1,op(i-1,dev)),
#		j=1..iquo(nops(op(i-1,dev)),2))},set(nonposint)) and 
#		op(i,dev)<p) do od;
#	if i>nops(dev) then [op(res),0,infinity]
#	else res
#	fi
#    else
	res
#    fi
end: # `equivalent/alglogeq/jungen`

# p is the exponent of the logarithm
# k is the number of terms we need to compute
# l means we are computing the coefficient of z^{n-l}
# purjungen returns the k first terms of the asymptotic expansion of
# [z^(n-l)](1-z)^alpha*log(1/(1-z))^p
`equivalent/alglogeq/purjungen`:=proc (p,k,l)
local f, i, g, pol, a, j, res, t;
    f:=series('exp'((l-1-1/_X)*'ln'(1+t*_X)+t),_X,k+2);
    a:=array(0..2*k,0..k,sparse);
    for j from 0 to k do
	g:=op(2*j+1,f);
	for i from j to 2*j do
	    a[i,j]:=coeff(g,t,i)
	od
    od;
    pol:=1;
    res:=1;
#   for i to 2*k do
#	pol:=subs(_U^(p+1)=0,expand(pol*(_AL+_U+i)));
#	res:=res+convert(['a[i,j]*_X^j'$
#	    'j'=iquo(i+1,2)..min(i,k)],`+`)*pol
#   od;
# the following 25 lines are faster:
    for i to min(k,p) do
	pol:=expand(pol*(_AL+_U+i));
	res:=res+convert(['a[i,j]*_X^j'$'j'=iquo(i+1,2)..i],`+`)*pol
    od;
    if k<p then
	for i from k+1 to min(p,2*k) do
	    pol:=expand(pol*(_AL+_U+i));
	    res:=res+convert(['a[i,j]*_X^j'$'j'=iquo(i+1,2)..k],`+`)*pol
	od;
	if 2*k>p then
	    for i from p+1 to 2*k do
		pol:=subs(_U^(p+1)=0,expand(pol*(_AL+_U+i)));
		res:=res+convert(['a[i,j]*_X^j'$'j'=iquo(i+1,2)..k],`+`)*pol
	    od
	fi
    else
	for i from p+1 to k do
	    pol:=subs(_U^(p+1)=0,expand(pol*(_AL+_U+i)));
	    res:=res+convert(['a[i,j]*_X^j'$'j'=iquo(i+1,2)..i],`+`)*pol
	od;
	for i from k+1 to 2*k do
	    pol:=subs(_U^(p+1)=0,expand(pol*(_AL+_U+i)));
	    res:=res+convert(['a[i,j]*_X^j'$'j'=iquo(i+1,2)..k],`+`)*pol
	od
    fi;
    series(exp(-_U*_Y)/GAMMA(-_U-_AL)*res,_U,p+1)
end: # `equivalent/alglogeq/purjungen`

`dev/clean`:=proc (dev)
local res, res2, i, j, lz;
    res:=[1];lz:=[];
    for i from 2 by 2 to nops(dev) do
	if type(op(i,dev),list) then
	    res2:=[2];
	    for j from 2 by 2 to nops(op(i,dev)) do
		if op(j,op(i,dev))<>0 then
		    res2:=[op(res2),op(j..j+1,op(i,dev))]
		fi;
	    od;
	    if nops(res2)=3 and op(3,res2)=0 then
		res2:=op(2,res2)
	    elif res2=[2] then res2:=0
	    fi
	else res2:=op(i,dev)
	fi;
	if res2<>0 then res:=[op(res),res2,op(i+1,dev)]; lz:=[]
	elif i<>nops(dev)-1 then lz:=[op(i+1,dev)]
	elif lz=[] then res:=[op(res),1,dev[nops(dev)]]
	elif op(i+1,dev)<>infinity then res:=[op(res),1,op(lz)]
## This replaces an exact computation with a O() term with infinity as exponent.
## I'm not sure this is the right thing to do.
## Modified BS. May 2009.
##	else res:=[op(res),1,infinity]
	else res:=[op(res),op(i,dev),infinity]
	fi
    od;
    res
end: # `dev/clean`

#savelib( `equivalent/alglogeq/jungen`,`equivalent/alglogeq/purjungen`,`dev/clean`);
