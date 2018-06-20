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
##    Title:	`dev/RootOf`
##    Created:	Wed Nov 22 11:37:24 1989
##    Author:	Bruno Salvy
##	<bruno.salvy@inria.fr>
##
## Description: all the job should be done by dev/implicit.
## All of this should be rewritten.

`dev/RootOf`:=proc (fctglob,p)
local f, x, sols, res, i, alg, lalg, fc, fct, j, y;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    try fct:=normal(fctglob);
    catch: fct:=fctglob
    end try;
    if not type(fct,algfun(anything,[_Xasy,_Z])) then
	# first be careful with algebraic numbers
	fc:=fct;
	lalg:=[op(indets(fct,RootOf))];
	fc:=subs({seq(lalg[i]=alg[i],i=1..nops(lalg))},fc);
	f:=[solve(fc,_Z)];
	if not has(f,RootOf) then
	    map(`dev/dev`,subs({seq(alg[i]=lalg[i],i=1..nops(lalg))},f),p,p)
	else f:=[solve(fc,_Xasy)];
	    if has(f,RootOf) then ERROR(`cannot expand this RootOf`) fi;
	    res:=`dev/dev`(subs(
		{seq(alg[i]=lalg[i],i=1..nops(lalg))},fc),1,1);
	    if not has(op(2,res),_Z) then sols:=[infinity]
	    else sols:=[solve(op(2,res),_Z)] fi;
	    res:=[];
	    for i in sols do
		if i=infinity then
		    res:=[op(res),op(`dev/implicit`(`dev/dev`(subs(
			_Z=_Xasy,op(1,f)),p,p),[1,1,-1,0,infinity],p))]
		elif i=0 then
		    res:=[op(res),op(map(`dev/pow`,`dev/implicit`(`dev/dev`
			(subs(_Z=1/_Xasy,op(1,f)),p,p),[1,1,-1,0,infinity],
			p),-1,p))]
		else
		    if type(i,RootOf(algebraic)) then
			Digits:=2*Digits;# because of Newton
			# j:=traperror(evalf(i));
			j:=evalf(i);
			Digits:=iquo(Digits,2);
			j:=RootOf(op(1,i),evalf(j))
		    else j:=i
		    fi;
		    res:=[op(res),op(map(proc(x,y,p) `dev/multbyreal`(
			`dev/add`(-1,`dev/pow`(x,-1,p)),y) end,
			`dev/implicit`(`dev/dev`(subs(_Z=j*(1-1/_Xasy),
			op(1,f)),p,p),[1,1,-1,0,infinity],p),-j,p))]
		fi
	    od;
	    res
	fi
    else # special case for algebraic functions
	`dev/RootOf/algfun`(numer(eval(subs([_Xasy=1/x,_Z=y],fct))),x,y,p)
    fi
end: # `dev/RootOf`

`dev/RootOf/algfun`:=proc (Pol,x,y,ord,optional_positive_slopes)
local pol, a, u, i, j, pts, alpha, mini, deg, theta, jmin, sl, lastpt, p, pu, nb2, res, r, nb, q, a0, normalizer, eq;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    pol:=collect(Pol,[y,x],evala);
    # Newton polygon
    deg:=degree(pol,y);
    pts:=select(proc(x) x[2]<>0 end,[seq([i,coeff(pol,y,i)],i=0..deg)]);
    pts:=[seq([i[1],ldegree(i[2],x)],i=pts)];
    nb:=0;
    lastpt:=pts[1];
    for i from 2 to nops(pts) do
	mini:=infinity;
	for j from i to nops(pts) do
	    theta:=(pts[j][2]-lastpt[2])/(pts[j][1]-lastpt[1]);
	    if theta<mini then mini:=theta; jmin:=j fi
	od;
	nb:=nb+1; alpha[nb]:=-mini; i:=jmin; lastpt:=pts[i]
    od;
    # Treat each slope
    nb2:=0;
    alpha:={seq(alpha[i],i=1..nb)};
    if nargs=5 then alpha:=select(type,alpha,'nonnegative') fi;
    for sl in alpha do
	r:=denom(sl);
	p:=collect(subs(x=x^r,y=x^numer(sl)*y,pol),x);
	if ldegree(p,x)<>0 then p:=collect(p/x^ldegree(p,x),x) fi;
	q:=collect(coeff(p,x,0),y);
	if ldegree(q,y)<>0 then q:=collect(q/y^ldegree(q,y),y) fi;
	for u in sqrfree(q,y)[2] do
	    try 
		a0:=[evala(RootOf(u[1],y))];
		# These 5 lines are not strictly necessary but 
		# they save much trouble in later computations.
	    catch "reducible RootOf detected.  Substitutions are":
		a0:=map(subs,lastexception[3],RootOf(u[1],y))
	    catch: error
	    end try;
	    for a0 in a0 do
		if u[2]=1 then	# regular case
		    if type(a0,RootOf) then normalizer:=evala
		    else normalizer:=normal fi;
		    a[0]:=a0; pu:=p;
		    for i to ord-1 do # ord relatively small
			pu:=collect(subs(y=a[i-1]+x*y,pu),x);
			# do not use solve: it does strange things with RootOf's
			# a[i]:=normalizer(solve(coeff(pu,x,i),y))
			eq:=coeff(pu,x,i); # this is assumed to be linear in y
			if degree(eq,y)=1 then
			    a[i]:=normalizer(-coeff(eq,y,0)/coeff(eq,y,1))
			else ERROR(`unforecast case`) fi
		    od; # in this loop, normal saves memory
		    nb2:=nb2+1;
		    res[nb2]:=map(op,[1,seq([a[i],sl+i/r],i=0..ord-1),1,ord])
		else		# several branches
		    for i in `dev/RootOf/algfun`(
			subs([x=x^u[2],y=a0+x*y],p),x,y,ord-1,1) do
			nb2:=nb2+1;
			res[nb2]:=map(op,[1,[a0,sl],seq([i[2*j],
			    (i[2*j+1]+1)/u[2]/r],j=1..iquo(nops(i),2))])
		    od
		fi
	    od
	od
    od;
    [seq(res[i],i=1..nb2)]
end: # `dev/RootOf/algfun`

#savelib(`dev/RootOf`,`dev/RootOf/algfun`);
