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
##    Title:	`dev/invert`
##    Created:	Thu Sep 19 22:47:00 1991
##    Author:	Bruno Salvy
##  <salvy@rully.inria.fr>
##
## Description: fast inversion of dense power series.

`dev/invert`:=proc (dev,n)
local ldev, i;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if not type(dev,list({name,numeric})) or
	not type([seq(op(2*i+1,dev),i=1..iquo(nops(dev),2)-1)],list(posint)) 
	or op(1,dev)<>1 then ERROR(FAIL)
    elif op(3,dev)=1 then
	`dev/invert/doit`(dev,n*igcd(seq(op(2*i+1,dev)-1,
	    i=1..iquo(nops(dev),2)-1)))
    else
	ldev:=`dev/invert/doit`(`dev/pow`(dev,1/op(3,dev),n),n);
	[1,seq(op([op(2*i,ldev),op(2*i+1,ldev)/op(3,dev)]),
	    i=1..iquo(nops(ldev,2)))]
    fi
end: # `dev/invert`

`dev/invert/doit`:=proc (dev,n)
local x, o, nd, i, k, p, ppv, pv, v;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    nd:=nops(dev);
    if dev[nd]<>infinity then o:=min(n,dev[nd]) else o:=n fi;
    if dev[nd]=infinity and nd=5 then RETURN([1,1/op(2,dev),1,0,infinity]) fi;
    v:=x/op(2,dev);
    p:=convert([seq(op(2*i,dev)*x^op(2*i+1,dev),i=1..iquo(nd,2)-1)],`+`);
    k:=1;
    while 2*k+1<=o do
    pv:=`dev/invert/powcompose`(p,v,x,2*k+1);
    ppv:=`dev/invert/pprimeknowingp`(pv,v,x,2*k+1);
    v:=v-`dev/invert/powdivide`(pv-x,ppv,x,2*k+1);
    k:=2*k+1
    od;
    if k<o then
    pv:=`dev/invert/powcompose`(p,v,x,o);
    ppv:=`dev/invert/pprimeknowingp`(pv,v,x,o);
    v:=v-`dev/invert/powdivide`(pv-x,ppv,x,o)
    fi;
    `dev/cleanup`([op(1,dev),seq(op([coeff(v,x,k),k]),k=0..o-1),1,o])
end: # `dev/invert/doit`

# pol, pol -> pol
`dev/invert/powcompose`:=proc (Q,P,x,n)
local m, pm, pr, pr1, l, i, s, p, q;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    p:=`dev/invert/powtruncate`(P,x,n);
    q:=`dev/invert/powtruncate`(Q,x,n);
    m:=isqrt(trunc(3.32192809*n/length(n)));
    pm:=`dev/invert/powtruncate`(p,x,m);
    pr:=1;pr1:=p-pm;
    l:=`dev/invert/powcomposesimple`(q,pm,x,n);
    s[0]:=l;
    for i to iquo(n,m)+1 do
    l:=`dev/invert/pprimeknowingp`(l,pm,x,n-i);
    pr:=`dev/invert/powtruncate`(expand(pr*pr1),x,n);
    s[i]:=`dev/invert/powtruncate`(expand(l*pr),x,n)/i!
    od;
    RETURN(convert([seq(s[i],i=0..iquo(n,m)+1)],`+`))
end: # `dev/invert/powcompose`

# pol, pol -> pol
`dev/invert/powcomposesimple`:=proc (q,p,x,n)
local s, j, pk, i;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    j:=degree(q,x);
    s:=1;while s<j do s:=s*2 od; s:=s/2;
    pk[1]:=`dev/invert/powtruncate`(p,x,n);
    i:=2; while i<=s do
    pk[i]:=`dev/invert/powtruncate`(expand(pk[i/2]^2),x,n); i:=2*i od;
    `dev/invert/powcomposesimpledoit`(q,s,pk,x,n)
end: # `dev/invert/powcomposesimple`

# pol, pol -> pol
`dev/invert/powcomposesimpledoit`:=proc (q, s, pk, x, n)
local q1, q2;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if s=1/2 then RETURN(subs(x=pk[1],q)) fi;
    q1:=`dev/invert/powtruncate`(q,x,s-1);
    q2:=normal((q-q1)/x^s);
    `dev/invert/powtruncate`(
    `dev/invert/powcomposesimpledoit`(q1, s/2, pk, x, n)+
    expand(pk[s]*`dev/invert/powcomposesimpledoit`(q2, s/2, pk, x, n-s)),x,n)
end: # `dev/invert/powcomposesimpledoit`

# pol, pol -> pol
`dev/invert/pprimeknowingp`:=proc (poff, f, x, n)
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    `dev/invert/powdivide`(diff(poff,x),diff(f,x),x,n)
end: # `dev/invert/pprimeknowingp`

# pol, pol -> pol
`dev/invert/powdivide`:=proc (p, q, x, n)
local u, bk, i, j;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    # assuming q[0]<>0
    if subs(x=0,q)=0 then ERROR(`not implemented`) fi;
    bk:=[seq(coeff(q,x,i),i=0..n)];
    u[0]:=coeff(p,x,0)/bk[1];
    for i to n do
    u[i]:=(coeff(p,x,i)-convert([seq(u[j]*bk[i-j+1],j=0..i-1)],`+`))/bk[1]
    od;
    convert([seq(u[i]*x^i,i=0..n)],`+`)
end: # `dev/invert/powdivide`

`dev/invert/powtruncate`:=proc (pol, x, n)
local i;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if degree(pol,x)<=n then RETURN(pol) fi;
    if ldegree(pol,x)>n then RETURN(0) fi;
    convert([seq(coeff(pol,x,i)*x^i,i=0..n)],`+`)
end: # `dev/invert/powtruncate`

`dev/invert/powinverse`:=proc (q, x, n)
local u, bk, i, j;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    # assuming q[0]<>0
    if subs(x=0,q)=0 then ERROR(`not implemented`) fi;
    bk:=[seq(coeff(q,x,i),i=0..n)];
    u[0]:=1/bk[1];
    for i to n do u[i]:=(-convert([seq(u[j]*bk[i+1-j],j=0..i-1)],`+`))*u[0] od;
    convert([seq(u[j]*x^j,j=0..n)],`+`)
end: # `dev/invert/powinverse`

#savelib( `dev/invert`, `dev/invert/doit`, `dev/invert/powcompose`,  `dev/invert/powcomposesimple`, `dev/invert/powdivide`, `dev/invert/powinverse`, `dev/invert/powcomposesimpledoit`, `dev/invert/powtruncate`, `dev/invert/pprimeknowingp`);
