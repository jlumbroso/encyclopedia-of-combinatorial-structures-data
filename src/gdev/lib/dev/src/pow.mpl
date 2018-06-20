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
##  Title:	`dev/pow`
##  Created:	Apr-Aug 1988
##  Author:	Bruno Salvy
##
##  Modified:	  Nov 1988
##  Modification: Listinf added. At most one level can introduce Listinf.
##
##  Modified:	  Mon Feb 27 19:48:58 1989
##  Modification: better care for algnum
##
##
##   Modified:    Tue Jul 12 11:30:41 1994
##   Modification: Listinf suppressed
##


`dev/pow`:=proc(u,p,n)
local expr, i, x, res1, res, ind, inter, binpow, q, lvar, roots;
global _EnvAllSolutions;
option remember,`Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then undefined
    elif not type(u,list) then
	if type(u,algnum) and has(u,RootOf) and type(p,integer) then
	    evala(Normal(u^p))
	elif has(u,RootOf) then u^p
	elif type(u,constant) or p>=0 or is(p>=0) then u^p
	else
	    # is u bounded away from 0 ?
	    ind:=indets(u,name) minus {constants};
	    lvar:=ind minus indets(u,_Xasytab[anything]);
	    if nops(lvar)=0 then res1:=evalc(abs(u)) fi;
	    for i in indets(res1,{specfunc(anything,sin),
		specfunc(anything,cos)}) do
		    res1:=subs(i='INTERVAL'(-1..1),res1)
	    od;
	    res1:=traperror(evalr(res1));
	    if res1<>lasterror and 
		type(res1,list) and evalr(Signum(op(1,op(res1))))=1 then u^p
	    elif _EnvXasyinteger=true and 
		indets(ind,name) minus {constants} = {_Xasytab[1]} then
#		expr:=expand(subs(_Xasytab[1]=1/x,u));
		expr:=(subs(_Xasytab[1]=1/x,u));
		_EnvAllSolutions:=true;
		roots:=[solve(expr,x)];
		if type(roots,set(name)) and is(op(roots),integer) then
		    undefined
		else u^p
		fi
	    elif res1=lasterror then ERROR(res1)
	    else u^p
	    fi
	fi
    elif nops(u)=3 or u[5]=infinity then
	if type(u[2],list) then
	    inter:=`dev/pow`([op(u[2]),0,infinity],p,n);
	    if inter[nops(inter)]<>infinity then [u[1],inter,p*u[3]]
	    elif nops(u)=3 then [u[1],[op(1..nops(inter)-2,inter)],p*u[3]]
	    else [u[1],[op(1..nops(inter)-2,inter)],p*u[3],0,infinity]
	    fi
	elif nops(u)=3 then [u[1],`dev/pow`(u[2],p,n),p*u[3]]
	else [u[1],`dev/pow`(u[2],p,n),p*u[3],0,infinity] fi
    elif type(p,integer) and p<>-1 then
	if p>0 then
	    q:=p;
	    res:=1;
	    binpow:=u;
	    while q>1 do
		if irem(q,2)=1 then res:=`dev/prd`(res,binpow) fi;
		binpow:=`dev/reduce`(`dev/prd`(binpow,binpow),n);
		q:=iquo(q,2)
	    od;
	    `dev/prd`(res,binpow)
	elif p=0 then 1
	else `dev/pow`(`dev/pow`(u,-p,n),-1,n)
	fi
    else
	`dev/prd`(`dev/pow`([u[1],u[2],u[3],0,infinity],p,n),
	    `dev/pow/endofdev`(`dev/prd`(
		`dev/pow`([u[1],u[2],u[3],0,infinity],-1,n),
		[u[1],op(4..nops(u),u)],n),p,n),n)
    fi
end:

`dev/pow/endofdev`:=proc (dev,p,n)
local expr, j, i;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(dev,undefined) then undefined
    elif dev=0 then 1
    else
	j:=p*(p-1)/2;
	for i from 3 to n do
	    j:=j*(p-i+1)/i;
	    expr[i]:=j,i
	od;
	`dev/endofdev`(dev,n,[1,0,p,1,p*(p-1)/2,2,seq(expr[i],i=3..n)])
    fi
end: # `dev/pow/endofdev`

#savelib( `dev/pow`,`dev/pow/endofdev`);
