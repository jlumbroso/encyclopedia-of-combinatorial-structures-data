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

`dev/erf`:=proc(u,n)
local fact,i,x,example,side, k,ck,j, coef, init, res, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	RETURN(erf(u))
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	RETURN(`dev/prd`(u,`dev/endofdev`(`dev/pow`(u,2,n)
	    ,n,[seq(op([(-1)^i/i!/(2*i+1)*2/sqrt(Pi),i]),i=0..n+1)])))
    elif sig=-1 then
	coef:=evalc(`dev/lcoeff`(u));
	if coeff(coef,I,1)<>0 then ERROR(FAIL) fi;
	side:=evalr(Signum(coef));
	if side<>1 and side<>-1 then ERROR(FAIL) fi;
	fact:=`dev/pow`(u,-2,n);
	example:=[1,0,-1/2,1];
	ck:=-1/2;
	for i from 2 to n do
	    ck:=-ck*(2*i-1)/2;
	    example:=[op(example),ck,i]
	od;
	res:=`dev/prd`(`dev/pow`(u,-1,n),`dev/endofdev`(fact,n,example));
	if side=1 then
	    res:=`dev/add`(1,`dev/multbyreal`(`dev/prd`(`dev/exp`(
	    `dev/multbyreal`(`dev/pow`(u,2,n),-1),n),res),-1/Pi^(1/2)))
	else
	    res:=`dev/add`(-1,`dev/multbyreal`(`dev/prd`(`dev/exp`(
	    `dev/multbyreal`(`dev/pow`(u,2,n),-1),n),res),Pi^(-1/2)));
	fi;
	if `dev/length`(res)>n and res[nops(res)]<>infinity then 
	    RETURN(res)
	elif op(nops(res),res)=infinity then
	    RETURN([op(1..nops(res)-2,res)])
	else RETURN(res)
	fi;
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/erf`([op(u[2]),0,infinity],n)
	else
	    init:=`dev/erf`(u[2],n)
	fi;
	if `dev/length`(init)>n and init[nops(init)]<>infinity then 
	    RETURN(init)
	else
	    example:=[init,0];
	    j:=erf(x);
	    k:=1;
	    for i to n do
		j:=diff(j,x);
		k:=k/i;
		example:=[op(example),subs(x=u[2],j)*k,i]
	    od;
	    fact:=subsop(2=NULL,3=NULL,u);
	    RETURN(`dev/endofdev`(fact,n,example))
	fi
    else ERROR(FAIL)
    fi
end:# `dev/erf`
#savelib(`dev/erf`);
