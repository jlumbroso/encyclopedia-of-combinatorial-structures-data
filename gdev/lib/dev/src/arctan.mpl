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

`dev/arctan`:=proc(u,n)
local fact,i,x, example, invu, init, j, k, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	RETURN(arctan(u))
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	fact:=`dev/pow`(u,2,n);
	RETURN(`dev/prd`(u,`dev/endofdev`(fact,n,[seq(op([(-1)^i/(2*i+1),i]),
	    i=0..n+1)])))
    elif sig=-1 then
	sig:=`dev/sign`(u[2]);
	if sig=FAIL then ERROR(FAIL)
	elif sig=1 then
	    fact:=`dev/pow`(u,-2,n-1);
	    RETURN(`dev/add`(Pi/2,`dev/prd`(`dev/pow`(u,-1,n-1),`dev/endofdev`(
		fact,n-1,[seq(op([(-1)^(i+1)/(2*i+1),i]),i=0..n-1)]))))
	elif sig=-1 then
	    invu:=`dev/pow`(u,-1,n-1);
	    fact:=`dev/pow`(invu,2,n-1);
	    RETURN(`dev/add`(-Pi/2,`dev/prd`(invu,`dev/endofdev`(
		fact,n,[seq(op([(-1)^i/(2*i+1),i]),i=0..n)]))))
	else ERROR(FAIL)
	fi;
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/arctan`([op(u[2]),0,infinity],n)
	else
	    init:=`dev/arctan`(u[2],n)
	fi;
	if `dev/length`(init)>n and init[nops(init)]<>infinity then 
	    RETURN(init)
	else
	    example:=[init,0];
	    j:=arctan(x);
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
end:
#savelib(`dev/arctan`);
