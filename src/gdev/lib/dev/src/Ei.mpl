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



`dev/Ei`:=proc(u,n)
local fact,i,x, j, example, k, init, sig, tomult, res, newres;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	init:=traperror(Ei(u));
	if init=lasterror then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	sig:=evalr(Signum(Re(`dev/lcoeff`(u))));
	if sig=-1 then
	    RETURN(`dev/multbyreal`(`dev/Ei`(`dev/multbyreal`(u,-1),n),-1))
	elif sig=0 then RETURN(undefined)
	elif sig=FAIL then ERROR(FAIL)
	fi;
	fact:=u;
	tomult:=1;
	res:=0;
	newres:=`dev/add`(`dev/ln`(u,n),gamma);
	for i to n-1 while(res<>newres) do
	    res:=newres;
	    tomult:=`dev/prd`(tomult,fact);
	    newres:=`dev/add`(newres,`dev/multbyreal`(tomult,1/i/i!))
	od;
	RETURN(`dev/reduce`(newres,n))
    elif sig=-1 then
	fact:=`dev/pow`(u,-1,n);
	example:=[1,1,1,2,2,3,6,4,24,5,120,6];
	j:=120;
	for i from 6 to n do
	    j:=j*i;
	    example:=[op(example),j,i+1]
	od;
	RETURN(`dev/prd`(`dev/exp`(u,n),`dev/endofdev`(fact,n,example)))
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/Ei`([op(u[2]),0,infinity],n)
	else
	    init:=`dev/Ei`(u[2],n)
	fi;
	if `dev/length`(init)>n and init[nops(init)]<>infinity then
	    RETURN(init)
	else
	    example:=[init,0];
	    j:=Ei(x);
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
end:# `dev/Ei`
#savelib(`dev/Ei`);
