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

`dev/dilog`:=proc(u,n)
local fact,i,x,example,itsln,j,k, init, newres, res, sig, tomult;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	init:=traperror(dilog(u));
	if init=lasterror then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	`dev/lcoeff`(u);
	fact:=u;
	itsln:=`dev/ln`(u,n);
	tomult:=1;
	res:=0;
	newres:=Pi^2/6;
	for i to n while res<>newres do
	    res:=newres;
	    tomult:=`dev/prd`(tomult,fact);
	    newres:=`dev/add`(res,`dev/prd`(tomult,`dev/add`(-1/i^2,
		`dev/multbyreal`(itsln,1/i))))
	od;
	if `dev/length`(newres)>n and newres[nops(newres)]<>infinity then
	    RETURN(newres)
	elif op(nops(newres),newres)=infinity then
	    RETURN([op(1..nops(newres)-2,newres)])
	else RETURN(newres)
	fi
    elif sig=-1 then
	sig:=evalr(Signum(Re(`dev/lcoeff`(u))));
	if sig=-1 or sig=0 then RETURN(undefined)
	elif sig=FAIL and type(u[2],constant) then ERROR(FAIL) fi;
	fact:=`dev/pow`(u,-1,n);
	itsln:=`dev/ln`(u,n);
	tomult:=1;
	res:=0;
	newres:=`dev/add`(-Pi^2/6,`dev/multbyreal`(
	    `dev/pow`(itsln,2,n),-1/2));
	for i to n while res<>newres do
	    res:=newres;
	    tomult:=`dev/prd`(tomult,fact);
	    newres:=`dev/add`(res,`dev/prd`(tomult,`dev/add`(1/i^2,
		`dev/multbyreal`(itsln,1/i))))
	od;
	if `dev/length`(newres)>n and newres[nops(newres)]<>infinity then
	    RETURN(newres)
	elif newres[nops(newres)]=infinity then
	    RETURN([op(1..nops(newres)-2,newres)])
	else RETURN(newres)
	fi;
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/dilog`([op(u[2]),0,infinity],n)
	else
	    init:=`dev/dilog`(u[2],n)
	fi;
	if `dev/length`(init)>n and init[nops(init)]<>infinity then
	    RETURN(init)
	else
	    example:=[init,0];
	    j:=dilog(x);
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
end:# `dev/dilog`
#savelib(`dev/dilog`);
