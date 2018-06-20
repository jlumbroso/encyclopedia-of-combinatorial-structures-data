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


`dev/Ci`:=proc(u,n)
local fact,i,x,example, sinus, cosinus, j, k, init, sig, res, term, newres, 
invu, tomult;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	init:=traperror(Ci(u));
	if init=lasterror then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	fact:=`dev/pow`(u,2,n);
	term:=1;
	res:=0;
	newres:=`dev/add`(gamma,`dev/ln`(u,n));
	for i to n-1 while (res<>newres) do
	    res:=newres;
	    term:=`dev/prd`(term,fact);
	    newres:=`dev/add`(newres,
		`dev/multbyreal`(term,(-1)^i/(2*i)!/(2*i)))
	od;
	RETURN(`dev/reduce`(newres,n))
    elif sig=-1 then
	sig:=evalr(Signum(Re(`dev/lcoeff`(u))));
	if sig=-1 or sig=0 then RETURN(undefined)
	elif sig=FAIL then ERROR(FAIL)
	fi;
	invu:=`dev/pow`(u,-1,n);
	fact:=`dev/pow`(invu,2,n);
	sinus:=`dev/cossin`(u,n);
	cosinus:=`dev/prd`(sinus[1],invu);
	sinus:=sinus[2];
	res:=`dev/add`(`dev/prd`(sinus,fact),`dev/prd`(fact,
	    `dev/multbyreal`(cosinus,-1)));
	tomult:=1;
	for i to trunc((n+1)/2) do
	    tomult:=`dev/prd`(tomult,fact);
	    res:=`dev/add`(res,`dev/add`(`dev/prd`(`dev/multbyreal`(sinus,
		(-1)^i*(2*i)!),tomult),`dev/prd`(`dev/multbyreal`(cosinus,
		(-1)^(i+1)*(2*i+1)!),tomult)))
	od;
	RETURN(`dev/reduce`(res,n))
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/Ci`([op(u[2]),0,infinity],n)
	else
	    init:=`dev/Ci`(u[2],n)
	fi;
	if `dev/length`(init)>n and init[nops(init)]<>infinity then
	    RETURN(init)
	else
	    example:=[init,0];
	    j:=Ci(x);
	    k:=1;
	    for i to n do
		j:=diff(j,x);
		k:=k/i;
		example:=[op(example),subs(x=u[2],j)*k,i]
	    od;
	    RETURN(`dev/endofdev`(fact,n,example))
	fi
    else ERROR(FAIL)
    fi
end:# `dev/Ci`
#savelib(`dev/Ci`);
