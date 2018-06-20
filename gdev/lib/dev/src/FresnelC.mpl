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

`dev/FresnelC`:=proc(u,n)
local fact,i,x,example, sinus, cosinus, k, j, init,invu,newres,res,sig,tomult;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	init:=traperror(FresnelC(u));
	if init=lasterror then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	example:=[1,0,-Pi^2/40,1];
	k:=-Pi^2/8;
	for i from 2 to n do
	    k:=-k*Pi^2/8/i/(2*i-1);
	    example:=[op(example),k/(4*i+1),i]
	od;
	RETURN(`dev/prd`(u,`dev/endofdev`(`dev/pow`(u,4,n),n,example)))
    elif sig=-1 then
	sig:=evalr(Signum(Re(`dev/lcoeff`(u))));
	if sig=-1 or sig=0 then RETURN(undefined)
	elif sig=FAIL then ERROR(FAIL)
	fi;
	cosinus:=`dev/cossin`(`dev/multbyreal`(
	    `dev/pow`(u,2,n),Pi/2),n);
	sinus:=cosinus[2];
	invu:=`dev/pow`(u,-1,n);
	fact:=`dev/pow`(invu,2,n);
	cosinus:=`dev/prd`(cosinus[1],fact);
	fact:=`dev/pow`(fact,2,n);
	res:=0;
	tomult:=1;
	newres:=`dev/add`(1/2,`dev/add`(`dev/multbyreal`(`dev/prd`(sinus,
	    invu),1/Pi),`dev/multbyreal`(`dev/prd`(cosinus,invu),-1/Pi^2)));
	for i to trunc((n+1)/2) while newres<>res do
	    res:=newres;
	    tomult:=`dev/prd`(tomult,fact);
	    newres:=`dev/add`(res,`dev/add`(`dev/multbyreal`(`dev/prd`(sinus,
		tomult),(-1)^i*product(2*'k'-1,'k'=1..2*i)/Pi^(2*i+1)),
		`dev/multbyreal`(`dev/prd`(cosinus,tomult),(-1)^(i+1)*
		product(2*'k'-1,'k'=1..2*i+1)/Pi^(2*i+2))))
	od;
	RETURN(`dev/reduce`(newres,n))
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/FresnelC`([op(u[2]),0,infinity],n)
	else
	    init:=`dev/FresnelC`(u[2],n)
	fi;
	if `dev/length`(init)>n and init[nops(init)]<>infinity then
	    RETURN(init)
	else
	    example:=[init,0];
	    j:=FresnelC(x);
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
end:# `dev/FresnelC`
#savelib(`dev/FresnelC`);
