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

`dev/FresnelS`:=proc(u,n)
local fact,i,x,example, cosinus, sinus, k,j,init,sig,invu, newres, res, tomult;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	init:=traperror(FresnelS(u));
	if init=lasterror then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	example:=[Pi/6,0];
	k:=Pi/2;
	for i to n do
	    k:=-k*Pi^2/8/i/(2*i+1);
	    example:=[op(example),k/(4*i+3),i]
	od;
	RETURN(`dev/prd`(`dev/pow`(u,3,n),
	    `dev/endofdev`(`dev/pow`(u,4,n),n,example)))
    elif sig=-1 then
	sig:=evalr(Signum(Re(`dev/lcoeff`(u))));
	if sig=-1 or sig=0 then RETURN(undefined)
	elif sig=FAIL then ERROR(FAIL)
	fi;
	sinus:=`dev/cossin`(`dev/multbyreal`(
	    `dev/pow`(u,2,n),Pi/2),n);
	cosinus:=sinus[1];
	invu:=`dev/pow`(u,-1,n);
	fact:=`dev/pow`(invu,2,n);
	sinus:=`dev/prd`(sinus[2],fact);
	fact:=`dev/pow`(fact,2,n);
	tomult:=1;
	res:=0;
	newres:=`dev/add`(1/2,`dev/add`(`dev/multbyreal`(`dev/prd`(cosinus,
	    invu),-1/Pi),`dev/multbyreal`(`dev/prd`(sinus,invu),-1/Pi^2)));
	for i to trunc((n+1)/2) while res<>newres do
	    res:=newres;
	    tomult:=`dev/prd`(tomult,fact);
	    newres:=`dev/add`(res,`dev/add`(`dev/multbyreal`(`dev/prd`(
		cosinus,tomult),(-1)^(i+1)*product(2*'k'-1,'k'=1..2*i)/
		Pi^(2*i+1)),`dev/multbyreal`(`dev/prd`(sinus,tomult),(-1)^(i+1)*
		product(2*'k'-1,'k'=1..2*i+1)/Pi^(2*i+2))))
	od;
	RETURN(`dev/reduce`(newres,n))
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/FresnelS`([op(u[2]),0,infinity],n)
	else
	    init:=`dev/FresnelS`(u[2],n)
	fi;
	if `dev/length`(init)>n and init[nops(init)]<>infinity then 
	    RETURN(init)
	else
	    example:=[init,0];
	    j:=FresnelS(x);
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
end:# `dev/FresnelS`
#savelib(`dev/FresnelS`);
