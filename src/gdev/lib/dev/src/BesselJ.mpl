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

`dev/BesselJ`:=proc(nu,u,n)
local fact,i,x,example, sinus, cosinus, k,j, init, invu, newres,res,sig,tomult;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if type(nu,list) then ERROR(`Not implemented`)
    fi;
    if not type(u,list) then
	init:=traperror(BesselJ(nu,u));
	if init=lasterror then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	j:=1/GAMMA(nu)/nu;
	example:=[j,0];
	for i to n do
	    j:=-j/4/i/(nu+i);
	    example:=[op(example),j,i]
	od;
	RETURN(`dev/multbyreal`(`dev/prd`(`dev/pow`(u,nu,n),
			`dev/endofdev`(`dev/pow`(u,2,n),n,example)),1/2^nu))
    elif sig=-1 then
	sig:=evalr(Signum(Re(`dev/lcoeff`(u))));
	if sig=-1 then RETURN(undefined)
	elif sig=FAIL then ERROR(FAIL) fi;
	sinus:=`dev/cossin`(`dev/add`(-nu*Pi/2-Pi/4,u),n);
	cosinus:=sinus[1];
	invu:=`dev/pow`(u,-1,n);
	sinus:=`dev/prd`(sinus[2],invu);
	fact:=`dev/pow`(invu,2,n);
	tomult:=1;
	res:=0;
	newres:=`dev/add`(cosinus,`dev/multbyreal`(sinus,-(nu^2-1/4)/2));
	for i to trunc(n/2)+1 while (res<>newres) do
	    res:=newres;
	    tomult:=`dev/prd`(tomult,fact);
	    newres:=`dev/add`(res,`dev/add`(`dev/multbyreal`(`dev/prd`(cosinus,
		tomult),(-1)^i*product(4*nu^2-(2*'k'-1)^2,'k'=1..2*i)/8^(2*i)/
		(2*i)!),`dev/multbyreal`(`dev/prd`(sinus,tomult),(-1)^(i+1)*
		product(4*nu^2-(2*'k'-1)^2,'k'=1..2*i+1)/8^(2*i+1)/(2*i+1)!)
		))
	od;
	res:=`dev/multbyreal`(`dev/prd`(newres,`dev/pow`(invu,1/2,n)),
	    (2/Pi)^(1/2));
	if `dev/length`(res)>n and res[nops(res)]<>infinity then
	    RETURN(res)
	elif res[nops(res)]=infinity then
	    RETURN([op(1..nops(res)-2,res)])
	else RETURN(res)
	fi;
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/BesselJ`(nu,[op(u[2]),0,infinity],n)
	else
	    init:=`dev/BesselJ`(nu,u[2],n)
	fi;
	if `dev/length`(init)>n and init[nops(init)]<>infinity then 
	    RETURN(init)
	else
	    example:=[init,0];
	    j:=BesselJ(nu,x);
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
end:# `dev/BesselJ`
#savelib(`dev/BesselJ`);
