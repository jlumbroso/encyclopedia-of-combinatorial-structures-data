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

`dev/Zeta`:=proc (u,n)
local sig, res, i, oldres, init, example;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if nargs=3 then ERROR(`not implemented yet`) fi;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	if zerotest(u,1)=true then RETURN(undefined)
	elif zerotest(Zeta(u))=true then RETURN(0)
	else RETURN(Zeta(u)) fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	RETURN(`dev/endofdev`(u,n,
	    [Zeta(0),0,seq(op([Zeta(i,0)/i!,i]),i=1..n)]))
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/Zeta`([op(u[2]),0,infinity],n)
	else
	    init:=`dev/Zeta`(u[2],n)
	fi;
	if `dev/length`(init)>n and init[nops(init)]<>infinity then 
	    RETURN(init) fi;
	if not type(init,undefined) then
	    if init=0 then
		example:=[seq(op([Zeta(i,u[2])/i!,i]),i=1..n+1)]
	    else
		example:=[init,0,seq(op([Zeta(i,u[2])/i!,i]),i=1..n)]
	    fi;
	    RETURN(`dev/endofdev`(subsop(2=NULL,3=NULL,u),n,example))
	else
	    example:=[gamma,0,seq(op([(-1)^i*gamma(i)/i!,i]),
		i=1..max(1,n-2))];
	    RETURN(`dev/add`(`dev/pow`(subsop(2=NULL,3=NULL,u),-1,n),
		`dev/endofdev`(subsop(2=NULL,3=NULL,u),n-1,example)))
	fi
    elif sig=-1 then
	sig:=evalr(Signum(Re(`dev/lcoeff`(u))));
	if sig=0 then ERROR(FAIL)
	elif sig=1 then
	    oldres:=0;
	    res:=1;
	    for i from 2 to n+1 while res<>oldres do
		oldres:=res;
		res:=`dev/add`(res,`dev/exp`(
		    `dev/multbyreal`(u,-ln(i)),n))
	    od;
	    if res[nops(res)]=infinity then
		res:=[op(1..nops(res)-2,res)]
	    fi;
	    RETURN(res)
	elif sig=-1 then
	    res:=`dev/add`(1,`dev/multbyreal`(u,-1));
	    RETURN(`dev/prd`(`dev/multbyreal`(`dev/prd`(`dev/exp`(
		`dev/multbyreal`(u,ln(2*Pi)),n),`dev/prd`(
		`dev/GAMMA`(res,n),`dev/Zeta`(res,n))),1/Pi),
		`dev/sin`(`dev/multbyreal`(u,Pi/2),n)))
	else ERROR(FAIL)
	fi
    else ERROR(FAIL)
    fi
end: # `dev/Zeta`

#savelib(`dev/Zeta`);
