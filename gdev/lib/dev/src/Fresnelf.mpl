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

`dev/Fresnelf`:=proc (u,n)
local fact,i,x,example, k,j, init, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	init:=traperror(Fresnelf(u));
	if init=lasterror then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	example:=[Pi,0];
	k:=Pi;
	for i to n-1 do
	    k:=-k*Pi^2/(4*i+1);
	    example:=[op(example),k,i]
	od;
	RETURN(`dev/add`(1/2,`dev/prd`(`dev/pow`(u,3,n-1),
	    `dev/endofdev`(`dev/pow`(u,4,n-1),n-1,example))))
    elif sig=-1 then
	sig:=evalr(Signum(Re(`dev/lcoeff`(u))));
	if sig=-1 or sig=0 then RETURN(undefined)
	elif sig=FAIL then ERROR(FAIL)
	fi;
	example:=[1/Pi,0,-3/Pi^3,1];
	k:=-3/Pi^3;
	for i from 2 to n do 
	    k:=-k*(4*i-1)*(4*i-3)/Pi^2;
	    example:=[op(example),k,i]
	od;
	RETURN(`dev/prd`(`dev/pow`(u,-1,n),
	    `dev/endofdev`(`dev/pow`(u,-4,n),n,example)))
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/Fresnelf`([op(u[2]),0,infinity],n)
	else
	    init:=`dev/Fresnelf`(u[2],n)
	fi;
	if `dev/length`(init)>n and init[nops(init)]<>infinity then
	    RETURN(init)
	else
	    example:=[init,0];
	    j:=Fresnel(x);
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
end: # `dev/Fresnelf`
#savelib(`dev/Fresnelf`);
