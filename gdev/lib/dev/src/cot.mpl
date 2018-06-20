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

`dev/cot`:=proc(u,n)
local fact,i, example, kfact, init, inter, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if not type(u,list) then
	init:=traperror(cot(u));
	if init=lasterror or has(init,infinity) then RETURN(undefined)
	elif zerotest(init)=true then RETURN(0)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	fact:=`dev/pow`(u,2,n);
	example:=[-1/3,0,-1/45,1,-2/945,2];
	kfact:=-4/45;
	for i from 4 to n do
	    kfact:=-kfact*2/i/(2*i-1);
	    example:=[op(example),kfact*bernoulli(2*i),i-1]
	od;
	RETURN(`dev/add`(`dev/pow`(u,-1,n),`dev/prd`(u,`dev/endofdev`(fact,
	    n-1,example))))
    elif sig=-1 then RETURN(undefined)
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/cot`([op(u[2]),0,infinity],n)
	else
	    init:=`dev/cot`(u[2],n)
	fi;
	if type(init,undefined) then 
	    `dev/pow`(`dev/tan`(u,n),-1,n)
	elif `dev/length`(init)>n and init[nops(init)]<>infinity then 
	    RETURN(init)
	else
	    inter:=`dev/cot`(subsop(2=NULL,3=NULL,u),n);
	    RETURN(`dev/prd`(`dev/add`(`dev/prd`(init,inter),-1),
			`dev/pow`(`dev/add`(init,inter),-1,n)))
	fi
    else ERROR(FAIL)
    fi
end:
#savelib(`dev/cot`);
