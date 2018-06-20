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

`dev/tan`:=proc(u,n)
local fact, i, example, expon, kfact, inter, init, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	init:=traperror(tan(u));
	if init=lasterror or has(init,infinity) then RETURN(undefined)
	elif zerotest(init)=true then RETURN(0)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	fact:=`dev/pow`(u,2,n);
	example:=[1,0,1/3,1,2/15,2,17/315,3];
	expon:=256;
	kfact:=-40320;
	for i from 5 to n+1 do
	    expon:=4*expon;
	    kfact:=-kfact*2*i*(2*i-1);
	    example:=[op(example),expon*(expon-1)/kfact*bernoulli(2*i),i-1]
	od;
	RETURN(`dev/prd`(u,`dev/endofdev`(fact,n,example)))
    elif sig=-1 then RETURN(undefined)
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/tan`([op(u[2]),0,infinity],n)
	else
	    init:=`dev/tan`(u[2],n)
	fi;
	if type(init,undefined) then
	    `dev/pow`(`dev/cot`(u,n),-1,n)
	elif `dev/length`(init)>n and init[nops(init)]<>infinity then
	    RETURN(init)
	else
	    inter:=`dev/tan`(subsop(2=NULL,3=NULL,u),n);
	    RETURN(`dev/prd`(`dev/add`(inter,init),`dev/pow`(`dev/add`(1,
			`dev/prd`(inter,`dev/multbyreal`(init,-1))),-1,n)))
	fi
    else ERROR(FAIL)
    fi
end:
#savelib(`dev/tan`);
