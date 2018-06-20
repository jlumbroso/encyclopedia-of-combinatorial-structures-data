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

`dev/tanh`:=proc (u,n)
local fact, i, ifact, expon, example, init, inter, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then
	RETURN(undefined)
    fi;
    if not type(u,list) then RETURN(tanh(u))
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	fact:=`dev/pow`(u,2,n);
	example:=[1,0,-1/3,1,2/15,2,-17/315,3];
	ifact:=40320;
	expon:=256;
	for i from 4 to n do
	    ifact:=ifact*(2*i+1)*2*(i+1);
	    expon:=4*expon;
	    example:=[op(example),expon*(expon-1)*bernoulli(2*i+2)/ifact,i]
	od;
	RETURN(`dev/prd`(u,`dev/endofdev`(fact,n+1,example)))
    elif sig=-1 then
	sig:=evalr(Signum(`dev/lcoeff`(u)));
	if sig=1 then
	    RETURN(`dev/endofdev`(`dev/exp`(`dev/prd`(u,-2),n),n,
		[1,0,seq(op([-2,2*i-1,2,2*i]),i=1..iquo(n,2)+1)]))
	else
	    RETURN(`dev/endofdev`(`dev/exp`(`dev/prd`(u,2),n),n,
		[-1,0,seq(op([2,2*i-1,-2,2*i]),i=1..iquo(n,2)+1)]))
	fi
    elif sig=0 then # should be made more efficient.
	init:=`dev/tanh`(u[2],n); 
	inter:=`dev/tanh`(subsop(2=NULL,3=NULL,u),n);
	RETURN(`dev/prd`(`dev/add`(inter,init),`dev/pow`(`dev/add`(1,
	    `dev/prd`(inter,init)),-1,n)))
    else
	ERROR(FAIL)
    fi;
end: # `dev/tanh`
#savelib(`dev/tanh`);
