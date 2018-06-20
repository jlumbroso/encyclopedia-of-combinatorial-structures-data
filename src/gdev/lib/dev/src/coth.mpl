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

`dev/coth`:=proc (u,n)
local fact, i, example, ifact, init, inter, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then
	RETURN(undefined)
    fi;
    if not type(u,list) then
	init:=traperror(coth(u[1]));
	if init=lasterror then 
	    RETURN(undefined)
	else
	    RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	fact:=`dev/pow`(u,2,n);
	example:=[1/3,0,-1/45,1,2/945,2];
	ifact:=4/45;
	for i from 3 to n-1 do
	    ifact:=ifact*2/(2*i+1)/(i+1);
	    example:=[op(example),ifact*bernoulli(2*i+2),i]
	od;
	RETURN(`dev/add`(`dev/pow`(u,-1,n),`dev/prd`(u,`dev/endofdev`(fact,
	    n-1,example))))
    elif sig=-1 then
	sig:=evalr(Signum(`dev/lcoeff`(u)));
	if sig=1 then
	    RETURN(`dev/endofdev`(`dev/exp`(`dev/multbyreal`(u,-1),n),n,
		[1,0,seq(op([2,i]),i=1..n)]))
	else
	    RETURN(`dev/endofdev`(`dev/exp`(u,n),n,[-1,0,seq(op([-2,i]),i=1..n)
	    ]))
	fi
    elif sig=0 then # same comment as above
	init:=`dev/coth`(u[2],n);
	inter:=`dev/coth`(subsop(2=NULL,3=NULL,u),n);
	RETURN(`dev/prd`(`dev/add`(1,`dev/prd`(init,inter)),
	    `dev/pow`(`dev/add`(init,inter),-1,n)))
    else
	ERROR(FAIL)
    fi;
end: # `dev/coth`
#savelib(`dev/coth`);
