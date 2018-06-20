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

`dev/csch`:=proc (u,n)
local i, example, expon, ifact, init, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then
	RETURN(undefined)
    fi;
    if not type(u,list) then
	init:=traperror(csch(u));
	if init=lasterror then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	example:=[-1/6,0,7/360,1,-31/15120,2];
	ifact:=720;
	expon:=32;
	for i from 3 to n-1 do  
	    ifact:=ifact*2*(i+1)*(2*i+1);
	    expon:=expon*4;
	    example:=[op(example),2*(expon-1)/ifact*bernoulli(2*i+2),i]
	od;
	RETURN(`dev/add`(`dev/pow`(u,-1,n),`dev/prd`(u,`dev/endofdev`(
	    `dev/pow`(u,2,n-1),n-1,example))))
    elif sig=-1 then
	sig:=evalr(Signum(`dev/lcoeff`(u)));
	if sig=1 then
	    init:=`dev/exp`(`dev/multbyreal`(u,-1),n);
	    if `dev/length`(init)>n and init[nops(init)]<>infinity then
		RETURN(`dev/multbyreal`(init,2))
	    else
		RETURN(`dev/multbyreal`(`dev/prd`(init,`dev/endofdev`(`dev/pow`(
		    init,2,n),n,[seq(op([1,i]),i=0..n)])),2))
	    fi
	else
	    init:=`dev/exp`(u,n);
	    if `dev/length`(init)>n and init[nops(init)]<>infinity then
		RETURN(`dev/multbyreal`(init,-2))
	    else
		RETURN(`dev/multbyreal`(`dev/prd`(init,`dev/endofdev`(`dev/pow`(
		    init,2,n),n,[seq(op([1,i]),i=0..n)])),-2))
	    fi
	fi;
    elif sig=0 then
	RETURN(`dev/pow`(`dev/sinh`(u,n),-1,n))
    else
	ERROR(FAIL)
    fi;
end: # `dev/csch`
#savelib(`dev/csch`);
