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

`dev/sech`:=proc (u,n)
local fact, i, example, ifact, init, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then
	RETURN(undefined)
    fi;
    if not type(u,list) then RETURN(sech(u))
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	fact:=`dev/pow`(u,2,n);
	example:=[1,0,-1/2,1,5/24,2,-61/720,3];
	ifact:=720;
	for i from 4 to n do
	    ifact:=ifact*2*i*(2*i-1);
	    example:=[op(example),euler(2*i)/ifact,i]
	od;
	RETURN(`dev/endofdev`(fact,n,example))
    elif sig=-1 then
	sig:=evalr(Signum(`dev/lcoeff`(u)));
	if sig=1 then
	    init:=`dev/exp`(`dev/multbyreal`(u,-1),n);
	    if `dev/length`(init)>n and init[nops(init)]<>infinity then
		RETURN(`dev/multbyreal`(init,2))
	    else
		RETURN(`dev/multbyreal`(`dev/prd`(init,`dev/endofdev`(`dev/pow`(
		    init,2,n),n,seq(op([1,i]),i=0..n))),2))
	    fi
	else
	    init:=`dev/exp`(u,n);
	    if `dev/length`(init)>n and init[nops(init)]<>infinity then
		RETURN(`dev/multbyreal`(init,-2))
	    else
		RETURN(`dev/multbyreal`(`dev/prd`(init,`dev/endofdev`(`dev/pow`(
		    init,2,n),n,seq(op([1,i]),i=0..n))),-2))
	    fi
	fi
    elif sig=0 then
	`dev/pow`(`dev/cosh`(u,n),-1,n)
    else
	ERROR(FAIL)
    fi;
end: # `dev/sech`
#savelib(`dev/sech`);
