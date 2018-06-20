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

`dev/sinh`:=proc (u,n)
local fact, i, example, ifact, init, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then
	RETURN(undefined)
    fi;
    if not type(u,list) then
	RETURN(sinh(u))
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	fact:=`dev/pow`(u,2,n);
	example:=[1,0,1/6,1,1/120,2];
	ifact:=120;
	for i from 4 to n+1 do
	    ifact:=ifact*(i-1)*i;
	    example:=[op(example),1/ifact,i-1]
	od;
	RETURN(`dev/prd`(u,`dev/endofdev`(fact,n,example)))
    elif sig=-1 then
	sig:=evalr(Signum(`dev/lcoeff`(u)));
	if sig=1 then
	    init:=`dev/exp`(u,n);
	    if `dev/length`(init)>n and init[nops(init)]<>infinity then
		RETURN(`dev/multbyreal`(init,1/2))
	    else
		RETURN(`dev/multbyreal`(`dev/add`(init,`dev/multbyreal`(
		    `dev/pow`(init,-1,n),-1)),1/2))
	    fi
	elif sig=-1 then
	    init:=`dev/exp`(`dev/multbyreal`(u,-1),n);
	    if `dev/length`(init)>n and init[nops(init)]<>infinity then
		RETURN(`dev/multbyreal`(init,-1/2))
	    else
		RETURN(`dev/multbyreal`(`dev/add`(`dev/multbyreal`(init,-1),
		    `dev/pow`(init,-1,n)),1/2))
	    fi
	else ERROR(FAIL)
	fi
    elif sig=0 then # this should be made more efficient either by the same
		  # technique as for sin, or by the exponential def, or 
		      # by taylor series.
	RETURN(`dev/add`(`dev/prd`(`dev/sinh`(u[2],n),
	    `dev/cosh`(subsop(2=NULL,3=NULL,u),n)),`dev/prd`(
	    `dev/cosh`(u[2],n),`dev/sinh`(subsop(2=NULL,3=NULL,u),n))))
    else
	ERROR(FAIL)
    fi
end: # `dev/sinh`
#savelib(`dev/sinh`);
