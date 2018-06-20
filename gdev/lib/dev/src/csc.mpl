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

`dev/csc`:=proc(u,n)
local fact,i, init, newres, res, tomult, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if not type(u,list) then
	init:=traperror(csc(u));
	if init=lasterror or has(init,infinity) then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	fact:=`dev/pow`(u,2,n);
	tomult:=u;
	res:=0;
	newres:=`dev/add`(`dev/pow`(u,-1,n),`dev/multbyreal`(u,1/6));
	for i from 2 to n+1 while newres<>res do	
	    res:=newres;
	    tomult:=`dev/prd`(tomult,fact);
	    newres:=`dev/add`(res,`dev/multbyreal`(tomult,
		(-1)^(i-1)*2*(2^(2*i-1)-1)*bernoulli(2*i)/(2*i)!))
	od;
	RETURN(`dev/reduce`(newres,n))
    elif sig=-1 then RETURN(undefined)
    elif sig=0 then
	RETURN(`dev/pow`(`dev/cossin`(u,n)[2],-1,n))
    else ERROR(FAIL)
    fi
end:
#savelib(`dev/csc`);
