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

`dev/sec`:=proc (u,n)
local fact,i, example, kfact, init, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	init:=traperror(sec(u));
	if init=lasterror or has(init,infinity) then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	fact:=`dev/pow`(u,2,n);
	example:=[1,0,1/2,1,5/24,2,61/720,3];
	kfact:=-720;
	for i from 4 to n do
	    kfact:=-kfact*2*i*(2*i-1);
	    example:=[op(example),euler(2*i)/kfact,i]
	od;
	RETURN(`dev/endofdev`(fact,n,example))
    elif sig=-1 then RETURN(undefined)
    elif sig=0 then
	RETURN(`dev/pow`(`dev/cossin`(u,n)[1],-1,n))
    else ERROR(FAIL)
    fi
end: # `dev/sec`
#savelib(`dev/sec`);
