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

`dev/arccsc`:=proc(u,n)
local fact, i, example, kfact, init, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	init:=traperror(arccsc(u));
	if init=lasterror or has(init,infinity) then
	    RETURN(undefined)
	else
	    RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=-1 then
	fact:=`dev/pow`(u,-2,n);
	example:=[1,0,1/6,1];
	kfact:=1/2;
	for i from 2 to n do
	    kfact:=kfact*(2*i-1)/2/i;
	    example:=[op(example),kfact/(2*i+1),i]
	od;
	RETURN(`dev/prd`(`dev/pow`(u,-1,n),`dev/endofdev`(fact,n,example)))
    elif sig=1 then RETURN(undefined)
    elif sig=0 then
	RETURN(`dev/arcsin`(`dev/pow`(u,-1,n),n))
    else ERROR(FAIL)
    fi
end: # `dev/arccsc`
#savelib(`dev/arccsc`);
