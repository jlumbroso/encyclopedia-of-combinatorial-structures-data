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

`dev/arccosh`:=proc (u,n)
local fact, i, example, x, j, k, coef, init, newres, res, tomult, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then
	RETURN(undefined)
    fi;
    if not type(u,list) then
	init:=traperror(arccosh(u));
	if init=lasterror then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	RETURN(undefined)
    elif sig=-1 then
	coef:=coeff(`dev/lcoeff`(u),I,0);
	if type(coef,constant) then
	    sig:=evalr(Signum(coef));
	    if sig=-1 then RETURN(undefined)
	    elif sig=FAIL then ERROR(FAIL)
	    fi
	else
	    # assume the user wants +infinity
	fi;
	fact:=`dev/pow`(u,-2,n);
	res:=0;
	tomult:=1;
	newres:=`dev/ln`(`dev/multbyreal`(u,2),n);
	for i to n while res<>newres do
	    res:=newres;
	    tomult:=`dev/prd`(tomult,fact);
	    newres:=`dev/add`(res,`dev/multbyreal`(tomult,-1*product(2*'k'-1,
		'k'=1..i)/product(2*'k','k'=1..i)/2/i))
	od;
	RETURN(newres)
    elif sig=0 then
	init:=`dev/arccosh`(u[2],n);
	if `dev/length`(init)>n and init[nops(init)]<>infinity then
	    RETURN(init)
	else
	    example:=[init,0];
	    j:=arccosh(x);
	    k:=1;
	    for i to n do
		j:=diff(j,x);
		k:=k/i;
		example:=[op(example),subs(x=u[2],j)*k,i]
	    od;
	    fact:=subsop(2=NULL,3=NULL,u);
	    RETURN(`dev/endofdev`(fact,n,example))
	fi
    else
	ERROR(FAIL)
    fi;
end: # `dev/arccosh`
#savelib(`dev/arccosh`);
