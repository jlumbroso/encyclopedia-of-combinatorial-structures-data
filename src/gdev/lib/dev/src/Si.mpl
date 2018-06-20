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

`dev/Si`:=proc(u,n)
local fact,i,x,example, sinus, cosinus, j, k, init, sig, invu, res, term;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then undefined
    elif not type(u,list) then
	init:=traperror(Si(u));
	if init=lasterror then undefined
	else init
	fi
    else
	sig:=evalr(Signum(u[3]));
	if sig=1 then
	    j:=-1/6;
	    for i from 2 to n do
		j:=-j/(4*i+2)/i;
		example[i]:=j/(2*i+1),i
	    od;
	    `dev/prd`(u,`dev/endofdev`(`dev/pow`(u,2,n),n,
		[1,0,-1/18,1,seq(example[i],i=2..n)]))
	elif sig=-1 then
	    sig:=evalr(Signum(Re(`dev/lcoeff`(u))));
	    if sig=-1 or sig=0 then undefined
	    elif sig=FAIL then ERROR(FAIL)
	    else
		fact:=`dev/pow`(u,-2,n);
		cosinus:=`dev/cossin`(u,n);
		invu:=`dev/pow`(u,-1,n);
		sinus:=`dev/prd`(cosinus[2],invu);
		cosinus:=cosinus[1];
		res:=`dev/add`(Pi/2,`dev/add`(`dev/prd`(`dev/multbyreal`(
		 cosinus,-1),invu),`dev/prd`(`dev/multbyreal`(sinus,-1),invu)));
		for i to iquo(n+1,2)-1 do
		    term:=`dev/prd`(term,fact);
		    res:=`dev/add`(res,`dev/add`(`dev/prd`(`dev/multbyreal`(
			cosinus,(-1)^(i+1)*(2*i)!),term),`dev/prd`(
			`dev/multbyreal`(sinus,(-1)^(i+1)*(2*i+1)!),term)))
		od;
		`dev/reduce`(res,n)
	    fi
	elif sig=0 then
	    if type(u[2],list) then init:=`dev/Si`([op(u[2]),0,infinity],n)
	    else init:=`dev/Si`(u[2],n) fi;
	    if `dev/length`(init)>n and init[nops(init)]<>infinity then init
	    else
		j:=Si(x);
		k:=1;
		for i to n do
		    j:=diff(j,x);
		    k:=k/i;
		    example[i]:=subs(x=u[2],j)*k,i
		od;
		`dev/endofdev`(subsop(2=NULL,3=NULL,u),n,
		    [init,0,seq(example[i],i=1..n)])
	    fi
	else ERROR(FAIL)
	fi
    fi
end:# `dev/Si`
#savelib(`dev/Si`);
