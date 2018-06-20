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

`dev/arcsin`:=proc(u,n)
local fact,i,k,x,example,j,kfact, init, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	RETURN(arcsin(u))
    fi;
    sig:=evalr(Signum(u[3]));;
    if sig=1 then
	fact:=`dev/pow`(u,2,n);
	example:=[1,0,1/6,1,3/40,2];
	kfact:=3/8;
	for i from 3 to n do
	    kfact:=kfact*(2*i-1)/2/i;
	    example:=[op(example),kfact/(2*i+1),i]
	od;
	RETURN(`dev/prd`(u,`dev/endofdev`(fact,n,example)))
    elif sig=-1 then RETURN(undefined)
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/arcsin`([op(u[2]),0,infinity],n)
	else
	    init:=`dev/arcsin`(u[2],n)
	fi;
	if `dev/length`(init)>n and init[nops(init)]<>infinity then
	    RETURN(init) fi;
	if not type(u[2],realcons) then ERROR(FAIL) fi;
	sig:=evalr(Signum(u[2]-1));
	if sig=1 then RETURN(undefined)
	elif sig=0 then
	    fact:=subsop(2=NULL,3=NULL,u);
	    example:=[1,0,-1/12,1];
	    kfact:=-1/4;
	    for i from 2 to n-1 do	
		kfact:=-kfact*(2*i-1)/4/i;
		example:=[op(example),kfact/(2*i+1),i]
	    od;
	    RETURN(`dev/add`(Pi/2,`dev/multbyreal`(`dev/prd`(`dev/pow`(
		`dev/multbyreal`(fact,-1),1/2,n-1),
		`dev/endofdev`(fact,n-1,example)),-2^(1/2))))
	elif sig=-1 then
	    sig:=evalr(Signum(-1-u[2]));
	    if sig=1 then RETURN(undefined)
	    elif sig=0 then
		fact:=subsop(2=NULL,3=NULL,u);
		example:=[1,0,1/12,1];
		kfact:=1/4;
		for i from 2 to n-1 do  
		    kfact:=kfact*(2*i-1)/4/i;
		    example:=[op(example),kfact/(2*i+1),i]
		od;
		RETURN(`dev/add`(-Pi/2,`dev/multbyreal`(`dev/prd`(`dev/pow`(
		    fact,1/2,n-1),`dev/endofdev`(fact,n-1,example)),
		    -2^(1/2))))
	    elif sig=-1 then
		init:=`dev/arcsin`(u[2],n);
		if `dev/length`(init)>n and init[nops(init)]<>infinity then
		    RETURN(init)
		else
		    example:=[init,0];
		    j:=arcsin(x);
		    k:=1;
		    for i to n do
			j:=diff(j,x);
			k:=k/i;
			example:=[op(example),subs(x=u[2],j)*k,i]
		    od;
		    fact:=subsop(2=NULL,3=NULL,u);
		    RETURN(`dev/endofdev`(fact,n,example))
		fi
	    else ERROR(FAIL)
	    fi
	else ERROR(FAIL)
	fi
    else ERROR(FAIL)
    fi
end:
#savelib(`dev/arcsin`);
