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

`dev/GAMMA`:=proc(u,n)
local fact,i,x,example, newu, j, example2, firstpart, k, init, sig, co;
option remember,`Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	init:=traperror(GAMMA(u));
	if init=lasterror then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 or sig=0 and type(u[2],negint) then
	if sig=1 then
	    newu:=u;
	    i:=0;
	    example:=[1,1,-1,0,infinity]
	else
	    newu:=subsop(2=NULL,3=NULL,u);
	    i:=u[2];
	    example:=`dev/dev`(1/expand(convert([seq(1-j*_Xasy,j=1..-i)],`*`)
		),n,n);
	    example:=[1,seq(op([example[2*j],j-2]),j=1..iquo(nops(example),2))]
	fi;
	example2:=`dev/prd`(example,`dev/GAMMA`([1,1,0,1,1,0,infinity],n));
	RETURN(`dev/add`(`dev/multbyreal`(`dev/pow`(newu,-1,n),example2[2]),
	    `dev/endofdev`(newu,n,subsop(1=NULL,2=NULL,3=NULL,example2))))
    elif sig=-1 then
	sig:=evalr(Signum(Re(`dev/lcoeff`(u))));
	if sig=-1 then RETURN(undefined)
	elif sig=FAIL then ERROR(FAIL)
	fi;
	firstpart:=`dev/multbyreal`(`dev/prd`(`dev/exp`(`dev/prd`(u,`dev/add`(
	-1,`dev/ln`(u,n))),n),`dev/pow`(u,-1/2,n)),(2*Pi)^(1/2));
	if `dev/length`(firstpart)>n and firstpart[nops(firstpart)]<>infinity
	then RETURN(firstpart) fi;
	example:=subsop(1=NULL,`dev/exp`(
	    [1,seq(op([bernoulli(2*i)/i/(4*i-2),2*i-1]),i=1..n+1)],n+1));
	fact:=`dev/pow`(u,-1,n);
	RETURN(`dev/prd`(firstpart,`dev/endofdev`(fact,n,example)))
    elif sig=0 then
	# An important special case
	if u[2]=1 then
	    example[0]:=1: example[1]:=0:
	    co:=[-gamma,seq((-1)^(i+1)*Zeta(i+1),i=1..n-1)];
	    for i to n do
		example[2*i]:=convert([seq(co[j+1]*example[2*i-2*j-2],j=0..i-1)]
		    ,`+`)/i;
		example[2*i+1]:=i od;
	    RETURN(`dev/endofdev`(subsop(2=NULL,3=NULL,u),n,
		[seq(example[i],i=0..2*n+1)]))
	elif type(u[2],integer) and u[2]>1 then
	    example:=`dev/dev`(expand(convert([seq(1+j*_Xasy,j=1..u[2]-1)],
		`*`)),
		n,n);
	    example:=[1,seq(op([example[2*j],j-1]),j=1..iquo(nops(example),2))];
	    if u[2]-1<n+1 then example:=subsop(2*u[2]+3=infinity,example) fi;
	    example2:=`dev/GAMMA`([1,1,0,1,1,0,infinity],n);
	    RETURN(`dev/endofdev`(subsop(2=NULL,3=NULL,u),n,subsop(1=NULL,
		`dev/prd`(example,example2))))
	else
	    if type(u[2],list) then
		init:=`dev/GAMMA`([op(u[2]),0,infinity],n)
	    else
		init:=`dev/GAMMA`(u[2],n)
	    fi;
	    if type(init, undefined) then RETURN(undefined) fi;
	    if `dev/length`(init)>n and init[nops(init)]<>infinity then
		RETURN(init)
	    else
		example:=[init,0];
		j:='GAMMA'(x);
		k:=1;
		for i to n do
		    j:=diff(j,x);
		    k:=k/i;
		    example:=[op(example),subs(x=u[2],j)*k,i]
		od;
		fact:=subsop(2=NULL,3=NULL,u);
		RETURN(`dev/endofdev`(fact,n,example))
	    fi
	fi
    else ERROR(FAIL)
    fi
end:# `dev/GAMMA`
#savelib(`dev/GAMMA`);
