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

`dev/Psi`:=proc(u,n)
local fact,i,x,example, newu, j, firstpart, example2,k, invu, init, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if nargs=3 then RETURN(`dev/polygamma`(args)) fi;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	init:=traperror(Psi(u));
	if init=lasterror then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 or sig=0 and type(u[2],negint) then
	if sig=1 then
	    newu:=u;
	    example:=[1,-1,-1,0,infinity]
	else
	    newu:=subsop(2=NULL,3=NULL,u);
	    i:=u[2];
	    example:=`dev/dev`(normal(convert([seq(1/(1-j*_Xasy),j=1..-i)],
		`+`)),n-1,n-1);
	    example:=[1,-1,-1,seq(op([-example[2*j],j-1]),j=1..n)]
	fi;
	example2:=`dev/add`(`dev/Psi`([1,1,0,1,1,0,infinity],n-1),example);
	RETURN(`dev/add`(`dev/multbyreal`(`dev/pow`(newu,-1,n),example2[2]),
	    `dev/endofdev`(newu,n,subsop(1=NULL,2=NULL,3=NULL,example2))))
    elif sig=-1 then
	sig:=evalr(Signum(Re(`dev/lcoeff`(u))));
	if sig=-1 then RETURN(undefined)
	elif sig=FAIL then ERROR(FAIL)
	fi;
	firstpart:=`dev/ln`(u,n);
	if `dev/length`(firstpart)>n and firstpart[nops(firstpart)]<>infinity 
	    then RETURN(firstpart) fi;
	example:=[seq(op([-bernoulli(2*i)/2/i,i]),i=1..n)];
	invu:=`dev/pow`(u,-1,n);
	RETURN(`dev/add`(`dev/add`(firstpart,`dev/multbyreal`(invu,-1/2)),
	    `dev/endofdev`(`dev/pow`(invu,2,n-1),n-1,example)))
    elif sig=0 then
	if u[2]=1 then
	    RETURN(`dev/endofdev`(subsop(2=NULL,3=NULL,u),n,
	    [-gamma,0,seq(op([(-1)^i*Zeta(i),i-1]),i=2..n+1)]))
	elif type(u[2],integer) then
	    example:=`dev/dev`(normal(convert([seq(1/(1+i*_Xasy),i=1..u[2]-1)]
		,`+`)),n,n);
	    example:=[1,seq(op([example[2*i],i-1]),i=1..n+1)];
	    example2:=`dev/Psi`([1,1,0,1,1,0,infinity],n);
	    RETURN(`dev/endofdev`(subsop(2=NULL,3=NULL,u),n,subsop(1=NULL,
		`dev/add`(example,example2))))
	else
	    if type(u[2],list) then
		init:=`dev/Psi`([op(u[2]),0,infinity],n)
	    else
		init:=`dev/Psi`(u[2],n)
	    fi;
	    if `dev/length`(init)>n and init[nops(init)]<>infinity then
		RETURN(init)
	    else
		example:=[init,0];
		j:=Psi(x);
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
end:# `dev/Psi`
#savelib(`dev/Psi`);
