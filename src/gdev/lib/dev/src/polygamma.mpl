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

`dev/polygamma`:=proc(k,u,n)
local fact,i,x,example, val, newu, example2, j, kfact, tomult, init, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(k,integer) then ERROR(FAIL) fi;
    if not type(u,list) then
	init:=traperror(Psi(k,u));
	if init=lasterror then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 or sig=0 and type(u[2],negint) then
	kfact:=(-1)^(k+1)*k!;
	if sig=0 then
	    val:=u[2];
	    newu:=subsop(2=NULL,3=NULL,u);
	    example:=`dev/dev`(normal(convert([seq(1/(1-i*_Xasy)^(k+1),
		i=1..-val)],`+`)),n-1,n-1);
	    example:=[1,kfact,-k-1,seq(op([example[2*i]*kfact,i-1]),i=1..n)]
	else val:=0; newu:=u;
	    example:=[1,kfact,-k-1,0,infinity]
	fi;
	example2:=`dev/polygamma`(k,[1,1,0,1,1,0,infinity],n-1);
	RETURN(`dev/endofdev`(newu,n,subsop(1=NULL,`dev/add`(example,
	    example2))))
    elif sig=-1 then
	sig:=evalr(Signum(Re(`dev/lcoeff`(u))));
	if sig=-1 then RETURN(undefined)
	elif sig=FAIL then ERROR(FAIL)
	fi;
	kfact:=(-1)^(k-1)*(k-1)!;
	example:=[kfact,k,kfact*k/2,k+1];
	for i to n-1 do
	    kfact:=kfact*(2*i+k-1)*(2*i+k-2)/2/i/(2*i-1);
	    example:=[op(example),kfact*bernoulli(2*i),2*i+k]
	od;
	RETURN(`dev/endofdev`(`dev/pow`(u,-1,n),n,example))
    elif sig=0 then
	if u[2]=1 then
	    kfact:=k!;
	    if irem(k,2)=0 then
		kfact:=-kfact;
		tomult:=(2*Pi)^(k+2)/2;
		example:=[kfact*Zeta(k+1),0,tomult/(k+2)*abs(bernoulli(k+2)),1];
		for i from 2 by 2 to n do
		    kfact:=kfact*(k+i)*(k+i-1)/i/(i-1);
		    tomult:=tomult*4*Pi^2/i/(i+1);
		    example:=[op(example),kfact*Zeta(k+1+i),i,tomult/(k+i+2)*
			abs(bernoulli(k+i+2)),i+1]
		od
	    else
		tomult:=(2*Pi)^(k+1)/2;
		kfact:=-kfact*(k+1);
		example:=[tomult/(k+1)*abs(bernoulli(k+1)),0,kfact*Zeta(k+2),1];
		for i from 2 by 2 to n do
		    tomult:=tomult*4*Pi^2/i/(i-1);
		    kfact:=kfact*(k+i+1)*(k+i)/(i+1)/i;
		    example:=[op(example),tomult/(k+i+1)*abs(bernoulli(k+i+1)),
			i,kfact*Zeta(k+i+2),i+1]
		od
	    fi;
	    RETURN(`dev/endofdev`(subsop(2=NULL,3=NULL,u),n,example))
	elif type(u[2],integer) then
	    kfact:=(-1)^k*k!;
	    example:=`dev/dev`(normal(convert([seq(1/(1+i*_Xasy)^(k+1),
		i=1..u[2]-1)],`+`)),n,n);
	    example:=[1,seq(op([example[2*i]*kfact,i-1]),i=1..n+1)];
	    example2:=`dev/polygamma`(k,[1,1,0,1,1,0,infinity],n);
	    RETURN(`dev/endofdev`(subsop(2=NULL,3=NULL,u),n,subsop(1=NULL,
		`dev/add`(example,example2))))
	else
	    if type(u[2],list) then
		init:=`dev/polygamma`(k,[op(u[2]),0,infinity],n)
	    else
		init:=`dev/polygamma`(k,u[2],n)
	    fi;
	    if `dev/length`(init)>n and init[nops(init)]<>infinity then
		RETURN(init)
	    else
		example:=[init,0];
		j:=Psi(k,x);
		kfact:=1;
		for i to n do
		    j:=diff(j,x);
		    kfact:=kfact/i;
		    example:=[op(example),subs(x=u[2],j)*kfact,i]
		od;
		fact:=subsop(2=NULL,3=NULL,u);
		RETURN(`dev/endofdev`(fact,n,example))
	    fi
	fi
    else ERROR(FAIL)
    fi
end:# `dev/polygamma`
#savelib(`dev/polygamma`);
