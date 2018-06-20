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

`dev/AiryAi`:=proc(u,n)
local fact,i,x,example, expfact, cosinus, sinus, fact2, k, c1, c2, ck, example1, example2, j, kfact, num1, num2, coef, init, sig, zeta;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	init:=traperror(AiryAi(u));
	if init=lasterror then RETURN(undefined)
	else RETURN(init)
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	c1:=3^(-2/3)/GAMMA(2/3);c2:=-3^(-1/3)/GAMMA(1/3);
	num1:=1;num2:=1;kfact:=1;
	example:=[c1,0,c2,1];
	for i to n-1 do
	    kfact:=kfact*(3*i-2)*(3*i-1)*3*i;
	    num1:=num1*(3*i-2);
	    num2:=num2*(3*i-1);
	    example:=[op(example),c1*num1/kfact,3*i,c2*num2/kfact/(3*i+1),
		3*i+1]
	od;
	RETURN(`dev/endofdev`(u,n,example))
    elif sig=-1 then
	coef:=evalc(`dev/lcoeff`(u));
	sig:=evalr(Signum(coeff(coef,I,0)));
	if sig=FAIL then ERROR(FAIL)
	elif sig=-1 and coeff(coef,I,1)=0 then
	    zeta:=`dev/multbyreal`(`dev/pow`(u,3/2,n),2/3);
	    fact:=`dev/pow`(zeta,-1,n+1);
	    fact2:=`dev/pow`(fact,2,n+1);
	    cosinus:=`dev/cossin`(`dev/add`(Pi/4,fact2),n);
	    sinus:=cosinus[2];
	    cosinus:=`dev/multbyreal`(`dev/prd`(cosinus[1],fact),-1);
	    example1:=[1,1];
	    example2:=[5/72,1];
	    ck:=5/72;
	    for i to iquo(n-1,2) do
		ck:=-ck*(12*i-1)*(12*i-5)/144/i;
		example1:=[op(example1),ck,i];
		ck:=ck*(12*i+5)*(12*i+1)/72/(2*i+1);
		example2:=[op(example2),ck,i]
	    od;
	    RETURN(`dev/multbyreal`(`dev/prd`(`dev/pow`(u,-1/4,n),`dev/add`(
	    `dev/prd`(sinus,`dev/endofdev`(fact2,iquo(n+1,2),example1)),
	    `dev/prd`(cosinus,`dev/endofdev`(fact2,iquo(n+1,2),example2)))),
	    Pi^(-1/2)))
	else
	    fact:=`dev/multbyreal`(`dev/pow`(u,3/2,n),2/3);
	    expfact:=`dev/exp`(`dev/multbyreal`(fact,-1),n+1);
	    if `dev/length`(expfact)>n and expfact[nops(expfact)]<>infinity 
	    then
		RETURN(`dev/reduce`(`dev/multbyreal`(`dev/prd`(`dev/pow`(
		u,-1/4,n),expfact),1/2/Pi^(-1/2)),n))
	    else
		fact:=`dev/pow`(fact,-1,n);
		example:=[1,0];
		ck:=1;
		for i to n do
		    ck:=-ck*(6*i-5)*(6*i-1)/72/i;
		    example:=[op(example),ck,i]
		od;
		RETURN(`dev/reduce`(`dev/multbyreal`(`dev/prd`(`dev/prd`(
		`dev/pow`(u,-1/4,n),`dev/endofdev`(fact,n,example)),
		expfact),1/2/Pi^(1/2)),n))
	    fi
	fi
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/AiryAi`([op(u[2]),0,infinity],n)
	else
	    init:=`dev/AiryAi`(u[2],n)
	fi;
	if `dev/length`(init)>n and init[nops(init)]<>infinity then
	    RETURN(init)
	else
	    example:=[init,0];
	    j:=AiryAi(x);
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
end:# `dev/AiryAi`
#savelib(`dev/AiryAi`);
