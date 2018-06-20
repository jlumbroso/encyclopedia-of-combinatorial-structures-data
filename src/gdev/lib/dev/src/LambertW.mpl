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

# The next version will have to be iterative ( it is not difficult )
`dev/LambertW`:=proc(u,n)
local fact,i,x,example,l1,l2, j,k, init, sig, res;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then RETURN(undefined) fi;
    if not type(u,list) then
	if zerotest(-u,exp(-1))=true then
	    RETURN(-1)
	else
	    init:=traperror(simplify(LambertW(u),LambertW));
	    if init=lasterror then ERROR(init)
	    else RETURN(init)
	    fi
	fi
    fi;
    sig:=evalr(Signum(u[3]));
    if sig=1 then
	if Im(`dev/lcoeff`(u))<>0 then RETURN(undefined) fi;
	RETURN(`dev/endofdev`(u,n,[seq(op([(-1)^(i-1)*i^(i-1)/i!,i]),
	    i=1..n+1)]))
    elif sig=-1 then
	sig:=evalr(Signum(Re(`dev/lcoeff`(u))));
	if sig=-1 or sig=0 then RETURN(undefined)
	elif sig=FAIL then ERROR(FAIL)
	fi;
	l1:=`dev/ln`(u,n);
	l2:=`dev/ln`(l1,n);
	if n=1 then
	    res:=`dev/add`(l1,`dev/multbyreal`(l2,-1))
	elif n=2 then
	    res:=`dev/add`(`dev/add`(l1,`dev/multbyreal`(l2,-1)),
	    `dev/prd`(l2,`dev/pow`(l1,-1,n)))
	elif n=3 then
	    res:=`dev/add`(`dev/add`(`dev/add`(l1,`dev/multbyreal`(l2,-1)),
	    `dev/prd`(l2,`dev/pow`(l1,-1,n))),
	    `dev/prd`(`dev/add`(`dev/multbyreal`(`dev/pow`(l2,2,n),1/2),
				      `dev/multbyreal`(l2,-1)),
			 `dev/pow`(l1,-2,n)))
	elif n=4 then
	    res:=`dev/add`(`dev/add`(`dev/add`(`dev/add`(l1,`dev/multbyreal`
	    (l2,-1)),`dev/prd`(l2,`dev/pow`(l1,-1,n))),
	    `dev/prd`(`dev/add`(`dev/multbyreal`(`dev/pow`(l2,2,n),1/2),
				      `dev/multbyreal`(l2,-1)),
			 `dev/pow`(l1,-2,n))),
	    `dev/prd`(`dev/add`(`dev/multbyreal`(`dev/pow`(l2,3,n),-1/3),
			 `dev/add`(`dev/multbyreal`(`dev/pow`(l2,2,n),-3/2)
			    ,l2)),`dev/pow`(l1,-3,n)))
	else ERROR(`Not implemented`)
	fi;
	RETURN(`dev/reduce`(res,n))
    elif sig=0 then
	if type(u[2],list) then
	    init:=`dev/LambertW`([op(u[2]),0,infinity],n)
	else
	    init:=`dev/LambertW`(u[2],n)
	fi;
	if `dev/length`(init)>n and init[nops(init)]<>infinity then 
	    RETURN(init)
	else
	    if zerotest(-u[2],exp(-1))<>true then
		example:=[init,0];
		j:=LambertW(x);
		k:=1;
		for i to n do
		    j:=diff(j,x);
		    k:=k/i;
		    example:=[op(example),subs(x=u[2],j)*k,i]
		od;
		fact:=subsop(2=NULL,3=NULL,u);
		RETURN(`dev/endofdev`(fact,n,example))
	    else
		fact:=`dev/pow`(subs(u[2]=-exp(-1),
		    subsop(2=NULL,3=NULL,u)),1/2,n);
#		    if `dev/length`(fact)>n and fact[nops(fact)]<>infinity then
#			RETURN(`dev/add`(`dev/multbyreal`(fact,2
#			    ^(1/2)*exp(1/2)),-1))
#		    fi;
#		    ERROR(FAIL); # what is below is wrong.
#		    example:=[-1,0];
#		    for i from 2 to n do
#			tomult:=0;
#			for j to iquo(i-1,2) do
#			    tomult:=tomult+(-1)^j*binomial(i+j,j-1)*
#				`dev/W/sumcoeff`(j,i+j-1)
#			od;
#			example:=[op(example),tomult*2^(i/2)*exp(1/2)^i,i];
#		    od;
# Miserable patch:
		if n<20 then
		    example:=[op(1..2*n+2,[-1,0,2^(1/2)*exp(1)^(1/2),1,-2/3*exp(1),2,
11/36*2^(1/2)*exp(1)^(3/2),3,-43/135*exp(1)^2,4,769/4320*2^(1/2)*exp(1)^(5/2),5,
-1768/8505*exp(1)^3,6,680863/5443200*2^(1/2)*exp(1)^(7/2),7,-3926/25515*exp(1)^4,8,
226287557/2351462400*2^(1/2)*exp(1)^(9/2),9,-23105476/189448875*exp(1)^5,10,
169709463197/2172751257600*2^(1/2)*exp(1)^(11/2),11,-2237022626/22165518375*
exp(1)^6,12,667874164916771/10168475885568000*2^(1/2)*exp(1)^(13/2),13,-8008409168/
93095177175*exp(1)^7,14,103663334225097487/1830325659402240000*2^(1/2)*exp(1)^(15/2),15,
-933803635064758/12463116844303125*exp(1)^8,16,21235294185086305043/
426727353734922240000*2^(1/2)*exp(1)^(17/2),17,-424162971577226404/
6393578941127503125*exp(1)^9,18,1,19])]
		else ERROR(FAIL)
		fi;
		RETURN(`dev/endofdev`(fact,n,example))
	    fi
	fi
    else ERROR(FAIL)
    fi
end:# `dev/LambertW`
#savelib(`dev/LambertW`);
