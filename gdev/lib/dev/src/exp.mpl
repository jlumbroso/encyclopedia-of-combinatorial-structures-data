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

# -*-maple-*-
##
##    Title:	`dev/exp`
##    Created:	June 87
##    Author:	Bruno Salvy
##	<salvy@tokay.inria.fr>
##
## Description: Computes the exponential of an expansion.
##
##    Modified:	    Thu Apr 13 14:05:49 1989
##    Author:	Bruno Salvy
##    Modification: commented
##

`dev/exp`:=proc(u,n)
local i, coef, nu, result, j, lcoef;
global _equivX;
option remember,`Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    # exp(undefined)=undefined
    if type(u,undefined) then undefined
    # constants
    elif not type(u,list) then exp(u)
    else
	# The O() has to be small for the expansion of the exponential to
	# be convergent
	nu:=nops(u);
	if u[nu]<>infinity and evalr(Signum(u[nu]))<>1 then
	     ERROR(`Not a convergent expansion`) fi;
	# I. Recover the large terms
	result:=1;
	for i from 3 by 2 to nu while evalr(Signum(u[i]))=-1 do
	    if u[i]=-1 and not type(u[i-1],list) then
		# it has the form exp(exp(...(exp(z))))
		if not assigned(_equivX[u[1]-1]) then
		    _equivX[u[1]-1]:=1/exp(1/_equivX[u[1]])
		fi;
		result:=`dev/prd`(result,
		    [u[1]-1,exp(`dev/instanc`(`dev/impart`(u[i-1]))
			/_Xasytab[u[1]]),-`dev/realpart`(u[i-1]),0,infinity]);
	    elif not type(u[i-1],list) then
		# we have to compute the label we will give to exp(X[order])
		result:=`dev/prd`(result,
		    `dev/multbyreal`(`dev/pow`(`dev/indexify`('exp'([u[1],1,
		    u[i]])),`dev/realpart`(u[i-1]),n),exp(`dev/instanc`([u[1],
		    `dev/impart`(u[i-1]),u[i],0,infinity]))))
	    else
		# we have to compute the label we will give to exp(X[order])
		for j from 3 by 2 to nops(u[i-1]) do
		    result:=`dev/prd`(result,`dev/multbyreal`(`dev/indexify`(
		      'exp'(`dev/realpart`([u[1],[op(1,u[i-1]),op(j-1..j,u[i-1])
			    ],u[i],0,infinity]))),
			exp(`dev/instanc`(`dev/impart`([u[1],[op(1,u[i-1]),
			    op(j-1..j,u[i-1])],u[i],0,infinity])))))
		od
	    fi
	od;
	# II. Take care of the possible constant term
	if op(i,u)=0 then
	    if not type(op(i-1,u),list) then
		result:=`dev/multbyreal`(result,`dev/exp`(op(i-1,u),n))
	    else
		result:=`dev/prd`(result,
		    `dev/exp`([op(op(i-1,u)),0,infinity],n))
	    fi;
	    i:=i+2
	fi;
	# III. The part which tends to zero
	# use 1+expr+expr^2/2+...
	if i<nu then
	    coef:=1/6;
	    lcoef:=[1,0,1,1,1/2,2,1/6,3];
	    for j from 4 to n do coef:=coef/j;lcoef:=[op(lcoef),coef,j] od;
	    `dev/prd`(result,`dev/endofdev`([u[1],op(i-1..nu,u)],n,lcoef))
	elif u[nu]=infinity then result
	else `dev/prd`(result,[u[1],1,0,1,u[nu]])
	fi
    fi
end: # `dev/exp`


#savelib( `dev/exp`);
