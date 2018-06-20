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
##    Title:	`dev/implicit`
##    Created:	Thu Jan 19 10:32:48 1989
##    Author:	Salvy Bruno
##	<Bruno.Salvy@inria.fr>
##
##  Given two expansions expr1 and expr2, yields the expansion
## of y, where y is the solution of expr1(y)=expr2(x).
## Warning : this procedure will loop endlessly when the
## implicit equation is not solvable by this iterative method.
## A reasonable bound to put on the number of iterations would be 10.
## It must be called explicitly because it is not a safe function.
##
##  Because of possible ramifications, the result is a list of expansions.
##
## The second part (`dev/implicit/itersum`) should be made more efficient.
## A `dev/RootOf` should be written on top of this.
##
##  All of this should be rewritten with the algorithm of Salvy and Shackell.

`dev/implicit`:=proc (expr1,expr2,p)
local x, k, d1, d2;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(expr1,undefined) or type(expr2,undefined) then RETURN(undefined) fi;
    if expr1[3]=0 then
	if expr2[3]=0 and expr1[2]=expr2[2] then
	    `dev/implicit`(subsop(2=NULL,3=NULL,expr1),
		subsop(2=NULL,3=NULL,expr2),p)
	elif expr2[3]<>0 and type(expr1[2],list) then # is this correct ?
	    `dev/implicit`([op(expr1[2]),0,infinity],expr2,p)
	elif type([op(3,expr2),op(2,expr2)],[0,list]) then # same comment
	    `dev/implicit`(expr1,[op(expr2[2]),0,infinity],p)
	else ERROR(`no solution found`)
	fi
    else
	if expr1[3]<>-1 then
	    if type(expr1[3],negint) then
		d1:=`dev/pow`(expr1,-1/expr1[3],p);
		d2:=`dev/pow`(expr2,-1/expr1[3],p);
		[seq(op(`dev/implicit`(d1,`dev/multbyreal`(d2,
		    exp(-2*I*k*Pi/expr1[3])),p)),k=0..-expr1[3]-1)]
#		map(op,map(proc(tomap,expr1,quote,expr2,p)
#		    `dev/implicit`(`dev/pow`(expr1,1/quote,p),
#		    `dev/multbyreal`(`dev/pow`(expr2,1/quote,p),tomap),p) end,
#		    map(evalc,[seq(exp(2*I*k*Pi/(-expr1[3])),k=0..-expr1[3]-1)]),
#		    expr1,-expr1[3],expr2,p))
	    else
		`dev/implicit`(`dev/pow`(expr1,-1/expr1[3],p),
		    `dev/pow`(expr2,-1/expr1[3],p),p)
	    fi
	else
	    if type(expr1[2],list) then
		`dev/implicit`(`dev/ln`(expr1,p),`dev/ln`(expr2,p),p)
	    elif zerotest(expr1[2],1)<>true then
		`dev/implicit`(`dev/multbyreal`(expr1,1/expr1[2]),
			    `dev/multbyreal`(expr2,1/expr1[2]),p)
	    elif `dev/length`(expr1)>1 and expr1[5]<>infinity then
		if expr1[1]<>1 then
		    `dev/implicit`([expr1[1],1,-1,0,infinity],
		    `dev/implicit/itersum`(`dev/dev`(subs(_Xasy=op(
		    `dev/implicit`([expr1[1],1,-1,0,infinity],[1,1,-1,0,infinity],p)
		    ),eval(subs(_Xasytab=_equivX,
		    `dev/instanc`(`dev/add`([op(1..3,expr1),0,infinity],
		    `dev/multbyreal`(expr1,-1)))))),p,p),expr2,p),p)
		else
		    [`dev/implicit/itersum`(`dev/multbyreal`(subsop(2=NULL,3=NULL,
			expr1),-1),expr2,p)]
		fi
	    else
		if expr1[1]=1 then RETURN([expr2])
		else
		    d1:=subs(_Xasy=x,eval(subs(_Xasytab=_equivX,
			1/`dev/instanc`([expr1[1],1,1],x))));
		    if op(0,d1)=exp then
			`dev/implicit`(`dev/ln`([expr1[1],1,-1,0,infinity],p),
				    `dev/ln`(expr2,p),p)
		    elif op(0,d1)=ln then
			`dev/implicit`(`dev/exp`([expr1[1],1,-1,0,infinity],p),
				    `dev/exp`(expr2,p),p)
		    else
			ERROR(`should not happen`)
		    fi
		fi
	    fi
	fi
    fi
end: # `dev/implicit`

`dev/implicit/itersum`:=proc(otherside,val,p)
local expr, res;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
#    expr:=normal(eval(subs(_Xasytab=_equivX,`dev/instanc`(otherside))));
    expr:=eval(subs(_Xasytab=_equivX,`dev/instanc`(otherside)));
    res:=val;
    to p do
	res:=`dev/reduce`(`dev/add`(val,`dev/dev`(subs(_Xasy=
	    `dev/print`(res,_Xasy,infinity),expr),p,p)),p)
    od
end:

#savelib( `dev/implicit`,`dev/implicit/itersum`);
