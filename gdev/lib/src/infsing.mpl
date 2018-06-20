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
##################################################
#
#				INFSING
#
#   This function returns the singularities of
# smallest modulus of the expression fct whose
# variable is var, the modulus being bounded
# by minsing.
# It returns a list whose first element is a list of 
# singularities and the second one is a boolean saying
# whether the coefficients of the expression are known 
# to be positive or not.
# 
# Modif BS 07/98. Added a 3rd element in the list which
# is the type of the dominant singularities. Ideally there
# should be one type per singularity, but it was easier to
# code it that way for this version.
##################################################

infsing:=proc(fct,var,r)
local val,i, minsing, singmin, singmini, locval, expon, rootval, type1, type2,f;
option remember,`Copyright Bruno Salvy, INRIA, France`;
    if nargs=3 then minsing:=r else minsing:=0 fi;
    if type(fct,polynom(anything,var)) then
	[[infinity],'polynom',type(expand(fct),polynom('nonnegative',var))]
    elif type(fct,ratpoly(anything,var)) then
	[`infsing/infsolve`(denom(fct),var,false,minsing),'polar',false]
    elif type(fct,{`+`,`*`}) then
	val:=[[infinity],'polynom',true];
	for i in fct do
	    singmini:=minsing;
	    to 10 do # bound on the number of iterations for cases like
		     # cos(3/8*Pi+1/2*Pi*z^2)/cos(Pi*z)
		locval:=infsing(i,var,singmini);
		if locval<>[] and locval[1][1]<>infinity then
		    val:=[`infsing/trie`(locval[1],val[1],fct,var,locval[2],val[2]),
			    evalb(locval[3] and val[3])];
		    if val[1][1]=infinity or 
			comparemodule(val[1][1],locval[1][1])=`>` then
			singmini:=abs(locval[1][1])
		    else break
		    fi
		else
		    break
		fi
	    od
	od;
	val
    elif type(fct,`^`) then
	expon:=op(2,fct);
	if type(expon,constant) then 
	    if type(expon,posint) then infsing(op(1,fct),var,minsing)
	    else
		val:=[[infinity],'polynom',true];
		singmin:=minsing;
		do
		    rootval:=`infsing/infsolve`(op(1,fct),var,false,singmin);
		    locval:=infsing(op(1,fct),var,singmin);
		    if not type(expon,negint) then
			if locval[2]='polar' then type2:='algebraic'
			else type2:=locval[2] fi;
			type1:='algebraic'
		    else type1:='polar'; type2:=locval[2]
		    fi;
		    val:=[`infsing/trie`(rootval,locval[1],fct,var,type1,type2),false];
		    if locval[1][1]<>infinity then
			if val[1][1]=infinity then
			    singmin:=abs(locval[1][1])
			else
			    break
			fi
		    else
			break
		    fi
		od;
		val
	    fi
	else infsing(exp(op(2,fct)*ln(op(1,fct))),var,minsing)
	fi
    elif type(fct,function) then
	f:=cat(`infsing/`,op(0,fct));
	if not type(f,procedure) then
	    ERROR(`Not implemented: infsing of `,op(0,fct))
	else f(fct,var,minsing)
	fi
    else ERROR(`Invalid expression`,fct)
    fi
end:

#savelib( infsing);
