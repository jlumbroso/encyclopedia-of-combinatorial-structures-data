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
############################################################
#
#				 COMPAREMODULE
#
#   This procedure compares the moduli of two constants, it
# returns <,>,=,<>.
#
###########################################################
comparemodule:=proc(expr1,expr2,justdiffneeded)
local s1,s2, res, real;
option `Copyright Bruno Salvy, INRIA, France`;
    # need to be careful with RootOfs since we want to separate the Roots.
    if not (hastype(expr1,RootOf) and hastype(expr2,RootOf)) and
	zerotest(expr1,expr2)=true then RETURN(`=`) fi;
    if expr2<>0 then	# Should use Coste-Roy coding here
	real:=true;
	if type(expr1,specfunc(anything,abs)) then
	    RETURN(procname(op(expr1),expr2)) fi;
	if type(expr2,specfunc(anything,abs)) then
	    RETURN(procname(expr1,op(expr2))) fi;
	# This one does not disappear with maple6
	if not assigned(`evalc/evalc`) then evalc(1) fi;
	if type(expr1,realcons) then s1:=[expr1,0]
	else s1:=`evalc/evalc`(expr1);real:=false fi;
	if type(expr2,realcons) then s2:=[expr2,0]
	else s2:=`evalc/evalc`(expr2);real:=false fi;
	if real then res:=comparexpr(s1[1]^2,s2[1]^2)
	else 
	    # cases where evalc did not succeed
	    if op(0,s1)=`evalc/unsplit` then 
		s1:=`evalc/evalc`(evalf(expr1));
		if op(0,s1)=`evalc/unsplit` then
		    ERROR(`Unable to compare`,'abs'(expr1),'abs'(expr2)) fi
	    fi;
	    if op(0,s2)=`evalc/unsplit` then 
		s2:=`evalc/evalc`(evalf(expr2));
		if op(0,s2)=`evalc/unsplit` then
		    ERROR(`Unable to compare`,'abs'(expr1),'abs'(expr2)) fi
	    fi;
	    # This is necessary to get 25 answers from
	    # infsing(1/(1-z)/(1-z^25),z)
	    if type(expr1,complex(float)) and type(expr2,complex(float)) then
		res:=procname(abs(expr1),abs(expr2))
	    else
		# sqrt is necessary for numerical stability problems
		res:=comparexpr(sqrt(op(1,s1)^2+op(2,s1)^2),
		    sqrt(op(1,s2)^2+op(2,s2)^2))
	    fi
	fi
    elif zerotest(expr1)<>true then res:=`>`
    else res:=`=`
#	s1:=evalc(expr1);
#	if comparexpr(coeff(s1,I,1),0)=`=` then
#	    res:=comparexpr(abs(coeff(s1,I,0)),0)
#	else res:=`>` fi;
    fi;
    if res=`<>` then
	if nargs<3 or not justdiffneeded then
	    ERROR(`Unable to compare`,abs(expr1),`and`,abs(expr2))
	else `<>`
	fi
    else res
    fi
end:

#savelib( comparemodule);
