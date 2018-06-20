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
#		    EQUIVALENT
#
#    The root of the program. Its arguments are:
#       * the function
#       * the expected order of the expansion (optional)
#       * the variable (optional)
#       * the variable used to express the result (optional, _N will be used)
#
##################################################

equivalent:=proc(fct::algebraic,x::name,n::name,optp::posint)
local p, res;
option `Copyright Bruno Salvy, INRIA, France`;
    if nargs=4 then p:=optp else p:=1 fi;
##    if not is(n,integer) then additionally(n,integer) fi;
    if is(_Xasy>100)=FAIL then additionally(_Xasy>100) fi;
    res:=`equivalent/equiv`(fct,p,x);
    if not has(res,_saddlepoint) then
	`dev/print`(res,n,p)
    else
	if op(2,res)<>infinity then
	    if nops(res)=3 then
	     eval(subs(x=1/(1-op(1,res)/op(2,res)),O=subs(_RES=res,_n=n,proc(x) 
		O(x/op(1,_RES)^_n)end),`dev/print`(`dev/multbyreal`(op(3,res),
		1/op(1,res)^n),x,p)))
	    elif op(3,res)=1 then
	      eval(subs(x=1/(1-op(1,res)/op(2,res)),O=subs(_RES=res,_n=n,proc(x)
		  O(x/op(1,_RES)^_n)end),`dev/print`(`dev/multbyreal`(op(4,res),
		    1/op(1,res)^n),x,p)))
	    else
		eval(Sum(subs(x=1/(1-_saddlepoint['i']/op(2,res)),
		    O=subs(SADD=_saddlepoint,_n=n,
			proc(x)    O(x/_saddlepoint['i']^_n) end),
		    `dev/print`(
		    `dev/multbyreal`(op(4,res),1/_saddlepoint['i']^n),x,p)),
		    'i'=1..op(3,res)))
	    fi
	elif nops(res)=3 then
	    eval(subs(x=op(1,res),O=subs(_RES=res,_n=n,
		proc(x) O(x/op(1,_RES)^_n) end),
		`dev/print`(`dev/multbyreal`(op(3,res),1/op(1,res)^n),x,p)))
	elif op(3,res)=1 then
	    eval(subs(x=op(1,res),O=subs(_RES=res,_n=n,
		proc(x) O(x/op(1,_RES)^_n) end),
		`dev/print`(`dev/multbyreal`(op(4,res),1/op(1,res)^n),x,p)))
	else # it still needs to be divided by _saddlepoint^n
	    Sum(`dev/print`(op(4,res),_saddlepoint['i'],p),'i'=1..op(3,res))
	fi
    fi;
end:

#savelib( equivalent);
