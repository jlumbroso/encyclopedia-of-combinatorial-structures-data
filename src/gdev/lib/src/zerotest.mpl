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
##	Title:	zerotest
##	Created:    Mon Feb 3 20:03:57 1992
##	Author: Bruno Salvy
##	<salvy@rully.inria.fr>
##
## Description: a heuristic mixture of numerical and symbolic computations to
## determine whether two expressions are equal or not, or whether an
## expression is identically zero.

zerotest:=proc (expr1::algebraic, expr2::algebraic)
local expr, i, var, s, j, OO, algexpr;
option `Copyright Bruno Salvy, INRIA, France`;
    if nargs=1 then expr:=expr1
    elif nargs=2 then
	if type([expr1,expr2],[function,function]) and op(0,expr1)=op(0,expr2)
	    and # it will be easier if the arguments are equal
	nops(expr1)=1 and nops(expr2)=1 and zerotest(op(expr1),op(expr2)) then
	    RETURN(true)
	fi;
	expr:=expr1-expr2
    fi;
    # Some of the following is common with testeq
    # simplify here for this version. It's slow, but should help.
#    if expr=0 or simplify(expr)=0 then RETURN(true)
#	Commented out simplify. Much too slow for encyclopedia to create its base. BS. Apr.09.
    if expr=0 then RETURN(true)
    elif type(expr,{float,rational,name}) then RETURN(false)
    elif type(expr,`^`) and type(op(2,expr),numeric) then
	if op(2,expr) > 0 then RETURN( zerotest(op(1,expr)) )
	else RETURN( false ) fi
    elif type(expr,`*`) then
	for i in expr do
	    zerotest(i);
	    if %=true then RETURN(true)
	    elif %=FAIL then RETURN(FAIL)
	    fi
	od;
	RETURN(false)
    elif type(expr,function) then
	if   op(0,expr)=exp then RETURN( false )
	elif op(0,expr)=ln  then RETURN( zerotest(op(1,expr)-1) )
	elif op(0,expr)=abs then RETURN( zerotest(op(expr)) )
	elif op(0,expr)=O   then RETURN( false )
	# the last two cases are for equivalent
	elif op(0,expr)='QuasiLog' then RETURN(zerotest(op(expr)))
	elif op(0,expr)='QuasiInverse' then RETURN( false )
	fi
    fi;
    # try to get a numerical value
    # indets does a correct dag traversal
    var:=indets(expr,name) minus {constants}; 
    if var<>{} then
	Digits:=Digits+5;
	for j to 3 do # so that we find values in the right domain
	    if j=1 then
		s:=traperror(subs(seq(op(i,var)=evalf((1789/1492)^(1/i)),
		    i=1..nops(var)),expr))
	    elif j=2 then
		s:=traperror(subs(seq(op(i,var)=evalf((1492/1789)^i),
		    i=1..nops(var)),expr))
	    else
		s:=traperror(subs(seq(op(i,var)=-evalf((1789/1492)^(1/i)),
		    i=1..nops(var)),expr))
	    fi;
	    s:=`zerotest/cst`(s);
	    if s<>FAIL then RETURN(s) else next fi
	od
    else
	s:=`zerotest/cst`(expr);
	if s<>FAIL then RETURN(s) fi
    fi;
    # Special cases where a normal form is available
    if type(expr,ratpoly(rational)) then RETURN(evalb(normal(expr)=0)) fi;
    algexpr:=convert(expr,RootOf);
    if type(algexpr,algnum) then
	s:=traperror(evala(Normal(algexpr)));
	if s<>lasterror then RETURN(evalb(s=0))
	elif type([s],['symbol',set]) then
#	    if nops(indets(algexpr,RootOf))=1 then # otherwise it's too hard
		s:=op(2,[s]);
		for i in s do
		    if not has(algexpr,op(1,i)) then
			ERROR(`bug in evala@Normal with input:`,algexpr,`output is reducible RootOf detected with`,op(1,i)) fi;
		    if zerotest(subs(i,algexpr))=true
			then RETURN(true) fi
		od;
		RETURN(false)
#	    fi
	fi
    fi;
    if has(expr,{'QuasiLog','QuasiInverse'}) then RETURN(zerotest(eval(subs(
	['QuasiLog'=proc(t)log(1/(1-t)) end,'QuasiInverse'=proc(t)1/(1-t)end],expr)))) fi;
    # if everything else failed, try testeq
    s:=testeq(expr);
    if s<>FAIL then RETURN(s) fi;
    # If the expression is a real constant then it is probably non-zero
    if var={} and not has(expr,I) then
	userinfo(3,'equivalent',`Warning: assumption made`,expr=0);
	RETURN(true)
    elif has(expr,RootOf) then
	var:=indets(expr,RootOf);
	if nops(var)=1 then
	     RETURN(zerotest(subs(op(var)=OO,numer(expr)),
	 subs(_Z=OO,op(1,op(var)))))
	fi
    fi;
    RETURN(FAIL)
end: # zerotest

`zerotest/cst`:=proc (expr)
local floatval;
    Digits:=Digits+5;
#    floatval:=traperror(evalf(expr));
    floatval:=traperror(`zerotest/dagevalf`(expr));
    if type(floatval,complex(numeric)) then 
	Digits:=Digits-5;
	evalb(abs(floatval)<Float(1,-Digits))
    else FAIL
    fi
end: # `zerotest/cst`

`zerotest/dagevalf`:=proc (f)
local i, fct;
options remember;
    if type(f,{numeric,name}) or type(f,complex({numeric,name})) then evalf(f)
    elif type(f,{`+`,`*`}) then map(`zerotest/dagevalf`,f)
    elif type(f,`^`) then evalf(`zerotest/dagevalf`(op(1,f))^op(2,f))
    elif type(f,function) then
	fct:=op(0,f);
	if fct=RootOf then evalf(f)
	elif nops(f)=1 then evalf(fct(`zerotest/dagevalf`(op(f))))
	else evalf(fct(op(1,f),
	    seq(`zerotest/dagevalf`(op(i,f)),i=2..nops(f))))
	fi
    else ERROR("bug")
    fi
end: # zerotest/dagevalf

#savelib( zerotest,`zerotest/cst`,`zerotest/dagevalf`);
