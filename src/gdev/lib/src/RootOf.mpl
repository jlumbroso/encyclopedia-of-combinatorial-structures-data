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
# $Source: /u/maple/research/lib/src/RCS/RootOf,v $
#
#	function RootOf, place holder of roots of equations, in
#	 particular the standard representation for Maple's
#	 algebraic numbers and algebraic functions.
#
#	RootOf( equation, var, selector )
#	RootOf( equation, selector )
#	RootOf( equation )
#
#	where	- equation is an equation or an algebraic expression;
#		if it is an algebraic expression it is assumed to
#		be equated to zero
#
#		- var is a variable name
#
#		- selector is a mechanism to specify a subset of the
#		roots.  It can take two forms, either a numerical (or
#		complex) value, in which case the selected root is the
#		one closest to the numerical value, or a range of
#		numerical (or complex numerical) values, in which case
#		the values are the ones enclosed in the given range.
#
#               - The selector may also be an equation of the form
#               index=i where i is either an integer or a symbol.
#               In case i is an integer, the indexed RootOf is
#               supposed to represent the i-th root of the equation.
#               The order is given by `RootOf/order`. MJR 02/96
#
#               - The selector may also be an equation of the form
#               label=exp where exp is any expression. Labels are
#               meant to distinguish roots. MJR 02/96
#		
#	If "var" is not given and "equation" is in the variable _Z,
#	the RootOf is in canonical form.
#
#	If "var" is not given and "equation" is in a single variable,
#	"var" is assumed to be this variable and it is changed to _Z.
#
#	If "equation" is of type algeb=algeb then it is converted into
#	an algebraic expression (equated to zero).
#
#	RootOfs can be nested to any depth, i.e. the coefficients in
#	"equation" may involve other RootOf expressions.
#
#	If "equation" is a polynomial in _Z, then the RootOf represents
#	an algebraic number or function.  In this case, the canonical
#	representation for the polynomial is the one resulting from
#	collect(equation,_Z,normal), i.e. the polynomial has its terms
#	collected in _Z and each coefficient is normalized.
#
#	RootOf is basically a place holder, so that other functions
#	can operate on it.  RootOf represents any (a multiple valued
#	function) of the solutions of the equation argument.
#	RootOf is the mechanism to represent algebraic numbers and
#	algebraic functions.
#
#	Maple knows how to do several computations with RootOfs.
#
#	RootOf will check the validity of its arguments, change
#	the arguments to a canonical form, remove contents and
#	substitute for the root in case of polynomials of degree 1.
#	Obvious multiple roots will be eliminated.
#
#	When the environment variable _EnvRootOfCompose is set to true,
#	RootOf will compose with any RootOfs which are in the argument.
#	E.g.  RootOf( _Z^2-RootOf(_Z^2-3)) -->  RootOf(_Z^4-3)
#
#
#					Gaston H. Gonnet (Oct 1987)
#                                       $Notify: gonnet@inf.ethz.ch$
#       Updated to handle algebraic functions 
#                                          Marc Rybowicz (Feb 1991)
#					Gaston H. Gonnet (Apr 1993)
#
#       The code for type/RootOf has been put here so that both are
#       modified together to avoid inconsistencies -- MBM Mar/95
#

#
#--> type(x,RootOf): check for a RootOf
#
# See also type/algext and type/radext
#
# Author: MBM Jun/89
# Updated: MBM Dec/93, Mar/95
# Do not give an error if called with an expression sequence, LB March 95.
#
# Modified BS Jul 98. Suppressed the conversion of the 2nd arg to a rational 
# followed by substitution which was much too expensive for large polynomials
# at large values of Digits.

`type/RootOf` := proc(x) options remember,system,
`Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
    if nargs = 1 then
        type(x,function) and op(0,x) = 'RootOf' and type([op(x)],{
                [algebraic], # RootOf(_Z^3-2)
                [algebraic,constant], # RootOf(_Z^3-2,1.2)
                # BUG [algebraic,type], # RootOf(_Z^3-2,realcons)
                [algebraic,range], # RootOf(_Z^3-2,1.0 .. 2.0)
		[algebraic,equation] # RootOf(_Z^3-2, index=1);
            })
    else false
    fi
end :
#savelib('`type/RootOf`'):

unprotect('RootOf');
RootOf := proc( e, x )
local ex, inam, z, freeze, rof, rofs, i, lc, inds, ex1, err, indexd;
option remember, 
    `Copyright (c) 1992 by the University of Waterloo. All rights reserved.`;

if nargs>=2 and type(x,name) then
	if x<>_Z and indets(e) intersect {_Z}<>{} then
	    ERROR(`should not use _Z as independent variable`)
	elif has(select(type,indets(e),RootOf),x) then
	    # try to resolve nested RootOfs by resultants
	    inds := select(type,indets(e),RootOf);
	    if nops(inds)=1 and type(e,polynom(anything,inds[1])) and
		type(op(1,inds[1]),polynom(anything,_Z)) then
		ex := resultant(subs(inds[1]=z,e), subs(_Z=z,op(1,inds[1])),z);
		RETURN( RootOf(ex, args[2..nargs]) )
		fi;
	    ERROR(`limitation: cannot handle nested RootOf's containing`,x)
	elif x<>_Z then 
            if nargs>2 then 
		`RootOf/selector`(args[3]);
                RETURN(RootOf(subs(x=_Z,e),args[3..nargs]))
            else RETURN(RootOf(subs(x=_Z,e))) fi 
	else RETURN(RootOf(e,args[3..nargs]))
	fi

elif not type(e,{algebraic,equation}) then ERROR( `invalid arguments` )
elif type(e,equation) then RETURN(RootOf(op(1,e)-op(2,e),args[2..nargs]))
elif type(e,{algnum,constant}) then ERROR(`expression independent of`, _Z) 
fi;

if nargs>1 then `RootOf/selector`(args[2],'indexd') else indexd:=false fi;

if type(e,`^`) and signum(op(2,e)) = 1 and not indexd then
	RETURN(RootOf(op(1,e),args[2..nargs]))
elif type(e,`*`) and type(op(1,e),numeric) then
	RETURN( RootOf(subsop(1=1,e),args[2..nargs]) )
fi;

ex := traperror( numer(normal(e)) );
if ex=lasterror then RETURN( 'RootOf(args)' ) fi;
inam := select(type,indets(ex),name);
if nops(inam)= 1 and op(1,inam) <> _Z then
	RETURN(RootOf(ex,op(1,inam),args[2..nargs]))
fi;
if not has(ex,_Z) then ERROR( `expression independent of`, _Z ) fi;
if ex <> e then RETURN( RootOf(ex,args[2..nargs]) ) fi;

if _EnvRootOfCompose = true and not indexd then
    for rof in indets(ex,specfunc(anything,RootOf)) do
	if type(op(1,rof),polynom(anything,_Z)) and
	   type(ex,polynom(anything,rof)) then
	   RETURN( RootOf( resultant(subs(_Z=z,op(1,rof)), subs(rof=z,ex), z),
		args[2..nargs] ) ) fi
	od
    fi;

# The next three lines added because 
# indets(RootOf(_Z^2-x)) -> {x,RootOf(_Z^2-x)}  (see the test below)
# - MJR 02/91
rofs := indets(ex,specfunc(anything,RootOf)); 
freeze := [seq(rofs[i]=rof[i],i=1..nops(rofs))];
ex := subs(freeze,ex);
if not has(subs(freeze,ex),_Z) then 
	ERROR( `expression independent of`, _Z ) fi;

# If e is not a polynom in _Z (but taking care of nested RootOfs)
#  then do not do anything (e.g. RootOf( _Z*exp(_Z)-x ))
if has( indets({frontend(coeffs,[collect(ex,_Z),_Z])}),_Z) then 
	RETURN( 'RootOf(args)' ) fi;

ex := frontend(subs,[_Z=z,e],[{`+`,`*`},{_Z=z}]);

if degree(ex,z) = 1 then
    if indexd then `RootOf/chkindex`(op(2,args[2]),1) fi; 
    if type(ex,polynom(algfun, z)) then
	 ex1 := traperror(evala(Normal(-coeff(ex,z,0)/coeff(ex,z,1))));
	 if ex1 <> lasterror then
              if nargs=2 and type(args[2],numeric..numeric) then
                 if is(ex1,RealRange(op(1,args[2]),op(2,args[2]))) then
                    RETURN( ex1 )
                 else 
                    # the root is outside the specified range, returns NULL
                    RETURN(NULL) fi;
              fi;
	      RETURN( ex1 );
         else ERROR(`leading coefficient should be invertible`) fi

    elif type(ex,polynom(anything, z)) then
         ex1 := normal( -coeff(ex,z,0) / coeff(ex,z,1) );
         if nargs=2 and type(args[2],numeric..numeric) then
              if is(ex1,RealRange(op(1,args[2]),op(2,args[2]))) then
                 RETURN( ex1 )
              else RETURN(NULL) fi;
         fi;
	 RETURN( ex1 )
	 fi;
    fi;

# if nargs=2 and type(x,complex(numeric)) then
# 	ex1 := convert(x,rational,'exact');
# 	if testeq(subs(z=ex1,ex)) then RETURN( ex1 ) fi
# 	fi;

if type(ex,polynom(algnum,z)) then # Updated MJR 02/91
	if has(ex,'RootOf') then 
	    # Primpart reduces the RootOfs *without* expanding products
	    # and  checks that the RootOf's are independent 
	    ex1 := traperror(evala(Primpart(ex,z)));
	    if ex1 <> lasterror  then
	         ex := ex1;
	         if  degree(ex,z) < 2 then
	 	      RETURN(RootOf(subs(z='_Z',ex),args[2..nargs])); 
	         elif not type(lcoeff(ex,z),'rational') then
	     	      ex := evala(Expand(ex/lcoeff(ex,z)))
		      fi; 
 	    elif has(lcoeff(ex,z),'RootOf')  then		
	         lc := traperror(evala(Expand(1/lcoeff(ex,z))));
	         if lc <> lasterror then ex := ex * lc fi;
	         ex := evala(Expand(ex)); fi
	    fi;
        if indexd then `RootOf/chkindex`(op(2,args[2]),degree(ex,z)) fi; 
	ex := ex/icontent(ex);
	lc := traperror(sign(lcoeff(ex,z)));
	if lc <> lasterror then ex := ex * lc; fi;
	ex := collect(subs(z=_Z,ex),_Z,normal);
	if not type(ex,`^`) or indexd then
		RootOf(ex,args[2..nargs]) := 'RootOf'(ex,args[2..nargs]); fi;
	RETURN( RootOf(ex,args[2..nargs]) ) fi;

if type(ex,polynom(algfun,z)) then  # MJR 02/91
	rofs := indets(ex,'function');
	rofs := map(proc(x) if op(0,x) = 'RootOf' then x fi end,rofs);
	inds := inam minus select(type,indets(rofs),name) minus {'_Z'};	
        if nops(rofs) > 0 then
	    # Primpart reduces the RootOfs *without* expanding products
	    # and  checks that the RootOf's are independent 
 	    ex1 := traperror( evala(Primpart(ex,z)) ); 
	    if ex1 <> lasterror  then
	         ex := ex1;
	         lc := lcoeff(lcoeff(ex,z),inds);
	         if degree(ex,z) < 2 then
	              RETURN(RootOf(subs(z='_Z',ex),args[2..nargs]));
	         elif has(lc,'RootOf') then
	   	      ex := evala(Expand(ex/lc))
		      fi; 
	    elif has(lcoeff(lcoeff(ex,z),inds),'RootOf') then
	         lc := lcoeff(lcoeff(ex,z),inds);
	         lc := traperror(evala(Expand(1/lc)));
	         if lc <> lasterror then ex := ex * lc fi;
	         ex := evala(Expand(ex)); fi
		 fi;   
        if indexd then `RootOf/chkindex`(op(2,args[2]),degree(ex,z)) fi; 
        # remove denominator (algebraic indets are viewed as constants
	# by `evala/Primpart`) 
        ex := primpart(ex,z); 
	lc := traperror(sign(lcoeff(lcoeff(ex,z),inds)));
	if lc <> lasterror then ex := ex * lc  fi;
	if type(ex,`+`)  then ex := collect(ex,z,normal) fi;
	ex := subs(z=_Z,ex);
        if not type(ex,`^`) or indexd then
                RootOf(ex,args[2..nargs]) := 'RootOf'(ex,args[2..nargs]); fi;
        RETURN( RootOf(ex,args[2..nargs]) ) fi;

if type(ex,polynom(anything,z)) then
	ex := traperror(primpart(ex,z));
        if indexd then `RootOf/chkindex`(op(2,args[2]),degree(ex,z)) fi; 
	if ex <> lasterror then
		ex := collect(ex,z);
		err := traperror(sign(lcoeff(ex,z)));
		if err = -1 then ex := - ex fi;
		ex := subs(z=_Z,ex);
        	if not type(ex,`^`) or indexd then
                	RootOf(ex,args[2..nargs]) := 'RootOf'(ex,args[2..nargs]); fi;
        	RETURN( RootOf(ex,args[2..nargs]) ) fi; fi;


'RootOf(args)'
end:
protect(RootOf);

`RootOf/selector` := proc(e,indexd)
local i;
option `Copyright (c) 1997 Waterloo Maple Inc. All rights reserved.`;
i := false;
if type(e,equation) then
    if op(1,e)='index' then
        if type(op(2,e),And(constant,Not(integer))) then
            ERROR(`a constant index should be an integer`)
        fi;
        i := true;
    elif op(1,e) <> 'label' then
ERROR(`left-hand side of the selector must be the name index or label`)
    fi;
elif not (type(e,constant) or type(e, range)) then
    ERROR(`selector must be a constant, a numeric range or an equation`)
fi;
if nargs>1 then indexd := i fi
end:

`RootOf/chkindex` := proc(e,n)
option `Copyright (c) 1997 Waterloo Maple Inc. All rights reserved.`;
if type(e,integer) and (e<1 or e>n) then
	ERROR(`index should be a positive integer less than`,n+1) fi
end:

#savelib('RootOf',`RootOf/selector`,`RootOf/chkindex`):
