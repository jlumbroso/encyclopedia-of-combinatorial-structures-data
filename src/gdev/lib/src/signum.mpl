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
# $Source: /u/maple/research/lib/src/RCS/signum,v $
# $Notify: deghare@uwaterloo.ca $
# signum:  Compute the "signum" ("sign") of a real or complex number.
#
# signum takes 1, 2 or 3 arguments, with the following definitions:
#
#    signum(x) :  Computes the sign of the complex-valued expression x as
#                 follows:
#                                   /  x / abs(x)    if x <> 0
#                      signum(x) = <
#                                   \  _Envsignum0   if x = 0 and _Envsignum0
#                                                            is assigned
#
#                 The default value of _Envsignum0 is "undefined".  This
#		  means that any transformations valid everywhere except at
#		  0 may be applied (e.g., signum(abs(x)) -> 1 and
#		  signum(-abs(x)) -> -1).  _Envsignum0 can be changed either
#                 by explicit assignment, or by using the 3-argument form
#		  of signum (see below).
#
#    signum(n,x) :  Computes the n'th derivative of signum at x.  This is
#                 defined only for non-0 real numbers.  Note that if n is
#                 any positive integer, then signum(n,x) = signum(1,x).
#                 signum(0,x) = signum(x) .
#
#    signum(0,x,s) :  Computes signum(x) subject to _Envsignum0 = s .  Note
#                 that x is evaluated before this assignment to _Envsignum0
#                 becomes effective.
#
# NOTE: signum does NOT assume that variables are real.
#
# KOG   (Apr 86)
# MBM   (Feb 87)
# GHG   (Nov 87)
# B.Salvy  (Aug 89: use interval arithmetic instead of floating-point
#	evaluation.)
# DEGH  (Sep 91: floating-point evaluation completely removed-- it is too
#	easily defeated; interval arithmetic is done at current setting
#       of Digits if x is of type constant; derivative)
# DEGH  (Jan 93: signum(0) is an environment variable; removed assumption
#       that variables are real)
# MBM   (Aug 93: slight correction for the handling of sums)
# DEGH  (Oct 94: allow signum/f routines to return FAIL)
#
# modif BS. Changed type/constant into type/myconstant
# so that non-algebraic RootOf's are catched.
# modif BS. Changed it to use type/realcons

unprotect('signum'):
signum := proc(a1::{algebraic,algebraic..algebraic},
		a2::{algebraic,algebraic..algebraic}, a3::algebraic)
    local n, x, r;
    option `Copyright (c) 1993 by Waterloo Maple Inc. All rights reserved.`;

    if nargs = 1 then
	x := a1;

	if type(x,'complex(numeric)') then
	    if x = 0 then
		subs('_Envsignum0'=0, eval(_Envsignum0,1));

	      elif type(x,'numeric') then
		sign(x);

              elif type(x,'complex(float)') then
                evalf(evalf(x/abs(x), Digits+2));

              else
                x/abs(x);

	    fi;

	  elif not assigned(_Envsignum0) then
	    subs(_Envsignum0=0,`signum/main`(x,_Envsignum0));

	  else
	    _Envsignum0 := freeze(eval(_Envsignum0,1));
	    thaw(`signum/main`(x,_Envsignum0));
	fi;

      elif nargs = 2 then
	n := a1;
	x := a2;

	if n = 0 then
	    signum(x);

	  elif type(n,'posint') then
	    if x = 0 then
		ERROR(`signum is not differentiable at 0`);

	      elif type(x,'numeric') or member(signum(0,x,0),{-1,1}) or
		    Im(x) = 0 and traperror(is(x <> 0)) = true then
		0;

	      elif type(x,'complex(numeric)') then
		ERROR(`signum is not differentiable at complex values`);

	      else
		'signum'(1,x);
	    fi;

	  elif type(n,'complex(numeric)') then
	    ERROR(`numeric 1st argument should be 0 or 1`);

	  else
	    'signum'(n,x);
	fi;

      elif nargs = 3 then
	n := a1;
	x := a2;

	if n = 0 then
	    if type(x,'complex(numeric)') then
		if x = 0 then
		    a3;
		  elif type(x,'numeric') then
		    sign(x);
		  else
		    x/abs(x);
		fi;

	      else
		_Envsignum0 := freeze(a3);
		r := signum(a2);

		if r = thaw(_Envsignum0) or type(r,'constant') then
		    r;
		  else
		    thaw(eval(subs(signum=(x->'signum'(0,x,_Envsignum0)), r)));
		fi;
	    fi;

	  elif type(n,'posint') then
	    _Envsignum0 := freeze(a3);
	    r := signum(1,x);

	    if r = 'signum'(1,x) then
		'signum'(1,x,thaw(_Envsignum0));
	      else
		r;
	    fi;

	  elif type(n,'complex(numeric)') then
	    ERROR(`numeric 1st argument should be 0 or 1`);

	  else
	    'signum'(args);
	fi;

      else
	ERROR(cat(`expecting 1, 2 or 3 arguments, got `,nargs));
    fi;
end:
protect(signum):

signum(infinity) := 1:
signum(-infinity) := -1:
# signum(1), signum(-1) are extremely common
signum(1) := 1:
signum(-1) := -1:

`signum/main` := proc(x, Env)
    local r, s, t, f, w, xr, xi, sg, k, sgp, sgn;

    option `Copyright (c) 1993 by Waterloo Maple Inc. All rights reserved.`;
    if type(x,'range') then
	s := map(signum,x);

	if s = 1..1 or s = -1..-1 or s = 0..0 then
	    r := op(1,s);
	  else
	    r := 'signum'(x);
	fi;

      elif type(x,`*`) then
	s := 1;
	t := 1;
	for w in x do
	    sg := signum(w);
	    if has(sg,'signum') then
		s := s * w;
	      else
		t := t * sg;
	    fi;
	od;

	if Env = 0 or Env = '_Envsignum0' then
	    if s = 1 then
		r := t;
	      elif s <> x then
		r := t*signum(s);
	    fi;

	  elif t = Env then
	    # These results are still valid if some component of the product
	    # is subsequently set to 0
	    if s = 1 then
		r := t;
	      elif s <> x then
		r := t*signum(s);
	    fi;

	  else
	    # if signum(0) is assigned and is <> 0 then signum does not map
	    # onto products, and only operands which are guaranteed to be
	    # non-zero can be simplified (e.g., signum(-abs(x)) does not
	    # simplify, but signum(-x*y) = signum(-y) if we know x>0 )
	    s := map(proc(x) local s;
		      s := signum(0,x,0);
		      if type(s,'constant') and s <> 0 and
			  indets(s,'function')={} then s;
			else x;
		      fi;
		      end,
		    x);

	    if s <> x then
		r := signum(s);
	    fi;
	fi;

      elif type(x,`^`) and type(op(2,x),'numeric') then
	if type(op(2,x),'even') and
	    (Im(op(1,x)) = 0 and (member(Env, {'_Envsignum0',1}) or
		traperror(is(op(1,x) <> 0)) = true) ) then
	    r := 1;
	  elif type(op(2,x),'odd') and
	    (Im(op(1,x)) = 0 and (member(Env, {'_Envsignum0',0,1,-1}) or
		traperror(is(op(1,x) <> 0)) = true) ) then
	    r := signum(op(1,x));
	  elif member(Env,{'_Envsignum0',0,1}) then
	    # 0 and 1 are the only numbers which equal all their powers
	    r := signum(op(1,x))^op(2,x);
	fi;

      elif type(x,`^`) and signum(op(1,x)) = 1 and Im(op(2,x)) = 0 then
	r := 1;

      elif type(x,'polynom(rational)') and nops(indets(x)) = 1 and
	    degree(x) = 1 and traperror(lcoeff(x))-1 <> 0 and
	    (member(Env, {'_Envsignum0',0}) or abs(lcoeff(x))-1 <> 0) then
	# Only handle linear functions over the rationals, because similar
	# results for more general cases are too messy and don't provide any
	# new information; in particular, the floating point case cannot be
	# normalized in this way due to round-off error
	t := op(indets(x));
	if member(Env, {'_Envsignum0',0}) then
	    r := signum(lcoeff(x)) * signum(t+coeff(x,t,0)/lcoeff(x));
	  else
	    # Can only take out a positive factor in this case
	    r := signum(signum(lcoeff(x))*t+coeff(x,t,0)/abs(lcoeff(x)));
	fi;

      elif type(x,`+`) and traperror(sign(x)) = -1 and 
	    member(Env, {'_Envsignum0',0}) then
	r := -signum(-x);

      elif type(x,'function') and type(op(0,x),'symbol') then
	f := cat(`signum/`,op(0,x));

	if type(f,'procedure') then
	    s := f(op(x));
	    if s <> FAIL then
		r := s;
	    fi;
	fi;
    fi;

    if not assigned(r) and type(x,`+`) then
	# catch the case of a simple sum of the form "real +- inf"
	t := member(infinity,[op(x)],'k') or member(-infinity,[op(x)],'k');

	if t and traperror(is(subsop(k=0,x),real)) = true then
	    r := signum(op(k,x));

	  else
	    s := {};
	    for t in x while nops(s) <= 1 do
		s := s union {signum(t)} minus {0};
	    od;
	    if nops(s) = 1 then
		r := s[1];
	    fi;
	fi;
    fi;

    if not assigned(r) and type(x,'realcons') and not has(x,I) then
	s := `evalr/Signum`(x);
	if s = 'INTERVAL'(0..0) then
	    r := Env;
	  elif s = 'INTERVAL'(1..1) or s = 'INTERVAL'(-1..-1) then
	    r := op(1,op(s));
	  elif type(s,'INTERVAL(numeric..numeric)') then
	    # this means that evalr was able to evaluate to a numeric
	    # range, but the endpoints of the range do not have the
	    # same sign; make sure we don't remember this, so that
	    # increasing Digits will be effective
	    RETURN('signum'(x));
	fi;
    fi;

    if assigned(r) then
	if type(r,'complex(float)') then
	    if frac(r) = 0 then
		trunc(r);
	      else
		r;
	    fi;
	  else
	    procname(args) := r;
	fi;

	RETURN(r);
    fi;

    if traperror(is(x,real)) = true and
	    member(traperror(is(x>0)), {true,false}) then
        sgp := is(x>0);
	if sgp = true then
	    r := 1;

	  else
            sgn := traperror(is(x<0));
            if sgn = true then
	      r := -1;

	    elif (sgp=false and sgn=false and type(x,'constant')) or 
                 traperror(is(x = 0)) = true then
	      if Env = '_Envsignum0' then
		  r := 0;
		else
		  r := Env;
	      fi;

	    elif Env = '_Envsignum0' then
	      if traperror(is(x >= 0)) = true then
		  r := 1;
		elif traperror(is(x <= 0)) = true then
		  r := -1;
	      fi;

	    elif Env = 1 and traperror(is(x >=0)) = true then
	      r := 1;

	    elif Env = -1 and traperror(is(x <= 0)) = true then
	      r := -1;
	  fi;
       fi;
    fi;

    if not assigned(r) and has(x,I) then
	xr := Re(x);

	if not has(xr,{Re,Im}) and xr <> x then
	    # the case xr = x should have been handled before now
	    xi := Im(x);

	    if not has(xi,{Re,Im}) then
                if not type(xr, infinity) and not type(xi, infinity) then
		    if xr = 0 then
			s := abs(xi);
		      elif xi = 0 then
			s := abs(xr);
		      else
			s := sqrt(xr^2+xi^2);
		    fi;
		    if s = 0 then
			r := Env;
		      elif Env = '_Envsignum0' or traperror(is(s <> 0)) = true then
			r := normal(x/s);
		    fi;
                  elif not type(xi, infinity) then
                      # xr is of type infinity
                      r := signum(subs(infinity=1, xr));
                  elif not type(xr, infinity) then
                      # xi is of type infinity
                      r := I*signum(subs(infinity=1, xi));
                  # if xr and xi are of type infinity, give up
                fi;

	    fi;

	fi;
    fi;

    if not assigned(r) then
	procname(args) := 'signum'(x);

      elif type(r,'complex(float)') then
	if frac(r) = 0 then
	    trunc(r);
	  else
	    r;
	fi;

      else
	procname(args) := r;
    fi;
end:

#savelib('signum','`signum/main`'):
