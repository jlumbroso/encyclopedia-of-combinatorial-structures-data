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
# $Source: /u/maple/research/lib/evalr/src/RCS/evalr,v $
##
##    Title: 	evalr/evalr
##    Created:	Aug 1988
##    Author: 	Bruno Salvy
##
## Description: parser.
#
# DEGH (Jun/92 - handle "indexed" functions and detect non-implemented 
#       multivariate cases of implemented univariate functions, e.g., GAMMA)
#

macro(    signumin=`evalr/signumin`,
    signumax=`evalr/signumax`,
    signumabs=`evalr/signumabs`);

macro (idx_fns = {'Psi','LambertW','erfc','Ei'},
       notimpl_fns = {'GAMMA','Beta','Zeta'});

`evalr/evalr`:=proc(expr,exact)
    local i, f, r, idx;
    option remember,
      `Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
    if has(expr,FAIL) then RETURN(FAIL) fi;
    if nops(map(proc(x) op(0,x) end,indets(expr,function))
       intersect{'min','max','abs','Signum'})=0  and type(expr,'realcons') then
	if has(expr,infinity) then
            r := `simplify/infinity`(expr);
            'INTERVAL'(r..r);
          elif exact then
            'INTERVAL'(expr..expr);
          else
	    r := evalf(expr);
	    'INTERVAL'(r..r);
        fi;
      elif type(expr,`+`) then
        r := `evalr/evalr`(op(1,expr),exact);
        for i from 2 to nops(expr) do
            r := `evalr/add`(r,`evalr/evalr`(op(i,expr),exact));
        od;
      elif type(expr,`*`) then
        r := `evalr/evalr`(op(1,expr),exact);
        for i from 2 to nops(expr) do
            r := `evalr/prd`(r,`evalr/evalr`(op(i,expr),exact),exact);
        od;
      elif type(expr,`^`) then
        if type(op(2,expr),'rational') then
            `evalr/pow`(`evalr/evalr`(op(1,expr),exact),op(2,expr),exact);
          else
	    `evalr/evalr`(exp(op(2,expr)*ln(op(1,expr))),exact);
        fi;
      elif type(expr,'function') and op(0,expr) <> 'INTERVAL' then
        f := op(0,expr);
        if f='Int' or f='int' then
            `evalr/int`(op(expr),exact);
          elif not hastype(expr,'range') and #_Z because of RootOfs
                nops(indets(expr,'name') minus ({constants} union {_Z}))=0 and
                member(f,['min','max','abs','Signum','shake']) then
            if f = 'min' then signumin(op(expr));
              elif f = 'max' then signumax(op(expr));
              elif f = 'abs' then signumabs(op(expr));
              elif f = 'Signum' then `evalr/Signum`(op(expr));
              else `evalr/shake`(op(expr),Digits);
            fi;
	  elif f = 'shake' then
	    FAIL;
          elif f='RootOf' then
            if nops(expr)=2 and type(op(2,expr),'complex(numeric)') then
                `evalr/shake`(op(2,expr),Digits);
              else RETURN(FAIL);
		ERROR(`Not implemented,   evalr/RootOf`);
            fi;
          else 
            f := cat(`evalr/`,f);
            if not type(f,'procedure') then
                ERROR(`Not implemented`,f);
              elif nops(expr)=1 or nops(expr)=2 and
                    member(op(0,expr), idx_fns) then
                # univariate case (including "indexed" functions)
                if nops(expr) = 1 then
                    idx := NULL;
                  else
                    idx := op(1,expr);
                fi;
		f(idx,`evalr/evalr`(op(nops(expr),expr),exact),exact);
	      elif member(op(0,expr), notimpl_fns) then
		ERROR(`multivariate case not implemented`);
	      else
		f(`evalr/evalr`(op(expr),exact),exact);
            fi;
	fi;
      elif type(expr,'name') then
          'INTERVAL'(
        'INTERVAL'(expr, -infinity .. infinity) ..
        'INTERVAL'(expr, -infinity .. infinity))
      elif (type(expr,specfunc(anything,'INTERVAL'))) and
	    map(proc(x) type(x,'range') end,{op(expr)}) = {true} then
        expr;
      elif type(expr,specfunc(anything,'INTERVAL')) and
           type(op(1,expr),'name') and
            map(proc(x) type(x,'range') end,
			{op(subsop(1 = NULL,expr))}) = {true} then
        r := map(`evalr/evalr`,op(2,expr),exact);
        'INTERVAL'(
        'INTERVAL'(op(1,expr), op(1,op(op(1,r))) .. op(2,op(op(2,r)))) ..
          'INTERVAL'(op(1,expr), op(1,op(op(1,r))) .. op(2,op(op(2,r)))))
      else
	ERROR(`Invalid type`,expr);
    fi;
end:

`evalr/signumin`:=proc ()
    local i, lexp, r;
    option 
      `Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
    if nargs=1 then RETURN(args) fi;
    if has([args],infinity) then
        if member(-infinity,[args]) then RETURN(-infinity)
          elif member(infinity,[args],'i') then
            RETURN(signumin(op(subsop(i=NULL,[args]))));
          else
            r := `simplify/infinity`(min(args));
            if type(r,'function') and op(0,r) = 'min' then
                lexp := [op(r)];
              else
		lexp := [r];
            fi;
        fi;
      else
	lexp := [args];
    fi;
    r := {lexp[1]};
    for i from 2 to nops(lexp) do
        r := map(proc(x,y) local s;
			s:=evalr(Signum(y-x)); if s=1 or s=0 then x
			  elif s=-1 then y else x, y fi end, r, lexp[i]);
    od;
    min(op(r));
end:

`evalr/signumax`:=proc ()
    local i, lexp, r;
    option 
      `Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
    if nargs=1 then RETURN(args) fi;
    if has([args],infinity) then
        if member(infinity,[args]) then RETURN(infinity);
          elif member(-infinity,[args],'i') then
            RETURN(signumax(op(subsop(i=NULL,[args]))));
          else
            r := `simplify/infinity`(max(args));
            if type(r,'function') and op(0,r) = 'max' then
                lexp := [op(r)];
              else
		lexp := [r];
            fi;
        fi;
      else
	lexp := [args];
    fi;
    r := {lexp[1]};
    for i from 2 to nops(lexp) do
        r := map(proc(x,y) local s;
			s:=evalr(Signum(y-x)); if s=-1 or s=0 then x
                          elif s=1 then y else x, y fi end, r, lexp[i]);
    od;
    max(op(r));
end:

`evalr/signumabs`:=proc (expr)
    local r;
    option 
      `Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
    r := evalr(Signum(expr));
    if r = 1 then expr;
      elif r = 0 then 0;
      elif r = -1 then -expr;
      else abs(expr);
    fi;
end:

#savelib('signumin','signumax','signumabs','`evalr/evalr`'):
