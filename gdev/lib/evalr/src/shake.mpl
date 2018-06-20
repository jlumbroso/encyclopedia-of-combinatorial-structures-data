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
# $Source: /u/maple/research/lib/evalr/src/RCS/shake,v $
#
#    Title:    `evalr/shake`
#    Created:   Aug 88
#    Author:    Bruno Salvy
#
# DEGH (Jun/92 - handle "indexed" functions)
#
# added option remember to shake/shake (!!!!)

`evalr/shake` := proc(expr,dig)
    local s;
    # expr is assumed to be a real constant
    # if not type(expr,'realcons') then ERROR(`Not a real constant`,expr) fi;
    option 
        `Copyright (c) 1992 by the University of Waterloo. All rights reserved.`;
    Digits := dig+2;
    s := traperror(`shake/shake`(expr,Float(10,-dig)));
    subsop(4=NULL,op(`shake/shake`));
    if s=lasterror then
        ERROR(s);
      else
        RETURN(s);
    fi;
end:

macro (idx_fns = {'Psi','LambertW','erfc','Ei'});

`shake/shake` := proc(expr,ampl)
    local r, i, res, expr2, f, s, idx;
    option 
        `Copyright (c) 1992 by the University of Waterloo. All rights reserved.`,remember;

    #if type(expr,'integer') or type(expr,'fraction') then [expr..expr];
    if type(expr,'numeric') or member(expr,{constants}) then
        if expr=I then
	    ERROR(FAIL);
          elif evalf(expr)>0 then
            'INTERVAL'(evalf(expr*(1-ampl))..evalf(expr*(1+ampl)));
          else
            'INTERVAL'(evalf(expr*(1+ampl))..evalf(expr*(1-ampl)));
        fi;

      elif type(expr,`+`) then
        r := `shake/shake`(op(1,expr),ampl);
        for i from 2 to nops(expr) do
            r := `evalr/add`(r,`shake/shake`(op(i,expr),ampl),false);
        od;

      elif type(expr,`*`) then
        r := `shake/shake`(op(1,expr),ampl);
        for i from 2 to nops(expr) do
            r := `evalr/prd`(r,`shake/shake`(op(i,expr),ampl),false,false);
        od;

      elif type(expr,`^`) then
        if type(op(2,expr),'rational') then
            expr2 := `shake/shake`(op(1,expr),ampl);
            res := 'INTERVAL'();
            for i to nops(expr2) do
                if op([i,1],expr2) <> op([i,2],expr2) then
                    r := 'INTERVAL'(op(i,expr2));
                  else
                    r := 'INTERVAL'(op([i,1],expr2)-ampl..
                                    op([i,2],expr2)+ampl);
                fi;
                res := 'INTERVAL'(op(res),op(`evalr/pow`(r,op(2,expr),false)));
            od;

          else
            `shake/shake`(exp(op(2,expr)*ln(op(1,expr))), ampl);
        fi;

      elif type(expr,'function') then
        if op(0,expr)='RootOf' then
            # a special case for RootOf with two arguments
            if nops(expr)=2 and type(op(2,expr),numeric) then
                `shake/shake`(evalf(expr),ampl);
              else
		ERROR(FAIL);
            fi;

          elif op(0,expr)='abs' and type(op(expr),'RootOf') and 
		nops(op(expr))=2 then
            # another special case for abs(RootOf(2 args))
            if not has(op(2,op(expr)),I) then
                `shake/shake`(abs(op(2,op(expr))), 
			max(subsop(1=1,op(2,op(expr))),ampl));
              else
                `shake/shake`(sqrt(coeff(op(2,op(expr)),I,0)^2+
                	coeff(op(2,op(expr)),I,1)^2),
			max(subsop(1=1,coeff(op(2,op(expr)),I,0)),
                        subsop(1=1,coeff(op(2,op(expr)),I,1)),ampl));
            fi;

          elif op(0,expr)='abs' and hastype(op(expr),'RootOf') and 
                hastype(op(expr),'float') then
            # dirtier and dirtier...
            `shake/shake`(eval(subs('RootOf'=
			proc(x,y) if nargs=2 then y;
                                    else RootOf(x) fi end,
				expr)),ampl);

          else
	    f := cat(`evalr/`,op(0,expr));
            if not type(f,'procedure') then
                ERROR(`Not implemented`,f);

              elif nops(expr) = 1 or nops(expr) = 2 and 
		    member(op(0,expr), idx_fns) then
                # univariate case (including "indexed" functions)
		if nops(expr) = 1 then
		    idx := NULL;
		  else
		    idx := op(1,expr);
		fi;

                expr2 := `shake/shake`(op(nops(expr),expr),ampl);
                res := 'INTERVAL'();
		
                for i to nops(expr2) do
                    if op([i,1],expr2)<>op([i,2],expr2) then
                        r := 'INTERVAL'(op(i,expr2));
                      else
                        r := 'INTERVAL'(op([i,1],expr2)-ampl..
                                        op([i,2],expr2)+ampl);
                    fi;
                    s := f(idx,r,false);
		    if s=FAIL then error(FAIL) fi;
                    if nops(s)=1 then
			res := 'INTERVAL'(op(res),
				evalf(op(1,op(s)))..evalf(op(2,op(s))));
                      else
			# some evalf should be used here too?
			res := 'INTERVAL'(op(res), op(s));
                    fi;
                od;

              else
		ERROR(`multivariate case not implemented`);
            fi;
	fi;

      elif type(expr,specfunc(anything,'INTERVAL')) and nops(expr)=2 and 
        type(op(1,expr),'name') 
        and type(op(2,expr),'range') then
        subsop(1=NULL,expr);

      else
	ERROR(`Invalid type`,expr);
    fi;
end:

#savelib('`evalr/shake`','`shake/shake`'):
