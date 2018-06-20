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

# $Source: /u/maple/research/lib/evalc/src/RCS/LambertW,v $
# $Notify: deghare@maplesoft.com $
#
# DEGH (92)
# modif BS. Aug 01.
#  k integer, x>0
#  k>0  evalc(LambertW(-k,+x)) --> conjugate(LambertW(k  ,+x))
#  k>1  evalc(LambertW(-k,-x)) --> conjugate(LambertW(k-1,-x))
#  k=1, x<-exp(-1) evalc(LambertW(-1,x)) --> conjugate(LambertW(0,-x))

macro ( unsplit = `evalc/unsplit`,
	split   = `evalc/split`,
	stdcx   = `evalc/stdcx`);

`evalc/LambertW` := proc()
    local a, n, x, y, s;
    option 
        `Copyright (c) 1992 by the University of Waterloo. All rights reserved.`;

    if nargs = 1 or nargs = 2 and args[1] = 0 then
        a := `evalc/evalc`(args[nargs]);

        if type(a,'unsplit(algebraic)') then
	    unsplit('LambertW'(op(a)));

          else
            x := op(1,a);
            y := op(2,a);

	    if type(x+I*y,'complex(float)') then
		`evalc/evalc`(LambertW(x+I*y));

              elif y <> 0 then
	        unsplit(LambertW(stdcx(a)));

	      elif member(signum(x+1/exp(1)),{0,1}) then
		split(LambertW(x),0);

	      else
		unsplit(LambertW(x));
	    fi;
	fi;

      elif nargs = 2 then
	n := args[1];
        a := `evalc/evalc`(args[2]);

        if not type(n,'integer') or type(a,'unsplit(algebraic)') then
	    unsplit('LambertW'(n, stdcx(a)));

          else
            x := op(1,a);
            y := op(2,a);

	    if y = 0 and n < 0 then
		s:=signum(x);
		if s=1 then RETURN(`evalc/conjugate`(`evalc/LambertW`(-n,x)))
		elif n<-1 and s=-1 or
			n=-1 and s=-1 and signum(x+1/exp(1))=-1 then 
	  	    RETURN(`evalc/conjugate`(`evalc/LambertW`(-n-1,x)))
		fi
	    fi;

	    if type(x+I*y,'complex(float)') then
		`evalc/evalc`(LambertW(n, x+I*y));

              elif y <> 0 then
	        unsplit(LambertW(n, stdcx(a)));

	      elif n = -1 and signum(x) = -1 and
		    member(signum(x+1/exp(1)),{0,1}) then
		split(LambertW(-1,x),0);

	      else
		unsplit(LambertW(n,x));
	    fi;
	fi;

      else
	error "expecting 1 or 2 arguments, got %1", nargs;
    fi;
end:

#savelib('`evalc/LambertW`'):
