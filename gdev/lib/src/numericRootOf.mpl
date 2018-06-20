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

## 	-*-Maple-*-
##
##	Title:	numericRootOf
##	Created:    Tue Feb 11 20:01:36 1992
##	Author: Bruno Salvy
##	<salvy@rully>
##
## Description: given a polynomial P in _Z and a numerical approximation val
## to the root, returns	 RootOf(Q,val) where Q is the irreducible factor of
## P having the root val.

numericRootOf:=proc (p,val)
local k, l, j;
option `Copyright Bruno Salvy, INRIA, France`;
    if not type(p,polynom) or irreduc(p) then RootOf(p,val)
    else k:=[seq(op(1,l),l=op(2,factors(p)))];
	if nops(k)>1 then
	    l:=map(abs,map(evalc,map(evalf,subs(_Z=val,k))));
	    member(min(op(l)),l,'j');
	    op(1,[RootOf(op(j,k),val),0])
	else op(1,[RootOf(op(k),val),0])
	fi
    fi
end: # numericRootOf

#savelib( numericRootOf);
