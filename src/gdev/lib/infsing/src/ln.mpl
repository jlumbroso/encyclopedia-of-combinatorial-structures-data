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
##    Title:	`infsing/ln`
##    Created:	Thu May 23 18:58:45 1991
##    Author:	Bruno Salvy
##	<salvy@rully.inria.fr>
##
## Description: smallest singularities of fct in the variable var
##  with modulus larger than singmin, p being the order of intermediate
##  expansions
`infsing/ln`:=proc (fct,var,sm)
local locval, val, singmin, expr;
    singmin:=sm;
    expr:=op(fct);
    do
	locval:=infsing(expr,var,singmin);
	val:=[`infsing/trie`(`infsing/infsolve`(expr,var,false,singmin),locval[1],fct,var,
	    'log','log'),false];
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
end: # `infsing/ln`

#savelib( `infsing/ln`);
