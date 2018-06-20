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
##    Title: 	dev/instanc
##    Created:	1989
##    Author: 	Bruno Salvy
##		<bruno.salvy@inria.fr>
##
## Description: translates from the internal representation into a sum of
## products.

`dev/instanc`:=proc(expr)
local i, res, lim, var;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if not type(expr,list) then res:=expr
    else
	if expr[nops(expr)]<>infinity then
	    lim:=nops(expr)
	else
	    lim:=nops(expr)-2
	fi;
	res:=0;
	var:=_Xasytab[expr[1]];
	for i from 2 by 2 to lim do
	    if type(expr[i],list) then
		res:=res+`dev/instanc`(expr[i])*var^expr[i+1]
	    else
		res:=res+expr[i]*var^expr[i+1]
	    fi
	od
    fi
end:

#savelib( `dev/instanc`);
