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
##    Title:	evenpart
##    Created:	Thu Sep 19 20:35:56 1991
##    Author:	Bruno Salvy
##	<salvy@rully.inria.fr>
##
## Description: returns the evenpart of an expansion. The argument should
## be a Puiseux series.

`dev/evenpart`:=proc (dev)
local a, n, i;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if not type(dev,list) then dev # a constant function is even
    elif op(1,dev)<>1 then ERROR(`a Puiseux series is expected`)
    else
	n:=iquo(nops(dev),2)-1;
	for i to n do
	    if type(op(2*i+1,dev),even) then a[i]:=op(2*i,dev),op(2*i+1,dev)
	    else a[i]:=NULL fi
	od;
	[op(1,dev),seq(a[i],i=1..n),op(2*n+2,dev),op(2*n+3,dev)]
    fi
end: # `dev/evenpart`

#savelib( `dev/evenpart`);
