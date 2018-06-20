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
##    Title:	`dev/cleanup`
##    Created:	Thu Sep 19 20:18:54 1991
##    Author:	Bruno Salvy
##	<salvy@rully.inria.fr>
##
## Description: given an expansion in the dev format, strips off the terms
## with coefficient 0.

`dev/cleanup`:=proc (dev)
local i, a, b;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if not type(dev,list) then
	if zerotest(dev)=true then 0 else dev fi
    else
	a:=map(procname,[seq(dev[2*i+2],i=0..iquo(nops(dev),2)-1)]);
	for i to nops(a) do
	    if op(i,a)=0 then b[i]:=NULL
	    elif type(op(i,a),[anything,anything,0]) then
		b[i]:=op(i,a)[2],dev[2*i+1]
	    else b[i]:=op(i,a),dev[2*i+1] fi
	od;
	b:=seq(b[i],i=1..nops(a));
	if b=NULL then 0 else [op(1,dev),b] fi
    fi
end: # `dev/cleanup`

#savelib( `dev/cleanup`);
