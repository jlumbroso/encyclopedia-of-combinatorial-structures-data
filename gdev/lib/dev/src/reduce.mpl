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
##    Title: 	dev/reduce
##    Created:	1989
##    Author: 	Bruno Salvy
##		<salvy@rully.inria.fr>
##
## Description: Given an expansion dev and an integer k, 
## suppress the terms of order greater than k.


`dev/reduce`:=proc(dev,n)
local i, rest, result, l;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if not type(dev,list) then dev
    elif not hastype({op(dev)},list) then [op(1..min(2*n+3,nops(dev)),dev)]
    elif `dev/length`(dev)<=n+1 then dev
    else
	rest:=n+1;
	for i from 2 by 2 to nops(dev) while rest<>0 do
	    if not type(dev[i],list) then
		result[i]:=dev[i],dev[i+1];
		rest:=rest-1
	    else
		l:=`dev/length`(dev[i]);
		if l<=rest then
		    result[i]:=dev[i],dev[i+1];
		    rest:=rest-l
		else 
		    result[i]:=`dev/reduce`(dev[i],rest-1),dev[i+1];
		    rest:=0
		fi
	    fi
	od;
	if (i=2 or (i=4 and rest=0)) and result[2][2]=0 then result[2][1]
	elif rest=0 then [dev[1],seq(result[2*i],i=1..iquo(i,2)-1)]
	else [dev[1],seq(result[2*i],i=1..iquo(i,2))]
	fi
    fi
end:
 
#savelib( `dev/reduce`);
