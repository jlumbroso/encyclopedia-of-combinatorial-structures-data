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
##    Title: 	dev/multbyreal
##    Created:	Tue Jul 12 11:25:10 1994
##    Author: 	Bruno Salvy
##		<salvy@rully.inria.fr>
##
## Description: external product

`dev/multbyreal`:=proc(tree,r)
local i,res;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if r=0 then 0
    elif type(tree,undefined) then undefined
    elif not type(tree,list) then r*tree
    else
	for i from 2 by 2 to nops(tree) do
	    res[i]:=`dev/multbyreal`(tree[i],r),tree[i+1]
	od;
	[tree[1],seq(res[2*i],i=1..iquo(nops(tree),2))]
    fi
end:

#savelib( `dev/multbyreal`);
