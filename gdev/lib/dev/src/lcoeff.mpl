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
##    Title: 	dev/lcoeff
##    Created:	1991
##    Author: 	Bruno Salvy
##		<salvy@rully.inria.fr>
##
## Description: leading coefficient of an expansion

`dev/lcoeff`:=proc(dev)
local u;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if not type(dev,list) then dev
    elif nops(dev)=3 then
	if not type(op(2,dev),list) then FAIL
	else `dev/lcoeff`(op(2,dev))
	fi
    else
	u:=dev;
	while type(u,list) do u:=op(2,u) od;
	u
    fi
end:

#savelib( `dev/lcoeff`);
