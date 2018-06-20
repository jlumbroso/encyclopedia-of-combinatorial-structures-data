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
##    Title: 	dev/divide
##    Created:	1988
##    Author: 	Bruno Salvy
##		<bruno.salvy@inria.fr>
##
## Description: This procedure is only used by the Luo system.

`dev/divide`:=proc(dev1,dev2,p)
local res;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    _EnvXasyinteger:=true;
    if not has(dev1,_saddlepoint) then
	res:=`dev/prd`(dev1,`dev/pow`(dev2,-1,p))
    else
	res:=[_saddlepoint,dev1[2],`dev/prd`(dev1[3],`dev/pow`(dev2[3],-1,p))]
    fi;
    RETURN(res)
end:

#savelib( `dev/divide`);
