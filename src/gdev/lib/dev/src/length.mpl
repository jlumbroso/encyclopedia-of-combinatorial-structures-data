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
##    Title:	`dev/length`
##    Created:	Sun Aug 13 12:44:26 1989
##    Author:	Bruno Salvy
##	<bsalvy@watmum.waterloo.edu>
##
## Description: Returns the number of terms in an expansion.

`dev/length`:=proc (dev)
local i;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if not type(dev,list) then 1
    else add(`dev/length`(dev[2*i]),i=1..iquo(nops(dev),2))
    fi
end: # `dev/length`

#savelib( `dev/length`);
