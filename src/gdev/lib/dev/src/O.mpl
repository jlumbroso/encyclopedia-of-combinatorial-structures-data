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
##    Title: 	`dev/O`
##    Created:	1987
##    Author: 	Bruno Salvy
##		<Bruno.Salvy@inria.fr>
##

`dev/O`:=proc(dev)
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(dev,undefined) then undefined
    elif not type(dev,list) then 1
    else [dev[1],`dev/O`(dev[2]),dev[3]]
    fi
end:

#savelib(`dev/O`);
