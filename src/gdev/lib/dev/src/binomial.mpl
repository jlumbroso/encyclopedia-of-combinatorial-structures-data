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
##    Title: 	dev/binomial
##    Created:	1987
##    Author: 	Bruno Salvy
##		<bruno.salvy@inria.fr>
##
## Description: expansion of binomials

`dev/binomial`:=proc (u,v,n)
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) or type(v,undefined) then undefined
    else 
	`dev/prd`(`dev/GAMMA`(`dev/add`(u,1),n),
	`dev/pow`(`dev/prd`(`dev/GAMMA`(`dev/add`(v,1),n),
	`dev/GAMMA`(`dev/add`(u,`dev/add`(`dev/multbyreal`(v,-1),1)),n)),-1,n))
    fi
end: # `dev/binomial`

#savelib( `dev/binomial`);
