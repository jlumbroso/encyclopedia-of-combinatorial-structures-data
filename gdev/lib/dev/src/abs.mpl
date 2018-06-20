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
##    Title: 	dev/abs
##    Created:	1987
##    Author: 	Bruno Salvy
##		<Bruno.Salvy@inria.fr>
##
## Description: expansion of a modulus

`dev/abs`:=proc(dev,n)
local impart, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(dev,undefined) then undefined
    elif not type(dev,list) then evalc(abs(dev))
    else
	impart:=`dev/impart`(dev);
	if not has(impart,I) then
	    sig:=evalr(Signum(`dev/lcoeff`(dev)));
	    if sig=1 then dev
	    elif sig=-1 then `dev/multbyreal`(dev,-1)
	    elif sig=0 then [0]
	    else FAIL
	    fi
	else
	    `dev/pow`(`dev/add`(`dev/pow`(`dev/realpart`(dev),2,n),
				`dev/pow`(subs(I=1,impart),2,n)),1/2)
	fi
    fi
end:


#savelib(`dev/abs`);
