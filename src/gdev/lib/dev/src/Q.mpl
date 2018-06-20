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
##    Title: 	dev/Q, dev/L
##    Created:	1988
##    Author: 	Bruno Salvy
##		<bruno.salvy@inria.fr>
##
## Description:  This is only used in the Luo system.
## These functions define the expansions
## of the quasi-inverse (Q) and of the quasi-logarithm (L).
## Few test are performed, since the input is supposed to be ok.

`dev/QuasiInverse`:=proc(u,n)
local init, sig, i;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if u[3]=0 then
	if not type(u[2],list) and zerotest(u[2]-1)<>false then
#	    if u[2]=1 then
		`dev/pow`(`dev/multbyreal`(subsop(2=NULL,3=NULL,u),-1),-1,n)
#	    else
#		`dev/pow`(`dev/multbyreal`(
#		    subs(u[2]=1,subsop(2=NULL,3=NULL,u)),-1),-1,n)
#	    fi
	elif not type(u[2],list) then
#	    init:=evalc(1/(1-u[2]));
	    init:=1/(1-u[2]);
	    `dev/multbyreal`(`dev/QuasiInverse`(`dev/multbyreal`(subsop(2=NULL,3=NULL,u),
		    init),n),init)
	else
	    `dev/pow`(`dev/add`(`dev/multbyreal`(u,-1),1),-1,n)
	fi
    else
	sig:=evalr(Signum(u[3]));
	if sig=-1 then
	    `dev/pow`(`dev/add`(`dev/multbyreal`(u,-1),1),-1,n)
	elif sig=FAIL then ERROR(FAIL)
	else `dev/endofdev`(u,n+1,map(op,[seq([1,i],i=0..n+1)]))
	fi
    fi
end: #`dev/QuasiInverse`

#savelib(`dev/QuasiInverse`);
