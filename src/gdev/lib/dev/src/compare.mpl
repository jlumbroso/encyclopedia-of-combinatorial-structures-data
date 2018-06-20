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
##    Title:	`dev/compare`
##    Created:	Jun-Aug 1988
##    Author:	Salvy Bruno
##
##  Compares two expansions Dev1 and Dev2.
##  Returns +1,-1 or 0.
##  0 means that the expansions are equal (even the multiplicative
##  constant).
##  This function is called by `dev/comparexplog` whose only user is
##  `dev/exp` and by `dev/int`, thus here the constants cannot be functions 
##  of _X[].

`dev/compare`:=proc(Dev1,Dev2)
local i, dev1, dev2, n1, n2, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if not type(Dev1,list) then
	if not type(Dev2,list) then
	    evalr(Signum(Dev1-Dev2))
	else
	    sig:=evalr(Signum(Dev2[3]));
	    if sig=1 then
		sig:=evalr(Signum(Dev1));
		if sig<>0 then sig else -`dev/compare`(Dev2[2],0) fi
	    elif sig=-1 then
		-`dev/compare`(Dev2[2],0)
	    elif sig=0 then
		`dev/compare`(Dev1,Dev2[2])
	    else ERROR(FAIL)
	    fi
	fi
    else
	if not type(Dev2,list) then
	    sig:=evalr(Signum(Dev1[3]));
	    if sig=1 then
		sig:=evalr(Signum(Dev2));
		if sig<>0 then -sig else `dev/compare`(Dev1[2],0) fi
	    elif sig=-1 then
		`dev/compare`(Dev1[2],0)
	    elif sig=0 then
		`dev/compare`(Dev1[2],Dev2)
	    else
		ERROR(FAIL)
	    fi
	else
	    if Dev1[1]<Dev2[1] then
		dev1:=Dev1;
		dev2:=[Dev1[1],Dev2,0]
	    elif Dev1[1]>Dev2[1] then
		dev1:=[Dev2[1],Dev1,0];
		dev2:=Dev2
	    else
		dev1:=Dev1;dev2:=Dev2
	    fi;
	    n1:=nops(dev1);n2:=nops(dev2);
	    for i from 3 by 2 to min(n1,n2) do
		sig:=evalr(Signum(dev1[i]-dev2[i]));
		if sig=-1 then
		    RETURN(`dev/compare`(dev1[i-1],0))
		elif sig=1 then
		    RETURN(`dev/compare`(0,dev2[i-1]))
		elif sig=0 then
		    sig:=`dev/compare`(dev1[i-1],dev2[i-1]);
		    if sig<>0 then
			RETURN(sig)
		    fi
		else
		    ERROR(FAIL)
		fi
	    od;
	    if n1=n2 then RETURN(0)
	    elif n1<n2 then
		`dev/compare`(0,dev2[n1+1])
	    else `dev/compare`(dev1[n2+1],0)
	    fi
	fi
    fi
end:

#savelib( `dev/compare`);
