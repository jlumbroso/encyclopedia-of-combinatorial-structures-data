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
`equivalent/equiv`:=proc(fct,p,x)
local sing, locfct, dev, ndev, k, hhs, infs, ty;
option remember;
    locfct:=fct;
    infs:=infsing(locfct,x,0);
    sing:=infs[1];
    ty:=infs[2];
    if sing<>[] and sing[1]<>infinity then
	`equivalent/equsing`(locfct,sing,p,p,x,ty)
    else
	try 
	    if type(normal(locfct),polynom(anything,x)) then
	    RETURN(0) fi
	end try;
	userinfo(1,'equivalent',`No singularities found`);
	if has(locfct,RootOf) then ERROR(`giving up`) fi;
	# check for stupid users whose functions are not regular at 0.
	dev:=`dev/dev`(subs(x=1/_Xasy,locfct),3,3);
	if type(dev,constant) or nops(dev)=3 then RETURN(0) fi;
	ndev:=nops(dev);
	if not type(dev,list(algebraic)) or not type({seq(op(2*k+1,dev),
	    k=1..iquo(ndev,2))},set(integer)) or has(dev,I)
	    then
#	    or	(dev[3]=0 and signum(dev[2])=1) then
	    ERROR(`Not regular at 0`,fct)
	fi;
	hhs:=`equivalent/saddlepoint/H_HS`(locfct,x);
	if hhs='HS' then
	    dev:=`equivalent/saddlepoint/HarrisSchoenfeld`(locfct,p,x,infinity);
	    if _NBSADDLEPOINTS=1 then [_saddlepoint,infinity,dev]
	    else [_saddlepoint[1],infinity,dev]
	    fi
	elif hhs='H' then
	    dev:=`equivalent/saddlepoint/Hayman`(locfct,x,infinity);
	    if has(dev,_saddlepoint) then
		if _NBSADDLEPOINTS=1 then [_saddlepoint,infinity,op(2,dev)]
		else [_saddlepoint[1],infinity,op(2,dev)]
		fi
	    else op(2,dev)
	    fi
	elif type(frontend(expand,[subs(Q=1/(1-x),L=ln(1/(1-x)),locfct)]),
	    polynom(anything,x)) then
	    RETURN([0])
	else
	    [_saddlepoint,infinity,`equivalent/saddlepoint/selle`(locfct,x,infinity)]
	fi;
    fi
end:

#savelib(`equivalent/equiv`);
