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

`infsing/exp`:=proc (fct,var,singmin)
local r, devint, i, exps, cofs;
    r:=infsing(op(1,fct),var,singmin);
    if r[2]<>'log' then subsop(2='essential',r)
    else # the singularity might disappear
	# this expansion has been remembered somewhere else
	devint:=`dev/dev`(subs(var=r[1][1]*(1-1/_Xasy),fct),2,3);
	if devint[1]=1 then
	    cofs:=[seq(devint[2*i],i=1..iquo(nops(devint),2))];
	    if nops(select(type,cofs,list))>1 
		or member(true,map(type,cofs,list),'i') and devint[i+1]<>0 then
		RETURN(subsop(2='essential',r))
	    else devint:=devint[i]
	    fi
	fi;
	if devint[1]=2 then
	    exps:=[seq(devint[2*i+1],i=1..iquo(nops(devint),2))];
	    if {op(exps)} minus {infinity,1}<>{} then subsop(2='essential',r)
	    elif member(1,exps,'i') and not type(devint[2*i],integer) then
		subsop(2='algebraic',r)
	    elif type(devint[2*i],posint) then subsop(2='polar',r)
	    else # not a singularity !
		`infsing/exp`(fct,var,abs(r[1][1]))
	    fi
	else subsop(2='essential',r) # can this happen ?
	fi
    fi
end: # `infsing/exp`
`infsing/cos`:=op(`infsing/exp`);
`infsing/sin`:=op(`infsing/exp`);
`infsing/cosh`:=op(`infsing/exp`);
`infsing/sinh`:=op(`infsing/exp`);
#savelib( `infsing/exp`, `infsing/cos`, `infsing/sin`, `infsing/sinh`, `infsing/cosh`);
