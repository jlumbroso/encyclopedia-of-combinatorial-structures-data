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
##    Title:	`dev/ln`
##    Created:	Apr-Aug 1988
##    Author:	Bruno Salvy
##
##    Modified:	 Nov 1988
##    Author:  Bruno Salvy
##    Modification: Listinf added. At most one level can introduce Listinf.
##
## Modified Jul 1994 BS. Handling of congruences/Listinf suppressed.

`dev/ln`:=proc(u,n)
local deg, expr, nu, roots, init, newvar, x;
global _equivX, _EnvAllSolutions;
option remember,`Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then undefined
    elif not type(u,list) then
	if zerotest(u) then ERROR(`ln(0)`)
	elif zerotest(u-1) then 0
	elif _EnvXasyinteger and map(op,indets(u,_Xasytab[anything]))={1}
	    then
	    expr:=expand(subs(_Xasytab[1]=1/x,u));
	    _EnvAllSolutions:=true;
	    roots:={solve(expr,x)};
	    # check whether expr is 0 at all integers
	    if type(roots,set(name)) and is(op(roots),integer) then
		ERROR(`ln(0)`)
	    # otherwise it is non-zero at an infinite number of points and
	    # we assume we are working there. This will be handled by
	    # a lastproviso.
	    else ln(u) fi
	else ln(u)
	fi
    else
	nu:=nops(u);
	if nu=5 and u[5]=infinity then
	    deg:=u[3];
	    if type(u[2],list) then init:=`dev/ln`([op(u[2]),0,infinity],n)
	    else init:=`dev/ln`(u[2]) fi;
	    if deg=0 then init
	    else
		# Here is hidden X[n+1]=f(X[n]) :
		if type(u[1],integer) then
		    if assigned(_equivX[u[1]+1]) then
			`dev/add`(init,[u[1]+1,-deg,-1,0,infinity])
		    else
			_equivX[u[1]+1]:=1/ln(1/_equivX[u[1]]);
			`dev/add`(init,[u[1]+1,-deg,-1,0,infinity])
		    fi
		else
		    newvar:=`dev/indexify`(ln(_Xasytab[u[1]]));
		    `dev/add`(init,subsop(2=
			`dev/multbyreal`(newvar[2],deg),newvar))
		fi
	    fi
	elif nu=3 then
	    init:=`dev/ln`([u[1],u[2],u[3],0,infinity],n);
	    if type(init,list) and init[nops(init)]=infinity then
		subsop(nops(init)=NULL,nops(init)-1=NULL,init)
	    else init fi
	else
	    `dev/add`(`dev/ln`([u[1],u[2],u[3],0,infinity],n),
		`dev/ln/endofdev`(`dev/prd`(
		    `dev/pow`([u[1],u[2],u[3],0,infinity],-1,n),
		    [u[1],op(4..nu,u)]),n))
	fi
    fi
end:

`dev/ln/endofdev`:=proc (dev,n)
local i;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(dev,undefined) then undefined
    else
	`dev/endofdev`(dev,n,map(op,
	    [seq([1/(2*i+1),2*i+1,-1/(2*i+2),2*i+2],i=0..iquo(n,2))]))
    fi
end: # `dev/ln/endofdev`

# This procedure is used only by the Luo system
`dev/lncoeff`:=proc(n)
option remember,`Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if n=1 then RETURN(1) fi;
    if n=2 then RETURN(1/2) fi;
    if n=3 then RETURN(5/12) fi;
    if n=4 then RETURN(47/120) fi;
    if n=5 then RETURN(12917/33840) fi;
    if n=6 then RETURN(329458703/874222560) fi;
    ERROR(`Only 6 nested logs handled`)
end:

#savelib( `dev/ln`,`dev/ln/endofdev`,`dev/lncoeff`);
