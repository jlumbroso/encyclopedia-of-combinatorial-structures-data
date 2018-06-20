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

# G.J. Fee's algorithm:
`dev/cossin`:=proc (u,n)
local i, i0, inival, newu, res, co, si, still0, fact, inter, infpart, newres, aux, nbit;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(u,undefined) then undefined
    elif not type(u,list) then [cos(u),sin(u)]
    elif n=0 then [1,0]
    else
	aux:=evalr(Signum(u[3]));
	if aux=1 or aux=0 then
	    res:=[];
	    if aux=0 then
		if not type(u[2],list) then inival:=`dev/cossin`(u[2])
		else inival:=`dev/cossin`([op(u[2]),0,infinity],n) fi;
		still0:=member(0,inival);
		newu:=subsop(2=NULL,3=NULL,u)
	    elif not type(u[2],list) then
		if nops(u)>3 then
		    co:=[u[1],1,0];si:=[u[1],u[2],u[3]];fact:=u[2];
		    for i from 2 by 2 to 2*n do
			fact:=-fact/i*u[2];
			co:=[op(co),fact,i*u[3]];
			fact:=fact/(i+1)*u[2];
			si:=[op(si),fact,(i+1)*u[3]]
		    od;
		    inival:=[co,si];
		    still0:=false;
		    newu:=subsop(2=NULL,3=NULL,u)
		else RETURN([[u[1],1,0,1,2*u[3]],u])
		fi
	    else inival:=[1,0];newu:=u;still0:=true;
	    fi;
	    if still0 and nops(newu)>3 then
		inter:=`dev/cossin`([newu[1],newu[2],newu[3],0,infinity],
			trunc(evalf((n+1)/newu[3])));
		if inival=[1,0] then inival:=inter
		elif inival=[0,1] then inival:=[`dev/multbyreal`(inter[2],-1),
		    inter[1]]
		elif inival=[-1,0] then
		    inival:=[`dev/multbyreal`(inter[1],-1),
			`dev/multbyreal`(inter[2],-1)]
		elif inival=[0,-1] then 
		    inival:=[inter[2],`dev/multbyreal`(inter[1],-1)]
		fi;
		newu:=subsop(2=NULL,3=NULL,newu);
	    fi;
	    i0:=newu[3];
	    if newu[nops(newu)]=infinity then nbit:=nops(newu)-3 
	    else nbit:=nops(newu)-1 fi;
	    newres:=inival;
	    for i from 2 by 2 to nbit while newres<>res do
		res:=newres;
		inter:=`dev/cossin`([newu[1],newu[i],newu[i+1],0,infinity],
		    trunc(evalf((n+1)*i0/newu[i+1]/2)));
		newres:=[`dev/add`(`dev/prd`(res[1],inter[1]),`dev/multbyreal`(
			`dev/prd`(res[2],inter[2]),-1)),
		 `dev/add`(`dev/prd`(res[1],inter[2]),`dev/prd`(res[2],inter[1]))]
	    od;
	    if res=[] then RETURN(inival) else RETURN(newres) fi;
	elif aux=-1 then
	    for i from 5 to nops(u) while member(evalr(Signum(u[i])),{-1,0})do od;
	    infpart:=`dev/instanc`([op(1..i-2,u)]);
	    if nops(u)>=i and u[i]<>infinity then
		inter:=`dev/cossin`([u[1],op(i-1..nops(u),u)],n);
		RETURN([`dev/add`(`dev/multbyreal`(inter[1],cos(infpart)),
			      `dev/multbyreal`(inter[2],-sin(infpart))),
		 `dev/add`(`dev/multbyreal`(inter[1], sin(infpart)),
			      `dev/multbyreal`(inter[2], cos(infpart)))])
	    else RETURN([cos(infpart),sin(infpart)])
	    fi
	else ERROR(FAIL)
	fi
    fi
end: # `dev/cossin`
#savelib(`dev/cossin`);
