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
##    Title: 	dev/prd
##    Created:	1987
##    Author: 	Bruno Salvy
##		<salvy@rully.inria.fr>
##
## Description: product of expansions
##
## Modification Jul 94. Handling of Listinf suppressed.
## The handling of degrees requires too much memory and should be rewritten.

`dev/prd`:=proc(U,V)
local deg, i, j, u, v, candidate, deg1, exactu, exactv, inter1, nu, nv, result, l, count, tomult1, tomult2, sig;
option remember,`Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(U,undefined) or type(V,undefined) then undefined
    elif not type(U,list) then
	if not type(V,list) then U*V
	else `dev/multbyreal`(V,U) fi
    elif not type(V,list) then `dev/multbyreal`(U,V)
    else
	if op(1,U)=op(1,V) then u:=U;v:=V
	elif op(1,U)<op(1,V) then
	    u:=U;
	    if V[nops(V)]=infinity then
		v:=[op(1,U),[op(1..nops(V)-2,V)],0,0,infinity]
	    else v:=[op(1,U),V,0] fi
	else
	    v:=V;
	    if U[nops(U)]=infinity then
		u:=[op(1,V),[op(1..nops(U)-2,U)],0,0,infinity]
	    else u:=[op(1,V),U,0] fi
	fi;
	nu:=nops(u); nv:=nops(v);
	exactu:=evalb(u[nu]=infinity);	
	exactv:=evalb(v[nv]=infinity);
	deg:=[[u[3]+v[3],2,2]];
	# deg is a sorted list of n-tuples whose first element is the total
	# degree, and the next ones are corresponding indices in u and v.
    	result[1]:=u[1];
	for count from 2 do
	    deg1:=deg[1];
	    inter1:=0;
	    for i from 2 by 2 to nops(deg1) do
		if deg1[i]=nu-1 or not type(u[deg1[i]],list) then
		    tomult1:=u[deg1[i]]
		else tomult1:=[op(u[deg1[i]]),0,infinity] fi;
		if deg1[i+1]=nv-1 or not type(v[deg1[i+1]],list) then
		    tomult2:=v[deg1[i+1]]
		else tomult2:=[op(v[deg1[i+1]]),0,infinity] fi;
		inter1:=`dev/add`(inter1,`dev/prd`(tomult1,tomult2))
	    od;
	    if type(inter1,list) and inter1[nops(inter1)]=infinity then
		inter1:=[op(1..nops(inter1)-2,inter1)]
	    fi;
	    if inter1<>0 then result[count]:=inter1,deg1[1]
	    else result[count]:=NULL fi;
	    if (not exactu and member(nu-1,[seq(deg1[2*i],
		i=1..iquo(nops(deg1),2))])) or
		(not exactv and member(nv-1,[seq(deg1[2*i+1],
		    i=1..iquo(nops(deg1),2))])) then
		if inter1=0 then result[count]:=1,deg1[1] fi;
		break
	    else
		# rebuild deg
		deg:=subsop(1=NULL,deg);
		for i from 2 by 2 to nops(deg1) do
		    if deg1[i]<>nu-1 and u[deg1[i]+3]<>infinity then
			candidate:=u[deg1[i]+3]+v[deg1[i+1]+1];
			sig:=1;
			for j to nops(deg) while sig=1 do
			    sig:=evalr(Signum(candidate-deg[j][1]));
			    if sig=-1 then
				deg:=[op(1..j-1,deg),[candidate,
				    deg1[i]+2,deg1[i+1]],op(j..nops(deg),deg)]
			    elif sig=0 then
				if not member([deg1[i]+2,deg1[i+1]],
				    [seq([deg[j][2*l],deg[j][2*l+1]],
					l=1..iquo(nops(deg[j]),2))]) then
				    deg:=subsop(j=[op(deg[j]),deg1[i]+2,
					deg1[i+1]],deg) fi
			    elif sig=FAIL then ERROR(FAIL) fi
			od;
			if sig=1 then
			    deg:=[op(deg),[candidate,deg1[i]+2,deg1[i+1]]]
			fi
		    fi;
		od;
		for i from 2 by 2 to nops(deg1) do
		    if deg1[i+1]<>nv-1 and v[deg1[i+1]+3]<>infinity then
			candidate:=u[deg1[i]+1]+v[deg1[i+1]+3];
			sig:=1;
			for j to nops(deg) while sig=1 do
			    sig:=evalr(Signum(candidate-deg[j][1]));
			    if sig=-1 then
				deg:=[op(1..j-1,deg),[candidate,deg1[i],
				    deg1[i+1]+2],op(j..nops(deg),deg)]
			    elif sig=0 then
				if not member([deg1[i],deg1[i+1]+2],
				    [seq([deg[j][2*l],deg[j][2*l+1]],
					l=1..iquo(nops(deg[j]),2))]) then
				    deg:=subsop(j=[op(deg[j]),deg1[i],
					deg1[i+1]+2],deg) fi
			    elif sig=FAIL then ERROR(FAIL) fi
			od;
			if sig=1 then
			    deg:=[op(deg),[candidate,deg1[i],deg1[i+1]+2]]
			fi
		    fi
		od;
		if nops(deg)=0 then break fi
	    fi
	od;
	if result[2][2]=0 and (count<3 or (count=3 and result[3][2]=infinity))
	then
	    if count<3 then result[2][1]
	    else [op(result[2][1]),0,infinity] fi
	elif nops(deg)=0 then
	    [seq(result[i],i=1..count),0,infinity]
	else [seq(result[i],i=1..count)]
	fi
    fi
end:

#savelib( `dev/prd`);
