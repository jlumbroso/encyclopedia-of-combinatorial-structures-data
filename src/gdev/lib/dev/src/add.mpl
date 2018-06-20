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
##    Title: 	dev/add
##    Created:	1987
##    Author: 	Bruno Salvy
##		<bruno.salvy@inria.fr>
##
## Description: sums of expansions

`dev/add`:=proc(U,V)
local i,j,result,u,v,nu,nv, degu, degv, inter, sig, toadd1, toadd2, s;
option remember,`Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(U,undefined) or type(V,undefined) then undefined
    elif U=0 then V
    elif V=0 then U
    elif not type(U,list) and not type(V,list) then
	result:=U+V;
	if zerotest(result)=true 
	or type([U,V],[float,float]) and
	    (abs(result)<subsop(1=1,U) or abs(result)<subsop(1=1,V)) then 0
	else result fi
    else
	if not type(U,list) then   u:=[V[1],U,0,0,infinity]; v:=V
	elif not type(V,list) then u:=U; v:=[U[1],V,0,0,infinity]
	elif op(1,U)=op(1,V) then  u:=U;v:=V
	elif op(1,U)<op(1,V) then  u:=U;
	    if V[nops(V)]=infinity then
		 v:=[op(1,U),[op(1..nops(V)-2,V)],0,0,infinity]
	    else v:=[op(1,U),V,0] fi
	else v:=V;
	    if U[nops(U)]=infinity then
		 u:=[op(1,V),[op(1..nops(U)-2,U)],0,0,infinity]
	    else u:=[op(1,V),U,0] fi
	fi;
	nu:=nops(u); nv:=nops(v);
	i:=3; j:=3;
	for s from 2 while (i<=nu and j<=nv) do 
	    degu:=u[i]; degv:=v[j];
	    if degu=infinity then result[s]:=op(j-1..nv,v); s:=s+1; break fi;
	    if degv=infinity then result[s]:=op(i-1..nu,u); s:=s+1; break fi;
	    sig:=signum(degu-degv); # this calls evalr/Signum
	    if type(sig,specfunc(anything,signum)) then
		sig:=comparexpr(degu,degv);
		if sig=`<` then sig:=-1
		elif sig=`>` then sig:=1
		fi
	    fi;
	    if sig=1 then result[s]:=v[j-1],v[j]; j:=j+2
	    elif sig=-1 then result[s]:=u[i-1],u[i]; i:=i+2
	    elif sig=FAIL and not type([degu,degv],['realcons','realcons'])
		then ERROR(FAIL)
	    # otherwise we assume FAIL means 0
	    else 
		if i=nu or not type(u[i-1],list) then
		     toadd1:=u[i-1]
		else toadd1:=[op(u[i-1]),0,infinity] fi;
		if j=nv or not type(v[j-1],list) then
		     toadd2:=v[j-1]
		else toadd2:=[op(v[j-1]),0,infinity] fi;
		inter:=`dev/add`(toadd1,toadd2);
		if type(inter,list) and (i<nu or j<nv) and 
		    inter[nops(inter)]=infinity then
		    inter:=[op(1..nops(inter)-2,inter)]
		fi;
		if inter<>0 then       result[s]:=inter,degu
		elif i=nu or j=nv then result[s]:=u[i-1],u[i]
		else                   result[s]:=NULL
		fi;
		i:=i+2; j:=j+2
	    fi
	od;
	result:=[op(1,u),seq(result[i],i=2..s-1)];
	if nops(result)=1 then
	    if i>nu then [u[1],u[nu-1],u[nu]]
	    else [v[1],v[nv-1],v[nv]] fi
	elif nops(result)=3 and result[3]=infinity then
	    if type(result[2],list) then [op(result[2]),0,infinity]
	    else 0 fi
	elif nops(result)=3 and result[3]=0 then    
	    if type(result[2],list) then result[2]
	    else [1,result[2],0] fi
	elif nops(result)=5 and result[5]=infinity and result[3]=0 then
	    if type(result[2],list) then [op(result[2]),0,infinity]
	    else result[2] fi
	else result
	fi
    fi
end:

#savelib( `dev/add`);
