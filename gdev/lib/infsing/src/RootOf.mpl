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
`infsing/RootOf`:=proc (Fct,var,Singmin)
local singmin, locval, val, i, j, y, fct, sig, tosubs;
    singmin:=Singmin;
    fct:=subs(_Z=y,op(1,Fct));
    do
	if not has(diff(fct,var),y) then
	    locval:=infsing(diff(fct,var),var,singmin)
	else
	    locval:=[[infinity],'polynom',true]
	fi;
#	val:=map(proc(x,var,singmin) local res;
#	    if op(1,op(1,x))=var then res:=op(2,op(1,x))
#	    else res:=op(2,op(2,x)) fi;
#	    if not type(res,function) or 
#		type(res,specfunc(RootOf,anything)) then
#		RETURN(res)
#	    else RETURN(op(`infsing/infsolve`(op(1,res),_Z,false,singmin))) fi
#	     end,
#	    [solve({fct,diff(fct,y)},{var,y})],var,singmin);
    	val:=map(subs,[solve({fct,diff(fct,y)},{var,y})],var);
	for i in indets(val,RootOf) do
	    if type(op(1,i),polynom(numeric,_Z)) then
		tosubs:=[seq(i=RootOf(op(1,i),j),
		    j=[fsolve(op(1,i),_Z,complex)])];
		for j in val do 
		    if has(j,i) then
			val:=subs(j=op(map(subs,tosubs,j)),val)
		    fi
		od
	    fi
	od;
	if nops(val)<>0 then
	    for i from 2 while nops(val)>=i do
		sig:=comparemodule(val[1],val[i]);
		if sig=`<` then
		    val:=subsop(i=NULL,val);
		    i:=i-1
		elif sig=`>` then
		    val:=[op(i..nops(val),val)];
		    i:=1
		fi
	    od;
	    val:=[`infsing/trie`(val,locval[1],Fct,var,'algebraic',locval[2]),false];
	    if locval[1][1]<>infinity then
		if val[1][1]=infinity then
		    singmin:=abs(locval[1][1])
		else
		    break
		fi
	    else
		break
	    fi
	else
	    break
	fi
    od;
    val
end: # `infsing/RootOf`

#savelib( `infsing/RootOf`);
