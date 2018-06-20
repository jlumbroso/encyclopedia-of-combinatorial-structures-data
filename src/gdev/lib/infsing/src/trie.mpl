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
############################################################
#
#				    TRIE
#
#   Given two lists of equi-modulus points, this procedure
# outputs the union with respect to the moduli. The
# function is taken into account, to avoid false singularities.
#
#############################################################

`infsing/trie`:=proc(u1,u2,fct,var,ty1,ty2)
local res,u,v,res2, i,j, sig, ty;
    if u1=[] then `infsing/trie`([infinity],u2,fct,var,'polynom',ty2)
    elif u2=[] then `infsing/trie`([infinity],u1,fct,var,ty1,'polynom')
    elif has(u1,infinity) then
	if `infsing/check`(u2,fct,var,'res') then res,ty2
	else u1,ty1
	fi
    elif has(u2,infinity) then
	if `infsing/check`(u1,fct,var,'res') then res,ty1
	else u2,ty2
	fi
    else
	u:=op(1,u1);v:=op(1,u2);
	res:=comparemodule(u,v);
	if res=`<` then
	    if `infsing/check`(u1,fct,var,'res') then res,ty1
	    elif `infsing/check`(u2,fct,var,'res') then res,ty2
	    else [infinity],'polynom'
	    fi
	elif res=`>` then
	    if `infsing/check`(u2,fct,var,'res') then res,ty2
	    elif `infsing/check`(u1,fct,var,'res') then res,ty1
	    else [infinity],'polynom'
	    fi
	elif res=`=` then
	    # check that a singularity is not present twice
	    # (ex. RootOf(_Z*exp(_Z)-1) and RootOf(_Z^2*exp(2*_Z)-1,)
	    `infsing/check`(u1,fct,var,'res');
	    `infsing/check`(u2,fct,var,'res2');
	    for i while i<=nops(res) do
		for j while j<=nops(res2) do
		    sig:=zerotest(res[i],res2[j]);
		    if sig=true then res2:=subsop(j=NULL,res2);j:=j-1
		    elif sig=FAIL and 
			type(evalf(evalc(res2[j]-res[i]),10),numeric) then
			# same modulus + difference real ==> equal
			if length(res2[j])>length(res[i]) then
			    res2:=subsop(j=NULL,res2); j:=j-1
			else res:=subsop(i=NULL,res); i:=i-1; break
			fi
		    fi
		od
	    od;
	    if ty1='polynom' then ty:=ty2
	    elif ty2='polynom' then ty:=ty1
	    elif ty1='polar' then ty:=ty2
	    elif ty2='polar' then ty:=ty1
	    elif ty1='algebraic' then ty:=ty2
	    elif ty2='algebraic' then ty:=ty2
	    elif ty1='log' then ty:=ty2
	    elif ty2='log' then ty:=ty1
	    else ty:=ty1 fi;
	    [op(res),op(res2)],ty
	else ERROR(`Too many variables`)
	fi
    fi
end:

`infsing/check`:=proc (pt,fct,var,result)
local i, res, devint, j;
    res:=[];
    for i in pt do
	if has(i,infinity) then res:=[op(res),i]
	else
	    # On the one hand we want to try to expand as  far as possible 
	    # since you can never know where your information will be found
	    # (but it is likely to lie before the order 2). So a good idea
	    # seems to be the order p since it will use the option remember
	    # for the second part of the program. On the other hand, these 
	    # computations are expensive and you are not sure you have found
	    # the right singularity yet, so the option remember will only
	    # fill your memory a little more. Hence the following choice of 2:
#	    devint:=`dev/dev`(eval(subs(var=i*(1-1/_Xasy),fct)),2,2);
	    devint:=`dev/dev`((subs(var=i*(1-1/_Xasy),fct)),2,3);
	    if {seq(op(1,j),j=indets(devint,list))} = {1} then
		for j from 3 by 2 to nops(devint)-2 while 
		    type(devint[j],nonnegint) do od;
		if j<nops(devint) or j=nops(devint) and 
		    not type(devint[j],nonnegint) and devint[j]-devint[j-2]<1
		    then res:=[op(res),i]
		fi
	    else
		res:=[op(res),i]
	    fi
	fi
    od;
    if res=[] then false
    else result:=res;
	true
    fi
end: # `infsing/check`

#savelib( `infsing/trie`,`infsing/check`);
