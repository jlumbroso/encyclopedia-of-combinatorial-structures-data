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

# This should be rewritten to share most of the code with gdev
glimit:=proc(fct::algebraic,gvar::{name,name=algebraic})
local pt, var, rightdir, res, i, sgn, coef;
option `Copyright Bruno Salvy, INRIA, France`;
    if type(args[2],name=algebraic) then
	var:=op(1,args[2]);
	pt:=op(2,args[2])
    else
	var:=args[2];
	pt:=0
    fi;
    if has(pt,infinity) and pt<>infinity and pt<>-infinity then
	ERROR(`Invalid limit point`,args[2])
    fi;
    rightdir:=true;
    if nargs>=3 then
	if args[3]='inverse' then
	    if pt<>infinity and pt<>-infinity then rightdir:=false fi
	elif args[3]<>'straight' then ERROR(`Invalid argument`,args[3])
	fi
    fi;
    if is(_Xasy>100)=FAIL then additionally(_Xasy>100) fi;
    _EnvXasyinteger:=false;
    if pt=infinity then
	_EnvXasyinteger:=is(var,integer);
	res:=traperror(`dev/dev`(subs(var=_Xasy,fct),1,1))
    elif pt=-infinity then
	_EnvXasyinteger:=is(var,integer);
	res:=traperror(`dev/dev`(subs(var=-_Xasy,fct),1,1))
    elif zerotest(pt)<>true then
	if rightdir then
	    res:=traperror(`dev/dev`(subs(var=pt*(1-1/_Xasy),fct),1,1))
	else
	    res:=traperror(`dev/dev`(subs(var=pt*(1+1/_Xasy),fct),1,1))
	fi
    else
	if rightdir then
	    res:=traperror(`dev/dev`(subs(var=1/_Xasy,fct),1,1))
	else
	    res:=traperror(`dev/dev`(subs(var=-1/_Xasy,fct),1,1))
	fi
    fi;
    if res=lasterror or res=FAIL then
	glimit(args):='glimit'(args);
	RETURN(%)
    elif type(res,undefined) then RETURN(undefined)
    fi;
    do
	if type(res,list) then
	    sgn:=evalr(Signum(res[3]));
	    if sgn=1 then RETURN(0)
	    elif sgn=-1 then
		coef:=`dev/lcoeff`(res);
		for i in indets(coef,function) do
		    if op(0,i)=sin or op(0,i)=cos then
			coef:=subs(i='INTERVAL'(-1..1),coef) fi
		od;
		sgn:=evalr(Signum(coef));
		if sgn=1 then RETURN(infinity)
		elif sgn=-1 then RETURN(-infinity)
		else glimit(args):='glimit'(args);RETURN('glimit'(args))
		fi
	    elif sgn=0 then
		if type(op(2,res),list) then res:=[op(op(2,res)),0,infinity]
		else res:=op(2,res) fi
	    else glimit(args):='glimit'(args);RETURN('glimit'(args))
	    fi
	elif not has(res,_Xasytab) then
	    RETURN(res)
	elif not (has(res,sin) or has(res,cos)) then RETURN(res)
	else
	    for i in indets(res,{sin(anything),cos(anything)}) do
		res:=subs(i='INTERVAL'(-1..1),res) od;
	    RETURN(evalr(res))
	fi
    od;
end:

#savelib( glimit);
