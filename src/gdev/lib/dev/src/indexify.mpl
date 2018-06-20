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
##    Title: 	dev/indexify
##    Created:	1987
##    Author: 	Bruno Salvy
##		<salvy@rully.inria.fr>
##
## Description: finds an index for a new monomial

`dev/exp/trunc`:=proc (x)
local res;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(x,integer) then if x>0 then x else 1 fi
    else res:=trunc(evalf(x))+1; if res>0 then res else 1 fi fi
end: # `dev/exp/trunc`

`dev/indexify`:=proc(expr)
local i, cst, fcn, toind, res;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if op(0,expr)=ln then
	# it can only be ln(X[rat]) and X[rat] must be some exponential
	res:=[1];
	for i from 2 while res[nops(res)]<>infinity do
	    res:=`dev/dev`(simplify(ln(_equivX[op(op(expr))]),ln,'symbolic'),i,i)
	od
    else
	if type(expr,list) then
	    subsop(3=-1,`dev/placeit`(eval(subs(_Xasytab=_equivX,
		`dev/instanc`(expr)))))
	elif type(expr,'exp'(list)) then
	    toind:=eval(subs(_Xasytab=_equivX,`dev/instanc`(op(expr))));
	    if type(toind,`*`) then
		fcn:=1;cst:=1;
		for i in toind do 
		    if has(i,_Xasy) then fcn:=fcn*i else cst:=cst*i fi
		od;
		subsop(3=-cst,`dev/placeit`(exp(fcn)))
	    else
		subsop(3=-1,`dev/placeit`(exp(toind)))
	    fi
	else ERROR('bug')
	fi
    fi
end:

`dev/placeit`:=proc(expr)
local ord;
option remember,`Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    ord:=`dev/Lorder`(expr);
    `dev/findplace`(expr,-ord,ord)
end:

`dev/Lorder`:=proc(dev)
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if not has(dev,ln) and not has(dev,exp) then 0
    elif type(dev,function) then 1+procname(op(dev))
    elif type(dev,{`+`,`*`}) then
	max(procname(op(1,dev)),procname(op(2,dev)))
    elif type(dev,`^`) then
	procname(op(1,dev))+procname(op(2,dev))
    else ERROR(`invalid expansion`,dev)
    fi
end:

`dev/findplace`:=proc(devt,lim1,lim2)
local i, sig;
global _equivX;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type([lim1,lim2],[integer,integer]) and lim2-lim1>1 then
	i:=trunc((lim1+lim2)/2);
	if type(i,integer) and not assigned(_equivX[i]) then
	    if i<=0 then
		_equivX[i]:=exp(_Xasy);
		to -i do _equivX[i]:=exp(_equivX[i]) od;
		_equivX[i]:=1/_equivX[i]
	    else
		_equivX[i]:=ln(_Xasy);
		to i-2 do _equivX[i]:=ln(_equivX[i]) od;
		_equivX[i]:=1/_equivX[i]
	    fi
	fi;
	sig:=`dev/comparexplog`(devt,eval(subs(_Xasytab=_equivX,
	     `dev/instanc`([i,1,-1]))));
	if sig=1 then procname(devt,lim1,i)
	elif sig=0 then [i,1,-1,0,infinity]
	else procname(devt,i,lim2)
	fi
    elif devt=1/eval(subs(_Xasytab=_equivX,`dev/instanc`([lim1,1,1]))) then
	[lim1,1,-1,0,infinity]
    elif devt=1/eval(subs(_Xasytab=_equivX,`dev/instanc`([lim2,1,1]))) then
	[lim2,1,-1,0,infinity]
    elif assigned(_equivX[(lim1+lim2)/2]) then
	sig:=`dev/comparexplog`(devt,1/eval(subs(_Xasytab=_equivX,
	    `dev/instanc`([(lim1+lim2)/2,1,1]))));
	if sig=0 then [(lim1+lim2)/2,1,-1,0,infinity]
	elif sig=1 then procname(devt,lim1,(lim1+lim2)/2)
	else procname(devt,(lim1+lim2)/2,lim2)
	fi
    else _equivX[(lim1+lim2)/2]:=1/devt;
	[(lim1+lim2)/2,1,-1,0,infinity]
    fi
end:

`dev/comparexplog`:=proc(u,v)
local sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if u=v then 0
    elif not has(u,_Xasy) and not has(v,_Xasy) then
	sig:=evalr(Signum(u-v));
	if sig=FAIL then ERROR(FAIL) else sig fi;
    elif type([u,v],[function,function]) and op(0,u)=op(0,v) then
	procname(op(1,u),op(1,v))
    elif type(u,'exp(anything)') then
	procname(op(u),expand(ln(v)))
    elif type(u,'ln(anything)') and expand(u)<>u then
	`dev/compare`(`dev/dev`(expand(u),1,1),`dev/dev`(v,1,1))
    elif type(u,{`+`,`^`,`*`,name}) then
	`dev/compare`(`dev/dev`(expand(ln(u)),1,1),
	    `dev/dev`(expand(ln(v)),1,1))
    else ERROR(`invalid expansion`,u)
    fi
end:

#savelib( `dev/indexify`,`dev/placeit`,`dev/Lorder`,`dev/findplace`,`dev/comparexplog`,`dev/exp/trunc`);
