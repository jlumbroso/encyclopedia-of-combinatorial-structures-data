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
##    Title: 	dev/print
##    Created:	1988
##    Author: 	Bruno Salvy
##		<Bruno.Salvy@inria.fr>
##
## Description: return an ordered expansion


`dev/print`:=proc(dev,var,globn)
local res, bigO, nb, n;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(dev,undefined) then undefined
    else
	if type(dev,list) then
	    n:=min(globn,`dev/length`(dev)-1);
	    res:=`dev/print/nfirst`(dev,n+1,'nb','bigO')
	else res:=`dev/print/nfirst`(dev,0,'nb','big0') fi;
#   This has been moved to dev/print/nfirst where it is easier to simplify
#	res:=eval(subs(_Xasytab=_equivX,res));
	if assigned(bigO) and bigO<>0 then
#	    res:=[op(res),`simplify/O`(O(frontend(simplify,[eval(subs(_Xasytab=_equivX,bigO))],[{`+`,`*`,`^`},{ln,exp}])),_Xasy)] fi;
	    res:=[op(res),`simplify/O`(O(eval(subs(_Xasytab=_equivX,
		simplify(bigO,infinity)))),_Xasy)] fi;
	convert(subs(_Xasy=var,res),`+`)
    fi
end:

`dev/print/nfirst`:=proc (tree,n,total,bigO)
local var, i, nb, locO, k, res, rest, inter;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if not type(tree,list) then
	if tree=0 then bigO:=0; total:=0; []
	else
	    total:=1;
	    if n=1 then bigO:=1 fi;
	    eval(subs(_Xasytab=_equivX,[tree])) # for exp(I*Pi*n)
	fi
    else
	var:=eval(subs(_Xasytab=_equivX,_Xasytab[tree[1]]));
	rest:=n;
	res:=[];
	for i from 2 by 2 to nops(tree) while rest>0 do
	    inter:=`dev/print/nfirst`(tree[i],rest,'nb','locO');
	    if not assigned(locO) then
		res:=[op(res),seq(inter[k]*frontend(simplify,[var^tree[i+1],power,exp],[{`+`,`*`,`^`},{ln,exp}]),
		    k=1..nb)];
		rest:=rest-nb
	    else
		res:=[op(res),seq(inter[k]*frontend(simplify,[var^tree[i+1],power,exp],[{`+`,`*`,`^`},{ln,exp}]),
		    k=1..nb-1)];
		rest:=0;
		bigO:=locO*frontend(simplify,[var^tree[i+1],power,exp],[{`+`,`*`,`^`},{ln,exp}])
	    fi
	od;
	total:=n-rest;
	res
    fi;
end: # `dev/print/nfirst`

`simplify/O`:=proc(f,var)
local a;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if not has(f,O) or nargs=1 then f
    elif type(f,'O(anything)') then
	a:=op(f);
	to 2 do
#	    a:=expand(`simplify/O/inO`(a,var));
	    a:=(`simplify/O/inO`(a,var));
	od;
	if a<>0 then O(a) else 0 fi
    elif type(f,`*`) then
	`simplify/O`(O(convert(map(op,select(type,[op(f)],'O(anything)')),`*`)),
	    var)
    else map(`simplify/O`,f,var)
    fi;
end:

`simplify/O/inO`:=proc (expr,var)
local f;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if type(expr,constant) and not has(expr,infinity) then
	if testeq(expr) then 0
	else 1 fi;
    elif type(expr,name) then if expr=var then var else 1 fi
    elif type(expr,`+`) then
	f:=map(procname,expr,var);
	if f<>expr then procname(f,var) else expr fi
    elif type(expr,`^`) then
	f:=evalc(op(2,expr));
	if has(f,I) then procname(op(1,expr)^coeff(f,I,0),var)
	else op(1,expr)^f fi
    elif type(expr,`*`) then map(procname,expr,var)
    elif type(expr,'exp(anything)') then
	f:=evalc(op(expr));
	if has(f,I) then procname(exp(coeff(f,I,0)),var)
	else expr fi
    elif type(expr,{'sin(anything)','cos(anything)'}) then 1
    elif type(expr,range) then procname(op(2,expr),var)
    else 
	f:=evalc(expr);
	if f<>expr then procname(f,var) else expr fi
    fi;
end: # `simplify/O/inO`

#savelib( `dev/print`,`dev/print/nfirst`,`simplify/O`,`simplify/O/inO`);
