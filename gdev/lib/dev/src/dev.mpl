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
###############################################################
#
#					DEV
#
#   The input to this procedure is :
#			    fct a function in _Xasy
#			    n	 the order of the expansion
#			    p	 its depth (number of non-zero terms)
#   The output is an expansion of the function in a generalized scale extending
#    ... ln(ln(x))^q ln(x)^r x^s exp(x)^t exp(exp(x))^u ...
# See the file structure for a description of the the data structure of 
# the expansions.
#
#  Created ca 1987 B. Salvy <Bruno.Salvy@inria.fr>
#  Modified Jul 94 BS: dev/dev split, handling of congruences suppressed.
#  
###############################################################

`dev/dev`:=proc(fct,p,n)
local inter, deve, fctname, i, j, f;
option remember,`Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    ## This should not happen anymore since arithmetic on lists
    ## has become automatic
	# # allow a function already partly expanded:
	# if type(fct,list) then fct
    if type(fct,list) then ERROR(fct)
    # constants are not changed
    elif not has(fct,{_Xasy,O}) then # and not hastype(fct,list) then
	if zerotest(fct)=true then 0 else fct fi
    # polynomials are the leaves of the expression --> chg of var
    elif type(fct,ratpoly(anything,_Xasy)) # and not hastype(fct,list)
	and not has(fct,O) then
	`dev/dev/ratpoly`(fct,n)
    elif type(fct,`+`) then `dev/dev/+`(fct,p,n)
    elif type(fct,`*`) then `dev/dev/*`(fct,p,n)
    elif type(fct,`^`) then
	if has(op(2,fct),_Xasy) then `dev/dev`(exp(op(2,fct)*ln(op(1,fct))),p,n)
	else `dev/dev/pow`(op(1,fct),op(2,fct),p,n) fi
    elif type(fct,function) then
	fctname:=op(0,fct);
	if fctname='ln' then `dev/dev/ln`(op(1,fct),p,n)
	elif fctname='exp' then
	    if type(op(fct),`&*`({'ln'(anything),freeof(_Xasy)})) then
		inter:=indets(op(fct),ln(dependent(_Xasy)));
		if nops(inter)=1 then
		    inter:=op(inter);
		    `dev/dev`(inter^(op(fct)/inter),p,n)
		else `dev/dev/exp`(op(fct),p,n) fi
	    else `dev/dev/exp`(op(fct),p,n) fi
 	elif fctname=O then
	    inter:=`dev/dev`(op(fct),1,1);
	    if inter=FAIL then FAIL else `dev/O`(inter) fi
#	elif member(fctname,{'int',Int}) then
#	    deve:=traperror(`dev/int`(n,p,op(fct)));
#	    if deve=lasterror then ERROR(deve)
#	    elif `dev/length`(deve)<=p and n<p+3 then
#		deve:=`dev/dev`(fct,p,n+min(3,1+n-`dev/length`(deve)))
#	    fi
	elif fctname='RootOf' then
	    # in the next version, every dev/f should get the argument of f,
	    # not its expansion
	    deve:=`dev/RootOf`(op(fct),p);
	    #`dev/RootOf` returns a list of branches. We have to make a choice.
	    # if there is only one branch, return it
	    if nops(deve)=1 then RETURN(deve[1]) fi;
	    # if one of them is singular, prefer it
	    for i in deve do
		if remove(type,{seq(i[2*j+1],j=1..iquo(nops(i),2))},nonnegint)
		    <>{} then RETURN(i) fi
	    od;
	    # arbitrarily choose the first one
	    deve[1]
	else
	    f:=cat(`dev/`,fctname);
	    if not type(f,procedure) then
		ERROR(`Not implemented : `,f)
	    fi;
	    inter:=map(`dev/dev`,[op(fct)],p,n);
	    if has(inter,FAIL) then FAIL
	    else
		deve:=f(op(inter),n);
		if not type(deve,list) or deve[nops(deve)]=infinity or
		    `dev/length`(deve)>p or n>=p+3 or hastype(fct,list) then
			deve
		elif n<p+3 then
		    `dev/dev`(fct,p,n+min(3,n+1-`dev/length`(deve)))
		else `dev/dev/simplify`(fct,deve,p,n)
		fi
	    fi
	fi
    fi;
end: # `dev/dev`

# Here we can rely on series
## Unfortunately, this is still too slow on large expressions.
`dev/dev/ratpoly`:=proc(fct,n)
local inter, i, t, ni, o;
    Testzero:=zerotest;
    for o from n+4 do
	# without normal, series can return an error (!)
#	inter:=series(normal(subs(_Xasy=1/t,fct)),t,o);
	inter:=traperror(series((subs(_Xasy=1/t,fct)),t,o));
	if inter<>lasterror and (
	    not type(inter,series) or op(nops(inter)-1,inter)<>O(1)
	    or nops(inter)>2*n+5
	    # this is to avoid a nasty infinite loop:
	    or testeq(subs(_Xasy=1/t,fct)-convert(inter,polynom)))
	    then break fi;
    od;
    if type(inter,series) then
	while zerotest(op(1,inter)) do inter:=subsop(1=0,inter) od;
	ni:=nops(inter);
	if op(ni-1,inter)=O(1) then
	    [1,seq(op(i,inter),i=1..ni-2),1,op(ni,inter)]
	else [1,seq(op(i,inter),i=1..ni),0,infinity] fi
    else inter fi
end: # `dev/dev/ratpoly`
 
`dev/dev/simplify`:=proc (fct,dev,p,n)
local newfct;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    newfct:=traperror(simplify(normal(eval(subs(L=proc(x)
	log(1/(1-x))end,Q=proc(x)1/(1-x)end,fct)))));
    if newfct<>lasterror and newfct<>fct and
#	traperror(testeq(newfct,fct))=true then
	traperror(zerotest(newfct,fct))=true then
	# this testeq won't be needed when simplify is correct
	`dev/dev`(newfct,p,n)
    else dev
    fi
end: # `dev/dev/simplify`

`dev/dev/operation`:=proc(fct,p,n)
local deve, i, inter;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    deve:=_NEUTRAL;
    for i in fct do
	inter:=`dev/dev`(i,p,n);
	if inter=FAIL or type(inter,undefined) then deve:=inter; break
	else deve:=_OPERATION(deve,inter) fi
    od;
    if not type(deve,list) or deve[nops(deve)]=infinity or
	`dev/length`(deve)>p or # hastype(fct,list)
	member(true,map(type,deve,list))
	then deve
    elif n<p+3 then
	`dev/dev`(fct,p,n+min(3,n+1-`dev/length`(deve)))
    else `dev/dev/simplify`(fct,deve,p,n)
    fi
end: # `dev/dev/operation`

`dev/dev/+`:=subs(_NEUTRAL=0,_OPERATION='`dev/add`',op(`dev/dev/operation`)):
`dev/dev/*`:=subs(_NEUTRAL=1,_OPERATION='`dev/prd`',op(`dev/dev/operation`)):

`dev/dev/pow`:=proc(A,B,p,n)
local sig, deve, inter;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    # try to catch some divergent expansions there
    sig:=evalr(Signum(B));
    if sig=-1 and type(A,'exp(anything)') then
	deve:=traperror(`dev/dev`(A,p,n));
	if deve<>lasterror and deve<>FAIL then
	    # we did not need to worry: it was convergent
	    `dev/pow`(deve,B,n)
	else
	    # we expand to the order n+1 to have the last
	    # interesting term with a minus sign
	    `dev/dev`(exp(-op(A))^(-B),p+1,n+1)
	fi
    else
	inter:=`dev/dev`(A,p,n);
	if inter=FAIL or type(inter,undefined) then inter
	else
	    deve:=`dev/pow`(inter,B,n);
	    if deve=FAIL or not type(deve,list) or deve[nops(deve)]=infinity or
		`dev/length`(deve)>p or hastype(A^B,list) then deve
	    elif n<p+2 then `dev/dev`(A^B,p,n+min(3,1+n-`dev/length`(deve)))
	    elif not hastype(A^B,list) then `dev/dev/simplify`(A^B,deve,p,n)
	    else deve
	    fi
	fi
    fi
end: # `dev/dev/pow`

`dev/dev/ln`:=proc(fct,p,n)
local deve, inter;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    inter:=`dev/dev`(fct,p,n+1);
    if inter=FAIL then `dev/dev/simplify`('ln'(fct),FAIL,p,n)
    else
	deve:=`dev/ln`(inter,n);
	if not type(deve,list) or deve[nops(deve)]=infinity or
	    `dev/length`(deve)>p or n>=p+3 then deve
	else `dev/dev`(ln(fct),p,n+min(3,1+n-`dev/length`(deve))) fi
    fi
end: # `dev/dev/ln`

`dev/dev/exp`:=proc(fct,p,n)
local inter, deve;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    inter:=`dev/dev`(fct,p,n);
    if inter=FAIL or type(inter,undefined) then inter
    else
	deve:=traperror(`dev/exp`(inter,n));
	if deve<>`Not a convergent expansion` then
	    if deve=lasterror then ERROR(deve)
	    elif not type(deve,list) or deve[nops(deve)]=infinity or
		`dev/length`(deve)>p or n>=p+3 or hastype(fct,list) then deve
	    elif n<p+3 then
		`dev/dev`(exp(fct),p,n+min(3,1+n-`dev/length`(deve)))
	    else `dev/dev/simplify`(exp(fct),deve,p,n)
	    fi
	elif n<p+3 then
	    # sometimes trying a little further may help
	    `dev/dev`(exp(fct),p,p+3)
	else FAIL
	fi
    fi
end: # `dev/dev/exp`

_equivX[1]:=1/_Xasy:

#savelib( `dev/dev`,`dev/dev/ratpoly`,`dev/dev/simplify`,`dev/dev/+`,`dev/dev/*`,`dev/dev/pow`,`dev/dev/ln`,`dev/dev/exp`,_equivX);
