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
##    Title:	infsolve
##    Created:	June 1987
##    Author:	Bruno Salvy
##	<salvy@poly.polytechnique.fr>
##
## Description: This function attempts to find the list of the
## least moduli roots.
##    fct: an expression
##    var: the only variable of fct
##    coefpos: boolean, true means that the Taylor coefficients
##	       of fct in 0 are all positive numbers
##    minsing: a lower bound on the modulus of the sought roots
##
##    One of the problems is to avoid the use of Cardano's formula 
## which make further computations VERY expensive.
##
##    Modified Nov 1988 BS	Listinf included
##    Modified Apr 1989 BS	More comments and a better treatment
##				of some algebraic expressions
##    Modified Jul 1994 BS	split into smaller functions
##    Modified Jul 2001 BS	fixed a pb with infinitelist/heuristic

`infsing/infsolve`:=proc(fctn,x,coefpos,minsing)
local fct,sol,val,nbr,i,j,lpol,lrac,k, l, f, aux, aux2, res;
global _EnvAllSolutions;
    # give their values to the quasi-inverse and the quasi-log
    if has(fctn,{'QuasiLog','QuasiInverse'}) then
	fct:=eval(subs('QuasiLog'=proc(x)ln(1/(1-x))end,'QuasiInverse'=proc(x)1/(1-x) end,fctn))
    else fct:=fctn fi;
    # try to fix a weakness of Maple with algebraic functions
    #  (this should be done somewhere else)
    if  (hastype(fct,anything^fraction) and not hastype(fct,float) or
	 has(map(op,indets(fct,RootOf)),x)) and
	type(convert(fct,RootOf),algfun(anything,x)) then
	RETURN(`infsing/infsolve/algfun`(fct,x,coefpos,minsing))
    fi;
    # Catch some periodicities at this stage
    l:=igcd(seq(op(2,i),i=indets(fct,identical(x)^integer)));
    if l>1 then
	f:=subs(x=x^(1/l),fct);
	if not hastype(f,identical(x)^fraction) then
	    lrac:=`infsing/infsolve`(f,x,coefpos,minsing^l);
	    nbr:=0;
	    for i in lrac do
		if i=infinity then nbr:=nbr+1; val[nbr]:=i
		else
		    for j from 0 to l-1 do
			val[nbr+1+j]:=i^(1/l)*exp(2*j*I*Pi/l) od;
		    nbr:=nbr+l
		fi
	    od;
	    RETURN([seq(val[i],i=1..nbr)])
	fi
    fi;
    # special case for polynomials
    if type(fct,polynom(anything,x)) then
	RETURN(`infsing/infsolve/polynom`(fct,x,minsing)) fi;
    # try to find a list of exact solutions
    _EnvAllSolutions:=true;
    # the extra eval below is due to a bug in solve (in maple6)
    # Example is infsing(2/(exp(z)+exp(-z)),z).
    sol:=eval([solve(fct,x)]);
    # if solve returned a RootOf, then we must find which of the roots has the
    # minimum modulus. Here we build the list
    if type(sol,[specfunc(polynom,RootOf)]) then
	RETURN(`infsing/infsolve/polynom`(subs(_Z=x,op(1,sol[1])),
	    x,minsing,fctn))
    # some algebraic expressions are solved by means of one or several RootOfs.
    # Once again we have to get rid of them.
    elif has(sol,RootOf) then
	lpol:=[seq(op(1,i),i=indets(sol,RootOf))];
	for i to nops(lpol) do
	    if type(lpol[i],polynom) then
		lrac:=`infsing/infsolve/polynom`(subs(_Z=x,lpol[i]),x);
		for j while j<=nops(sol) do
		    if has(sol[j],RootOf(lpol[i])) then
			sol:=[op(subsop(j=NULL,sol)),seq(subs(RootOf(lpol[i])=
			    k,sol[j]),k=lrac)];
			j:=j-1
		    fi
		od
	    elif select(proc(x) evalb(is(x,integer)=true) end,indets(i,name))={}
	    then
		# once again, it will be good to know an approximate value
		try 
		    aux:=fsolve(subs(_Z=x,lpol[i]),x);
		    if type(aux,float) then
			lrac:=numericRootOf(lpol[i],aux);
			if coefpos then `infsing/helpsignum`(lrac,aux) fi;
			for j while j<=nops(sol) do
			    if has(sol[j],RootOf(lpol[i])) then
				sol:=[op(subsop(j=NULL,sol)),
				    subs(RootOf(lpol[i])=lrac,sol[j])]
			    fi
			od
		    fi
		catch:
		end try
	    fi
	od
    fi;
    # suppress multiple roots and the origin
    sol:=[op({op(sol)} minus {0})];
    # cleanup a problem with LambertW in V.5
    aux:=map2(op,1,select(proc(x) evalb(nops(x)=2) end,
	indets(sol,specfunc(anything,LambertW))));
    if sol<>[] then
	# if we got an infinite list of roots, then we select the smallest ones
	aux:=aux union select(proc(x) evalb(is(x,integer)=true) end,indets(sol,name));
	if aux<>{} then
	    RETURN(`infsing/infsolve/infinitelist`(sol,minsing,aux)) fi;
	# When the function has positive Taylor coefficients, then
	# the singularity is a positive real number
	# if coefpos then
	#     j:=0;
	#     for i to nops(sol) do
	# 	if evalr(Signum(sol[i]))=1 then j:=j+1; val[j]:=sol[i] fi od;
	#     if j=0 then RETURN([infinity]) fi;
	#     sol:=[seq(val[i],i=1..j)]
	# fi;
	# Suppress the roots which are smaller than minsing
	if minsing<>0 then sol:=`infsing/infsolve/largerroots`(sol,minsing) fi;
	# Suppress some roots which were not roots (solve may be wrong)
	# e.g.	8*(1-4*z)**(1/2)+3-10*z+(1-4*z)**2-4*(1-4*z)**(3/2)+
	# 6*(1-4*z)*z-12*(1-4*z)**(1/2)*z
	sol:=`infsing/infsolve/checkroots`(sol,fct,x);
	# Now we build the list of the smallest roots among sol
	RETURN(`infsing/infsolve/smallest`(sol))
    # if there was no symbolic solution and the function is not a polynom, 
    # there still remains a little hope:
    elif not type(fct,polynom) then
	# most of the functions we encounter in the analysis of algorithms have
	# a real singularity smaller than one, try to see if this is the case
	try
	    aux:=fsolve(fct,x,0..1);
	    if aux=NULL or not type(aux,numeric) then
		# if this did not work, we also know that the root we are 
		# looking for is smaller than the smallest singularity of the 
		# expression, and that it is equal in some cases 
		# (e.g. sqrt(1-z))
		lrac:=infsing(fct,x,0)[1];
		aux:=[];
		for i to nops(lrac) do
		    try 
		    if zerotest(eval(subs(x=lrac[i],fct)))=true then
			aux:=[op(aux),lrac[i]]
		    fi
		    catch:
		    end try
		od;
		try aux2:=fsolve(fct,x,0..evalf(abs(lrac[1])));
		# if this did not yield any result, then there is probably no
		# solution
		if aux2=NULL or not type(aux2,numeric) then
		    if aux=[] then RETURN([infinity])
		    else RETURN(aux)
		    fi
		elif abs(aux2)>Float(1,-Digits+2) then aux:=aux2
		else RETURN([infinity]) 
		fi
		catch:
		end try
	    else
		# Since we are looking for non zero roots, we must be careful 
		# with the floating point solver.
		if abs(aux)<Float(1,-Digits+2) then RETURN([infinity]) fi;
	    fi
	end try;
	aux:=`infsing/infsolve/largerroots`([aux],minsing);
	if aux<>[] then
	    val:=[RootOf(fct,x,aux[1])]; # there should be only one elt
	    if coefpos then
		`infsing/helpsignum`(val[1],aux[1]);
		# for functions with positive coefficients,
		# we can get the other roots
		k:=select(has,indets(fct),x);
		if type(k,set(identical(x)^integer)) then
		    res:=igcd(seq(op(2,l),l=k));
		    if res>1 then
			val:=[seq(val[1]*exp(2*I*k*Pi/res),k=0..res-1)]
		    fi
		fi
	    fi;
	    RETURN(val)
	else RETURN([infinity])
	fi
    else RETURN([infinity])
    fi
end:

`infsing/helpsignum` := proc(expr, val)
global `property/object`;
local intvl;
	intvl:=shake(val);
	# This helps signum in later computations
	if type(intvl,specfunc(range,'INTERVAL')) then
	    `property/object`[expr]:='RealRange'(op(map(Open,map(op,intvl))));
	    # also Im needs to be helped
	    if type(expr,specfunc(anything,RootOf)) then 
		`Im/RootOf`(op(expr)):=0;
		`Im/RootOf`(op(applyop(simplify,1,expr))):=0
	    fi
	fi;
end proc: # `infsing/helpsignum`

`infsing/infsolve/algfun`:=proc (fct,x,coefpos,minsing)
local f, ind, i, u, carefulsubs;
    f:=numer(normal(convert(fct,RootOf)));
    ind:=[op(indets(f,RootOf))];
    # sort them
    for i while i<nops(ind) do
	if not has(op(1,op(i,ind)),x) then ind:=subsop(i=NULL,ind); i:=i-1
	elif has([op(i+1..nops(ind),ind)],op(i,ind)) then
	    ind:=[op(1..i-1,ind),op(i+1..nops(ind),ind),op(i,ind)];i:=i-1
	fi
    od;
    if ind=[] then `infsing/infsolve`(f,x,coefpos,minsing)
    else
	carefulsubs:=proc(x,u)
	    if not has(x,RootOf) then subs(_Z=u,x)
	    elif type(x,RootOf) then x
	    else map(procname,x,u) fi end:
	for i in ind do f:=resultant(subs(i=u,f),carefulsubs(op(1,i),u),u) od;
	f:=quo(f,gcd(f,diff(f,x)),x);
	if subs(x=0,f)=0 then f:=expand(f/x) fi;
	`infsing/infsolve/polynom`(f,x,minsing,fct)
    fi
end: # `infsing/infsolve/algfun`

`infsing/infsolve/polynom`:=proc (pol, x, minsing, fct)
local p, sol, i;
    p:=collect(pol,x);
    if degree(p,x)<=2 then sol:=solve(pol,x)
    else sol:=fsolve(pol, x, complex) fi;
    sol:={sol} minus {0};
    if nargs>2 and minsing<>0 then
	sol:=`infsing/infsolve/largerroots`(sol,evalf(minsing)) fi;
    if nargs>3 then sol:=`infsing/infsolve/checkroots`(sol,fct,x) fi;
    if nargs>2 then sol:=`infsing/infsolve/smallest`(sol) fi;
    if sol=[infinity] or degree(p,x)<=2 then sol
    else [op({seq(numericRootOf(subs(x=_Z,pol),i),i=sol)})] fi
end: # `infsing/infsolve/polynom`

`infsing/infsolve/largerroots`:=proc(l,minsing)
local i, j, res;
    j:=0;
    for i to nops(l) do
	if comparemodule(l[i],minsing)=`>` then j:=j+1; res[j]:=l[i] fi od;
    [seq(res[i],i=1..j)]
end: # `infsing/infsolve/largerroots`

`infsing/infsolve/checkroots`:=proc(sol,fct,x)
local i, check, res, j, g;
    j:=0;
    g:='abs'(fct);
    for i to nops(sol) do
	try 
	    check:=evalf(subs(x=sol[i],g));
	    if type(check,numeric) and check<0.001 then j:=j+1; res[j]:=sol[i]
	    # elif type(sol[i],float) then # roots should be refined there
	    fi
	catch: 
	end try
    od;
    [seq(res[i],i=1..j)]
end: # `infsing/infsolve/checkroots`

`infsing/infsolve/smallest`:=proc(sol)
local nb, res, i, aux;
    if nops(sol)=0 then [infinity]
    else
	nb:=1;
	res[1]:=sol[1];
	for i from 2 to nops(sol) do
	    aux:=comparemodule(res[1],sol[i]);
	    if aux=`=` then nb:=nb+1; res[nb]:=sol[i]
	    elif aux=`>` then nb:=1;  res[nb]:=sol[i]
	    fi
	od;
	[seq(res[i],i=1..nb)]
    fi
end: # `infsing/infsolve/smallest`

`infsing/infsolve/infinitelist`:=proc (gsol, mini, inds)
local var, a, b, aux, x, c, d, ia, ib, lpos, ra, rb, expr, res, gvar, sol, exps, tosubs;
    gvar:=select(proc(x) evalb(is(x,integer)=true) end,inds);
    # filter out phantom variables
    exps:=select(has,map(op,select(type,indets(gsol),specfunc(linear(Pi),exp))),
	gvar);
    if exps<>{} then # this could be refined and made more efficient
	for expr in exps do
	    if is(coeff(expr,Pi,1)/I,even) then
		tosubs[expr]:='exp'(expr)=exp(subs(Pi=0,expr))
	    else tosubs[expr]:=NULL fi
	od;
	sol:=subs([seq(tosubs[expr],expr=exps)],gsol);
	gvar:=(inds minus gvar) union 
	    select(proc(x) evalb(is(x,integer)=true) end,indets(sol,name))
    else sol:=gsol; gvar:=inds
    fi;
    for expr in sol do
	var:=gvar intersect indets(expr,name);
	if nops(var)=0 then res[expr]:=expr
	elif nops(var)>1 or not type(expr,linear(op(var))) then
	    res[expr]:=`infsing/infsolve/infinitelist/heuristic`(expr,var,mini)
	else
	    var:=op(var);
	    a:=evalc(coeff(expr,var,1));
	    if a=0 then res[expr]:=b
	    else
		b:=evalc(coeff(expr,var,0));
		rb:=coeff(b,I,0);
		ib:=coeff(b,I,1);
		ra:=coeff(a,I,0);
		ia:=coeff(a,I,1);
		# Compute the distance from the line a_x+b to the origin
		aux:=evalr(Signum(abs(ib*ra-rb*ia)/sqrt(ra^2+ia^2)-mini));
		if aux=1 then
		    # the smallest modulus will be larger than mini anyway.
		    # Compute x such that ax+b 
		    # is the closest point to the origin
		    x:=evalr(`evalr/intpart`(-(ra*rb+ia*ib)/(ra^2+ia^2)));
		    if not type(x,integer) then x:=evalf(x) fi;
		    res[expr]:=a*x+b,a*(x+1)+b
		elif aux=-1 or aux=0 then
		    # Compute the intersections of the line and the circle
		    aux:=sqrt(2*ia*ib*ra*rb-ra^2*ib^2+ra^2*mini^2
			-ia^2*rb^2+ia^2*mini^2);
		    c:=-(ra*rb+ia*ib);
		    d:=ra**2+ia**2;
		    aux:=[(c-aux)/d,(c+aux)/d];
		    # Check if they might be integer
		    lpos:=map(shake,aux,Digits);
		    if not type(lpos,['INTERVAL'(range),'INTERVAL'(range)])
			then res[expr]:=NULL
		    else
			aux:=floor(op(1,op(lpos[1])));
			if aux>=op(1,op(lpos[1])) and aux<=op(2,op(lpos[1]))
			    then lpos:=subsop(1=aux-1,lpos)
			else lpos:=subsop(1=aux,lpos)
			fi;
			aux:=ceil(op(1,op(lpos[2])));
			if aux>=op(1,op(lpos[2])) and aux<=op(2,op(lpos[2]))
			    then lpos:=subsop(2=aux+1,lpos)
			else lpos:=subsop(2=aux,lpos)
			fi;
			res[expr]:=a*lpos[1]+b,a*lpos[2]+b
		    fi
		else
		    res[expr]:=NULL
		fi
	    fi
	fi
    od;
    `infsing/infsolve/smallest`([seq(res[expr],expr=sol)])
end: # `infsing/infsolve/infinitelist`

# select those solutions that correspond to small values of the parameter(s)
# if those are in the right area wrt mini.
`infsing/infsolve/infinitelist/heuristic`:=proc (expr, vars, mini)
local i, var, val, j, res, nbtests;
    nbtests:=5; 
    if nops(vars)=1 then 
	var:=op(vars);
	val:=[seq(abs(evalf(subs(var=i,expr))),i=-nbtests..nbtests)];
	if member(min(op(val)),val,'i') and (i=nops(val) or i=1) then
	    ERROR(`cannot find smallest singularity`) fi;
	i:=i-nbtests-1;
	for j from 0 to 10 while comparemodule(subs(var=i+j,expr),mini)<>`>`
	    do od;
	if j=11 then ERROR(`cannot find smallest singularity`) fi;
	res:=subs(var=i+j,expr);
	if j<>0 then # go backwards
	    for j to 10 while comparemodule(subs(var=i-j,expr),mini)<>`>`do od;
	    if j=11 then ERROR(`cannot find smallest singularity`) fi;
	    res,subs(var=i-j,expr)
	else res 
	fi
    else 
	seq(procname(subs(op(1,vars)=i,expr),subsop(1=NULL,vars),mini),i=-nbtests..nbtests)
    fi
end: # `infsing/infsolve/infinitelist/heuristic`

#savelib( `infsing/infsolve`,`infsing/infsolve/algfun`,`infsing/infsolve/polynom`,`infsing/helpsignum`,`infsing/infsolve/checkroots`,`infsing/infsolve/largerroots`,`infsing/infsolve/smallest`,`infsing/infsolve/infinitelist`,`infsing/infsolve/infinitelist/heuristic`);
