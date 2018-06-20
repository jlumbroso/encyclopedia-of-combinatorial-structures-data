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
##################################################
#
#				EQUSING
#
#   Given an expression and its singularities, this
# procedure returns an expansion of the coefficients.
#
##################################################

`equivalent/equsing`:=proc(fct,Sing,palg,pdev,x,typesing)
local i,res, devint, resint, f, ord, nextsing, sing, p, mult, j, mmult, remdev, found, root, co;
    res:=0;
    _EnvXasyinteger:=true;
    sing:=Sing;
    # special case for rational functions (important for denumerants)
    # Ideally, it would be better to follow the multiplicities while
    # computing the polar singularities. This means changing infsolve
    # and the type of the output of infsing.
    if type(fct,ratpoly(rational,x)) then
	f:=normal(fct);
	if type(f,polynom) then RETURN(0) fi;
	p:=sqrfree(denom(f),x)[2];
	mmult:={seq(j[2],j=p)};
	if nops(mmult)>1 then
	    mmult:=max(op(mmult));
	    for j to mmult do mult[j]:=NULL od;
	    for i in Sing do
		for j in p do
		    if type(i,specfunc(anything,RootOf)) and 
			gcd(j[1],subs(_Z=x,op(1,i)))<>1 or
			expand(subs(x=i,j[1]))=0 then
			mult[j[2]]:=mult[j[2]],i; break fi od
	    od;
	    for i from mmult by -1 to 1 while mult[i]=NULL do od;
	    sing:=[seq(mult[j],j=max(1,i-palg+1)..i)]
	else i:=op(mmult)
	fi;
	# Speed-up for a very special case.
	# This should be useless once ratasympt is efficient.
	if palg=1 and member(1,sing) and pdev<=2 then
	    mult:=i;
	    resint:=[1,1/(mult-1)!,-mult+1,1,-mult+2];
	    p:=factors(convert(map(proc(x) if x[2]=i then x[1]fi end,p),`*`));
	    p:=[seq(j[1],j=p[2])];
	    if nops(p)>1 then
		for i in sing do
		    for j in p do
			if type(i,specfunc(anything,RootOf)) and 
			    gcd(j,subs(_Z=x,op(1,i)))<>1 or
			    expand(subs(x=i,j))=0 then
			    found[j]:=1;
			    if not assigned(root[j]) then
				root[j]:={i}
			    else root[j]:=root[j] union {i}
			    fi;
			    break
			fi
		    od
		od;
		p:=map(proc(x,found)if assigned(found[x]) then x fi end,p,found)
	    else root[op(p)]:=sing
	    fi;
	    # this sets up the right variables in alglogeq
	    `equivalent/alglogeq`(`dev/dev`(subs(x=1-1/_Xasy,f),2,2),1,1);
	    # bug fix (May 2001, BS): member was assumed to return true
	    if not member(0,subs(x=1,p),'j') then RETURN(0) fi;
	    p:=[p[j],op(subsop(j=NULL,p))];
	    for j in p do
		co:=(-1)^mult*normal(f*j^mult/diff(j,x)^mult);
		co:=rem(numer(co),j,x)/rem(denom(co),j,x);
		for i in root[j] do
		    res:=`dev/add`(res,
			`dev/prd`(`dev/prd`(subs(x=i,co),resint),
			`dev/exp`(`dev/prd`([1,1,-1,0,infinity],
			    `dev/ln`(`dev/pow`(i,-1),2)),1)))
		od
	    od;
	    RETURN(res)
	fi
    fi;
    remdev:=op(4,op(`dev/dev`));
    for i to nops(sing) do
    # It would be a good idea to deal with one of the conjugates and obtain
    # the other results by conjugation, when possible.
	f:=subs(x=sing[i]*(1-1/_Xasy),fct);
	if typesing='polar' and remdev<>NULL
	    and assigned(remdev[f,2,3]) then
	    #use the remember table of dev/dev to avoid computing too many terms
	    ord:=min(pdev,-op(3,`dev/dev`(f,2,3)))
	else
	    ord:=max(pdev,2)
	fi;
	devint:=`dev/dev`(f,ord,ord);
	resint:=`equivalent/alglogeq`(devint,palg);
	if resint='H' then
	    resint:=`equivalent/saddlepoint/Hayman`(eval(subs(L=proc(x)ln(1/(1-x)) end,
		Q=proc(x) 1/(1-x) end,fct)),x,sing[i]);
#		This test should be useless since the function has been 
#	       declared H-admissible.
#		if nops(sing)>1 then
#		    ERROR(`cannot handle multiple saddle points`)
#		fi;
	    if has(resint,_saddlepoint) then
		if _NBSADDLEPOINTS=1 then 
		    RETURN([_saddlepoint,sing[i],op(2,resint)])
		else RETURN([_saddlepoint[1],sing[i],op(2,resint)])
		fi
	    else
		RETURN(op(2,resint))
	    fi
	elif resint='HS' then
	    resint:=`equivalent/saddlepoint/HarrisSchoenfeld`(eval(subs(L=proc(x)ln(1/(1-x)) end,
		Q=proc(x) 1/(1-x) end,fct)),palg,x,sing[i]);
	    if nops(sing)>1 then
		ERROR(`cannot handle multiple saddle points`)
	    fi;
	    if _NBSADDLEPOINTS=1 then
		RETURN([_saddlepoint,sing[i],resint])
	    else
		RETURN([_saddlepoint[1],sing[i],resint])
	    fi
	elif resint='pointcol' then
	    resint:=`equivalent/saddlepoint/selle`(eval(subs(L=proc(x)ln(1/(1-x)) end,
		Q=proc(x) 1/(1-x) end,fct)),x,sing[i]);
	    if nops(sing)>1 then
		ERROR(`cannot handle difficult multiple saddle points`)
	    fi;
	    RETURN([_saddlepoint,sing[i],resint])
	fi;
	## I do not understand the following tests anymore and I suppress
	## them. BS. Aug 01.
	# these tests avoid expensive computations for complex singularities
	#if not type(resint,list) or not type(res,list) or resint[1]<>res[1] or
	#    resint[3]<res[-1] then 
	## Frequent special case:
	    if i=2 and nops(sing)=2 and sing[1]=conjugate(sing[2]) then 
		# f is not necessarily real here (but something better should
		# be done in that case)
		res:=`dev/add`(res,`dev/prd`(resint,
		    `dev/conjugate`(
		    `dev/exp`(`dev/prd`([1,1,-1,0,infinity],
		    `dev/ln`(`dev/pow`(sing[1],-1),pdev)),palg))))
	    else
		res:=`dev/add`(res,`dev/prd`(resint,
		    `dev/exp`(`dev/prd`([1,1,-1,0,infinity],
		    `dev/ln`(`dev/pow`(sing[i],-1),pdev)),palg)))
	    fi
	#fi;
    od;
    if pdev<palg+2 and type(res,list) and (
	(res[1]<>true and `dev/length`(res)<=palg) or
	(res[1]=true and max(seq(`dev/length`(op(2,i)),i=subsop(1=NULL,res)))
	    <=palg)) then
	if typesing<>'polar' then
	    `equivalent/equsing`(fct,sing,palg,pdev+1,x,typesing)
	else # subtraction of singularities
	    try
    		nextsing:=infsing(fct,x,abs(sing[1]));
    		if nextsing[1]<>[] and nextsing[1][1]<>infinity and
    		nextsing[2]<>'essential' then
    		if res[1]=1 and res[-1]<>infinity then
    		    res:=subsop(-2=0,-1=infinity,res)
    		elif res[1]=0 and res[-2][-1]<>infinity then
    		    res:=subsop(-2=subsop(-2=NULL,-1=NULL,res[-2]),
    			-1=(res[-1],0,infinity),res)
    		fi;
    		`dev/add`(res,`equivalent/equsing`(fct,nextsing[1],
    		    (palg-`dev/length`(res)+1)$2,x,nextsing[2]))
    	        else res
    		fi
	    catch: res
	    end try
	fi
    else
	res
    fi
end:

#savelib( `equivalent/equsing`);
