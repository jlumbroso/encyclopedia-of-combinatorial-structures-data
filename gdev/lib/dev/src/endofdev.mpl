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
##    Title:	`dev/endofdev`
##    Created:	Tue May 16 11:35:36 1989
##    Author:	Bruno Salvy
##	<bruno.salvy@inria.fr>
##
## Description: Input: an expansion tending to 0, an order, a polynomial.
##		Output: the substitution of the expansion in the polynomial,
## performing as few computations as possible, or at least as few as I could.
##  The polynomial is a list of coeff,deg. It is better if it is dense.

`dev/endofdev`:=proc (dev,k,Pol)
local n, res, posmin, mini, j, ind, pos, val, candidates, result, interm, i, 
comblin, p, devcoeff, exact, currdeg, degpol, pol, tocomp, sig;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if dev=[1,1,1,0,infinity] then [1,op(1..min(2*k+2,nops(Pol)),Pol)]
    elif type(dev,undefined) then undefined
    else
	n:=nops(dev);
	if dev[n]=infinity then
	    exact:=true;
	    n:=iquo(n,2)-1
	else
	    exact:=false;
	    n:=iquo(n,2)
	fi;
	# the exponents in the resulting expansion will be linear combinations
	# with positive integer coefficients of the exponents of the elements in
	# dev. The difficulty is that these exponents are not necessarily 
	# integers.
	# The first linear combination is 0:
	res:=[{[0$n]}];
	# which corresponds to the 0th-power
	val:=[0];
	# and whose value is 1:
	devcoeff[0$n]:=1;
	# At the begining, the maximal exponent of pol which may be used is the 
	# first one:
	pos:=1;
	if pos<n or exact then
	    if type(dev[2*pos],list) then
		devcoeff[0$(pos-1),1,0$(n-pos)]:=[op(dev[2*pos]),0,infinity]
	    else
		devcoeff[0$(pos-1),1,0$(n-pos)]:=dev[2*pos]
	    fi
	else
	    if type(dev[2*pos],list) then
		devcoeff[0$(pos-1),1,0$(n-pos)]:=dev[2*pos]
	    else
		devcoeff[0$(pos-1),1,0$(n-pos)]:=1
	    fi
	fi;
	# the corresponding exponent in the result will be the first one
	ind[1]:=1;
	# We need a dense representation of the polynomial:
	if Pol[nops(Pol)]=infinity then
	    degpol:=Pol[nops(Pol)-2];
	    pol:=array(0..degpol,sparse);
	    for i by 2 to nops(Pol)-3 do pol[Pol[i+1]]:=Pol[i] od
	else
	    degpol:=Pol[nops(Pol)];
	    pol:=array(0..degpol,sparse);
	    for i by 2 to nops(Pol)-1 do pol[Pol[i+1]]:=Pol[i] od;
	fi;
	# the final expansion will be stored in result:
	if pol[0]<>0 then result:=[dev[1],pol[0],0];
	else result:=[dev[1]];
	fi;
	currdeg:=0;
	while (`dev/length`(result)<k+1) and currdeg<degpol
	    and (exact or pos<n or ind[n]=1) do
	    # Find the smallest possible exponent:
	    mini:=dev[3]+val[ind[1]];
	    # and the list of the corresponding multipliers:
	    posmin:=[1];
	    for j from 2 to min(pos,n) do
		tocomp:=dev[2*j+1]+val[ind[j]];
		sig:=evalr(Signum(tocomp-mini));
		if sig=-1 then
		    mini:=tocomp;
		    posmin:=[j]
		elif sig=0 then posmin:=[op(posmin),j]
		fi
	    od;
	    # When the current maximal possible exponent is used for the first
	    # time, then append the next one to the list of possible multipliers
	    if posmin[nops(posmin)]=pos and pos<n then
		pos:=pos+1;
		ind[pos]:=1;
		if pos<n or exact then
		    if type(dev[2*pos],list) then
			devcoeff[0$(pos-1),1,0$(n-pos)]:=
			    [op(dev[2*pos]),0,infinity]
		    else
			devcoeff[0$(pos-1),1,0$(n-pos)]:=dev[2*pos]
		    fi
		else
		    if type(dev[2*pos],list) then
			devcoeff[0$(pos-1),1,0$(n-pos)]:=dev[2*pos]
		    else
			devcoeff[0$(pos-1),1,0$(n-pos)]:=1
		    fi
		fi
	    fi;
	    # Now use this information for the actual computation of the
	    # expansion
	    candidates:={};
	    interm:=0;
	    for j to nops(posmin) do
		for i in res[ind[posmin[j]]] do
		    # the new linear combination:
		    comblin:=[op(1..posmin[j]-1,i),i[posmin[j]]+1,
			op(posmin[j]+1..n,i)];
		    if not member(comblin,candidates) then
			candidates:=candidates union {comblin};
			# this necessitates the computation of a new product:
			if i<>[0$n] then
			    devcoeff[op(comblin)]:=`dev/prd`(devcoeff[op(i)],
				devcoeff[0$(posmin[j]-1),1,0$(n-posmin[j])])
			fi;
			# which we multiply by the proper coefficient to get
			# the result:
			currdeg:=convert([seq(comblin[p],p=1..n)],`+`);
			if currdeg<=degpol and pol[currdeg]<>0 then
			    interm:=`dev/add`(interm,`dev/multbyreal`(
				devcoeff[op(comblin)],pol[currdeg]
				*currdeg!/convert(map(factorial,comblin),`*`)))
			fi
		    fi
		od;
		ind[posmin[j]]:=ind[posmin[j]]+1
	    od;
	    res:=[op(res),candidates];
	    val:=[op(val),mini];
	    if pos<n or exact or ind[n]=1 then
		if type(interm,list) then
		    interm:=[op(1..nops(interm)-2,interm)]
		fi
	    else
		if not type(interm,list) then interm:=1 fi
	    fi;
	    if interm<>0 then
		result:=[op(result),interm,mini]
	    fi
	od;
	if `dev/length`(result)<k+1 and exact and pos=n then
	    [op(result),0,infinity]
	else
	    result
	fi
    fi
end: # `dev/endofdev`

#savelib( `dev/endofdev`);
