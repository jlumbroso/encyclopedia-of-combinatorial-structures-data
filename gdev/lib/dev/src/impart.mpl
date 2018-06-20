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
##    Title:	impart
##    Created:	Thu Sep 19 20:35:56 1991
##    Author:	Bruno Salvy
##	<salvy@rully.inria.fr>
##
## Description: returns the imaginary part of an expansion.

`dev/impart`:=proc(expr)
local i, result, n, aux;
option `Copyright Bruno Salvy, INRIA Rocquencourt, France`;
    if not type(expr,list) then
	# coeff(evalc(),I,1) does not work for ln(1/(-LambertW(1,-exp(-2))-2)) 
	# and evalc@Im is needed because of RootOf(..,a+I*b)
	if type(expr,realcons) then 0 else I*evalc(Im(expr)) fi
    else
	n:=nops(expr);
	for i from 2 by 2 to n-3 do
	    if type(expr[i],list) then
		aux:=`dev/impart`([op(expr[i]),0,infinity]);
		if type(aux,list) and aux[nops(aux)]=infinity then
		    aux:=subsop(nops(aux)=NULL,nops(aux)-1=NULL,aux) fi
	    else aux:=`dev/impart`(expr[i]) fi;
	    if aux<>0 then result[i]:=aux,expr[i+1] else result[i]:=NULL fi
	od;
	result:=seq(result[2*i],i=1..iquo(n-3,2));
	if expr[n]=infinity then
	    if result=NULL then 0
	    elif nops([result])=2 and result[2]=0 then result[1]
	    else [expr[1],result,0,infinity] fi
	elif type(expr[n-1],list) then
	    [expr[1],result,`dev/impart`(expr[n-1]),expr[n]]
	else [expr[1],result,1,expr[n]]
	fi
    fi
end:

#savelib( `dev/impart`);
