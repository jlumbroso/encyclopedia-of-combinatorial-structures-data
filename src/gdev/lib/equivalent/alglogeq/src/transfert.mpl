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
##    Title:	transfert
##    Created:	June 87
##    Author:	Bruno Salvy
##	<salvy@poly.polytechnique.fr>
##
## Description: uses Flajolet-Odlyzko's theorem to translate the
## local expansion into the expansion of the coefficients when the
## exponents of the logarithms are not positive integers. 
##
##    Modified:	    Wed Nov 15 14:34:59 1989
##    Author:	Bruno Salvy
##    Modification: completely rewritten
##

`equivalent/alglogeq/transfert`:=proc(dev,p)
local f, x, res, j, k, nb;
    if type(op(3,dev),integer) and op(3,dev)<=0 then nb:=2 else nb:=1 fi;
    f:=series(1/GAMMA(-x),x=op(3,dev),p+nb+1);
    res:=0;
    if op(2,f)=0 then
       for j from 2 by 2 to nops(op(2,dev)) while
	op(j+1,op(2,dev))<p+op(3,op(2,dev)) do
	   res:=`dev/add`(res,[2,seq(op([op(j,op(2,dev))*(-1)^k*
	    binomial(-op(j+1,op(2,dev)),k)*op(2*k+1,f)*k!,k+op(j+1,op(2,dev))])
	    ,k=0..p+op(3,op(2,dev))-op(j+1,op(2,dev)))])
       od
    else # it must be 1
       for j from 2 by 2 to nops(op(2,dev)) while
	op(j+1,op(2,dev))<p+op(3,op(2,dev)) do
	    res:=`dev/add`(res,[2,seq(op([op(j,op(2,dev))*(-1)^k*
	    binomial(-op(j+1,op(2,dev)),k)*op(2*k-1,f)*k!,k+op(j+1,op(2,dev))])
	    ,k=1..p+1+op(3,op(2,dev))-op(j+1,op(2,dev)))])
       od
    fi;
    [1,res,op(3,dev)+1]
end:

#savelib( `equivalent/alglogeq/transfert`);
