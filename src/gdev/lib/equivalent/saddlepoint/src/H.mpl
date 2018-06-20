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
#			    Hayman
#
#  Apply Hayman's formula for H-admissible functions.
#
##################################################

`equivalent/saddlepoint/Hayman`:=proc(fct,var,sing)
local a,b,r,x,tosubs,aux, n;
#    aux:=normal(fct);
#    if aux<>fct then procname(aux,var,sing)
#    else
	x:=var;
	# expand because of examples like z*exp(z)
	a:=expand(x*diff(fct,x)/fct);
	b:=x*diff(a,x);
if assigned(_EnvSolveSaddlePoint) and _EnvSolveSaddlePoint=false then
    r:=[]
else
	try 
	    r:=[(solve(a=n,x))]
	catch: r:=[]
	end try
fi;
	tosubs:=`equivalent/saddlepoint/saddle`(r,sing,fct,var,false,n);
	if tosubs=NULL then
	    if sing<>infinity then
		[_saddlepoint,`dev/reduce`(`dev/dev`(subs(x=sing*(1-1/_Xasy),
		    fct/(2*Pi)^(1/2)/b^(1/2)),1,1),2)]
	    else
		[_saddlepoint,`dev/reduce`(`dev/dev`(subs(x=_Xasy,
		    fct/(2*Pi)^(1/2)/b^(1/2)),1,1),2)]
	    fi
	else # There is always only one relevant saddle point
	    tosubs:=`dev/print`(op(1,tosubs),_Xasy,infinity);
	    try 
		aux:=(`dev/reduce`(
		    `dev/dev`(subs(x=tosubs,fct/(2*Pi)^(1/2)/b^(1/2))*
		    'exp'(-_Xasy*ln(tosubs)),2,2),2));
		if aux[nops(aux)]<>infinity then [0,aux]
		else [0,`dev/prd`(aux,[1,1,0,1,1])]
		fi
	    catch "Not a convergent expansion": 
		print(`Saddle point's expansion : `);
		print(`dev/print`(tosubs,n,3));
		if sing<>infinity then
		    [_saddlepoint,`dev/reduce`(`dev/dev`(
		    subs(x=sing*(1-1/_Xasy),fct/(2*Pi)^(1/2)/b^(1/2)),1,1),2)]
		else
		    [_saddlepoint,`dev/reduce`(`dev/dev`(subs(x=_Xasy,
			fct/(2*Pi)^(1/2)/b^(1/2)),1,1),2)]
		fi
	    catch: error
	    end try
	fi
#    fi
end:

#savelib( `equivalent/saddlepoint/Hayman`);
