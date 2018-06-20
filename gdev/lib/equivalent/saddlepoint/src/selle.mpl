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
#				    SELLE
#
#   Apply saddle-point method, the user is asked to
# check the validity by himself.
#
##################################################

`equivalent/saddlepoint/selle`:=proc(fct,var,sing)
local u,h,x,n;
    x:=var;
    h:=ln(fct)-(n+1)*ln(x);
if assigned(_EnvSolveSaddlePoint) and _EnvSolveSaddlePoint=false then
    u:=[]
else
    u:=[solve(diff(h,x)=0,x)];
fi;
    `equivalent/saddlepoint/saddle`(u,sing,fct,var,true,n);
    print(`Assuming the saddle point method is valid in this context,`);
    print(`the result is:`);
    if sing<>infinity then
	_NBSADDLEPOINTS,`dev/reduce`(`dev/dev`(
	    subs(x=sing*(1-1/_Xasy),fct/(2*Pi)^(1/2)/diff(h,x,x)^(1/2)),1,1),2)
    else
	_NBSADDLEPOINTS,`dev/reduce`(`dev/dev`(subs(x=_Xasy,fct/x^n/
	    (2*Pi)^(1/2)/diff(h,x,x)^(1/2)),1,1),2)
    fi
end:
#savelib( `equivalent/saddlepoint/selle`);
