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
`equivalent/saddlepoint/pointcol`:=proc(f,x,sing,p)
local aux, aux2;
    aux:=max(3,p);
    if sing<>infinity then
	aux2:=`dev/implicit`(`dev/dev`(subs(x=sing*(1-1/_Xasy),x*diff(f,x)/f),
	    aux,aux),[1,1,-1,0,infinity],aux)
    else
	aux2:=`dev/implicit`(`dev/dev`(subs(x=_Xasy,x*diff(f,x)/f),aux,aux),
	    [1,1,-1,0,infinity],aux)
    fi;
    if aux2<> FAIL then
	if sing=infinity then aux2
	else
	    map(`dev/multbyreal`,map(`dev/add`,map(`dev/pow`,
		aux2,-1,p),-1),-sing)
	fi
    else FAIL
    fi
end:

#savelib( `equivalent/saddlepoint/pointcol`);
