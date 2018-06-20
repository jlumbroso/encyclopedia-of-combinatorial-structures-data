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
##    Title:	saddle
##    Created:	Fri Feb 24 20:01:59 1989
##    Author:	Bruno Salvy
##	<salvy@gigondas.inria.fr>
##
## Description: determines the saddle point(s) and its (their) expansion(s) 
## to the right order.

`equivalent/saddlepoint/saddle`:=proc (l, sing, fct, var, printit, n)
local i,tosubs, j, r, pts, alreadytried, res, sig;
global _NBSADDLEPOINTS;
    r:=l;
    if nops(r)>0 and length(r)<100 and not has(r,RootOf) then
	for i while i<=nops(r) do
	    res:=`dev/dev`(subs(n=_Xasy,op(i,r)),1,1);
	    if (not type(res,list) or op(3,res)=0) and sing<>infinity then
		if (not type(res,list) and zerotest(sing,res)=true) or
		    zerotest(op(2,res),sing)=true then
		    if i>1 then
			if zerotest(op(1,r),op(i,r))=true then
			    r:=subsop(i=NULL,r);
			    i:=i-1
			else
			    for j to 5 do
				sig:=`dev/compare`(
				    `dev/dev`(subs(n=_Xasy,op(i,r)),j,j),
				    `dev/dev`(subs(n=_Xasy,op(1,r)),j,j));
				if sig=-1 then
				    r:=subsop(1=NULL,r);
				    i:=i-1;
				    j:=5
				elif sig=1 then
				    r:=subsop(i=NULL,r);
				    i:=i-1;
				    j:=5
				fi
			    od
			fi
		    fi
		else
		    r:=subsop(i=NULL,r);
		    i:=i-1
		fi
	    elif (op(3,res)<0
		and evalr(Signum(`dev/lcoeff`(op(2,res))))=1) or
		    (op(3,res)=0 and type(op(2,res),list) and op(3,op(2,res))<0 and
			evalr(Signum(`dev/lcoeff`(op(2,op(2,res)))))=1) then
		if i>1 then
		    if zerotest(op(1,r),op(i,r))=true then
			r:=subsop(i=NULL,r);
			i:=i-1
		    else
			for j to 5 do
			    sig:=`dev/compare`(
				`dev/dev`(subs(n=_Xasy,op(i,r)),j,j),
				`dev/dev`(subs(n=_Xasy,op(1,r)),j,j));
			    if sig=1 then
				r:=subsop(1=NULL,r);
				i:=i-1;
				j:=5
			    elif sig=-1 then
				r:=subsop(i=NULL,r);
				i:=i-1;
				j:=5
			    fi
			od
		    fi
		fi
	    else
		r:=subsop(i=NULL,r);
		i:=i-1
	    fi
	od;
#	r:=op(r);
	pts:=map(`dev/dev`,subs(n=_Xasy,r),3,3);
	tosubs:=[];
	for i while i<=nops(pts) do
	    if not type(pts[i],list) or pts[i][nops(pts[i])]=infinity then
		tosubs:=[op(tosubs),pts[i]]
	    else
		if op(1,pts[i])=1 then
		    if (sing=infinity and pts[i][nops(pts[i])]-pts[i][3]>1) or
			(sing<>infinity and pts[i][nops(pts[i])]>1) then
			tosubs:=[op(tosubs),pts[i]]
		    else
			# give it another chance
			pts:=subsop(i=`dev/dev`(subs(n=_Xasy,op(i,r)),5,5),pts);
			if (sing=infinity and pts[i][nops(pts[i])]-pts[i][3]>1) or
			    (sing<>infinity and pts[i][nops(pts[i])]>1) then
			    tosubs:=[op(tosubs),pts[i]]
			elif nops(pts)=1 then
			    print(`The saddle point is `,op(1,r));
			    print(`Saddle point's expansion:`);
			    print(`dev/print`(pts[1],n,5))
			else
			    print(cat(`The `,i,`th saddle point is `,op(i,r)));
			    print(`Its expansion is:`);
			    print(`dev/print`(pts[i],n,5))
			fi
		    fi
		elif nops(pts)=1 then
		    print(`The saddle point is `,r[1]);
		    print(`Saddle point's expansion:`);
		    print(`dev/print`(pts[1],n,3))
		else
		    print(cat(`The `,i,`th saddle point is `,op(i,r)));
		    print(`Its expansion is:`);
		    print(`dev/print`(pts[i],n,3))
		fi
	    fi
	od
    else
	pts:=`equivalent/saddlepoint/pointcol`(fct,var,sing,3);
	tosubs:=[];alreadytried:=false;
	if pts<>FAIL then
	    for i while(i<=nops(pts)) do
		if pts[i][1]=1 then
		    if not printit and (
			pts[i][nops(pts[i])]=infinity or (sing=infinity and
			pts[i][nops(pts[i])]-pts[i][3]>1) or (sing<>infinity 
			    and pts[i][nops(pts[i])]>1 or pts[i][nops(pts[i])]=1 and
				type(pts[i][nops(pts[i])-1],list))) then
			tosubs:=[op(tosubs),pts[i]]
		    elif not printit and nops(pts[i])>3 and not alreadytried then
			# give it another chance
			alreadytried:=true;
			res:=`equivalent/saddlepoint/pointcol`(fct,var,sing,5);
			if (sing=infinity and res[1][nops(res[1])]-res[1][3]>1) or
			    (sing<>infinity and res[1][nops(res[1])]>1) then
			    tosubs:=[];
			    pts:=res;
			    i:=0
			elif nops(pts)=1 then
			    print(`Saddle point's expansion:`);
			    print(`dev/print`(pts[1],n,3))
			else
			    print(cat(`Saddle point number `,i,`:`));
			    print(`dev/print`(pts[i],n,3))
			fi
		    elif nops(pts)=1 then
			print(`Saddle point's expansion:`);
			print(`dev/print`(pts[1],n,3))
		    else
			print(cat(`Saddle point number `,i,`:`));
			print(`dev/print`(pts[i],n,3))
		    fi
		elif nops(pts)=1 then
		    print(`Saddle point's expansion:`);
		    print(`dev/print`(pts[1],n,3))
		else
		    print(cat(`Saddle point number `,i,`:`));
		    print(`dev/print`(pts[i],n,3))
		fi
	    od
	else
	    print(`Warning: saddle point value not found`)
	fi
    fi;
    _NBSADDLEPOINTS:=nops(pts); # Yes, this is dirty.
    if tosubs<>[] then tosubs else NULL fi
end: # saddle

#savelib( `equivalent/saddlepoint/saddle`);
