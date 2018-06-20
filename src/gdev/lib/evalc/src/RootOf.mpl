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
#
# modif BS: two-argument RootOf handled. If second arg numeric then use it.
#            

# this one does not disappear with maple6

`evalc/RootOf`:=proc(p)
local x,y,p1,p2,a,b,v, c, rof, g;
options `Copyright 1993 by Waterloo Maple Software`;
    if nargs=1 or (nargs=2 and type(args[2],`=`(identical(label),anything))) 
	and type(p,'polynom(algnum,_Z)') then
	if type(p,'polynom(numeric,_Z)') and
	    sturm(sturmseq(p,_Z),_Z,-infinity,infinity)=degree(p,_Z) then
	    # p has real coefficients and real roots only
	    `evalc/remember`('RootOf'(p)):=`evalc/split`('RootOf'(p),0)
	else
	    v := `evalc/evalc`(subs(_Z = x+I*y,p));
	    a := op(1,v);
	    b := op(2,v);
	    p1 := subs(y = _Z,resultant(a,b,x));
	    p2 := subs(x = _Z,resultant(a,b,y));
	    # make them square free
	    g:=gcd(p1,diff(p1,_Z));
	    if g<>1 then p1:=quo(p1,g,_Z) fi;
	    g:=gcd(p2,diff(p2,_Z));
	    if g<>1 then p2:=quo(p2,g,_Z) fi;
	    # remove _Z if it's a factor to reduce the cost of RootOf
	    if subs(_Z=0,p1)=0 then p1:=quo(p1,_Z,_Z) fi;
	    if subs(_Z=0,p2)=0 then p2:=quo(p2,_Z,_Z) fi;
	    p1:=RootOf(p1);
	    p2:=RootOf(p2);
	    `evalc/remember`(p1):=`evalc/split`(p1,0);
	    `evalc/remember`(p2):=`evalc/split`(p2,0);
	    `evalc/remember`('RootOf'(p)):=`evalc/split`(p2,p1)
	fi
    elif nargs=2 then
	c:=traperror(`evalc/evalc`(args[2]));
	if c<>lasterror and op(2,c)=0 then # real case, nothing to do
	    `evalc/split`('RootOf'(p,args[2]),0)
	elif type(p,'polynom(algnum,_Z)') then
	    rof:=`evalc/evalc`('RootOf'(p)); # this uses evalc/remember
	    if c=lasterror then rof
	    else `evalc/split`('RootOf'(op(op(1,rof)),op(1,c)),
			       'RootOf'(op(op(2,rof)),op(2,c)))
	    fi
	else `evalc/unsplit`('RootOf'(args));
	fi
    else `evalc/unsplit`('RootOf'(args));
    fi
end:

#savelib( `evalc/RootOf`);
