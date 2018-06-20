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
##    Title:	gdev, glimit
##    Created:	Jun 87
##    Author:	Bruno Salvy
##	<salvy@poly.polytechnique.fr>
##
## Description: 
##	      gdev(expr,pt,<dir>,<order>);
##	      glimit(expr,pt,<dir>);
##
##   gdev yields an expansion of expr at pt in the direction
## dir at the order order. glimit returns only the limit.
## - pt is either a number, or +/-infinity, or an equation
##    x = pt where x is the variable and pt follows the 
##    previous rules
## - dir is straight or inverse. Straight means "from 0" but in 0
##   where it means "from the right". Inverse means left for 0
##   and "from infinity" for other complex numbers.
##   Default dir is straight.
## - order is the number of non-zero terms in the expansion.
##   Default is one.
##
## Changed BS Jul 98 to take the variable as an argument.
#######################################################

gdev:=proc(fct::algebraic,gvar::{name,name=algebraic})
local pt,ord,var,rightdir, res;
option `Copyright Bruno Salvy, INRIA, France`;
global _EnvXasyinteger;
    if type(args[2],name=algebraic) then
	var:=op(1,args[2]);
	pt:=op(2,args[2])
    else
	var:=args[2];
	pt:=0
    fi;
    if has(pt,infinity) and pt<>infinity and pt<>-infinity then
	ERROR(`Invalid limit point`,args[2])
    fi;
    rightdir:=true;
    if nargs>=3 then
	if type(args[3],nonnegint) then
	    ord:=args[3]
	elif args[3]='inverse' then
	    if pt<>infinity and pt<>-infinity then rightdir:=false fi
	elif args[3]<>'straight' then ERROR(`Invalid argument`,args[3])
	fi
    fi;
    if nargs=4 then
	if type(args[4],nonnegint) and not assigned(ord) then
	    ord:=args[4]
	elif args[4]='inverse' then
	    if pt<>infinity and pt<>infinity then
		rightdir:=false
	    fi
	elif args[4]<>'straight' then ERROR(`Invalid argument`,args[4])
	fi
    fi;
    if not assigned(ord) then ord:=1 fi;
    if is(_Xasy>100)=FAIL then additionally(_Xasy>100) fi;
    _EnvXasyinteger:=false;
    if pt=infinity then
	_EnvXasyinteger:=is(var,integer);
	res:=traperror(`dev/print`(`dev/dev`(subs(var=_Xasy,fct),
	    ord,ord),var,ord))
    elif pt=-infinity then
	_EnvXasyinteger:=is(var,integer);
	res:=traperror(subs(var=-var,`dev/print`(`dev/dev`(
	    subs(var=-_Xasy,fct),ord,ord),var,ord)))
    elif zerotest(pt)<>true then
	if rightdir then
	    res:=traperror(subs(var=1/(1-var/pt),`dev/print`(`dev/dev`(
		subs(var=pt*(1-1/_Xasy),fct),ord,ord),var,ord)))
	else
	    res:=traperror(subs(var=1/(var/pt-1),`dev/print`(`dev/dev`(
		subs(var=pt*(1+1/_Xasy),fct),ord,ord),var,ord)))
	fi
    else
	if rightdir then
	    res:=traperror(subs(var=1/var,`dev/print`(`dev/dev`(
		subs(var=1/_Xasy,fct),ord,ord),var,ord)))
	else
	    res:=traperror(subs(var=-1/var,`dev/print`(`dev/dev`(
		subs(var=-1/_Xasy,fct),ord,ord),var,ord)))
	fi
    fi;
    if res=lasterror or res=FAIL then gdev(args):='gdev'(args)
    elif type(res,undefined) then undefined
    else res
    fi
end:

#savelib(gdev);
