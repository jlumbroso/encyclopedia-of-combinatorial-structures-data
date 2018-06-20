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
# $Source: /u/maple/research/lib/src/RCS/evalr,v $
##########################################################
#
#                      evalr
#
#       A package to handle ranges in expressions.
#
#    A range has the form INTERVAL(a..b) where it is assumed that a<=b.
# Variables are allowed in the form INTERVAL(x,a..b), by default a name
# x is converted to INTERVAL(x,-infinity..infinity) and converted back
# to x in the output when possible. Integrals are handled and the
# functions abs, min and max can be (sometimes) integrated this way.
# There is a natural loss of information in the process, so
# the result is a range or a list of evalr which contain
# the right result. When the extremities of the result are equal,
# then the result is exact and its value is returned.
#    Currently the evalr in the boundaries of an integral are
# not handled, but the variables are handled inthe case of
# multiple integrals.
#
#                                  B.Salvy (Aug88)
#
#    If what you are really looking for is interval arithmetic, then
# use the function shake.
#
#  All of this is not very optimised and will probably be much faster in
# later versions.
#
# Modified to traverse the dag and free the memory afterwards. BS. Jul. 98
##########################################################

unprotect('evalr');
evalr:=proc(expr)
local var,r,i,j,u;
option 
    `Copyright (c) 1991 by the University of Waterloo. All rights reserved.`;
if not hastype(expr,range) and
  nops(map(proc(x) op(0,x) end,indets(expr,function)) intersect
	{'min','max','abs','Signum','shake','Int','int'})=0 then
	expr
else
	r:=traperror(`evalr/evalr`(expr,true));
	subsop(4=NULL,eval(`evalr/evalr`));
	if r=lasterror then RETURN(FAIL) fi;
	if has(r,infinity) then
		r:=`simplify/infinity`(r)
	fi;
	if type(r,'list') or type(r,specfunc(anything,'INTERVAL')) then
		for j to nops(r) do
			for u to 2 do
				if not type(op([j,u],r),constant) then
				var:=`evalr/var`(op([j,u],r));
				for i to nops(var) do
					if op([i,2],var)=-infinity..infinity
					then
						r:=subsop(j=subsop(u=subs(
						op(i,var)=op([i,1],var),
						op([j,u],r)),op(j,r)),r)
					fi
				od
				fi
			od;
			if op([j,1],r)=op([j,2],r) then
				r:=subsop(j=op([j,1],r),r) fi
		od;
		if not hastype(r,range) then op(r) else r fi
	else r
	fi
fi
end:
protect(evalr);
# two of the most useful ones:
evalr('min(1,infinity)'):=1:
evalr(Signum(0)):=0:

#savelib('evalr'):
