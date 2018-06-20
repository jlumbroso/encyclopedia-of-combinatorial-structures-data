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
# $Source: /u/maple/research/lib/evalr/src/RCS/univar,v $
##
##    Title:    `evalr/univar`
##    Created:   Aug 88
##    Author:    Bruno Salvy
##      <bsalvy@watmum>
##

`evalr/univar`:=proc(expression,ex1,ex2)
local x,rg,expr,deriv,res,i,err,t1,t2,t3;
   option 
       `Copyright (c) 1991 by the University of Waterloo. All rights reserved.`;
   x:=op(indets(expression,name) minus {constants});
   if hastype(expression,range) then
      rg:=indets(expression,range);
      expr:=subs([seq('INTERVAL'(x,i)=x,i=rg)],expression);
      if nops(rg)=1 then rg:=op(rg)
      else rg:=min(seq(op(1,i),i=rg))..max(seq(op(2,i),i=rg)) fi
   else
      rg:=-infinity..infinity;
      expr:=expression
   fi;
   deriv:=diff(expr,x);
   res:=[solve(deriv,x)];
   for i while i<=nops(res) do
      if evalr(Signum(op(1,rg)-op(i,res)))=1 or
         evalr(Signum(op(i,res)-op(2,rg)))=1 then
         res:=subsop(i=NULL,res);
         i:=i-1
      fi
   od;
   res:={op(res)};
   if hastype(deriv,float) then
      err := {traperror(fsolve(deriv,x,rg))};
      if err={lasterror} or has(err,'fsolve') or nops(err)<=nops(res) then
         res:={op(res)}
      else
         res:={op(err)}
      fi
   else res:={op(res)}
   fi;
   t1 := map(proc(x,y,z) subs(z=x,y) end,res,expr,x);
   if ex1 then t2 := {0} else t2 := {limit(expr,x=op(1,rg),right)} fi;
   if ex2 then t3 := {0} else t3 := {limit(expr,x=op(2,rg),left)} fi;
   t1 := op(t3 union t2 union t1);
   'INTERVAL'(evalr(min(t1))..evalr(max(t1)))
end:

#savelib('`evalr/univar`'):
