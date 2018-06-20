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
# $Source: /u/maple/research/lib/evalr/src/RCS/intersect,v $
##
##    Title:    `evalr/intersect`
##    Created:   Aug 88
##    Author:    Bruno Salvy
##      <bsalvy@watmum>
##

`evalr/intersect`:=proc(liste)
local i,tmp;
option 
    `Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
if nops(liste)>=2 then
   tmp:=op(1,liste);
   for i from 2 to nops(liste) do
      if member(evalr(Signum(op([i,1],liste)-op(2,tmp))),{0,1}) or
         member(evalr(Signum(op([i,2],liste)-op(1,tmp))),{-1,0}) then
         RETURN(NULL)
      else tmp:=evalr(max(op([i,1],liste),op(1,tmp)))..
           evalr(min(op([i,2],liste),op(2,tmp)))
      fi
   od
else
   liste
fi
end:

#savelib('`evalr/intersect`'):
