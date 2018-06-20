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
# $Source: /u/maple/research/lib/evalr/src/RCS/add,v $
##
##    Title:    `evalr/add`
##    Created:   Aug 88
##    Author:    Bruno Salvy
##      <bsalvy@watmum>
##

`evalr/add`:=proc(rg1,rg2) local t1;
option 
    `Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
if type(rg1,range) and type(rg2,range) then
   t1 := op(1,rg1)+op(1,rg2)..op(2,rg1)+op(2,rg2);
   if has(t1,infinity) then `simplify/infinity`(t1)
   else t1 fi;
elif rg1=FAIL or rg2=FAIL then FAIL
else
   `evalr/union`(map(proc(x,y) op(map(`evalr/add`,y,x))end,rg1,rg2))
fi
end:

#savelib('`evalr/add`'):
