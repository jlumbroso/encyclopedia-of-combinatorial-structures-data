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

interface(echo=0):
macro(W=LambertW):
test:=proc(f,x,k,p,eps)
local eq, n, v1, v2;
   eq:=equivalent(f,x,n,p);
   v1:=evalf(subs(n=k,eval(eq,O=0)));
   v2:=evalf(coeff(eval(convert(series(f,x,k+5),polynom),O=0),x,k));
   # lprint(abs((v1-v2)/v1))
   if abs((v1-v2)/v1)<eps then okay else f,v1,v2 fi
end:
