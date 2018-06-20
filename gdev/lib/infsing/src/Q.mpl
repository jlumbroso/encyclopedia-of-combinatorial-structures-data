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
`infsing/QuasiInverse`:=proc (fct,var,singmin) # we assume the coeff to be >=0
local val;
    val:=`infsing/trie`(`infsing/infsolve`(op(1,fct)-1,var,true,singmin),[infinity],fct,var,
	'polar','polynom');
    if val[1][1]=infinity then infsing(op(1,fct),var,singmin)
    else [val,true]
    fi
end: # `infsing/QuasiInverse`

#savelib( `infsing/QuasiInverse`);
