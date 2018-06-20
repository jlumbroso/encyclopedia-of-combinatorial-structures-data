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

_EnvSolveSaddlePoint:=false: # due to a weakness in solve
read `enc_functions.mpl`;
macro(maintable=`encyclopedia/maintable`);
maintable:=table():
read `database.mpl`;
# aussi fait dans saveall, mais c'est mieux si jamais createindices bloque
save maintable,`maintable.m`;
indGF:=table():
indCF:=table():
indExpr:=table():
indCount:=table():
createindices(maintable,'indGF','indCF','indExpr','indCount',
    1,maintable['Count']);
# Creation de fichiers intermediaires
save `encyclopedia/maintable`,`maintable.mpl`;
save `encyclopedia/maintable`,`maintable.m`;
ind:=[indGF,indCF,indExpr,indCount]:
save indGF,indCF,indExpr,indCount,ind,`ind.m`;
quit
