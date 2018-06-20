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
unprotect('shake');
shake:=proc (expr)
local ampl, res;
	option 
	    `Copyright (c) 1991 by the University of Waterloo. All rights reserved.`;
	if nargs=2 and type(args[2],integer) and args[2]>0 then ampl:=args[2]
	else ampl:=Digits fi;
	if indets(expr,name) minus ({constants} minus {I}) = {} then
		`evalr/shake`(expr,ampl)
	elif type(expr,name) then 'INTERVAL'(-infinity..infinity)
	else res:=`evalr/evalr`(map(shake,expr,ampl),false);
	    subsop(4=NULL,op(`evalr/evalr`)); res
	fi
end: # shake
protect(shake);
#savelib('shake'):
