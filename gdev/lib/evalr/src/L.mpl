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

# This is only used in the Luo system
`evalr/QuasiLog`:=proc(rg,exact)
    `evalr/ln`(`evalr/pow`(`evalr/add`([1..1],
	`evalr/prd`([-1..-1],rg,exact)),-1,exact),exact)
end: # `evalr/QuasiLog`
#savelib( `evalr/QuasiLog`);
