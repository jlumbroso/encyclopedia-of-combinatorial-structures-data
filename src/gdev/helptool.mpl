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

helptool := proc(the_file, the_topic, the_localization, the_hdb)
  local N, the_line, line, common ;
  N := 0 ;
  the_line := readline(the_file) ;
  while the_line<>0 do
    N := N+1 ;
    line[N] := the_line ;
    the_line := readline(the_file)
  end do ;
  common := insert, topic=the_topic, library=the_hdb ;
  print(INTERFACE_HELP(common,
    op(select(has, the_localization, [parent, aliases])),
    text=TEXT(seq(line[i], i=1..N)))) ;
  print(INTERFACE_HELP(common, op(the_localization))) ;
  NULL
end :
