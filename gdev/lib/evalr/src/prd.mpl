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
# $Source: /u/maple/research/lib/evalr/src/RCS/prd,v $
##
##    Title:    `evalr/prd`
##    Created:   Aug 88
##    Author:    Bruno Salvy
##      <bsalvy@watmum>
##

`evalr/prd`:=proc(rg1,rg2,exact)
local ispos,var,t1;
option 
    `Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
if type(rg1,range) and type(rg2,range) then
   if op(1,rg1)=op(2,rg1) and type(op(1,rg1),constant) then
      t1 := evalr(Signum(op(1,rg1)));
      if t1=1 or t1=0 then
         t1 := op(1,rg1)*op(1,rg2)..op(1,rg1)*op(2,rg2)
      elif t1=-1 then
         t1 := op(1,rg1)*op(2,rg2)..op(1,rg1)*op(1,rg2)
      elif t1=FAIL then RETURN(-infinity..infinity)
      fi;
      if has(t1,infinity) then RETURN(`simplify/infinity`(t1))
      elif hastype(op(1,t1),float) and not type(op(1,t1),float) or
         hastype(op(2,t1),float) and not type(op(2,t1),float) then
         RETURN(evalf(t1))
      else RETURN(t1)
      fi
   elif op(1,rg2)=op(2,rg2) and type(op(1,rg2),constant) then
      t1 := evalr(Signum(op(1,rg2)));
      if t1=1 or t1=0 then
         t1 := op(1,rg1)*op(1,rg2)..op(2,rg1)*op(1,rg2)
      elif t1=-1 then
         t1 := op(2,rg1)*op(1,rg2)..op(1,rg1)*op(1,rg2)
      elif t1=FAIL then RETURN(-infinity..infinity)
      fi;
      if has(t1,infinity) then RETURN(`simplify/infinity`(t1))
      else RETURN(t1)
      fi
   fi;
   if type(op(1,rg1),constant) and type(op(2,rg1),constant) and
      type(op(1,rg2),constant) and type(op(2,rg2),constant)  then
      #a special case for 0..infinity and -infinity..0 which are strict ranges
      if member(0..infinity,{rg1,rg2}) or
         member(-infinity..0,{rg1,rg2}) then
         t1 := `evalr/prd`(op(subs(0..infinity=1..infinity,
                          -infinity..0=-infinity..-1,[rg1,rg2])),exact);
         if t1=infinity..infinity or t1=-infinity..-infinity then RETURN(t1) fi
      fi;
      t1 := 
      evalr(min(op(1,rg1)*op(1,rg2),op(1,rg1)*op(2,rg2),
          op(2,rg1)*op(1,rg2),op(2,rg1)*op(2,rg2)))..
      evalr(max(op(1,rg1)*op(1,rg2),op(1,rg1)*op(2,rg2),
          op(2,rg1)*op(1,rg2),op(2,rg1)*op(2,rg2)));
      if has(t1,infinity) then `simplify/infinity`(t1) else t1 fi
     else
      var:=map(proc(x) if op(2,x)=-infinity..infinity then x fi end,
       [op({op(`evalr/var`(op(1,rg1)))}union{op(`evalr/var`(op(2,rg1)))})]);
      if nops(var)>0 then
         var:=op(1,var);
         RETURN(`evalr/prd`(op(subs(var='INTERVAL'(op(1,var),-infinity..0),
			    [rg1,rg2])),exact),
                `evalr/prd`(op(subs(var='INTERVAL'(op(1,var),0..infinity),
			    [rg1,rg2])),exact))
      fi;
      var:=map(proc(x) if op(2,x)=-infinity..infinity then x fi end,
       [op({op(`evalr/var`(op(1,rg2)))}union{op(`evalr/var`(op(2,rg2)))})]);
      if nops(var)>0 then
         var:=op(1,var);
         RETURN(`evalr/prd`(op(subs(var='INTERVAL'(op(1,var),-infinity..0),
				    [rg1,rg2])),exact),
                `evalr/prd`(op(subs(var='INTERVAL'(op(1,var),0..infinity),
				    [rg1,rg2])),exact))
      fi;
      ispos:=`evalr/ispos`(op(1,rg1)*op(2,rg2)-op(2,rg1)*op(1,rg2),false,
         false,exact);
      if ispos=true or ispos=0 then
         RETURN(`evalr/prd/case`(rg1,rg2,exact))
      elif ispos=false then
         RETURN(`evalr/prd/case`(rg2,rg1,exact))
      elif ispos=FAIL then RETURN(-infinity..infinity)
      else RETURN(map(proc(x,y) `evalr/prd/case`(op(x),y) end,
         `evalr/chvar`([rg1,rg2],op(1,ispos)),exact),
                  map(proc(x,y) `evalr/prd/case`(op(x),y) end,
         `evalr/chvar`([rg2,rg1],op(2,ispos)),exact))
      fi
   fi
else
   t1 := map(proc(x,y,z) op(map(`evalr/prd`,y,x,z)) end,rg1,rg2,exact);
   if nops(t1)=1 then t1 else `evalr/union`(t1) fi
fi
end:

`evalr/prd/case`:=proc(rg1,rg2,exact)
local ispos;
   option 
       `Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
   ispos:=`evalr/ispos`(op(1,rg1)*op(1,rg2)-op(2,rg1)*op(2,rg2),
							false,false,exact);
   if ispos=true or ispos=0 then
      RETURN(`evalr/prd/case1`(rg1,rg2,exact))
   elif ispos=false then
      RETURN(`evalr/prd/case2`(rg1,rg2,exact))
   elif ispos=FAIL then RETURN(-infinity..infinity)
   else RETURN(map(proc(x,y) `evalr/prd/case1`(op(x),y) end,
         `evalr/chvar`([rg1,rg2],op(1,ispos)),exact),
               map(proc(x,y) `evalr/prd/case2`(op(x),y) end,
         `evalr/chvar`([rg1,rg2],op(2,ispos)),exact))
   fi
end:

`evalr/prd/case1`:=proc(rg1,rg2,exact)
local ispos;
   option 
       `Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
   ispos:=`evalr/ispos`(op(1,rg1)*op(1,rg2)-op(1,rg1)*op(2,rg2),
							false,false,exact);
   if ispos=true or ispos=0 then
      RETURN(`evalr/prd/case11`(rg1,rg2,exact))
   elif ispos=false then
      RETURN(`evalr/prd/case12`(rg1,rg2,exact))
   elif ispos=FAIL then RETURN(-infinity..infinity)
   else RETURN(map(proc(x,y) `evalr/prd/case11`(op(x),y) end,
      `evalr/chvar`([rg1,rg2],op(1,ispos)),exact),
               map(proc(x,y) `evalr/prd/case12`(op(x),y) end,
      `evalr/chvar`([rg1,rg2],op(2,ispos)),exact))
   fi
end:

`evalr/prd/case2`:=proc(rg1,rg2,exact)
local ispos;
   option 
       `Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
   ispos:=`evalr/ispos`(op(1,rg1)*op(2,rg2)-op(2,rg1)*op(2,rg2),
							false,false,exact);
   if ispos=true or ispos=0 then
      RETURN(`evalr/prd/case21`(rg1,rg2,exact))
   elif ispos=false then
      RETURN(`evalr/prd/case22`(rg1,rg2,exact))
   elif ispos=FAIL then RETURN(-infinity..infinity)
   else RETURN(map(proc(x,y) `evalr/prd/case21`(op(x),y) end,
            `evalr/chvar`([rg1,rg2],op(1,ispos)),exact),
               map(proc(x,y) `evalr/prd/case22`(op(x),y) end,
            `evalr/chvar`([rg1,rg2],op(2,ispos)),exact))
   fi
end:

`evalr/prd/case11`:=proc(rg1,rg2,exact)
local ispos;
   option 
       `Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
   ispos:=`evalr/ispos`(op(1,rg2)*op(2,rg1)-op(2,rg1)*op(2,rg2),
							false,false,exact);
   if ispos=true or ispos=0 then
      RETURN(op(2,rg1)*op(2,rg2)..op(1,rg1)*op(1,rg2))
   elif ispos=false then
      RETURN(op(1,rg2)*op(2,rg1)..op(1,rg1)*op(1,rg2))
   elif ispos=FAIL then RETURN(FAIL)
   else RETURN(op(`evalr/chvar`(op(2,rg1)*op(2,rg2)..op(1,rg1)*op(1,rg2),
                  op(1,ispos))),
               op(`evalr/chvar`(op(1,rg2)*op(2,rg1)..op(1,rg1)*op(1,rg2),
                  op(2,ispos))))
   fi
end:

`evalr/prd/case12`:=proc(rg1,rg2,exact)
local ispos;
   option 
       `Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
   ispos:=`evalr/ispos`(op(1,rg2)*op(2,rg1)-op(2,rg1)*op(2,rg2),
							false,false,exact);
   if ispos=true or ispos=0 then
      RETURN(op(2,rg1)*op(2,rg2)..op(1,rg1)*op(2,rg2))
   elif ispos=false then
      RETURN(op(1,rg2)*op(2,rg1)..op(1,rg1)*op(2,rg2))
   elif ispos=FAIL then RETURN(-infinity..infinity)
   else RETURN(op(`evalr/chvar`(op(2,rg1)*op(2,rg2)..op(1,rg1)*op(2,rg2),
                  op(1,ispos))),
               op(`evalr/chvar`(op(1,rg2)*op(2,rg1)..op(1,rg1)*op(2,rg2),
                  op(2,ispos))))
   fi
end:

`evalr/prd/case21`:=proc(rg1,rg2,exact)
local ispos;
   option 
       `Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
   ispos:=`evalr/ispos`(op(1,rg2)*op(2,rg1)-op(1,rg1)*op(1,rg2),
							false,false,exact);
   if ispos=true or ispos=0 then
      RETURN(op(1,rg1)*op(1,rg2)..op(1,rg1)*op(2,rg2))
   elif ispos=false then
      RETURN(op(1,rg2)*op(2,rg1)..op(1,rg1)*op(2,rg2))
   elif ispos=FAIL then RETURN(-infinity..infinity)
   else RETURN(op(`evalr/chvar`(op(1,rg1)*op(1,rg2)..op(1,rg1)*op(2,rg2),
                  op(1,ispos))),
               op(`evalr/chvar`(op(1,rg1)*op(2,rg1)..op(1,rg1)*op(2,rg2),
                  op(2,ispos))))
   fi
end:

`evalr/prd/case22`:=proc(rg1,rg2,exact)
local ispos;
   option 
       `Copyright (c) 1990 by the University of Waterloo. All rights reserved.`;
   ispos:=`evalr/ispos`(op(1,rg2)*op(2,rg1)-op(1,rg1)*op(1,rg2),
							false,false,exact);
   if ispos=true or ispos=0 then
      RETURN(op(1,rg1)*op(1,rg2)..op(2,rg1)*op(2,rg2))
   elif ispos=false then
      RETURN(op(1,rg2)*op(2,rg1)..op(2,rg1)*op(2,rg2))
   elif ispos=FAIL then RETURN(-infinity..infinity)
   else RETURN(op(`evalr/chvar`(op(1,rg1)*op(1,rg2)..op(2,rg1)*op(2,rg2),
                  op(1,ispos))),
               op(`evalr/chvar`(op(1,rg1)*op(2,rg1)..op(2,rg1)*op(2,rg2),
                  op(2,ispos))))
   fi
end:

#savelib( `evalr/prd`,`evalr/prd/case`,	`evalr/prd/case1`,`evalr/prd/case2`,`evalr/prd/case11`,	`evalr/prd/case12`,`evalr/prd/case21`,	`evalr/prd/case22`);
