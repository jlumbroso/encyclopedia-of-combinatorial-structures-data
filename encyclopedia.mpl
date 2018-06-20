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

               ####################################################
               #           Encyclopedia Functions                 #
               #                                                  #
               #   Contains search functions:                     #
               #   - gfsearch (by a generating function)          # 
               #   - keywordssearch (by one or several words)     #
               #   - cfsearch (by a closed form)                  #
               #   - seqsearch (by a counting sequence)           #
               #   - nbsearch (by ECS number)                     #
               #                                                  #
               ####################################################


macro(GFSearch=encyclopedia[gfsearch],
      CFSearch=encyclopedia[cfsearch],
      CountSearch=encyclopedia[seqsearch],
      ExprSearch=encyclopedia[keywordssearch],
      ECSnbSearch=encyclopedia[nbsearch],
      SequenceSize=20,
      SEQ=3,
      subupperlower=`encyclopedia/subupperlower`,
      WordsList=`encyclopedia/wordlist`,
      ind=`encyclopedia/ind`,
    indGF=`encyclopedia/indGF`,
    indCF=`encyclopedia/indCF`,
  indExpr=`encyclopedia/indExpr`,
 indCount=`encyclopedia/indCount`,
maintable=`encyclopedia/maintable`,
      Tab=encyclopedia[computeentry],
      GF=4,
      CLOSEDFORM=6);



################################################
# Search function by a generating function     #
################################################

GFSearch:=proc(GF)
option `Copyright Stephanie Petit, Inria Rocquencourt, France, 1998`;
local i, GF1,var, T;

# To check that the function given by the user is univariate
var:=indets(GF,name) minus {constants};
T:=ind[1];
if nops(var)=1 
 then 
   # The variable of the given function is replaced with _x 
   GF1:=normal(subs(op(var)=_x,GF));
   if type(T[GF1],list) 
     then T[GF1] 
     else FAIL 
   fi;
else  ERROR("not a closed form with a single variable", GF)
fi ;  
end;  

################################################    
# Search function by one or several words      #
################################################

ExprSearch:=proc(Expr)
option `Copyright Stephanie Petit, Inria Rocquencourt, France, 1998`;
  local r, word, WL, List, T;
  WL:=WordsList(Expr);
  T:=ind[3];
  if assigned(T[op(1,WL)])
   then List:={op(T[op(1,WL)])};
       for word in WL minus {op(1,WL)} 
       do
         List:=List intersect {op(T[word])}; 
       od;      
       List
   else {}
  fi;    
end;  
 

#######################################
# Search function by a closed form    #
#######################################

CFSearch:=proc(ClosedForm)
option `Copyright Stephanie Petit, Inria Rocquencourt, France, 1998`;
local i, ClosedForm1,var, T;

var:=indets(ClosedForm,name) minus {constants};
T:=ind[2];
if nops(var)=1
 then 
   var:=op(var);
   ClosedForm1:=T[simplify(subs(var=_n,ClosedForm),'power', 'radical','GAMMA')];
   if type(ClosedForm1,list) 
    then ClosedForm1 
     else FAIL 
   fi;
 else ERROR("not a closed form with a single variable", GF)
fi ;  

end;  
  

#############################################   
# Search function by a Counting Sequence    #
#############################################

`encyclopedia/Contains`:=proc(Lt,Lm)
local i,lg;
    lg:=nops(Lm);
    for i to nops(Lt)-lg+1 do
        if Lm=Lt[i..i+lg-1] then RETURN(true) fi;
    od;
    false
end:

CountSearch:=proc(List)
option `Copyright Stephanie Petit, Inria Rocquencourt, France, 1998`;
local i,nl,List1,rList,result,T, r, out;

if nops(List)>SequenceSize
 then List1:=List[1..SequenceSize];
    rList:=[seq(List[SequenceSize-i],i=0..SequenceSize-1)]
 else List1:=List;
    nl:=nops(List);
    rList:=[seq(List[nl-i],i=0..nl-1)]
fi;
T:=ind[4];
result:={op(T[rList[1]])};
for i from 2 to nops(rList) do
  result:=result intersect {op(T[rList[i]])};
od;

for r in result do
    if `encyclopedia/Contains`(op(SEQ,r),List1) then out[r]:=r
    else out[r]:=NULL fi
od;
result:={seq(out[r],r=result)};

end;  



################################################
# Search function by ECS number     #
################################################
#MS
ECSnbSearch:=proc(nb)
 if not assigned(`encyclopedia/maintable`[nb])
    then ERROR (nb, "Not a valid ECS number")
    else `encyclopedia/maintable`[nb]
 fi;
end:
#MSfin
