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
##################################################################
#
#				ALGLOGEQ
#
##################################################################
`equivalent/alglogeq`:=proc(dev,p)
local i,result,listjungen,listtransfert, totr, j, l2, s, k, l3;
    if not type(dev,list) then RETURN(0) fi;
    if max(seq(op(1,i),i=indets(dev,list)))>2 then
	ERROR(`Maybe in our next version...`)
    fi;
    if op(1,dev)<1 then RETURN(`equivalent/H_HS_sing`(dev)) fi;
    listjungen:=[1];
    listtransfert:=[1];
    if op(1,dev)=1 then
	for i from 2 by 2 to nops(dev) do
	    if not type(op(i,dev),list) then
		if i<>nops(dev)-1 then
		    listjungen:=[op(listjungen),op(i,dev),op(i+1,dev)]
		else
		    result:=[0,op(i+1,dev)]
		fi
	    else
		if i<>nops(dev)-1 then
		    s:=op(i,dev)
		else
		    if nops(op(i,dev))>3 then
			s:=[op(1..nops(op(i,dev))-2,op(i,dev))]
		    else
			s:=[]
		    fi;
		    result:=[-op(nops(op(i,dev)),op(i,dev)),op(i+1,dev)]
		fi;
		if type(op(i+1,dev),'nonnegint') and 
		    member(0,[seq(op(2*j+1,s),j=1..iquo(nops(s),2))],'j') then
		    s:=subsop(2*j=NULL,2*j+1=NULL,s) fi;
		if s<>[] then
		    totr:=map(type,[seq(op(2*j+1,s),j=1..iquo(nops(s),2))],
			'nonposint');
		    if not member(false,totr) then
			listjungen:=[op(listjungen),s,op(i+1,dev)]
		    else
			l2:=[2];l3:=[2];k:=0;
			while member(false,totr,'j') do
			    if j>1 then l2:=[op(l2),op(2*k+2..2*k+2*j-1,s)] fi;
			    l3:=[op(l3),op(2*k+2*j..2*k+2*j+1,s)];
			    k:=k+j;
			    totr:=[op(j+1..nops(totr),totr)]
			od;
			l2:=[op(l2),op(2*k+2..2*k+2*nops(totr)+1,s)];
			if l2<>[2] then
			    listjungen:=[op(listjungen),l2,op(i+1,dev)]
			fi;
			if l3<>[2] then
			    listtransfert:=[op(listtransfert),l3,op(i+1,dev)]
			fi
		    fi
		fi
	    fi
	od
    else
	if nops(dev)>3 then s:=[op(1..nops(dev)-2,dev)] else s:=[] fi;
	if op(nops(dev),dev)<>infinity then
	    result:=[0,op(nops(dev),dev)]
	else
	    result:=[0,infinity]
	fi;
	if member(0,[seq(op(2*j+1,s),j=1..iquo(nops(s),2))],'j') then
	    s:=subsop(2*j=NULL,2*j+1=NULL,s) fi;
	if s<>[] then
	    totr:=map(type,[seq(op(2*j+1,s),j=1..iquo(nops(s),2))],'nonposint');
	    if not member(false,totr) then
		listjungen:=[1,s,0]
	    else
		l2:=[2];l3:=[2];k:=0;
		while member(false,totr,'j') do
		    l2:=[op(l2),op(2*k+2..2*k+2*j-1,s)];
		    l3:=[op(l3),op(2*k+2*j..2*k+2*j+1,s)];
		    k:=k+j;
		    totr:=[op(j+1..nops(totr),totr)]
		od;
		l2:=[op(l2),op(2*k+2..2*k+2*nops(totr)+1,s)];
		if l2<>[2] then listjungen:=[1,l2,0] fi;
		if l3<>[2] then listtransfert:=[1,l3,0] fi
	    fi
	fi;
    fi;
    result:=`equivalent/Otransf`(result);
    if listjungen<>[1] then
	result:=`dev/add`(result,`equivalent/alglogeq/jungen`(listjungen,p))
    fi;
    if listtransfert<>[1] then
	result:=`dev/add`(result,`equivalent/alglogeq/transfert`(listtransfert,p))
    fi;
    result
end:

`equivalent/Otransf`:=proc(expr)
    if expr<>[0,infinity] then
	if type(op(1,expr),{negint,0}) then
	    if op(1,expr)<>0 then
		# If you read this, this line may help you :
		[1,[2,1,-op(1,expr)+1],op(2,expr)+1]
	    else [1,1,op(2,expr)+1]
	    fi
	else [1,[2,1,-op(1,expr)],op(2,expr)+1]
	fi
    else 0
    fi
end:

`equivalent/H_HS_sing`:=proc(dev)
local res;
    res:=`dev/ln`(dev,1);
    if op(1,res)=1 and op(3,res)<0 and evalr(Signum(op(2,res)))=1 then 'H'
    elif op(1,res)<=0 then 'HS'
    else 'pointcol'
    fi
end:

#savelib( `equivalent/alglogeq`,`equivalent/Otransf`,`equivalent/H_HS_sing`);
