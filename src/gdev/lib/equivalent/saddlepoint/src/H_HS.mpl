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
##
##    Title:	 H_HS
##    Created:	  June 87
##    Author:	  Bruno Salvy
##  <salvy@tokay.inria.fr>
##
## Description: Determine the admissibility of an entire function.
##    Returns H if the function is no better than Hayman-admissible
##	HS if the function is Harris-Schoenfeld admissible
##	FAIL otherwise
##   This is based on the paper by Hayman and the paper by 
## Odlyzko and Richmond plus some facts that are not yet demonstrated but
## are certainly true.
##
##
##    Modified:	    Fri Apr 14 10:07:51 1989
##    Author:	  Bruno Salvy
##    Modification: commented the file
##

`equivalent/saddlepoint/H_HS`:=proc(fct,var)
local j,polprd,indic,i,pol,d,autre,coeffi,x,aux, aux2;
    x:=var;
    # a polynom has no admissibility
    if type(fct,polynom(anything,x)) then FAIL
    elif type(fct,`*`) then
	# in case of a product, then if one of the terms is admissible
	# in the sense of Hayman and the remainder is a polynom with a 
	# positive leading coefficient then the function is Hayman admissible.
	# It is very probably also true for Harris-Schoenfeld admissibility
	# and it is implemented that way.
	polprd:=1;
	indic:=FAIL;
	for i to nops(fct) do 
	    aux:=op(i,fct);
	    if type(aux,polynom(anything,x)) then polprd:=polprd*aux
	    else
		aux:=`equivalent/saddlepoint/H_HS`(aux,x);
		if aux<>FAIL then
		    if aux<>indic then
			if aux='HS' then
			    indic:='HS'
			elif aux='H' and indic=FAIL then
			    indic:='H'
			fi
		    fi
		else
		    FAIL
		fi
	    fi
	od;
	if indic<>FAIL and evalr(Signum(lcoeff(expand(polprd),x),0))=1 then
	    indic
	else FAIL
	fi
    elif type(fct,`^`) then # powers do not affect us
	aux:=op(2,fct);
	if type(aux,posint) then `equivalent/saddlepoint/H_HS`(op(1,fct),x)
	elif type(op(1,fct),polynom) then FAIL
	else `equivalent/saddlepoint/H_HS`('exp'(aux*ln(op(1,fct))),x)
	fi
    elif type(fct,'exp(anything)') then
	# This is the center of the procedure:
	# under certain circumstances, the exponential of a polynom is
	# Hayman admissible, and Odlyzko-Richmond tells us that the exponential
	# of a Hayman admissible function is Harris-Schoenfeld admissible.
	# Of course the exponential of a Harris-Schoenfeld admissible function
	# is also Harris-Schoenfeld admissible.
	pol:=op(1,fct);
	if member(`equivalent/saddlepoint/H_HS`(pol,x),{'H','HS'}) then 'HS'
	elif type(pol,polynom(anything,x)) then
	    d:=degree(pol,x);
	    # this loop is sufficient to ensure that there is only one 
	    # admissible path in the sense of Wyman.
	    if evalr(Signum(lcoeff(pol,x)))<>1 then FAIL
	    else
		indic:=0;
		for i from 2 to d while indic<>FAIL do
		    j:=d;
		    while j>1 and indic<>FAIL do
			if modp(j,i)<>0 then j:=j-1
			else
			    aux:=evalr(Signum(coeff(pol,x,j)));
			    if aux=0 then j:=j-1
			    elif aux=-1 then indic:=FAIL
			    elif aux=1 then j:=1
			    else ERROR(`Cannot decide admissibility`)
			    fi
			fi
		    od
		od;
		if indic=FAIL then FAIL else 'H' fi
	    fi
	fi
    elif type(fct,`+`) then
	# The sum of two Hayman admissible functions is Hayman admissible,
	#this is probably also true for Harris-Schoenfeld admissible functions.
	# The sum of an admissible function with something else is admissible
	# when the other terms are smaller (not demonstrated for HS I think).
	indic:=FAIL;
	autre:=0;
	coeffi:=[0];
	for i to nops(fct) do
	    aux:=op(i,fct);
	    if not type(aux,polynom(anything,x)) then
		aux2:=`equivalent/saddlepoint/H_HS`(aux,x);
		if aux2='HS' then
		    if indic='HS' then
			# select the largest one for later comparisons
			# with other terms of the sum
			if `dev/compare`(`dev/dev`(subs(x=_Xasy,aux),1,1),
			    coeffi)=1 then coeffi:=aux fi
		    else
			indic:='HS';
			coeffi:=`dev/dev`(subs(x=_Xasy,aux),1,1)
		    fi
		elif aux2='H' then
		    if indic='H' then
		    # select the largest one
			if `dev/compare`(`dev/dev`(subs(x=_Xasy,aux),1,1),
			    coeffi)=1 then coeffi:=aux fi
		    elif indic=FAIL then
			indic:='H';
			coeffi:=`dev/dev`(subs(x=_Xasy,aux),1,1)
		    fi
		else autre:=autre+aux
		fi
	    fi
	od;
	if indic<>FAIL then
	    if `dev/compare`(coeffi,`dev/dev`(subs(x=_Xasy,autre),1,1))=1 then
		indic
	    else FAIL
	    fi
	fi
    else FAIL
    fi
end:

#savelib( `equivalent/saddlepoint/H_HS`);
