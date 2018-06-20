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
               # Contains :                                       #
               # - inserttable: to add a data to the table        #
               # - computeentry: to compute generating function,  #
               #   recurrence, ... Finction called by insertable. #
               # - createindices: To create an indices table.     #
               #                                                  #
               ####################################################
##
## Created by Stephanie Petit, June 1998.
##
# Fixed a workaround one of rsolve's weakness.         BS 07/00

# FC, Sep 2009: The calculation of ClosedForm by rsolve in Maple12
# finds 792 closed forms out of 1075 structures, in a matter of
# minutes.  Using Maple13 runs forever on a few structures, so I added
# a time limit of 10 seconds, which then makes Maple13 able to find
# 793 closed forms.  This is better than replacing rsolve by
# LREtools[dAlembertiansols], which, even with a time limit of 10
# seconds, would find only 648 closed forms.

macro(count=combstruct[count],
      gfsolve=combstruct[gfsolve],
      holexprtodiffeq=gfun[holexprtodiffeq],
      diffeqtorec=gfun[diffeqtorec],
      invborel=gfun[invborel],
      ratpolytocoeff=gfun[ratpolytocoeff],
      SequenceSize=20,
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

## Using macro(Tab=encyclopedia[computeentry]) makes it difficult to profile
# encyclopedia[computeentry]:=Tab:

###################################
# To insert a new data in tabname #
###################################

# The use of loctab makes it easier to profile
inserttable:=proc (tabname)
local loctab;
option `Copyright Stephanie Petit, Inria Rocquencourt, France, 1998`;
    loctab:=tabname;
    if not assigned(loctab['Count']) then loctab['Count']:=1
    else loctab['Count']:=loctab['Count']+1 fi;
    print(args[2..nargs]);
    loctab[loctab['Count']]:=Tab(args[2..nargs],loctab['Count'])
end:


######################
# A display function #
######################

`print/Encline`:=proc(name, specification1, CountingSeq, GF, Recurrence, ClosedForm,
   Asymptotic, Description, references, optind)
option `Copyright Stephanie Petit, Inria Rocquencourt, France, 1998`;
nprintf(cat("%-23s %s\n %-22s %A\n %-22s %A\n %-22s %A\n %-22s %A\n %-22s %A\n %-22s %A\n %-22s %s\n %-22s %s\n",`if`(nargs=10,"%-23s %d\n ",""),"\n"),
   "Name: ", name,
   "Specification: ", specification1, "First terms in the sequence: ",
   CountingSeq, "Generating function: ", GF, "Recurrence: ",
   Recurrence, "Closed form: ", ClosedForm,
   "Asymptotics of the coefficients: ",
   Asymptotic, "Description: ", Description, "References: ",
   references,
   `if`(nargs=10,op([" ECS number: ",optind]),NULL))
end:


#################################################
# To replace capital letters with small letters #
#################################################

subupperlower:=proc(String)
option `Copyright Stephanie Petit, Inria Rocquencourt, France, 1998`;
local letter, rangeletter, String1, var;
global substitution;
    if not assigned(substitution["A"]) then
        for letter from "A" to "Z" do
            substitution[letter]:=substring("abcdefghijklmnopqrstuvwxyz",
                searchtext(letter,"ABCDEFGHIJKLMNOPQRSTUVWXYZ"));
        od
    fi;
    for rangeletter to length(String) do
        var:=substring(String,rangeletter);
        if assigned(substitution[var]) then
            String1[rangeletter]:=substitution[var]
        else String1[rangeletter]:=var
        fi
    od;
    cat(seq(String1[rangeletter],rangeletter=1..length(String)))
end:


#########################################
# To convert a string into a words list #
#########################################

WordsList:=proc(String)
option `Copyright Stephanie Petit, Inria Rocquencourt, France, 1998`;
local r, const;
    if String=" " or String="" then NULL
    else
        r:=min(op({SearchText(" ",String),
                   SearchText(",",String),
                   SearchText(";",String),
                   SearchText("'",String),
                   SearchText("(",String),
                   SearchText(")",String),
                   SearchText("s ",String),
                   SearchText("s,",String)
          } minus {0}));
        if r<>infinity then
            {subupperlower(substring(String,1..r-1))} union
                WordsList(substring(String,r+1..-1))
        elif substring(String,-1)="s" then
            {subupperlower(substring(String,1..-2))}
        else {subupperlower(String)}
        fi
    fi
end:


#######################################################################
# Program which gives generating function, recurrence, closed form... #
# from a combstruct grammar specification                             #
#######################################################################
# Modified BS. Sep 01. Replaced isholonomic with try/catch holexprtodiffeq
Tab:=proc(name_::string,
          specification::[name,set,{identical(labeled),identical(unlabeled),
                                    identical(labelled),identical(unlabelled)}],
          Description::string,
          references::string,
          optind::integer)
option `Copyright Stephanie Petit, Inria Rocquencourt, France, 1998`;
local CountingSeq,GF,i,specification1,S,Recurrence,DEq,ClosedForm,Format,Shift1, Asymptotic,n, shiftedrec
,ti ;
global TI ;

    specification1:=subs(specification[1]=S,specification);

    CountingSeq:=[seq(count(specification,size=i),i=0..SequenceSize)];
    Recurrence:=FAIL;
    ClosedForm:=FAIL;
    Asymptotic:=FAIL;
    GF:=gfsolve(specification1[2],specification1[3],_x);
    forget(`combstruct/gfeqns`);
    if GF<>FAIL then
        GF:=subs(GF,S(_x));
        # Recurrence and closed form of the nth coefficient
        # are only searched for holonomic functions.
        try
            DEq:=holexprtodiffeq(GF,y(_x));
            Recurrence:=diffeqtorec(DEq,y(_x),_f(_n));
            if specification[3]=labelled then
                Recurrence:=invborel(Recurrence,_f(_n))
            fi;
            # If GF is a rational function, ratpolytocoeff gives
            # an explicit value of the nth Taylor coefficient.
            if type(GF,ratpoly) then
                ClosedForm:=simplify(ratpolytocoeff(GF,_x,_n),
                    'power','radical');
                if member(specification[3],{'labelled','labeled'}) then
                    ClosedForm:=ClosedForm*_n! fi
            else
                # When the number of initial conditions is higher than
                # the recurrence ## degree.
                # degree -> span (BS 07/00)
                Format:=gfun[formatrec]([Recurrence,_f(_n)],_f,_n,'init'):
                Shift1:=nops(init)-nops(Format)+2:
                for i from 2 while Format[i]=0 do Shift1:=Shift1+1 od;
                if Shift1>0 then
                    ##### COMMENTED OUT 07/00 BS
                    # Recurrence:=subs(_n=_n-Shift1,Recurrence);
                    # Recurrence:=remove(type,Recurrence,_f(integer)=anything)
                    # union remove(type,{seq(_f(op([1,1],i)-1)=op(2,i),i=init)}
                    # ,_f(negint)=anything);
                    # Format:=`gfun/formatrec`([Recurrence,_f(_n)],
                    #_f,_n,'init');
                    # Recurrence:=remove(type,Recurrence,_f(integer)=anything)
                    # union remove(type,{seq(_f(op([1,1],i)+1)=op(2,i),i=init)}
                    # , _f(negint)=anything);
                    # Recurrence:=subs(_n=_n+Shift1,Recurrence);
                    ##### REPLACED BY:
                    shiftedrec:=gfun:-makerec(subs(_n=_n+Shift1,Format),_f,_n,
                        remove(type,{seq(_f(op([1,1],i)-Shift1)=op(2,i),i=init)
                            }, _f(negint)=anything));
                else shiftedrec:=Recurrence
                fi;

                # Works well in Maple12, but runs forever on some
                # instances in Maple13.  See the comment at the start
                # of the file.
                ClosedForm := timelimit(10, rsolve(shiftedrec,_f(_n))) ;

                # test to see if rsolve has solved the recurrence
                if type(op(1,ClosedForm),set)
                then ClosedForm:=FAIL;
                else ClosedForm:=subs(_n=_n-Shift1,ClosedForm);
                  ClosedForm:=simplify(ClosedForm,_n,'power','radical')
                fi
            fi
        catch:
        end try;

        try
            Asymptotic:=eval(equivalent(subs(_x=x,GF),x,n),[O=0,n=_n]);
            if has(Asymptotic,_saddlepoint) then Asymptotic:=FAIL fi;
        catch:
        end try;

    fi;

    Encline(name_, specification1, CountingSeq, GF, Recurrence, ClosedForm,
            Asymptotic, Description, references,`if`(nargs=5,optind,NULL))
end:

addtotable:=proc (globtab, index, val)
option `Copyright Stephanie Petit, Inria Rocquencourt, France, 1998`;
local tab;
    tab:=globtab;
    if not type(tab[index],list) then tab[index]:=[val]
    else tab[index]:=[op(tab[index]),val] fi
end: # addtotable

removefromtable:=proc (tab, index, val)
option `Copyright Stephanie Petit, Inria Rocquencourt, France, 1998`;
local i, loctab;
    loctab:=tab;
    if type(loctab[index],list) and member(val,loctab[index],'i') then
        loctab[index]:=subsop(i=NULL,loctab[index]) fi;
    if loctab[index]=[] then tab[index]:=evaln(tab[index]) fi
end: # removefromtable

##############################################
# Function which create the indices table    #
##############################################
# The sixth argument is used when adding to the tables of indices
# instead of starting from scratch.
createindices:=proc(T,indGF,indCF,indExpr,indCount,start::posint,last::posint)
option `Copyright Stephanie Petit, Inria Rocquencourt, France, 1998`;
local i,j, word;
    for i from start to last do
        addtotable(indGF,evala(op(GF,T[i])),T[i]);
        addtotable(indCF,simplify(op(CLOSEDFORM,T[i]),GAMMA),T[i]);
        for word in WordsList(op(8,T[i])) union
            WordsList(op(9,T[i])) union WordsList(op(1,T[i])) do
            addtotable(indExpr,word,T[i])
        od;
        for j to SequenceSize do addtotable(indCount,op(3,T[i])[j],T[i]) od
    od;
    NULL
end:

# var is the index of the sequence which must be removed
removeindices:=proc(T,indGF,indCF,indExpr,indCount,var)
option `Copyright Stephanie Petit, Inria Rocquencourt, France, 1998`;
local j, word, start, entry;
    entry:=T[var];
    removefromtable(indGF,evala(op(GF,entry)),entry);
    removefromtable(indCF,simplify(op(CLOSEDFORM,entry),GAMMA),entry);
    for word in WordsList(op(8,entry)) union
        WordsList(op(9,entry)) union WordsList(op(1,entry)) do
        removefromtable(indExpr,word,entry)
    od;
    for j to SequenceSize do removefromtable(indCount,op(3,entry)[j],entry) od;
    NULL
end:

updateencyclopedia:=proc(index::integer)
global maintable;
    if not assigned(maintable[index]) then
        ERROR("Not an index in the encyclopedia",index) fi;
    removeindices(maintable,indGF,indCF,indExpr,indCount,index);
    maintable[index]:=Tab(args[2..nargs],index);
    createindices(maintable,indGF,indCF,indExpr,indCount,index,index)
end:


