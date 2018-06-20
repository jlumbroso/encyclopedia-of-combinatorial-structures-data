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

##    -*-Maple-*-
##
##    Title:	STRUCTURE OF THE GENERALIZED ASYMPTOTIC EXPANSIONS
##    Created:	Mon Dec 26 11:51:50 1988
##    Author:	Bruno Salvy
##	<salvy@poly.polytechnique.fr>
##    Modified:	    Sun Aug 13 12:01:52 1989
##    Author:	Bruno Salvy
##    Modification: exprseq instead of product and index as first elt instead
##		    of X[index] everywhere.
##
##     A generalized asymptotic expansion is:
##	 * either undefined
##       * or a constant (coding itself)
##	 * or a list coding a sum,
##		the first term is the index of the variable which may be 
##		an integer or a rational number:
##		 1  --> 1/x
##		 2  --> 1/log(x)
##		 3  --> 1/log(log(x)) ...
##		 0  --> 1/exp(x)
##		 -1 --> 1/exp(exp(x)) ...
##	      	non-integer indices are determined by dev/indexify
##	    the other terms encode products
##		      C*X[i]^alpha, where
##		 - i is the index
##		 - alpha is real
##		 - C is an asymptotic expansion whose lowest monomial is
##		       smaller than X[i] or a constant term which may be 
##		       a bounded function of any X[i] 
##	       This product is encoded as an exprseq of 2 elements: C,alpha
##	    the last term is the power of the remainder (to appear in the O())
##		infinity means that the expansion is exact.
##
quit
