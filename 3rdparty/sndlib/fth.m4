## fth.m4 -- Autoconf macros for configuring FTH   -*- Autoconf -*-

## Copyright (C) 2006 Michael Scholz

## Author: Michael Scholz <scholz-micha@gmx.de>
## Created: Mon Mar 13 17:14:46 CET 2006
## Changed: Thu Mar 23 13:46:43 CET 2006
## Ident: $Id: fth.m4,v 1.1.1.1 2006/03/25 21:29:50 mi-scholz Exp $

## This file is part of FTH.

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

## Commentary:

# FTH_CHECK_LIB(action-if-found, [action-if-not-found])
# 
# Usage: FTH_CHECK_LIB([AC_DEFINE([HAVE_FORTH])])
#
# Don't quote this macro: [FTH_CHECK_LIB(...)] isn't correct.
# Instead call it FTH_CHECK_LIB(...).
# 
# Six variables will be substituted:
#
# FTH               fth program path         or no
# FTH_VERSION       version string           or ""
# FTH_CFLAGS        -I${prefix}/include/fth  or ""
# FTH_LIBS          -L${prefix}/lib -lfth    or ""
# FTH_HAVE_COMPLEX  yes or no
# FTH_HAVE_RATIO    yes or no

## Code:

# AC_CHECK_LIB was written by David MacKenzie.
# This version is slightly changed to fit to FTH_CHECK_LIB.

AC_DEFUN([fth_AC_CHECK_LIB],
[
  m4_ifval([$3], , [AH_CHECK_LIB([$1])])dnl
  AS_LITERAL_IF([$1],
	        [AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1_$2])],
	      	[AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1''_$2])])dnl
  AC_CACHE_CHECK([m4_default([$4], [for $2 in -l$1])], ac_Lib,
  		 [fth_check_lib_save_LIBS=$LIBS
		  LIBS="-l$1 $5 $LIBS"
		  AC_LINK_IFELSE([AC_LANG_CALL([], [$2])],
	       	                 [AS_VAR_SET(ac_Lib, yes)],
				 [AS_VAR_SET(ac_Lib, no)])
		  LIBS=$fth_check_lib_save_LIBS])
  AS_IF([test AS_VAR_GET(ac_Lib) = yes],
        [m4_default([$3], [AC_DEFINE_UNQUOTED(AS_TR_CPP(HAVE_LIB$1)) LIBS="-l$1 $LIBS"])])dnl
  AS_VAR_POPDEF([ac_Lib])dnl
])# fth_AC_CHECK_LIB

AC_DEFUN([FTH_CHECK_LIB],
[
  [AC_PATH_PROG([FTH], [fth], [no])]
  FTH_VERSION=""
  FTH_CFLAGS=""
  FTH_LIBS=""
  FTH_HAVE_COMPLEX=no
  FTH_HAVE_RATIO=no
  AC_MSG_CHECKING([for Forth])
  if test "${FTH}" != no ; then
    FTH_VERSION=`${FTH} --no-init-file --eval .version`
    FTH_CFLAGS=`${FTH} --no-init-file --eval .cflags`
    FTH_LIBS=`${FTH} --no-init-file --eval .libs`
    AC_MSG_RESULT([FTH version ${FTH_VERSION}])
    fth_AC_CHECK_LIB([fth], [fth_make_complex], [FTH_HAVE_COMPLEX=yes],
    			    [whether FTH supports complex numbers], [${FTH_LIBS}])
    fth_AC_CHECK_LIB([fth], [fth_ratio_floor], [FTH_HAVE_RATIO=yes],
    			    [whether FTH supports rational numbers], [${FTH_LIBS}])
    [$1]
  else
    AC_MSG_RESULT([no])
    [$2]
  fi
  AC_SUBST([FTH_VERSION])
  AC_SUBST([FTH_CFLAGS])
  AC_SUBST([FTH_LIBS])
  AC_SUBST([FTH_HAVE_COMPLEX])
  AC_SUBST([FTH_HAVE_RATIO])
])# FTH_CHECK_LIB
	
## fth.m4 ends here
