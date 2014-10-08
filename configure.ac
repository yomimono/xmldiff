#################################################################################
#                Xmldiff                                                        #
#                                                                               #
#    Copyright (C) 2014 Institut National de Recherche en Informatique          #
#    et en Automatique. All rights reserved.                                    #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU Lesser General Public License version        #
#    3 as published by the Free Software Foundation.                            #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU Lesser General Public           #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#                                                                               #
#################################################################################

# check for one particular file of the sources
AC_INIT(master.Makefile.in)

VERSION=0.4.0
PACKAGE=xmldiff

# The root directory where we will compile
ROOT=`pwd`
USER=`basename $HOME`
GROUP=`groups | cut -d' ' -f 1`
OCAMLFIND_PATH=`ocamlfind printconf path`

ocaml unix.cma str.cma checkocaml.ml ocaml_config.sh || exit 1
. ./ocaml_config.sh

if test "$prefix" = "NONE" ; then
  prefix=`dirname $OCAMLBIN`
fi

if test "$datadir" = "\${datarootdir}" ; then
        datadir=${prefix}/share/stog
fi
if test "$datadir" = "\${prefix}/share" ; then
        datadir=$prefix/share
fi
SHARE_DIR=$datadir


PACKAGES=xmlm

#############################
# substitutions to perform
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLYACC)
AC_SUBST(OCAMLBEST)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLLIB)
AC_SUBST(OCAMLBIN)
AC_SUBST(OCAMLDOC)
AC_SUBST(OCAMLFIND)
AC_SUBST(EXEC_PREFIX)
AC_SUBST(ROOT)
AC_SUBST(USER)
AC_SUBST(GROUP)
AC_SUBST(OCAMLFIND_PATH)
AC_SUBST(VERSION)
AC_SUBST(PACKAGE)
AC_SUBST(SHARE_DIR)
AC_SUBST(JS_OF_OCAML)
AC_SUBST(PACKAGE_JS)
AC_SUBST(LIB_JS)
AC_SUBST(PACKAGES)

# Finally create the master.Makefile.in
ACFILES="master.Makefile META"
AC_OUTPUT($ACFILES)
chmod a-w master.Makefile
chmod a-w META


echo "
### Results of configuration ###

Version is $VERSION
Packages to be installed: $PACKAGE $PACKAGE_JS
"