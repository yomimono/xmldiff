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

VERSION=0.1

PACKAGES=xmlm,xtmpl
OF_FLAGS=-package $(PACKAGES)
COMPFLAGS=-annot -rectypes -g
OCAMLPP=

OCAMLFIND=ocamlfind

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

all: byte opt
byte: xmldiff.cmo
opt: xmldiff.cmx xmldiff.cmxs

xmldiff.cmx: xmldiff.cmi xmldiff.ml
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -c $(COMPFLAGS) xmldiff.ml

xmldiff.cmxs: xmldiff.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -shared -o $@ $(COMPFLAGS) xmldiff.cmx

xmldiff.cmo: xmldiff.cmi xmldiff.ml
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -c $(COMPFLAGS) xmldiff.ml

xmldiff.cmi: xmldiff.mli
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -c $(COMPFLAGS) $<

test-xmldiff: xmldiff.cmx test_xmldiff.ml
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) $(COMPFLAGS) -o $@ -linkpkg $^

##########
.PHONY: doc
doc:
	$(MKDIR) doc
	$(OCAMLDOC) $(INCLUDES) xmldiff.mli -t Xtmpl -d doc -html

webdoc: doc
	$(MKDIR) ../xmldiff-gh-pages/refdoc
	$(CP) doc/* ../xmldiff-gh-pages/refdoc/
	$(CP) web/index.html web/style.css ../xmldiff-gh-pages/


##########
install: xmldiff.cmo xmldiff.cmx xmldiff.cmxs
	ocamlfind install xmldiff META LICENSE \
		xmldiff.cmi xmldiff.mli xmldiff.cmo xmldiff.cmx xmldiff.cmxs xmldiff.o

uninstall:
	ocamlfind remove xmldiff

# archive :
###########
archive:
	git archive --prefix=xmldiff-$(VERSION)/ HEAD | gzip > ../xmldiff-gh-pages/xmldiff-$(VERSION).tar.gz

#####
clean:
	$(RM) *.cm* *.o *.annot *.a

# headers :
###########
HEADFILES=Makefile *.ml *.mli
.PHONY: headers noheaders
headers:
	headache -h header -c .headache_config $(HEADFILES)

noheaders:
	headache -r -c .headache_config $(HEADFILES)


