##############################################################################
##                                Savadur                                   ##
##                                                                          ##
##                        Copyright (C) 2007-2008                           ##
##                     Pascal Obry - Olivier Ramonat                        ##
##                                                                          ##
##  This library is free software; you can redistribute it and/or modify    ##
##  it under the terms of the GNU General Public License as published by    ##
##  the Free Software Foundation; either version 2 of the License, or (at   ##
##  your option) any later version.                                         ##
##                                                                          ##
##  This library is distributed in the hope that it will be useful, but     ##
##  WITHOUT ANY WARRANTY; without even the implied warranty of              ##
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       ##
##  General Public License for more details.                                ##
##                                                                          ##
##  You should have received a copy of the GNU General Public License       ##
##  along with this library; if not, write to the Free Software Foundation, ##
##  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       ##
##############################################################################

include mk.config

GENERAL_OPTIONS = CP="$(CP)" MKDIR="$(MKDIR)" RM="$(RM)" SED="$(SED)" \
	GNATMAKE="$(GNATMAKE)" GNATCLEAN="$(GNATCLEAN)" \
	GNATCHECK="$(GNATCHECK)" GNATCHOP="$(GNATCHOP)" \
	EXEEXT="$(EXEEXT)" DIFF="$(DIFF)" ADA2WSDL="$(ADA2WSDL)" \
	WSDL2AWS="$(WSDL2AWS)" RUNTEST="$(RUNTEST)"

OPTIONS = MODE="$(MODE)" $(GENERAL_OPTIONS)

BIN_DIR=".build/$(shell echo $(MODE) | tr [[:upper:]] [[:lower:]])/bin"

VERSION     = $(shell git describe --abbrev=0 2>/dev/null)
VERSION_ALL = $(shell git describe 2>/dev/null)

all: build

build: setup
	$(GNATMAKE) -XPRJ_BUILD=$(MODE) -P savadur-main

setup:
#  If git is not present then use the version.ads provided in distrib
ifneq ("$(VERSION)", "")
	sed -e 's,\$$VERSION\$$,$(VERSION),g' \
	-e 's,\$$VERSION_ALL\$$,$(VERSION_ALL),g' \
	src/savadur-version.tads > src/savadur-version.ads
endif
	$(MAKE) -C soap setup $(OPTIONS)

regtests:
	$(MAKE) -C test regtests $(OPTIONS)

regtests_bootstrap:
	$(MAKE) -C test regtests_bootstrap $(OPTIONS)

regtests_clientserver:
	$(MAKE) -C test regtests_clientserver $(OPTIONS)

clean:
	$(GNATCLEAN) -XPRJ_BUILD=$(MODE) -P savadur-main
	make -C test clean $(OPTIONS)
	$(MAKE) -C soap clean

check:
	$(GNATCHECK) -dd -XPRJ_BUILD=$(MODE) -P savadur-main \
		-rules -from=savadur.check
install:
	@$(MKDIR) $(INSTALL)/example
	@$(MKDIR) $(INSTALL)/savadurdir/client/scm
	@$(MKDIR) $(INSTALL)/savadurdir/client/servers
	@$(MKDIR) $(INSTALL)/savadurdir/client/share/templates
	@$(MKDIR) $(INSTALL)/savadurdir/server/config
	@$(MKDIR) $(INSTALL)/savadurdir/server/projects
	@$(MKDIR) $(INSTALL)/savadurdir/server/scripts
	@$(MKDIR) $(INSTALL)/savadurdir/server/htdocs/templates
	@$(MKDIR) $(INSTALL)/savadurdir/server/htdocs/img
	@$(MKDIR) $(INSTALL)/savadurdir/server/htdocs/css
	@$(MKDIR) $(INSTALL)/savadurdir/server/share/templates
	@$(MKDIR) $(INSTALL)/savadurdir/server/scm
	@$(CP) db/data/create_database.sh $(INSTALL)/savadurdir/server/scripts/
	@$(CP) config/scm/* $(INSTALL)/savadurdir/server/scm
	@$(CP) templates/*.thtml $(INSTALL)/savadurdir/server/htdocs/templates
	@$(CP) templates/*.html $(INSTALL)/savadurdir/server/htdocs/templates
	@$(CP) templates/*.txml $(INSTALL)/savadurdir/server/htdocs/templates
	@$(CP) templates/*.css $(INSTALL)/savadurdir/server/htdocs/css
	@$(CP) templates/img/*.* $(INSTALL)/savadurdir/server/htdocs/img
	@$(CP) templates/config/*.txml $(INSTALL)/savadurdir/server/share/templates/
	@$(CP) templates/config/*.txml $(INSTALL)/savadurdir/client/share/templates/
	@$(CP) test/config/*.xml $(INSTALL)/example/
	cp $(BIN_DIR)/savadur $(INSTALL)
	@echo savadur is installed in $(INSTALL)

distrib: setup
	git archive --prefix=savadur/ HEAD > savadur.tar
	tar -C ../ -r --file=savadur.tar savadur/src/savadur-version.ads
	gzip -f savadur.tar
