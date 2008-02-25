##############################################################################
##                                Savadur                                   ##
##                                                                          ##
##                           Copyright (C) 2007                             ##
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

GENERAL_OPTIONS = CP="$(CP)" MKDIR="$(MKDIR)" RM="$(RM)" \
	GNATMAKE="$(GNATMAKE)" GNATCLEAN="$(GNATCLEAN)" \
	GNATCHECK="$(GNATCHECK)" GNATCHOP="$(GNATCHOP)" \
	EXEEXT="$(EXEEXT)" DIFF="$(DIFF)" ADA2WSDL="$(ADA2WSDL)" \
	WSDL2AWS="$(WSDL2AWS)"

OPTIONS = MODE="$(MODE)" $(GENERAL_OPTIONS)

BIN_DIR=".build/$(shell echo $(MODE) | tr [[:upper:]] [[:lower:]])/bin"

VERSION     = $(shell git describe --abbrev=0 2>/dev/null)
VERSION_ALL = $(shell git describe 2>/dev/null)

all: build

build: setup
	$(GNATMAKE) -XPRJ_BUILD=$(MODE) -P savadur-main

setup:
# If git is not present then use the version.ads provided in distrib
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
	@$(MKDIR) $(INSTALL)/client-savadurdir/scm
	@$(MKDIR) $(INSTALL)/client-savadurdir/servers
	@$(MKDIR) $(INSTALL)/server-savadurdir/config
	@$(MKDIR) $(INSTALL)/server-savadurdir/projects
	@$(MKDIR) $(INSTALL)/server-savadurdir/scripts
	@$(MKDIR) $(INSTALL)/server-savadurdir/htdocs/templates
	@$(MKDIR) $(INSTALL)/server-savadurdir/htdocs/css
	@$(MKDIR) $(INSTALL)/server-savadurdir/scm
	@$(CP) db/data/create_database.sh $(INSTALL)/server-savadurdir/scripts/
	@$(CP) config/scm/* $(INSTALL)/server-savadurdir/scm
	@$(CP) templates/*.thtml $(INSTALL)/server-savadurdir/htdocs/templates
	@$(CP) templates/*.txml $(INSTALL)/server-savadurdir/htdocs/templates
	@$(CP) templates/*.css $(INSTALL)/server-savadurdir/htdocs/css
	@$(CP) test/config/*.xml $(INSTALL)/example/
	cp $(BIN_DIR)/savadur $(INSTALL)
	@echo savadur is installed in $(INSTALL)

distrib: setup
	git archive --prefix=savadur/ HEAD > savadur.tar
	tar -C ../ -r --file=savadur.tar savadur/src/savadur-version.ads
	gzip -f savadur.tar
