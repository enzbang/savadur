##############################################################################
##                                Savadur                                   ##
##                                                                          ##
##                           Copyright (C) 2007                             ##
##                            Olivier Ramonat                               ##
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
	EXEEXT="$(EXEEXT)" DIFF="$(DIFF)"

OPTIONS = MODE="$(MODE)" $(GENERAL_OPTIONS)

BIN_DIR=".build/$(shell echo $(MODE) | tr [[:upper:]] [[:lower:]])/bin"

VERSION     = $(shell git describe --abbrev=0 2>/dev/null)
VERSION_ALL = $(shell git describe 2>/dev/null)

all: build

setup:
# If git is not present then use the version.ads provided in distrib
ifneq ("$(VERSION)", "")
	sed -e 's,\$$VERSION\$$,$(VERSION),g' \
	-e 's,\$$VERSION_ALL\$$,$(VERSION_ALL),g' \
	src/savadur-version.tads > src/savadur-version.ads
endif

build:
	$(GNATMAKE) -XPRJ_BUILD=$(MODE) -P savadur

regtests:
	$(MAKE) -C test regtests $(OPTIONS)

regtests_bootstrap:
	$(MAKE) -C test regtests_bootstrap $(OPTIONS)

clean:
	$(GNATCLEAN) -P savadur
	make -C test clean $(OPTIONS)

check:
	$(GNATCHECK) -dd -Psavadur -rules -from=savadur.check

install:
	@$(MKDIR) $(INSTALL)/example
	@$(CP) -r config/scm $(INSTALL)/scm
	@$(CP) test/ex_project.xml $(INSTALL)/example/
	@cp $(BIN_DIR)/savadur-client $(INSTALL)
	@echo savadur is installed in $(INSTALL)

distrib: setup
	git archive --prefix=savadur/ HEAD > savadur.tar
	tar -C ../ -r --file=savadur.tar savadur/src/savadur-version.ads
	gzip -f savadur.tar
