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

all: build

build:
	$(GNATMAKE) -P savadur_regtests

regtests: setup build
	@echo "Harness ..."
	./harness

setup:
	$(RM) -r test-dir/client
	$(MKDIR) test-dir/client
	$(CP) -r ../config/scm test-dir/client
	$(CP) -r config/servers test-dir/client
	$(CP) -r config/config test-dir/client
	$(CP) -r test-dir/projects test-dir/client
	$(CP) -r test-dir/env test-dir/client
	$(CP) config/style_checker.xml test-dir/client/projects/
	$(CP) config/dummy.xml test-dir/client/projects/
	$(CP) config/uiop.xml test-dir/client/projects/

regtests_bootstrap: setup
	$(GNATMAKE) -XPRJ_BUILD=$(MODE) -XIN_TEST=TRUE -P ../savadur-main
	SAVADUR_DIR=$(PWD)/test/test-dir ./bin/savadur --standalone \
		--project bootstrap

clean:
	$(RM) -r test-dir/work test-dir/scm
	$(GNATCLEAN) -P savadur_regtests

regtests_clientserver: setup_clientserver
	(cd ../db/data/; sh create_database.sh)
	$(CP) ../db/data/logs.db test-dir/server/
	make setup -C ../ MODE=$(MODE)
	$(GNATMAKE) -XPRJ_BUILD=$(MODE) -XIN_TEST=TRUE -P ../savadur-main
	./clientserver_regtest.sh

setup_clientserver:
	@$(RM) -r test-dir/client
	@$(RM) -r test-dir/server
	@$(RM) -r test-dir/logs
	@$(RM) -r test-dir/newproj
	@$(MKDIR) test-dir/client/servers
	@$(MKDIR) test-dir/server/projects
	@$(MKDIR) test-dir/server/config
	@$(MKDIR) test-dir/server/htdocs/templates
	@$(MKDIR) test-dir/client/share/templates
	@$(MKDIR) test-dir/logs
	@$(MKDIR) test-dir/newproj
	@$(CP) -r ../config/scm/ test-dir/server
	@$(CP) config/style_checker.xml test-dir/server/projects/
	@$(CP) config/morzhol.xml test-dir/server/projects/
	@$(CP) config/newproj.xml test-dir/server/projects/
	@$(SED) -e "s,@LOGS@,$(PWD),g" config/servers/localhost.xml \
		> test-dir/client/servers/localhost.xml
	@$(CP) test-dir/project_list.xml test-dir/server/config/
	@$(CP) ../templates/*.txml test-dir/server/htdocs/templates
	@$(CP) ../templates/config/*.txml test-dir/client/share/templates
