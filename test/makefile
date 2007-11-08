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

all: build

build:
	$(GNATMAKE) -P savadur_regtests

regtests: build
	@echo "Harness ..."
	./harness

regtests_bootstrap:
	$(MAKE) -C ../ build
	$(MKDIR) savadur-dir/projects
	$(CP) ex_project.xml savadur-dir/projects/savadur_bootstrap.xml
	$(CP) -r ../config/scm savadur-dir/
	../bin/savadur-client -project savadur_bootstrap \
	-savadurdir $(PWD)/test/savadur-dir

clean:
	$(RM) -r savadur-dir
	$(GNATCLEAN) -P savadur_regtests
