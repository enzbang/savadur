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


setup: client.wsdl server.wsdl

client.wsdl: ../src/savadur-web_services-client.ads
	$(ADA2WSDL) -q -f -I../src savadur-web_services-client.ads \
		-o client.wsdl -s Savadur-Client -a http://localhost:8181
	$(WSDL2AWS) -q -f -cb -spec Savadur.Web_Services.Client client.wsdl

server.wsdl: ../src/savadur-web_services-server.ads
	$(ADA2WSDL) -q -f -I../src savadur-web_services-server.ads \
		-o server.wsdl -s Savadur-Server -a http://localhost:8282
	$(WSDL2AWS) -q -f -cb -spec Savadur.Web_Services.Server server.wsdl

clean:
	$(RM) -f client.wsdl server.wsdl
