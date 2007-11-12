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

client.wsdl: ../src/savadur-soap-client.ads
	$(ADA2WSDL) -q -f -I../src savadur-soap-client.ads -o client.wsdl
	$(WSDL2AWS) -q -f -cb -spec Savadur.SOAP.Client client.wsdl

server.wsdl: ../src/savadur-soap-server.ads
	$(ADA2WSDL) -q -f -I../src savadur-soap-server.ads -o server.wsdl
	$(WSDL2AWS) -q -f -cb -spec Savadur.SOAP.Server server.wsdl

clean:
	$(RM) -f client.wsdl server.wsdl
