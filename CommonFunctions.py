#!/usr/bin/env python
#Filename: CommonFunctions.py


 ###############################################################################################################
#                                                                                                               #
#                                               CommonFunctions                                                 #
#                                                                                                               #
#   Purpose:    Function library for ewN2HDECAY. Contains often used functions.                                 #
#   Copyright:  Copyright (C) 2019, Marcel Krause and Milada Margarete Muehlleitner                             #
#   License:    GNU General Public License (GNU GPL-3.0-or-later)                                               #
#                                                                                                               #
#               ewN2HDECAY is released under GNU General Public License (GNU GPL-3.0-or-later).                 #
#               This program is free software: you can redistribute it and/or modify it under the terms of the  #
#               GNU General Public License as published by the Free Software Foundation, either version 3 of    #
#               the License, or any later version.                                                              #
#                                                                                                               #
#               This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       #
#               without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       #
#               See the GNU General Public License for more details.                                            #
#                                                                                                               #
#               You have received a copy (LICENSE.md) of the GNU General Public License along with this program #
#               in the ewN2HDECAY root directory.                                                               #
#                                                                                                               #
 ###############################################################################################################


#------------------------------#
#        Import Modules        #
#------------------------------#
import sys
import os
import errno

#-------------------------#
#        Functions        #
#-------------------------#
def queryBoolean(question):
	'''
		For a given yes/no question, check the validity of the answer and return the corresponding Boolean value.
	'''
	validTrue = {"yes": True, "y": True, "ye": True, "j": True, "ja": True, "1": True}
	validFalse = {"no": False, "n": False, "nein": False, "0": False}
	prompt = " [y/n] "

	while True:
		sys.stdout.write(question + prompt)
		# Compatibility for Python 2 and 3
		if(sys.version_info > (3,0)):
			choice = input().lower()
		else:
			choice = raw_input().lower()
		if choice in validTrue:
			return True
		elif choice in validFalse:
			return False
		else:
			sys.stdout.write('Error: invalid input. Enter "y" or "n".\n\n')
