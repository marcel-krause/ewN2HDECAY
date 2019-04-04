#!/usr/bin/env python
#Filename: ewN2HDECAY.py


 #################################################################################################################################
#                                                                                                                                 #
#                                                                 ewN2HDECAY                                                      #
#                                                                                                                                 #
#   Purpose:    A program for the calculation One-Loop Electroweak Corrections to Higgs Decays in the Next-to-Minial              #
#               Two-Higgs-Doublet Model (N2HDM) Including State-of-the-Art QCD Corrections                                        #
#   Authors:    Marcel Krause (marcel.krause@kit.edu)                                                                             #
#               Prof. Dr. M. Margarete Muehlleitner (margarete.muehlleitner@kit.edu)                                              #
#   Version:    1.0.0                                                                                                             #
#   Date:       04.04.2019                                                                                                        #
#   Copyright:  Copyright (C) 2019, Marcel Krause and Milada Margarete Muehlleitner                                               #
#   License:    GNU General Public License (GNU GPL-3.0-or-later)                                                                 #
#                                                                                                                                 #
#               ewN2HDECAY is released under GNU General Public License (GNU GPL-3.0-or-later). This program is free software:    #
#               you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the    #
#               Free Software Foundation, either version 3 of the License, or any later version.                                  #
#                                                                                                                                 #
#               This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the        #
#               implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for   #
#               more details.                                                                                                     #
#                                                                                                                                 #
#               You have received a copy (LICENSE.md) of the GNU General Public License along with this program in the            #
#               ewN2HDECAY root directory.                                                                                        #
#                                                                                                                                 #
#   Citation:   When you use this program, please acknowledge the work of our and other groups by citing the following papers:    #
#                   The manual for ewN2HDECAY:                                                                                    #
#                    - M. Krause, M. Muhlleitner, arXiv:1904.02103 (hep-ph)                                                       #
#                   The manuals for HDECAY and N2HDECAY:                                                                          #
#                    - A. Djouadi, J. Kalinowski, M. Spira, Comp. Phys. Commun. 108 (1998) 56, hep-ph/9704448                     #
#                    - A. Djouadi, J. Kalinowski, M. Muhlleitner, M. Spira, arXiv:1801.09506 (hep-ph)                             #
#                    - M. Muhlleitner, M. O. P. Sampaio, R. Santos, J. Wittbrodt, JHEP 1703 (2017) 094, arXiv:1612.01309 (hep-ph) #
#                   The paper on the EW correction to the N2HDM decays:                                                           #
#                    - M. Krause, D. Lopez-Val, M. Muhlleitner, R. Santos, JHEP 12 (2017) 077, arXiv:1708.01578 (hep-ph)          #
#                   The publication of LoopTools:                                                                                 #
#                    - T. Hahn, M. Perez-Victoria, Comp. Phys. Commun. 118 (1999) 153-165, hep-ph/9807565                         #
#                                                                                                                                 #
 #################################################################################################################################


#------------------------------#
#         Import Modules       #
#------------------------------#
import sys
import os
from shutil import copyfile, rmtree
from math import pi, sqrt
import subprocess
# import multiprocessing
import CommonFunctions      # Provides common, often used functions for different scripts of ewN2HDECAY

#-------------------------#
#        Settings         #
#-------------------------#
# WARNING: do not change these settings if you do not know what they do!
lineToInsert = 134      # This is the line at which the temporary input file ends and at which we append the electroweak corrections
lineWhereAlphaAtMZ = 27 # This is the line at which in the temporary input file the fine-structure constant at the Z boson mass MZ is specified
lineWhereGFCalc = 29    # This is the line at which in the temporary input file the calculated Fermi constant GFCALC is specified
lineWhereMZ = 32        # This is the line at which in the temporary input file the Z boson mass MZ is specified
lineWhereMW = 33        # This is the line at which in the temporary input file the W boson mass MW is specified
lineWhereOSMC = 23      # This is the line at which the OS MC value has to be inserted in the temporary input file
lineWhereOSMB = 24      # This is the line at which the OS MB value has to be inserted in the temporary input file
lineWhereParamType = 57 # This is the line at which the parameter type is specified
lineWhereRefScheme = 77 # This is the line at which the reference renormalization scheme is specified

#----------------------------#
#        Main Program        #
#----------------------------#

if __name__ == "__main__":
	# Print the welcome screen
	print('''
+---------------------------------------+
|                                       |
|           ewN2HDECAY 1.0.0            |
|                                       |
|                             /         |
|                            /          |
|                           /           |
|                      --- /            |
|      ______________/     \            |
|                    \     /            |
|                      --- \            |
|                           \           |
|                            \          |
|                             \         |
|                                       |
+---------------------------------------+

When you use this program please cite:
	The manual for ewN2HDECAY:
	 - M. Krause, M. Muhlleitner, arXiv:1904.02103 (hep-ph)
	The manuals for HDECAY and N2HDECAY:
	 - A. Djouadi, J. Kalinowski, M. Spira, Comp. Phys. Commun. 108 (1998) 56, hep-ph/9704448
	 - A. Djouadi, J. Kalinowski, M. Muhlleitner, M. Spira, arXiv:1801.09506 (hep-ph)
	 - M. Muhlleitner, M. O. P. Sampaio, R. Santos, J. Wittbrodt, JHEP 1703 (2017) 094, arXiv:1612.01309 (hep-ph)
	The paper on the EW correction to the N2HDM decays:
	 - M. Krause, D. Lopez-Val, M. Muhlleitner, R. Santos, JHEP 12 (2017) 077, arXiv:1708.01578 (hep-ph)
	The publication of LoopTools:
	 - T. Hahn, M. Perez-Victoria, Comp. Phys. Commun. 118 (1999) 153-165, hep-ph/9807565

ewN2HDECAY is released under GNU General Public License (GNU GPL-3.0-or-later). This program is free software: 
you can redistribute it and/or modify it under the terms of the GNU General Public License as published by 
the Free Software Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the 
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
for more details.

You have received a copy (LICENSE.md) of the GNU General Public License along with this program in the ewN2HDECAY
root directory.

Copyright 2019, Marcel Krause and Milada Margarete Muehlleitner.
	''')

	# Get a list of all input files
	inputPath = "Input"
	inputFileList = os.listdir(inputPath)
	if '..' in inputFileList:
		inputFileList.remove('..')
	if '.' in inputFileList:
		inputFileList.remove('.')

	# Iterate over all input files
	for inputFileTemp in inputFileList:
		print("Calculating corrections for input file " + inputFileTemp + " ...\n")

		# Copy the input file to the N2HDECAY subfolder
		print("Copying input files into N2HDECAY folder...")
		filenameIn = "Input" + os.sep + inputFileTemp
		filenameOut = "N2HDECAY" + os.sep + "n2hdecay.in"
		# Remove any existing input and fermion masses file in N2HDECAY
		if os.path.isfile(filenameOut):
			os.remove(filenameOut)
		if os.path.isfile("N2HDECAY" + os.sep + "fermionmasses.dat"):
			os.remove("N2HDECAY" + os.sep + "fermionmasses.dat")
		if os.path.isfile("N2HDECAY" + os.sep + "alphaandbeta.dat"):
			os.remove("N2HDECAY" + os.sep + "alphaandbeta.dat")
		copyfile(filenameIn, filenameOut)
		with open("N2HDECAY" + os.sep + "alphaandbeta.dat", 'w') as fileHandler:
			fileHandler.write(' alpha1 = 0D0\n alpha2 = 0D0\n alpha3 = 0D0\n beta  = 0D0\n')
		print("... done.\n")

		# Let N2HDECAY run in minimal mode to produce the fermion mass file
		print("Starting N2HDECAY in minimal mode...")
		os.chdir('N2HDECAY')
		prompt = ['./run', '1']
		subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
		os.chdir('..')
		print("N2HDECAY in minimal mode terminated.\n")

		# Read the fermion masses from the fermion mass file
		filenameMasses = "N2HDECAY" + os.sep + "fermionmasses.dat"
		massFileLines = list(line.rstrip('\n') for line in open(filenameMasses))
		MCOSCalc = float((massFileLines[0].split('='))[1].strip())
		MBOSCalc = float((massFileLines[1].split('='))[1].strip())
		
		# Copy the file name to the N2HDECAY folder and truncate it at the end
		print("Copying input files into N2HDECAY folder...")
		filenameIn = "Input" + os.sep + inputFileTemp
		filenameOut = "N2HDECAY" + os.sep + "n2hdecay.in"
		if os.path.isfile(filenameOut):
			os.remove(filenameOut)
		fileHandler = open(filenameIn, "r")
		convertedFileHandler = []
		lineCount = 1
		for line in fileHandler:
			# If the parameter type is 2, then the reference renormalization scheme has to be set to zero
			if (lineCount == lineWhereRefScheme):
				refScheme = int((line.split())[2])
				if (refScheme == 0):
					print("\nERROR: REFSCHEM=0 is given as input, but REFSCHEM must be greater than zero.")
					print("ewN2HDECAY will be terminated now (ERROR: REFSCHEM=0 was set).")
					sys.exit()
				if (refScheme != 0) and (paramType == 2):
					convertedFileHandler.append("REFSCHEM = 0\n")
					print("\nWARNING: REFSCHEM (line 59) is given as a non-zero value, but TYPE=2 (line 57, lambdas as input) is set.")
					print("REFSCHEM is overwritten to zero, automatic parameter conversion is deactivated!")
					lineCount += 1
					continue
			# Write the current line in an array
			convertedFileHandler.append(line)
			# Check for the parameter type
			if lineCount == lineWhereParamType:
				paramType = int((line.split())[2])
			# Pick out the values of MW, MZ and alphaAtMZ
			if lineCount == lineWhereMZ:
				massMZ = float((line.split())[2])
			if lineCount == lineWhereMW:
				massMW = float((line.split())[2])
			if lineCount == lineWhereAlphaAtMZ:
				alphaAtMZ = float((line.split())[2])
			lineCount += 1
		fileHandler.close()

		# Write a copy of the file to the output folder, but add GFCALC, MCOSCALC and MBOSCALC with the calculated values
		GFcalc = pi/sqrt(2)*alphaAtMZ/(massMW**2*(1-massMW**2/massMZ**2))
		GFline = "GFCALC   = " + str(GFcalc) + "\n"
		MCOSline = "MCOSCALC = " + str(MCOSCalc) + "\n"
		MBOSline = "MBOSCALC = " + str(MBOSCalc) + "\n"
		lineCount = 1
		convertedFile = ''
		renScaleIsDynamic = '0'
		for line in convertedFileHandler:
			if "OUTSCALE" in line and "MIN" in line:
				renScaleIsDynamic = '1'
			if lineCount == lineWhereGFCalc:
				convertedFile += GFline
			elif lineCount == lineWhereOSMC:
				convertedFile += MCOSline
				convertedFile += MBOSline
				convertedFile += line
			else:
				convertedFile += line
			lineCount += 1
		fileHandler = open(filenameOut, "w")
		fileHandler.write(convertedFile)
		fileHandler.close()
		print("... done.\n")
		
		# Calculate the electroweak corrections
		print("Calculating electroweak corrections...\n")
		prompt = ['./electroweakCorrections', '0', '0', '0', '1', 'N2HDECAY' + os.sep + 'n2hdecay.in', 'n2hdecay.in', renScaleIsDynamic]
		subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
		print("Calculation of electroweak corrections done.\n")

		# Replace the newline character in each file with a proper newline
		print("Postprocessing temporary input file...")
		fileHandler = open(filenameOut, "r")
		convertedFile = ''
		lineCount = 1
		for line in fileHandler:
			# Convert the literal newlines to actual ones and remove the leading whitespace from the Fortran output
			if (lineCount == lineToInsert):
				lineToReplace = line.replace('\\n', '\n')[1:]
			else:
				lineToReplace = line.replace('\\n', '\n')
			convertedFile += lineToReplace
			# print(lineToReplace)
			lineCount += 1
		fileHandler.close()

		# Store the results file in the correct directory
		if os.path.isfile(filenameOut):
			os.remove(filenameOut)
		fileHandler = open(filenameOut, "w")
		fileHandler.write(convertedFile)
		fileHandler.close()
		print("... done.\n")

		# Start N2HDECAY in the normal (non-minimal) configuration
		print("Starting N2HDECAY in standard mode...")
		os.chdir('N2HDECAY')
		prompt = ['./run']
		subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False)
		os.chdir('..')
		print("N2HDECAY in standard mode terminated.\n")

		# Copy the output files to the results folder
		print("Copying input files into output folder...")
		filenameIn = "N2HDECAY" + os.sep + "slha.out"
		filenameOut = "Results" + os.sep + inputFileTemp.replace('.in', '_BR.out')
		copyfile(filenameIn, filenameOut)
		filenameIn = "N2HDECAY" + os.sep + "ewpartialwidth.out"
		filenameOut = "Results" + os.sep + inputFileTemp.replace('.in', '_EW.out')
		copyfile(filenameIn, filenameOut)
		print("... done.\n")

		# Cleaning
		if os.path.isfile("N2HDECAY" + os.sep + "fermionmasses.dat"):
			os.remove("N2HDECAY" + os.sep + "fermionmasses.dat")
		if os.path.isfile("N2HDECAY" + os.sep + "alphaandbeta.dat"):
			os.remove("N2HDECAY" + os.sep + "alphaandbeta.dat")

		print("Corrections for input file " + inputFileTemp + " done.\n")

	# End of program is reached
	print("All calculations finished. Thanks for using ewN2HDECAY!")

	# Print the end screen 
	print('''
When you use this program please cite:
	The manual for ewN2HDECAY:
	 - M. Krause, M. Muhlleitner, arXiv:1904.02103 (hep-ph)
	The manuals for HDECAY and N2HDECAY:
	 - A. Djouadi, J. Kalinowski, M. Spira, Comp. Phys. Commun. 108 (1998) 56, hep-ph/9704448
	 - A. Djouadi, J. Kalinowski, M. Muhlleitner, M. Spira, arXiv:1801.09506 (hep-ph)
	 - M. Muhlleitner, M. O. P. Sampaio, R. Santos, J. Wittbrodt, JHEP 1703 (2017) 094, arXiv:1612.01309 (hep-ph)
	The paper on the EW correction to the N2HDM decays:
	 - M. Krause, D. Lopez-Val, M. Muhlleitner, R. Santos, JHEP 12 (2017) 077, arXiv:1708.01578 (hep-ph)
	The publication of LoopTools:
	 - T. Hahn, M. Perez-Victoria, Comp. Phys. Commun. 118 (1999) 153-165, hep-ph/9807565

ewN2HDECAY is released under GNU General Public License (GNU GPL-3.0-or-later). This program is free software: 
you can redistribute it and/or modify it under the terms of the GNU General Public License as published by 
the Free Software Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the 
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
for more details.

You have received a copy (LICENSE.md) of the GNU General Public License along with this program in the ewN2HDECAY
root directory.

Copyright 2019, Marcel Krause and Milada Margarete Muehlleitner.
	''')

	sys.exit()
