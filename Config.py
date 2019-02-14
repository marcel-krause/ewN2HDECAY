#!/usr/bin/env python
#Filename: Config.py


 ##################################################################################################################
#                                                                                                                  #
#                                                  Configuration                                                   #
#                                                                                                                  #
#   Purpose:    Main configuration file of ewN2HDECAY. Contains all configuration                                  #
#               settings needed to change the program.                                                             #
#   Copyright:  Copyright (C) 2019, Marcel Krause and Milada Margarete Muehlleitner                                #
#   License:    GNU General Public License (GNU GPL-3.0-or-later)                                                  #
#                                                                                                                  #
#               ewN2HDECAY is released under GNU General Public License (GNU GPL-3.0-or-later).                    #
#               This program is free software: you can redistribute it and/or modify it under the terms of the     #
#               GNU General Public License as published by the Free Software Foundation, either version 3 of       #
#               the License, or any later version.                                                                 #
#                                                                                                                  #
#               This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;          #
#               without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.          #
#               See the GNU General Public License for more details.                                               #
#                                                                                                                  #
#               You have received a copy (LICENSE.md) of the GNU General Public License along with this program    #
#               in the ewN2HDECAY root directory.                                                                  #
#                                                                                                                  #
 ##################################################################################################################


#---------------------#
#       Shared        #
#---------------------#
useRelativeLoopToolsPath = True								# Set True if you want to set the path to LoopTools relative to the ewN2HDECAY installation path (useful if you installed LoopTools e.g. in a subdirectory of the ewN2HDECAY folder) or False if you want to use an absolute path to LoopTools
pathLoopTools = 'LoopTools-2.14/i686-CYGWIN_NT-10.0-WOW'	# Specify the path to the LoopTools root folder (IMPORTANT: the path must never *end* with '/' and if useRelativePath is True, it must not *start* with '/' either! If useRelativePath is False, it depends on the OS if the full absolute path starts with '/' or not: on Windows, it typically does not, on Linux, it typically does)
pathLoopToolsLibs = 'lib'									# Specify the LoopTools subfolder (relative to pathLoopTools) where the LoopTools libraries are contained (NOTE: this depends on the OS and chip architecture; on Windows, this is normally 'lib', on Linux and macOS, it is normally 'lib64')
pathLoopToolsExecs = 'bin'									# Specify the LoopTools subfolder (relative to pathLoopTools) where the LoopTools libraries are contained (NOTE: this depends on the OS and chip architecture; on Windows, this is normally 'lib', on Linux and macOS, it is normally 'lib64')
pathToCygwin = 'C:\\cygwin\\bin\\bash.exe'                  # Specify the path to the Cygwin bash executable (for Windows only)
loopToolsVersion = 'LoopTools-2.14'                         # Specify the LoopTools version that shall be downloaded (recommended, as checked for compatibility: LoopTools-2.14)
renScaleDefinitions = ['MIN', 'MIN*DSQRT(2D0)**(-1)']       # Give definitions for possible renormalization scales in terms of the decaying Higgs mass MIN (NOTE: use FORTRAN functions and formats!)