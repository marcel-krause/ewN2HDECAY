# ewN2HDECAY

A program for the Calculation of Electroweak One-Loop Corrections to Higgs Decays in the Next-to-Minimal Two-Higgs-Doublet Model Including State-of-the-Art QCD Corrections

## Program information

**Program** ewN2HDECAY 1.0.0

**Authors** [Marcel Krause](mailto:marcel.krause@kit.edu) and [M. Margarete Mühlleitner](mailto:margarete.muehlleitner@kit.edu)

**Manual** https://arxiv.org/abs/YYMM.XXXXX

**Citations** When you use this program, please acknowledge the work of our and other groups by citing the following papers:
- The manual for ewN2HDECAY:
  - M. Krause, M. Muhlleitner, arXiv:YYMM.XXXXX
- The manuals for HDECAY and N2HDECAY:
  - A. Djouadi, J. Kalinowski, M. Spira, Comp. Phys. Commun. 108 (1998) 56, hep-ph/9704448
  - A. Djouadi, J. Kalinowski, M. Muhlleitner, M. Spira, arXiv:1801.09506 (hep-ph)
  - M. Muhlleitner, M. O. P. Sampaio, R. Santos, J. Wittbrodt, JHEP 1703 (2017) 094, arXiv:1612.01309 (hep-ph)
- The paper on the electroweak correction to the N2HDM decays:
  - M. Krause, D. Lopez-Val, M. Muhlleitner, R. Santos, JHEP 12 (2017) 077, arXiv:1708.01578 (hep-ph)
- The publication of LoopTools:
  - T. Hahn, M. Perez-Victoria, Comp. Phys. Commun. 118 (1999) 153-165, hep-ph/9807565

**Abstract** We present the program package ewN2HDECAY for the calculation of the partial decay widths and branching ratios of the Higgs bosons of a general CP-conserving Next-to-Minimal 2-Higgs doublet model (N2HDM). The tool includes the full electroweak one-loop corrections to all two-body onshell Higgs decays in the N2HDM that are not loop-induced. It combines them with the state-of-the-art QCD corrections that are already implemented in the program HDECAY. For the renormalization of the electroweak sector an on-shell scheme is implemented for most of the renormalization parameters. Exceptions are the soft-![](https://latex.codecogs.com/gif.latex?%5Cmathbb%7BZ%7D_2 "\mathbb{Z}_2")-breaking squared mass scale ![](https://latex.codecogs.com/gif.latex?m_%7B12%7D%5E2 "m_{12}^2") and the singlet vacuum expectation value ![](https://latex.codecogs.com/gif.latex?v_S "v_S"), where an ![](https://latex.codecogs.com/gif.latex?%5Coverline%7B%5Ctext%7BMS%7D%7D "\overline{\text{MS}}") condition is applied, as well as the N2HDM mixing angles ![](https://latex.codecogs.com/gif.latex?%5Calpha%20_i%20~%28i%3D1%2C2%2C3%29 "\alpha _i~(i=1,2,3)") and ![](https://latex.codecogs.com/gif.latex?%5Cbeta "\beta"), for which several distinct renormalization schemes are implemented. The tool ewN2HDECAY can be used for phenomenological analyses of the branching ratios of Higgs decays in the N2HDM. Furthermore, the separate output of the electroweak contributions to the tree-level partial decay widths for several different renormalization schemes allows for an efficient analysis of the impact of the electroweak corrections and the remaining theoretical error due to missing higher-order corrections. The latest version of the program package ewN2HDECAY can be downloaded from the URL https://github.com/marcel-krause/ewN2HDECAY.

**Changelog** For a documentation about the changes made in ewN2HDECAY, check the [Changelog.md](Changelog.md) file.

**Copyright** Copyright (C) 2019, Marcel Krause and Milada Margarete Mühlleitner

**License** GNU General Public License (GNU GPL-3.0-or-later). ewN2HDECAY is released under GNU General Public License (GNU GPL-3.0-or-later). This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You have received a copy ([LICENSE.md](LICENSE.md)) of the GNU General Public License along with this program.

**Contact** For feedback, complaints and bug reports, please send an e-mail to <marcel.krause@kit.edu> and <margarete.muehlleitner@kit.edu>.

## Getting Started

For more detailed information about installing and using the program, please have a look at the [full manual](Documentation/manual.pdf "full manual").

### System requirements

Supported operating systems:
- Windows 7 and Windows 10 (with [Cygwin](https://www.cygwin.com/ "Cygwin") installed)
- Linux
- macOS

The following components must be installed on your system to compile and run ewN2HDECAY:
- GNU gcc (tested with versions 6.4.0 and 7.3.1)
- GNU g++
- GNU gfortran
- Python 2 or 3 (tested with versions 2.7.14 and 3.5.0)
- find
- cURL

If you install ewN2HDECAY on Windows, make sure that you install [Cygwin](https://www.cygwin.com/ "Cygwin") first, including the following components:
- GNU gcc
- GNU g++
- GNU gfortran
- find
- cURL

### Installing

For an easy installation, we recommend using the automatic installer of ewN2HDECAY, which guides you through the installation. Download the latest version of ewN2HDECAY from https://github.com/marcel-krause/ewN2HDECAY. Open a shell, navigate to the ewN2HDECAY root folder and execute the following command:
```
python setup.py
```

The installer asks you if you want to download and install LoopTools, which is required for ewN2HDECAY to run. If you do not have LoopTools installed on your system already type y. The installer downloads the LoopTools version specified in the file [Config.py](Config.py); if you prefer another version, change the corresponding entry in that file first before executing the setup.
If you already have LoopTools on your system, you can choose not to download and install LoopTools. In that case, open the file [Config.py](Config.py) and set
```
useRelativeLoopToolsPath = False
```
and change the variable pathLoopTools to the *absolute* path to the LoopTools main folder (containing the bin, include and library subfolders). Additionally, modify the variables pathLoopToolsLibs and pathLoopToolsExecs to the relative paths to the library and bin subfolders with respect to the LoopTools main directory.

The installer further asks you whether the makefile and electroweakCorrections.F90 file shall be created and whether it should make the program. Type y for all these questions in order to compile ewN2HDECAY.

### Using ewN2HDECAY

Using ewN2HDECAY is simple. Save all input files that you want to use in the 'Input' subdirectory. You can put as many input files in the directory as you wish and the input files can have arbitrary file names. Make sure that the input files have the exact input format required by ewN2HDECAY, as described in the [full manual](Documentation/manual.pdf "full manual").

To start ewN2HDECAY, execute the following command in the main folder of ewN2HDECAY:
```
python ewN2HDECAY.py
```
The program will iterate over all input files and calculate all higher-order corrections according to the parameter and renormalization scheme choices as given in your input files. The resulting output files are saved with the same file name as the input files in the 'Results' subdirectory. For each input file, two output files are generated: one file containing the branching ratios with and without the electroweak corrections, indicated by a filename suffix '_BR', as well as one file containing the electroweak partial decay widths at tree-level and one-loop order, indicated by the filename suffix '_EW'.