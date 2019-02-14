module constants
    implicit none
    save

	! Copyright (C) 2019, Marcel Krause and Milada Margarete Muehlleitner
	
	! License: GNU General Public License (GNU GPL-3.0-or-later)

	! ewN2HDECAY is released under GNU General Public License (GNU GPL-3.0-or-later).
	! This program is free software: you can redistribute it and/or modify it under the terms of the
	! GNU General Public License as published by the Free Software Foundation, either version 3 of
	! the License, or any later version.

	! This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
	! without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
	! See the GNU General Public License for more details.

	! You have received a copy (LICENSE.md) of the GNU General Public License along with this program
	! in the ewN2HDECAY root directory.

	! Integer for choosing whether the electroweak corrections should be calculated or not
	integer :: omitELCorr

    ! Imaginary unit for convenience
    double complex, parameter :: I = (0D0, 1D0)

    ! Pi and its square
    double precision, parameter :: PI = 4.D0*atan(1.D0)
    double precision, parameter :: PI2 = PI**2

    ! "Photon mass" for the IR divergence regulation (the final amplitude is independent of IRLambda)
    double precision :: IRLambda

    ! Gauge-fixing parameters
    double precision :: GaugeXiW = 1.0D0
    double precision :: GaugeXiZ = 1.0D0
    double precision :: GaugeXiA = 1.0D0

	! vev for the tree-level mass calculation
	double precision :: vevCalc

	! Fermi constant for consistency cross-checks
	double precision :: GFinput

	! UV scale and regularization scale
	double precision :: UVDelta = 0D0
	double precision :: InputScale, OutputScale, EvalScale
	character(len=50) :: OutputScaleReadIn

	! 2HDM type, input parameter type (1: masses and alpha are given; 2: lambda1 to lambda5 are given), renormalization scheme and reference scheme
	integer :: TypeOf2HDM
	integer :: parameterType
	integer :: RenormScheme
	integer :: RefScheme

    ! Standard Model parameters; the values are stored in the input file in the ./Parameters/input folder (1101.0593 [hep-ph], 1503.07589 [hep-ex],  Chin. Phys. C38 (2014) 090001)
	double precision :: alphaAtMZ
	double precision :: MW
	double precision :: MZ
	double precision :: ME
	double precision :: MM
	double precision :: ML
	double precision :: MU
	double precision :: MC
	double precision :: MT
	double precision :: MD
	double precision :: MS
	double precision :: MB
	double precision :: EL
	double precision :: SW
	double precision :: CW
	double precision :: CKM11
	double precision :: CKM12
	double precision :: CKM13
	double precision :: CKM21
	double precision :: CKM22
	double precision :: CKM23
	double precision :: CKM31
	double precision :: CKM32
	double precision :: CKM33
	double precision :: CKMC11
	double precision :: CKMC12
	double precision :: CKMC13
	double precision :: CKMC21
	double precision :: CKMC22
	double precision :: CKMC23
	double precision :: CKMC31
	double precision :: CKMC32
	double precision :: CKMC33

    ! Squared Standard Model parameters
	double precision :: MW2
	double precision :: MZ2
	double precision :: ME2
	double precision :: MM2
	double precision :: ML2
	double precision :: MU2
	double precision :: MC2
	double precision :: MT2
	double precision :: MD2
	double precision :: MS2
	double precision :: MB2
	double precision :: EL2
	double precision :: SW2
	double precision :: CW2

    ! 2HDM-specific parameters (these are set in the folder ./Parameters/input by reading the respective input files)
    double precision :: MH1
    double precision :: MH2
    double precision :: MH3
	double precision :: MA0
	double precision :: MHp
	double precision :: alpha1
	double precision :: alpha2
	double precision :: alpha3
	double precision :: CA1
	double precision :: CA2
	double precision :: CA3
	double precision :: SA1
	double precision :: SA2
	double precision :: SA3
	double precision :: beta
	double precision :: CB
	double precision :: SB
	double precision :: TB
    double precision :: YukS1Lep1
	double precision :: YukS1Lep2
	double precision :: YukS1Lep3
    double precision :: YukS2Lep1
	double precision :: YukS2Lep2
    double precision :: YukS3Lep1
	double precision :: YukS3Lep2
    double precision :: YukS1Quark1
	double precision :: YukS1Quark2
	double precision :: YukS1Quark3
    double precision :: YukS2Quark1
	double precision :: YukS2Quark2
    double precision :: YukS3Quark1
	double precision :: YukS3Quark2
	double precision :: m12squared
	double precision :: vS
	double precision :: hdecayLam1
	double precision :: hdecayLam2
	double precision :: hdecayLam3
	double precision :: hdecayLam4
	double precision :: hdecayLam5

    ! Squared 2HDM-specific parameters
    double precision :: MH12
	double precision :: MH22
	double precision :: MH32
	double precision :: MA02
	double precision :: MHp2
    double precision :: CA12
    double precision :: CA22
    double precision :: CA32
	double precision :: SA12
	double precision :: SA22
	double precision :: SA32
	double precision :: TB2
	double precision :: SB2
	double precision :: CB2

	! Scalar interaction parameters
	double precision :: RR11, RR12, RR13, RR21, RR22, RR23, RR31, RR32, RR33
    double precision :: Lam1, Lam2, Lam3, Lam4, Lam5, Lam6, Lam7, Lam8
	double precision :: CS1S1S1f111, CS1S1S1f112, CS1S1S1f113, CS1S1S1f121, CS1S1S1f122, CS1S1S1f123, CS1S1S1f131, CS1S1S1f132
	double precision :: CS1S1S1f133, CS1S1S1f211, CS1S1S1f212, CS1S1S1f213, CS1S1S1f221, CS1S1S1f222, CS1S1S1f223, CS1S1S1f231
	double precision :: CS1S1S1f232, CS1S1S1f233, CS1S1S1f311, CS1S1S1f312, CS1S1S1f313, CS1S1S1f321, CS1S1S1f322, CS1S1S1f323
	double precision :: CS1S1S1f331, CS1S1S1f332, CS1S1S1f333, CS2S2S1f111, CS2S2S1f112, CS2S2S1f113, CS2S2S1f121, CS2S2S1f122
	double precision :: CS2S2S1f123, CS2S2S1f211, CS2S2S1f212, CS2S2S1f213, CS2S2S1f221, CS2S2S1f222, CS2S2S1f223, CS1S3S3f111
	double precision :: CS1S3S3f112, CS1S3S3f121, CS1S3S3f122, CS1S3S3f211, CS1S3S3f212, CS1S3S3f221, CS1S3S3f222, CS1S3S3f311
	double precision :: CS1S3S3f312, CS1S3S3f321, CS1S3S3f322, CS1S1S1S1f1111, CS1S1S1S1f1112, CS1S1S1S1f1113, CS1S1S1S1f1121
	double precision :: CS1S1S1S1f1122, CS1S1S1S1f1123, CS1S1S1S1f1131, CS1S1S1S1f1132, CS1S1S1S1f1133, CS1S1S1S1f1211
	double precision :: CS1S1S1S1f1212, CS1S1S1S1f1213, CS1S1S1S1f1221, CS1S1S1S1f1222, CS1S1S1S1f1223, CS1S1S1S1f1231
	double precision :: CS1S1S1S1f1232, CS1S1S1S1f1233, CS1S1S1S1f1311, CS1S1S1S1f1312, CS1S1S1S1f1313, CS1S1S1S1f1321
	double precision :: CS1S1S1S1f1322, CS1S1S1S1f1323, CS1S1S1S1f1331, CS1S1S1S1f1332, CS1S1S1S1f1333, CS1S1S1S1f2111
	double precision :: CS1S1S1S1f2112, CS1S1S1S1f2113, CS1S1S1S1f2121, CS1S1S1S1f2122, CS1S1S1S1f2123, CS1S1S1S1f2131
	double precision :: CS1S1S1S1f2132, CS1S1S1S1f2133, CS1S1S1S1f2211, CS1S1S1S1f2212, CS1S1S1S1f2213, CS1S1S1S1f2221
	double precision :: CS1S1S1S1f2222, CS1S1S1S1f2223, CS1S1S1S1f2231, CS1S1S1S1f2232, CS1S1S1S1f2233, CS1S1S1S1f2311
	double precision :: CS1S1S1S1f2312, CS1S1S1S1f2313, CS1S1S1S1f2321, CS1S1S1S1f2322, CS1S1S1S1f2323, CS1S1S1S1f2331
	double precision :: CS1S1S1S1f2332, CS1S1S1S1f2333, CS1S1S1S1f3111, CS1S1S1S1f3112, CS1S1S1S1f3113, CS1S1S1S1f3121
	double precision :: CS1S1S1S1f3122, CS1S1S1S1f3123, CS1S1S1S1f3131, CS1S1S1S1f3132, CS1S1S1S1f3133, CS1S1S1S1f3211
	double precision :: CS1S1S1S1f3212, CS1S1S1S1f3213, CS1S1S1S1f3221, CS1S1S1S1f3222, CS1S1S1S1f3223, CS1S1S1S1f3231
	double precision :: CS1S1S1S1f3232, CS1S1S1S1f3233, CS1S1S1S1f3311, CS1S1S1S1f3312, CS1S1S1S1f3313, CS1S1S1S1f3321
	double precision :: CS1S1S1S1f3322, CS1S1S1S1f3323, CS1S1S1S1f3331, CS1S1S1S1f3332, CS1S1S1S1f3333, CS2S2S2S2f1111
	double precision :: CS2S2S2S2f1112, CS2S2S2S2f1121, CS2S2S2S2f1122, CS2S2S2S2f1211, CS2S2S2S2f1212, CS2S2S2S2f1221
	double precision :: CS2S2S2S2f1222, CS2S2S2S2f2111, CS2S2S2S2f2112, CS2S2S2S2f2121, CS2S2S2S2f2122, CS2S2S2S2f2211
	double precision :: CS2S2S2S2f2212, CS2S2S2S2f2221, CS2S2S2S2f2222, CS3S3S3S3f1111, CS3S3S3S3f1112, CS3S3S3S3f1121
	double precision :: CS3S3S3S3f1122, CS3S3S3S3f1211, CS3S3S3S3f1212, CS3S3S3S3f1221, CS3S3S3S3f1222, CS3S3S3S3f2111
	double precision :: CS3S3S3S3f2112, CS3S3S3S3f2121, CS3S3S3S3f2122, CS3S3S3S3f2211, CS3S3S3S3f2212, CS3S3S3S3f2221
	double precision :: CS3S3S3S3f2222, CS2S2S1S1f1111, CS2S2S1S1f1112, CS2S2S1S1f1113, CS2S2S1S1f1121, CS2S2S1S1f1122
	double precision :: CS2S2S1S1f1123, CS2S2S1S1f1131, CS2S2S1S1f1132, CS2S2S1S1f1133, CS2S2S1S1f1211, CS2S2S1S1f1212
	double precision :: CS2S2S1S1f1213, CS2S2S1S1f1221, CS2S2S1S1f1222, CS2S2S1S1f1223, CS2S2S1S1f1231, CS2S2S1S1f1232
	double precision :: CS2S2S1S1f1233, CS2S2S1S1f2111, CS2S2S1S1f2112, CS2S2S1S1f2113, CS2S2S1S1f2121, CS2S2S1S1f2122
	double precision :: CS2S2S1S1f2123, CS2S2S1S1f2131, CS2S2S1S1f2132, CS2S2S1S1f2133, CS2S2S1S1f2211, CS2S2S1S1f2212
	double precision :: CS2S2S1S1f2213, CS2S2S1S1f2221, CS2S2S1S1f2222, CS2S2S1S1f2223, CS2S2S1S1f2231, CS2S2S1S1f2232
	double precision :: CS2S2S1S1f2233, CS2S2S3S3f1111, CS2S2S3S3f1112, CS2S2S3S3f1121, CS2S2S3S3f1122, CS2S2S3S3f1211
	double precision :: CS2S2S3S3f1212, CS2S2S3S3f1221, CS2S2S3S3f1222, CS2S2S3S3f2111, CS2S2S3S3f2112, CS2S2S3S3f2121
	double precision :: CS2S2S3S3f2122, CS2S2S3S3f2211, CS2S2S3S3f2212, CS2S2S3S3f2221, CS2S2S3S3f2222, CS1S1S3S3f1111
	double precision :: CS1S1S3S3f1112, CS1S1S3S3f1121, CS1S1S3S3f1122, CS1S1S3S3f1211, CS1S1S3S3f1212, CS1S1S3S3f1221
	double precision :: CS1S1S3S3f1222, CS1S1S3S3f1311, CS1S1S3S3f1312, CS1S1S3S3f1321, CS1S1S3S3f1322, CS1S1S3S3f2111
	double precision :: CS1S1S3S3f2112, CS1S1S3S3f2121, CS1S1S3S3f2122, CS1S1S3S3f2211, CS1S1S3S3f2212, CS1S1S3S3f2221
	double precision :: CS1S1S3S3f2222, CS1S1S3S3f2311, CS1S1S3S3f2312, CS1S1S3S3f2321, CS1S1S3S3f2322, CS1S1S3S3f3111
	double precision :: CS1S1S3S3f3112, CS1S1S3S3f3121, CS1S1S3S3f3122, CS1S1S3S3f3211, CS1S1S3S3f3212, CS1S1S3S3f3221
	double precision :: CS1S1S3S3f3222, CS1S1S3S3f3311, CS1S1S3S3f3312, CS1S1S3S3f3321, CS1S1S3S3f3322 

contains
    ! This is the three-point function with C0(0,p,p,0,0,m), appearing in calculations for xi != 1, which diverges individually,
	! but cancels overall in the full amplitude; hence, we set it to zero to avoid the artificial divergence
    double complex function C0Mine(a,b,c,d,e,f)
        implicit none
        double precision, intent(in) :: a,b,c,d,e,f
        C0Mine = (0D0,0D0)
    end function C0Mine

	! This is the four-point function with zero arguments, appearing in calculations for xi != 1, which diverges individually,
	! but cancels overall in the full amplitude; hence, we set it to zero to avoid the artificial divergence
	double complex function D0Mine(a,b,c,d,e,f,g,h,j,k)
        implicit none
        double precision, intent(in) :: a,b,c,d,e,f,g,h,j,k
        D0Mine = (0D0,0D0)
    end function D0Mine

    ! These are the derivatives of the three-point function with C0(0,p,p,0,0,m) w.r.t. p^2, appearing in calculations for xi != 1, 
	! which diverge individually, but which cancel overall in the full amplitude; hence, we set them to zero to avoid these artificial divergence
    double complex function DC01Mine(a,b,c,d,e,f)
        implicit none
        double precision, intent(in) :: a,b,c,d,e,f
        DC01Mine = (0D0,0D0)
    end function DC01Mine
    double complex function DC02Mine(a,b,c,d,e,f)
        implicit none
        double precision, intent(in) :: a,b,c,d,e,f
        DC02Mine = (0D0,0D0)
    end function DC02Mine

    double complex function DiracGamma(a)
        implicit none
        double precision, intent(in) :: a
        DiracGamma = (0D0, 0D0)
    end function DiracGamma

end module constants
