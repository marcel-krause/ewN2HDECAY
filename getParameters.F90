subroutine getParameters(OStype, param2HDMonly)
    use constants
    implicit none
    character(50) dump, dump2, dump3
    ! double precision dump2
    ! double precision MHHTemp, MA0Temp, MHpTemp, alpha1Temp, alpha2Temp, alpha3Temp, TBTemp, m12squaredTemp
    ! integer TypeOf2HDMTemp
    integer statOpen, statRead
    integer :: currentLine = 1
    logical fileExists
    character isContinue
    integer, intent(in) :: OStype, param2HDMonly
    character(2) :: OSpathSeparator
    character(300), parameter :: pathToInputFiles = 'N2HDECAY'
    integer m
    double precision M11SqPot, M22SqPot, M12SqPot, tmpMass

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

    ! Check if we want all parameters or only the 2HDM ones
    if (param2HDMonly .EQ. 0) then
        ! Get the correct OS path separator
        if (OStype .eq. 0) then
            OSpathSeparator = '\\'
        else 
            OSpathSeparator = '/ '
        end if

        ! Check if the input file exists
        inquire(file=trim(trim(pathToInputFiles)//trim(OSpathSeparator))//'n2hdecay.in', exist=fileExists)
        if (.not. fileExists) then
            do
                print *, "ERROR: Could not find input file!"
                print *, ">>> Do you want to continue with the evaluation of the program? [y/n]"
                read (*,*) isContinue
                if (isContinue == 'n') then
                    print *, "Termination requested by user. ewN2HDECAY will be terminated now."
                    stop
                else if (isContinue == 'y') then
                    exit
                else
                    print *, "Invalid character. Enter y or n."
                end if
            end do
        end if

        ! Read the input file
        open(unit=42, file=trim(trim(pathToInputFiles)//trim(OSpathSeparator))//'n2hdecay.in', iostat=statOpen)
            if (statOpen == 0) then

                ! Skip the first five lines
                do m = 1, 5, 1
                    read(42, *)
                end do

                ! Read if the electroweak corrections should be calculated or not
                read(42, *) dump, dump2, omitELCorr

                ! In the input file, the SM block does begin at line 19
                do m = 1, 13, 1
                    read(42, *)
                end do

                ! Read the s quark mass
                read(42, *) dump, dump2, MS

                ! Skip two lines
                read(42, *)
                read(42, *)

                ! Read the c quark mass
                read(42, *) dump, dump2, MC

                ! Read the b quark mass
                read(42, *) dump, dump2, MB

                ! Read the t quark mass
                read(42, *) dump, dump2, MT

                ! Read the tau lepton mass
                read(42, *) dump, dump2, ML

                ! Read the muon mass
                read(42, *) dump, dump2, MM

                ! Skip a line
                read(42, *)
                
                ! Read the inverse fine-structure constant at the Z mass
                read(42, *) dump, dump2, alphaAtMZ

                ! Read the Fermi constant for consistency checks
                read(42, *) dump, dump2, GFinput
                
                ! Skip three lines
                read(42, *)
                read(42, *)
                read(42, *)

                ! Read the Z boson mass
                read(42, *) dump, dump2, MZ

                ! Read the W boson mass
                read(42, *) dump, dump2, MW

                ! Read the CKM elements
                read(42, *) dump, dump2, CKM33
                read(42, *) dump, dump2, CKM32
                read(42, *) dump, dump2, CKM31
                read(42, *) dump, dump2, CKM23
                read(42, *) dump, dump2, CKM22
                read(42, *) dump, dump2, CKM21
                read(42, *) dump, dump2, CKM13
                read(42, *) dump, dump2, CKM12
                read(42, *) dump, dump2, CKM11

                ! Skip 14 lines (after that, the 2HDM input block begins)
                do m = 1, 14, 1
                    read(42, *)
                end do

                ! Read the parameter type (1: masses and alpha are given; 2: lambda1 to lambda5 are given)
                read(42, *) dump, dump2, parameterType

                ! Read the 2HDM type
                read(42, *) dump, dump2, TypeOf2HDM

                ! Read the renormalization scheme
                read(42, *) dump, dump2, RenormScheme

                ! Read the reference scheme
                read(42, *) dump, dump2, RefScheme

                ! Skip a line
                read(42, *)

                ! Read Tan(beta)
                read(42, *) dump, TB
                
                ! Read m12^2
                read(42, *) dump, dump2, m12squared

                ! Read the input and output scales
                read(42, *) dump, dump2, InputScale
                read(42, *) dump, dump2, OutputScaleReadIn

                ! Skip 4 lines
                do m = 1, 4, 1
                    read(42, *)
                end do

                ! Read MA0 (relevant if parameter type = 1)
                read(42, *) dump, dump2, MA0

                ! Read MHp (relevant if parameter type = 1)
                read(42, *) dump, dump2, MHp

                ! Skip 8 line
                do m = 1, 8, 1
                    read(42, *)
                end do

                ! Read MH1 (relevant if parameter type = 1)
                read(42, *) dump, dump2, MH1

                ! Read MH2 (relevant if parameter type = 1)
                read(42, *) dump, dump2, MH2

                ! Read MH3 (relevant if parameter type = 1)
                read(42, *) dump, dump2, MH3

                ! Read alpha1 (relevant if parameter type = 1)
                read(42, *) dump, dump2, alpha1

                ! Read alpha2 (relevant if parameter type = 1)
                read(42, *) dump, dump2, alpha2

                ! Read alpha3 (relevant if parameter type = 1)
                read(42, *) dump, dump2, alpha3

                ! Read vS (relevant if parameter type = 1)
                read(42, *) dump, dump2, vS

                ! ! Read lambda1 (relevant if parameter type = 2)
                ! read(42, *) dump, dump2, hdecayLam1

                ! ! Read lambda2 (relevant if parameter type = 2)
                ! read(42, *) dump, dump2, hdecayLam2

                ! ! Read lambda3 (relevant if parameter type = 2)
                ! read(42, *) dump, dump2, hdecayLam3

                ! ! Read lambda4 (relevant if parameter type = 2)
                ! read(42, *) dump, dump2, hdecayLam4

                ! ! Read lambda5 (relevant if parameter type = 2)
                ! read(42, *) dump, dump2, hdecayLam5

            else
                do
                    print *, "ERROR: Generic error when reading input file!"
                    print *, ">>> Do you want to continue with the evaluation of the program? [y/n]"
                    read (*,*) isContinue
                    if (isContinue == 'n') then
                        print *, "Termination requested by user. ewN2HDECAY will be terminated now."
                        stop
                    else if (isContinue == 'y') then
                        exit
                    else
                        print *, "Invalid character. Enter y or n."
                    end if
                end do
            end if
        close(42)
        
        ! The light quark masses MU and MD and the electron mass ME are set to zero in HDECAY; we use very small values here to avoid numerical instability
        ! IMPORTANT: especially the light quarks u and d should not have exactly the same small mass, because in this case, terms like 1/(MU2 - MD2) become numerically unstable
        MU = 1.0e-5
        MD = 1.5e-5
        ME = 1.8e-5

        ! Calculate the sine and cosine of the Weinberg angle through MW and MZ 
        CW = MW/MZ 
        SW = DSQRT(1D0 - CW**2)

        ! Take the inverse of the read-in fine-structure constant (in the input file, we provide 1/alphaAtMZ, not alphaAtMZ!)
        ! alphaAtMZ = 1D0/alphaAtMZ

        ! Calculate the electromagnetic coupling constant out of the fundamental constants alphaAtMZ, MZ, MW 
        ! (This set is our convention; in HDECAY, we use GF instead of alphaAtMZ. The difference between the two is minimal, 
        ! and we can use the tree-level formula EL = DSQRT(8D0*GFermi*SW**2*MW**2/DSQRT(2D0)) to switch from our scheme to the one used in HDECAY)
        EL = DSQRT(4D0*PI*alphaAtMZ)

        ! Set the complex values of the CKM matrix to the non-complex ones (we do not consider CP violation)
        CKMC11 = CKM11
        CKMC12 = CKM12
        CKMC13 = CKM13
        CKMC21 = CKM21
        CKMC22 = CKM22
        CKMC23 = CKM23
        CKMC31 = CKM31
        CKMC32 = CKM32
        CKMC33 = CKM33

        ! Calculate the current beta with the given tan(beta)
        beta = datan(TB)
        CB = dcos(beta)
        SB = dsin(beta)

        ! Calculate the vev for the conversion of the masses
        ! vevCalc = 1D0/DSQRT(DSQRT(2D0)*GFinput)
        vevCalc = 2D0*MW*SW/EL

        ! PARAMETER TYPE 2 NOT IMPLEMENTED AT THIS POINT
        ! If parameter type 1 is chosen, then alpha and the scalar masses are the relevant parameters and the potential parameters lamdba1 to lambda5 are calculated
        ! If parameter type 2 is chosen, then lambda1 to lambda5 are the relevant parameters and alpha and the scalar masses are calculated through these lambdas
        ! The conversion from alpha and the masses to the lambdas and vice versa is described in hep-ph/0408364
        if (parameterType .eq. 1) then
            ! CA = dcos(alpha)
            ! SA = dsin(alpha)
            ! S2A = dsin(2D0*alpha)
            ! hdecayLam1 = 1D0/vevCalc**2/CB**2*( -SB**2*(2D0*m12squared/S2B) + SA**2*Mh0**2 + CA**2*MHH**2 )
            ! hdecayLam2 = 1D0/vevCalc**2/SB**2*( -CB**2*(2D0*m12squared/S2B) + CA**2*Mh0**2 + SA**2*MHH**2 )
            ! hdecayLam3 = 1D0/vevCalc**2*( -(2D0*m12squared/S2B) + 2D0*MHp**2 + S2A/S2B*(MHH**2 - Mh0**2) )
            ! hdecayLam4 = 1D0/vevCalc**2*( (2D0*m12squared/S2B) + MA0**2 - 2D0*MHp**2 )
            ! hdecayLam5 = 1D0/vevCalc**2*( (2D0*m12squared/S2B) - MA0**2 )
        else if (parameterType .eq. 2) then
            ! ! CP-even mass matrix elements
            ! M11SqPot = (hdecayLam1*CB**4 + hdecayLam2*SB**4 + 2D0*(hdecayLam3 + hdecayLam4 + hdecayLam5)*CB**2*SB**2)*vevCalc**2
            ! M12SqPot = ( -hdecayLam1*CB**2 + hdecayLam2*SB**2 + (hdecayLam3 + hdecayLam4 + hdecayLam5)*(CB**2 - SB**2) )*CB*SB*&
            !             &vevCalc**2
            ! M22SqPot = (2D0*m12squared/S2B) + 1D0/8D0*( hdecayLam1 + hdecayLam2 - 2D0*(hdecayLam3 + hdecayLam4 + hdecayLam5) )*&
            !             &(1D0 - dcos(4D0*beta))*vevCalc**2

            ! ! Calculate the mixing angle alpha
            ! alpha = 1D0/2D0*datan( 2D0*M12SqPot/(M11SqPot - M22SqPot) ) + beta

            ! ! Calculate the CP-even masses
            ! Mh0 = DSQRT( (dsin(alpha-beta))**2*M11SqPot - dsin(2D0*(alpha-beta))*M12SqPot + (dcos(alpha-beta))**2*M22SqPot )
            ! MHH = DSQRT( (dcos(alpha-beta))**2*M11SqPot + dsin(2D0*(alpha-beta))*M12SqPot + (dsin(alpha-beta))**2*M22SqPot )

            ! ! If the mass hierarchy should be inverted, we switch the mass conventions such that h0 is still the smaller mass
            ! if(Mh0 .gt. MHH) then
            !     alpha = alpha - Pi/2D0 
            !     tmpMass = MHH
            !     MHH = Mh0 
            !     Mh0 = tmpMass
            ! endif

            ! ! Calculate the charged and CP-odd Higgs masses 
            ! MHp = DSQRT( (2D0*m12squared/S2B) - 1D0/2D0*(hdecayLam4 + hdecayLam5)*vevCalc**2 )
            ! MA0 = DSQRT( (2D0*m12squared/S2B) - hdecayLam5*vevCalc**2 )
            write(*,*) "ERROR: parameter type 2 is not supported!"
        else
            write(*,*) "Error: unknown parameter type. Please choose the integers 1 or 2 for the parameter type in the input file!"
        end if

        ! Generate the square of the input parameters
        MH12 = MH1**2
        MW2 = MW**2
        MZ2 = MZ**2
        ME2 = ME**2
        MM2 = MM**2
        ML2 = ML**2
        MU2 = MU**2
        MC2 = MC**2
        MT2 = MT**2
        MD2 = MD**2
        MS2 = MS**2
        MB2 = MB**2
        EL2 = EL**2
        SW2 = SW**2
        CW2 = CW**2

        ! Calculate the rest of the 2HDM parameters
        CA1 = dcos(alpha1)
        CA2 = dcos(alpha2)
        CA3 = dcos(alpha3)
        SA1 = dsin(alpha1)
        SA2 = dsin(alpha2)
        SA3 = dsin(alpha3)
        ! Lambda5 = EL2*m12squared/(2D0*SW2*MW2*SB*CB)
        if (TypeOf2HDM == 1) then
            YukS1Lep1 = CA2*SA1/SB
            YukS1Lep2 = ( CA1*CA3 - SA1*SA2*SA3 )/SB
            YukS1Lep3 = -( CA1*SA3 + CA3*SA1*SA2 )/SB
            YukS2Lep1 = 1D0
            YukS2Lep2 = CB/SB
            YukS3Lep1 = 1D0
            YukS3Lep2 = CB/SB
            YukS1Quark1 = CA2*SA1/SB
            YukS1Quark2 = ( CA1*CA3 - SA1*SA2*SA3 )/SB
            YukS1Quark3 = -( CA1*SA3 + CA3*SA1*SA2 )/SB
            YukS2Quark1 = 1D0
            YukS2Quark2 = CB/SB
            YukS3Quark1 = 1D0
            YukS3Quark2 = CB/SB
        else if (TypeOf2HDM == 2) then
            YukS1Lep1 = CA1*CA2/CB
            YukS1Lep2 = -( CA3*SA1 + CA1*SA2*SA3 )/CB
            YukS1Lep3 = ( SA1*SA3 - CA1*CA3*SA2 )/CB
            YukS2Lep1 = 1D0
            YukS2Lep2 = -SB/CB
            YukS3Lep1 = 1D0
            YukS3Lep2 = -SB/CB
            YukS1Quark1 = CA1*CA2/CB
            YukS1Quark2 = -( CA3*SA1 + CA1*SA2*SA3 )/CB
            YukS1Quark3 = ( SA1*SA3 - CA1*CA3*SA2 )/CB
            YukS2Quark1 = 1D0
            YukS2Quark2 = -SB/CB
            YukS3Quark1 = 1D0
            YukS3Quark2 = -SB/CB
        else if (TypeOf2HDM == 3) then
            YukS1Lep1 = CA1*CA2/CB
            YukS1Lep2 = -( CA3*SA1 + CA1*SA2*SA3 )/CB
            YukS1Lep3 = ( SA1*SA3 - CA1*CA3*SA2 )/CB
            YukS2Lep1 = 1D0
            YukS2Lep2 = -SB/CB
            YukS3Lep1 = 1D0
            YukS3Lep2 = -SB/CB
            YukS1Quark1 = CA2*SA1/SB
            YukS1Quark2 = ( CA1*CA3 - SA1*SA2*SA3 )/SB
            YukS1Quark3 = -( CA1*SA3 + CA3*SA1*SA2 )/SB
            YukS2Quark1 = 1D0
            YukS2Quark2 = CB/SB
            YukS3Quark1 = 1D0
            YukS3Quark2 = CB/SB
        else
            YukS1Lep1 = CA2*SA1/SB
            YukS1Lep2 = ( CA1*CA3 - SA1*SA2*SA3 )/SB
            YukS1Lep3 = -( CA1*SA3 + CA3*SA1*SA2 )/SB
            YukS2Lep1 = 1D0
            YukS2Lep2 = CB/SB
            YukS3Lep1 = 1D0
            YukS3Lep2 = CB/SB
            YukS1Quark1 = CA1*CA2/CB
            YukS1Quark2 = -( CA3*SA1 + CA1*SA2*SA3 )/CB
            YukS1Quark3 = ( SA1*SA3 - CA1*CA3*SA2 )/CB
            YukS2Quark1 = 1D0
            YukS2Quark2 = -SB/CB
            YukS3Quark1 = 1D0
            YukS3Quark2 = -SB/CB
        end if

        ! Generate the square of the additional input parameters
        MH22 = MH2**2
        MH32 = MH3**2
        MA02 = MA0**2
        MHp2 = MHp**2
        CA12 = CA1**2
        CA22 = CA2**2
        CA32 = CA3**2
        SA12 = SA1**2
        SA22 = SA2**2
        SA32 = SA3**2
        TB2 = TB**2
        SB2 = SB**2
        CB2 = CB**2

        ! Set the scalar couplings
RR11 = CA1*CA2
RR12 = CA2*SA1
RR13 = SA2
RR21 = -1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3
RR22 = CA1*CA3 - 1.D0*SA1*SA2*SA3
RR23 = CA2*SA3
RR31 = -1.D0*CA1*CA3*SA2 + SA1*SA3
RR32 = -1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3
RR33 = CA2*CA3

Lam1 = (0.25D0*EL2*((-1.D0*m12squared*SB)/CB + MH12*DBLE(RR11**INT(2.D0)) + MH22*DBLE(RR21**INT(2.D0)) + MH32*DBLE(RR31**INT(2.D0&
  &))))/(CB2*MW2*SW2)
Lam2 = (0.25D0*EL2*((-1.D0*CB*m12squared)/SB + MH12*DBLE(RR12**INT(2.D0)) + MH22*DBLE(RR22**INT(2.D0)) + MH32*DBLE(RR32**INT(2.D0&
  &))))/(MW2*SB2*SW2)
Lam3 = (0.25D0*EL2*(2.D0*MHp2 - (1.D0*m12squared)/(CB*SB) + (MH12*RR11*RR12 + MH22*RR21*RR22 + MH32*RR31*RR32)/(CB*SB)))/(MW2*SW2&
  &)
Lam4 = (0.25D0*EL2*(MA02 - 2.D0*MHp2 + m12squared/(CB*SB)))/(MW2*SW2)
Lam5 = (0.25D0*EL2*(-1.D0*MA02 + m12squared/(CB*SB)))/(MW2*SW2)
Lam6 = (MH12*DBLE(RR13**INT(2.D0)) + MH22*DBLE(RR23**INT(2.D0)) + MH32*DBLE(RR33**INT(2.D0)))*DBLE(vS**INT(-2.D0))
Lam7 = (0.5D0*EL*(MH12*RR11*RR13 + MH22*RR21*RR23 + MH32*RR31*RR33))/(CB*MW*SW*vS)
Lam8 = (0.5D0*EL*(MH12*RR12*RR13 + MH22*RR22*RR23 + MH32*RR32*RR33))/(MW*SB*SW*vS)

CS1S1S1f111 = -1.D0*RR13*(Lam7*RR11*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR12*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR11*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f112 = -1.D0*RR13*(Lam7*RR11*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR12*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR11*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f113 = -1.D0*RR13*(Lam7*RR11*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR12*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR11*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f121 = -1.D0*RR13*(Lam7*RR21*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR22*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR21*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f122 = -1.D0*RR13*(Lam7*RR21*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR22*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR21*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f123 = -1.D0*RR13*(Lam7*RR21*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR22*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR21*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f131 = -1.D0*RR13*(Lam7*RR31*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR32*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR31*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f132 = -1.D0*RR13*(Lam7*RR31*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR32*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR31*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f133 = -1.D0*RR13*(Lam7*RR31*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR32*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR31*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f211 = -1.D0*RR23*(Lam7*RR11*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR12*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR11*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f212 = -1.D0*RR23*(Lam7*RR11*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR12*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR11*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f213 = -1.D0*RR23*(Lam7*RR11*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR12*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR11*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f221 = -1.D0*RR23*(Lam7*RR21*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR22*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR21*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f222 = -1.D0*RR23*(Lam7*RR21*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR22*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR21*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f223 = -1.D0*RR23*(Lam7*RR21*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR22*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR21*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f231 = -1.D0*RR23*(Lam7*RR31*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR32*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR31*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f232 = -1.D0*RR23*(Lam7*RR31*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR32*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR31*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f233 = -1.D0*RR23*(Lam7*RR31*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR32*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR31*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f311 = -1.D0*RR33*(Lam7*RR11*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR12*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR11*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f312 = -1.D0*RR33*(Lam7*RR11*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR12*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR11*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f313 = -1.D0*RR33*(Lam7*RR11*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR12*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR11*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f321 = -1.D0*RR33*(Lam7*RR21*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR22*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR21*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f322 = -1.D0*RR33*(Lam7*RR21*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR22*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR21*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f323 = -1.D0*RR33*(Lam7*RR21*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR22*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR21*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f331 = -1.D0*RR33*(Lam7*RR31*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR32*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR31*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f332 = -1.D0*RR33*(Lam7*RR31*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR32*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR31*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f333 = -1.D0*RR33*(Lam7*RR31*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR32*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR31*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))

CS2S2S1f111 = -1.D0*CB*(Lam5*SB*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + CB*((2.D0*CB*Lam1*MW*RR11*SW)/EL + (2.D0*(L&
  &am3 + Lam4 - 1.D0*Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*SB*(CB*Lam5*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/&
  &EL) + SB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR11*SW)/EL + (2.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS2S2S1f112 = -1.D0*CB*(Lam5*SB*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + CB*((2.D0*CB*Lam1*MW*RR21*SW)/EL + (2.D0*(L&
  &am3 + Lam4 - 1.D0*Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*SB*(CB*Lam5*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/&
  &EL) + SB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR21*SW)/EL + (2.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS2S2S1f113 = -1.D0*CB*(Lam5*SB*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + CB*((2.D0*CB*Lam1*MW*RR31*SW)/EL + (2.D0*(L&
  &am3 + Lam4 - 1.D0*Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*SB*(CB*Lam5*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/&
  &EL) + SB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR31*SW)/EL + (2.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS2S2S1f121 = -1.D0*CB*(CB*Lam5*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) - 1.D0*SB*((2.D0*CB*Lam1*MW*RR11*SW)/EL + (2.&
  &D0*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*SB*(-1.D0*Lam5*SB*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*R&
  &R11*SB*SW)/EL) + CB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR11*SW)/EL + (2.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS2S2S1f122 = -1.D0*CB*(CB*Lam5*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) - 1.D0*SB*((2.D0*CB*Lam1*MW*RR21*SW)/EL + (2.&
  &D0*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*SB*(-1.D0*Lam5*SB*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*R&
  &R21*SB*SW)/EL) + CB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR21*SW)/EL + (2.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS2S2S1f123 = -1.D0*CB*(CB*Lam5*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) - 1.D0*SB*((2.D0*CB*Lam1*MW*RR31*SW)/EL + (2.&
  &D0*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*SB*(-1.D0*Lam5*SB*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*R&
  &R31*SB*SW)/EL) + CB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR31*SW)/EL + (2.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS2S2S1f211 = SB*(Lam5*SB*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + CB*((2.D0*CB*Lam1*MW*RR11*SW)/EL + (2.D0*(Lam3 + &
  &Lam4 - 1.D0*Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*CB*(CB*Lam5*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + &
  &SB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR11*SW)/EL + (2.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS2S2S1f212 = SB*(Lam5*SB*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + CB*((2.D0*CB*Lam1*MW*RR21*SW)/EL + (2.D0*(Lam3 + &
  &Lam4 - 1.D0*Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*CB*(CB*Lam5*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + &
  &SB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR21*SW)/EL + (2.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS2S2S1f213 = SB*(Lam5*SB*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + CB*((2.D0*CB*Lam1*MW*RR31*SW)/EL + (2.D0*(Lam3 + &
  &Lam4 - 1.D0*Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*CB*(CB*Lam5*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + &
  &SB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR31*SW)/EL + (2.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS2S2S1f221 = SB*(CB*Lam5*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) - 1.D0*SB*((2.D0*CB*Lam1*MW*RR11*SW)/EL + (2.D0*(La&
  &m3 + Lam4 - 1.D0*Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*CB*(-1.D0*Lam5*SB*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB&
  &*SW)/EL) + CB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR11*SW)/EL + (2.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS2S2S1f222 = SB*(CB*Lam5*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) - 1.D0*SB*((2.D0*CB*Lam1*MW*RR21*SW)/EL + (2.D0*(La&
  &m3 + Lam4 - 1.D0*Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*CB*(-1.D0*Lam5*SB*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB&
  &*SW)/EL) + CB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR21*SW)/EL + (2.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS2S2S1f223 = SB*(CB*Lam5*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) - 1.D0*SB*((2.D0*CB*Lam1*MW*RR31*SW)/EL + (2.D0*(La&
  &m3 + Lam4 - 1.D0*Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*CB*(-1.D0*Lam5*SB*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB&
  &*SW)/EL) + CB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR31*SW)/EL + (2.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))

CS1S3S3f111 = 0.5D0*(-1.D0*RR12*(CB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + SB*((2.D0*CB2*(Lam4 + La&
  &m5)*MW*SW)/EL + (4.D0*Lam2*MW*SB2*SW)/EL)) - 1.D0*RR11*(SB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) &
  &+ CB*((4.D0*CB2*Lam1*MW*SW)/EL + (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR13*(CB2*Lam7 + Lam8*SB2)*vs)
CS1S3S3f112 = 0.5D0*(-1.D0*RR12*(SB*((4.D0*CB*Lam2*MW*SB*SW)/EL - (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + CB*((2.D0*CB2*(Lam4 + La&
  &m5)*MW*SW)/EL - (4.D0*Lam3*MW*SB2*SW)/EL)) - 1.D0*RR11*(CB*((-4.D0*CB*Lam1*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL)&
  & + SB*((4.D0*CB2*Lam3*MW*SW)/EL - (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR13*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*vs)
CS1S3S3f121 = 0.5D0*(-1.D0*RR12*(-1.D0*SB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + CB*((2.D0*CB2*(Lam&
  &4 + Lam5)*MW*SW)/EL + (4.D0*Lam2*MW*SB2*SW)/EL)) - 1.D0*RR11*(CB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW&
  &)/EL) - 1.D0*SB*((4.D0*CB2*Lam1*MW*SW)/EL + (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR13*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*vs&
  &)
CS1S3S3f122 = 0.5D0*(-1.D0*RR12*(CB*((4.D0*CB*Lam2*MW*SB*SW)/EL - (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) - 1.D0*SB*((2.D0*CB2*(Lam4&
  & + Lam5)*MW*SW)/EL - (4.D0*Lam3*MW*SB2*SW)/EL)) - 1.D0*RR11*(-1.D0*SB*((-4.D0*CB*Lam1*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW&
  &*SB*SW)/EL) + CB*((4.D0*CB2*Lam3*MW*SW)/EL - (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR13*(CB2*Lam8 + Lam7*SB2)*vs)
CS1S3S3f211 = 0.5D0*(-1.D0*RR22*(CB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + SB*((2.D0*CB2*(Lam4 + La&
  &m5)*MW*SW)/EL + (4.D0*Lam2*MW*SB2*SW)/EL)) - 1.D0*RR21*(SB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) &
  &+ CB*((4.D0*CB2*Lam1*MW*SW)/EL + (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR23*(CB2*Lam7 + Lam8*SB2)*vs)
CS1S3S3f212 = 0.5D0*(-1.D0*RR22*(SB*((4.D0*CB*Lam2*MW*SB*SW)/EL - (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + CB*((2.D0*CB2*(Lam4 + La&
  &m5)*MW*SW)/EL - (4.D0*Lam3*MW*SB2*SW)/EL)) - 1.D0*RR21*(CB*((-4.D0*CB*Lam1*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL)&
  & + SB*((4.D0*CB2*Lam3*MW*SW)/EL - (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR23*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*vs)
CS1S3S3f221 = 0.5D0*(-1.D0*RR22*(-1.D0*SB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + CB*((2.D0*CB2*(Lam&
  &4 + Lam5)*MW*SW)/EL + (4.D0*Lam2*MW*SB2*SW)/EL)) - 1.D0*RR21*(CB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW&
  &)/EL) - 1.D0*SB*((4.D0*CB2*Lam1*MW*SW)/EL + (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR23*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*vs&
  &)
CS1S3S3f222 = 0.5D0*(-1.D0*RR22*(CB*((4.D0*CB*Lam2*MW*SB*SW)/EL - (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) - 1.D0*SB*((2.D0*CB2*(Lam4&
  & + Lam5)*MW*SW)/EL - (4.D0*Lam3*MW*SB2*SW)/EL)) - 1.D0*RR21*(-1.D0*SB*((-4.D0*CB*Lam1*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW&
  &*SB*SW)/EL) + CB*((4.D0*CB2*Lam3*MW*SW)/EL - (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR23*(CB2*Lam8 + Lam7*SB2)*vs)
CS1S3S3f311 = 0.5D0*(-1.D0*RR32*(CB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + SB*((2.D0*CB2*(Lam4 + La&
  &m5)*MW*SW)/EL + (4.D0*Lam2*MW*SB2*SW)/EL)) - 1.D0*RR31*(SB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) &
  &+ CB*((4.D0*CB2*Lam1*MW*SW)/EL + (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR33*(CB2*Lam7 + Lam8*SB2)*vs)
CS1S3S3f312 = 0.5D0*(-1.D0*RR32*(SB*((4.D0*CB*Lam2*MW*SB*SW)/EL - (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + CB*((2.D0*CB2*(Lam4 + La&
  &m5)*MW*SW)/EL - (4.D0*Lam3*MW*SB2*SW)/EL)) - 1.D0*RR31*(CB*((-4.D0*CB*Lam1*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL)&
  & + SB*((4.D0*CB2*Lam3*MW*SW)/EL - (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*vs)
CS1S3S3f321 = 0.5D0*(-1.D0*RR32*(-1.D0*SB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + CB*((2.D0*CB2*(Lam&
  &4 + Lam5)*MW*SW)/EL + (4.D0*Lam2*MW*SB2*SW)/EL)) - 1.D0*RR31*(CB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW&
  &)/EL) - 1.D0*SB*((4.D0*CB2*Lam1*MW*SW)/EL + (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*vs&
  &)
CS1S3S3f322 = 0.5D0*(-1.D0*RR32*(CB*((4.D0*CB*Lam2*MW*SB*SW)/EL - (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) - 1.D0*SB*((2.D0*CB2*(Lam4&
  & + Lam5)*MW*SW)/EL - (4.D0*Lam3*MW*SB2*SW)/EL)) - 1.D0*RR31*(-1.D0*SB*((-4.D0*CB*Lam1*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW&
  &*SB*SW)/EL) + CB*((4.D0*CB2*Lam3*MW*SW)/EL - (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR33*(CB2*Lam8 + Lam7*SB2)*vs)

CS1S1S1S1f1111 = -1.D0*RR13*(2.D0*Lam7*RR13*DBLE(RR11**INT(2.D0)) + 2.D0*Lam8*RR13*DBLE(RR12**INT(2.D0)) + RR13*(Lam7*DBLE(RR11**&
  &INT(2.D0)) + Lam8*DBLE(RR12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*DBLE(R&
  &R12**INT(2.D0)) + 2.D0*Lam7*RR11*DBLE(RR13**INT(2.D0)) + RR11*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR1&
  &2**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*DBLE(RR11**INT(2.D0)) + 2.D0*Lam8*RR1&
  &2*DBLE(RR13**INT(2.D0)) + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13*&
  &*INT(2.D0))))
CS1S1S1S1f1112 = -1.D0*RR13*(Lam7*RR11*(RR13*RR21 + RR11*RR23) + Lam8*RR12*(RR13*RR22 + RR12*RR23) + RR13*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR21 + RR11*RR22) + Lam7*RR13*(RR13*RR21 + RR11&
  &*RR23) + RR11*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR13*(RR13*RR22 + RR12*RR23) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f1113 = -1.D0*RR13*(Lam7*RR11*(RR13*RR31 + RR11*RR33) + Lam8*RR12*(RR13*RR32 + RR12*RR33) + RR13*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR31 + RR11*RR32) + Lam7*RR13*(RR13*RR31 + RR11&
  &*RR33) + RR11*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR13*(RR13*RR32 + RR12*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f1121 = -1.D0*RR13*(Lam7*RR11*(RR13*RR21 + RR11*RR23) + Lam8*RR12*(RR13*RR22 + RR12*RR23) + RR13*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR21 + RR11*RR22) + Lam7*RR13*(RR13*RR21 + RR11&
  &*RR23) + RR11*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR13*(RR13*RR22 + RR12*RR23) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f1122 = -1.D0*RR13*(2.D0*Lam7*RR11*RR21*RR23 + 2.D0*Lam8*RR12*RR22*RR23 + RR13*(Lam7*DBLE(RR21**INT(2.D0)) + Lam8*DBLE(R&
  &R22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*RR21*RR22 + 2.D0*Lam7*RR13*RR2&
  &1*RR23 + RR11*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR22**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1&
  &.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR21*RR22 + 2.D0*Lam8*RR13*RR22*RR23 + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS1S1S1S1f1123 = -1.D0*RR13*(Lam7*RR11*(RR23*RR31 + RR21*RR33) + Lam8*RR12*(RR23*RR32 + RR22*RR33) + RR13*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*(RR22*RR31 + RR21*RR32) + Lam7*RR13*(RR23*RR31 + RR21&
  &*RR33) + RR11*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR13*(RR23*RR32 + RR22*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f1131 = -1.D0*RR13*(Lam7*RR11*(RR13*RR31 + RR11*RR33) + Lam8*RR12*(RR13*RR32 + RR12*RR33) + RR13*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR31 + RR11*RR32) + Lam7*RR13*(RR13*RR31 + RR11&
  &*RR33) + RR11*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR13*(RR13*RR32 + RR12*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f1132 = -1.D0*RR13*(Lam7*RR11*(RR23*RR31 + RR21*RR33) + Lam8*RR12*(RR23*RR32 + RR22*RR33) + RR13*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*(RR22*RR31 + RR21*RR32) + Lam7*RR13*(RR23*RR31 + RR21&
  &*RR33) + RR11*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR13*(RR23*RR32 + RR22*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f1133 = -1.D0*RR13*(2.D0*Lam7*RR11*RR31*RR33 + 2.D0*Lam8*RR12*RR32*RR33 + RR13*(Lam7*DBLE(RR31**INT(2.D0)) + Lam8*DBLE(R&
  &R32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*RR31*RR32 + 2.D0*Lam7*RR13*RR3&
  &1*RR33 + RR11*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR32**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1&
  &.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR31*RR32 + 2.D0*Lam8*RR13*RR32*RR33 + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS1S1S1S1f1211 = -1.D0*RR13*(2.D0*Lam7*RR11*RR13*RR21 + 2.D0*Lam8*RR12*RR13*RR22 + RR23*(Lam7*DBLE(RR11**INT(2.D0)) + Lam8*DBLE(R&
  &R12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR22 + 2.D0*Lam7*RR11*RR1&
  &3*RR23 + RR21*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR12**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1&
  &.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR21 + 2.D0*Lam8*RR12*RR13*RR23 + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS1S1S1S1f1212 = -1.D0*RR13*(Lam7*RR21*(RR13*RR21 + RR11*RR23) + Lam8*RR22*(RR13*RR22 + RR12*RR23) + RR23*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR21 + RR11*RR22) + Lam7*RR23*(RR13*RR21 + RR11&
  &*RR23) + RR21*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR23*(RR13*RR22 + RR12*RR23) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f1213 = -1.D0*RR13*(Lam7*RR21*(RR13*RR31 + RR11*RR33) + Lam8*RR22*(RR13*RR32 + RR12*RR33) + RR23*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR31 + RR11*RR32) + Lam7*RR23*(RR13*RR31 + RR11&
  &*RR33) + RR21*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR23*(RR13*RR32 + RR12*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f1221 = -1.D0*RR13*(Lam7*RR21*(RR13*RR21 + RR11*RR23) + Lam8*RR22*(RR13*RR22 + RR12*RR23) + RR23*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR21 + RR11*RR22) + Lam7*RR23*(RR13*RR21 + RR11&
  &*RR23) + RR21*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR23*(RR13*RR22 + RR12*RR23) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f1222 = -1.D0*RR13*(2.D0*Lam7*RR23*DBLE(RR21**INT(2.D0)) + 2.D0*Lam8*RR23*DBLE(RR22**INT(2.D0)) + RR23*(Lam7*DBLE(RR21**&
  &INT(2.D0)) + Lam8*DBLE(RR22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*DBLE(R&
  &R22**INT(2.D0)) + 2.D0*Lam7*RR21*DBLE(RR23**INT(2.D0)) + RR21*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR2&
  &2**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR22*DBLE(RR21**INT(2.D0)) + 2.D0*Lam8*RR2&
  &2*DBLE(RR23**INT(2.D0)) + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23*&
  &*INT(2.D0))))
CS1S1S1S1f1223 = -1.D0*RR13*(Lam7*RR21*(RR23*RR31 + RR21*RR33) + Lam8*RR22*(RR23*RR32 + RR22*RR33) + RR23*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*(RR22*RR31 + RR21*RR32) + Lam7*RR23*(RR23*RR31 + RR21&
  &*RR33) + RR21*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR23*(RR23*RR32 + RR22*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f1231 = -1.D0*RR13*(Lam7*RR21*(RR13*RR31 + RR11*RR33) + Lam8*RR22*(RR13*RR32 + RR12*RR33) + RR23*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR31 + RR11*RR32) + Lam7*RR23*(RR13*RR31 + RR11&
  &*RR33) + RR21*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR23*(RR13*RR32 + RR12*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f1232 = -1.D0*RR13*(Lam7*RR21*(RR23*RR31 + RR21*RR33) + Lam8*RR22*(RR23*RR32 + RR22*RR33) + RR23*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*(RR22*RR31 + RR21*RR32) + Lam7*RR23*(RR23*RR31 + RR21&
  &*RR33) + RR21*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR23*(RR23*RR32 + RR22*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f1233 = -1.D0*RR13*(2.D0*Lam7*RR21*RR31*RR33 + 2.D0*Lam8*RR22*RR32*RR33 + RR23*(Lam7*DBLE(RR31**INT(2.D0)) + Lam8*DBLE(R&
  &R32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR22*RR31*RR32 + 2.D0*Lam7*RR23*RR3&
  &1*RR33 + RR21*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR32**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1&
  &.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR31*RR32 + 2.D0*Lam8*RR23*RR32*RR33 + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS1S1S1S1f1311 = -1.D0*RR13*(2.D0*Lam7*RR11*RR13*RR31 + 2.D0*Lam8*RR12*RR13*RR32 + RR33*(Lam7*DBLE(RR11**INT(2.D0)) + Lam8*DBLE(R&
  &R12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR32 + 2.D0*Lam7*RR11*RR1&
  &3*RR33 + RR31*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR12**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1&
  &.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR31 + 2.D0*Lam8*RR12*RR13*RR33 + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS1S1S1S1f1312 = -1.D0*RR11*((3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)*RR31 + (Lam3 + Lam4 + Lam5)*&
  &(RR12*RR21 + RR11*RR22)*RR32 + Lam7*(RR13*RR21 + RR11*RR23)*RR33) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*(RR12*RR21 + RR11*RR22)*RR&
  &31 + ((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23)*RR32 + Lam8*(RR13*RR22 + RR12*RR23)*RR33) - 1.D0*&
  &RR13*(Lam7*(RR13*RR21 + RR11*RR23)*RR31 + Lam8*(RR13*RR22 + RR12*RR23)*RR32 + (Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23)*RR33)
CS1S1S1S1f1313 = -1.D0*RR13*(Lam7*RR31*(RR13*RR31 + RR11*RR33) + Lam8*RR32*(RR13*RR32 + RR12*RR33) + RR33*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR32*(RR12*RR31 + RR11*RR32) + Lam7*RR33*(RR13*RR31 + RR11&
  &*RR33) + RR31*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR33*(RR13*RR32 + RR12*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f1321 = -1.D0*RR11*((3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)*RR31 + (Lam3 + Lam4 + Lam5)*&
  &(RR12*RR21 + RR11*RR22)*RR32 + Lam7*(RR13*RR21 + RR11*RR23)*RR33) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*(RR12*RR21 + RR11*RR22)*RR&
  &31 + ((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23)*RR32 + Lam8*(RR13*RR22 + RR12*RR23)*RR33) - 1.D0*&
  &RR13*(Lam7*(RR13*RR21 + RR11*RR23)*RR31 + Lam8*(RR13*RR22 + RR12*RR23)*RR32 + (Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23)*RR33)
CS1S1S1S1f1322 = -1.D0*RR13*(2.D0*Lam7*RR21*RR23*RR31 + 2.D0*Lam8*RR22*RR23*RR32 + RR33*(Lam7*DBLE(RR21**INT(2.D0)) + Lam8*DBLE(R&
  &R22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR22*RR32 + 2.D0*Lam7*RR21*RR2&
  &3*RR33 + RR31*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR22**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1&
  &.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR22*RR31 + 2.D0*Lam8*RR22*RR23*RR33 + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS1S1S1S1f1323 = -1.D0*RR13*(Lam7*RR31*(RR23*RR31 + RR21*RR33) + Lam8*RR32*(RR23*RR32 + RR22*RR33) + RR33*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR32*(RR22*RR31 + RR21*RR32) + Lam7*RR33*(RR23*RR31 + RR21&
  &*RR33) + RR31*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR33*(RR23*RR32 + RR22*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f1331 = -1.D0*RR13*(Lam7*RR31*(RR13*RR31 + RR11*RR33) + Lam8*RR32*(RR13*RR32 + RR12*RR33) + RR33*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR32*(RR12*RR31 + RR11*RR32) + Lam7*RR33*(RR13*RR31 + RR11&
  &*RR33) + RR31*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR33*(RR13*RR32 + RR12*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f1332 = -1.D0*RR13*(Lam7*RR31*(RR23*RR31 + RR21*RR33) + Lam8*RR32*(RR23*RR32 + RR22*RR33) + RR33*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR32*(RR22*RR31 + RR21*RR32) + Lam7*RR33*(RR23*RR31 + RR21&
  &*RR33) + RR31*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR33*(RR23*RR32 + RR22*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f1333 = -1.D0*RR13*(2.D0*Lam7*RR33*DBLE(RR31**INT(2.D0)) + 2.D0*Lam8*RR33*DBLE(RR32**INT(2.D0)) + RR33*(Lam7*DBLE(RR31**&
  &INT(2.D0)) + Lam8*DBLE(RR32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR31*DBLE(R&
  &R32**INT(2.D0)) + 2.D0*Lam7*RR31*DBLE(RR33**INT(2.D0)) + RR31*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR3&
  &2**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR32*DBLE(RR31**INT(2.D0)) + 2.D0*Lam8*RR3&
  &2*DBLE(RR33**INT(2.D0)) + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33*&
  &*INT(2.D0))))
CS1S1S1S1f2111 = -1.D0*RR23*(2.D0*Lam7*RR13*DBLE(RR11**INT(2.D0)) + 2.D0*Lam8*RR13*DBLE(RR12**INT(2.D0)) + RR13*(Lam7*DBLE(RR11**&
  &INT(2.D0)) + Lam8*DBLE(RR12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*DBLE(R&
  &R12**INT(2.D0)) + 2.D0*Lam7*RR11*DBLE(RR13**INT(2.D0)) + RR11*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR1&
  &2**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*DBLE(RR11**INT(2.D0)) + 2.D0*Lam8*RR1&
  &2*DBLE(RR13**INT(2.D0)) + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13*&
  &*INT(2.D0))))
CS1S1S1S1f2112 = -1.D0*RR23*(Lam7*RR11*(RR13*RR21 + RR11*RR23) + Lam8*RR12*(RR13*RR22 + RR12*RR23) + RR13*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR21 + RR11*RR22) + Lam7*RR13*(RR13*RR21 + RR11&
  &*RR23) + RR11*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR13*(RR13*RR22 + RR12*RR23) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f2113 = -1.D0*RR23*(Lam7*RR11*(RR13*RR31 + RR11*RR33) + Lam8*RR12*(RR13*RR32 + RR12*RR33) + RR13*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR31 + RR11*RR32) + Lam7*RR13*(RR13*RR31 + RR11&
  &*RR33) + RR11*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR13*(RR13*RR32 + RR12*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f2121 = -1.D0*RR23*(Lam7*RR11*(RR13*RR21 + RR11*RR23) + Lam8*RR12*(RR13*RR22 + RR12*RR23) + RR13*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR21 + RR11*RR22) + Lam7*RR13*(RR13*RR21 + RR11&
  &*RR23) + RR11*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR13*(RR13*RR22 + RR12*RR23) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f2122 = -1.D0*RR23*(2.D0*Lam7*RR11*RR21*RR23 + 2.D0*Lam8*RR12*RR22*RR23 + RR13*(Lam7*DBLE(RR21**INT(2.D0)) + Lam8*DBLE(R&
  &R22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*RR21*RR22 + 2.D0*Lam7*RR13*RR2&
  &1*RR23 + RR11*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR22**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1&
  &.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR21*RR22 + 2.D0*Lam8*RR13*RR22*RR23 + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS1S1S1S1f2123 = -1.D0*RR23*(Lam7*RR11*(RR23*RR31 + RR21*RR33) + Lam8*RR12*(RR23*RR32 + RR22*RR33) + RR13*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*(RR22*RR31 + RR21*RR32) + Lam7*RR13*(RR23*RR31 + RR21&
  &*RR33) + RR11*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR13*(RR23*RR32 + RR22*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f2131 = -1.D0*RR23*(Lam7*RR11*(RR13*RR31 + RR11*RR33) + Lam8*RR12*(RR13*RR32 + RR12*RR33) + RR13*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR31 + RR11*RR32) + Lam7*RR13*(RR13*RR31 + RR11&
  &*RR33) + RR11*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR13*(RR13*RR32 + RR12*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f2132 = -1.D0*RR23*(Lam7*RR11*(RR23*RR31 + RR21*RR33) + Lam8*RR12*(RR23*RR32 + RR22*RR33) + RR13*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*(RR22*RR31 + RR21*RR32) + Lam7*RR13*(RR23*RR31 + RR21&
  &*RR33) + RR11*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR13*(RR23*RR32 + RR22*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f2133 = -1.D0*RR23*(2.D0*Lam7*RR11*RR31*RR33 + 2.D0*Lam8*RR12*RR32*RR33 + RR13*(Lam7*DBLE(RR31**INT(2.D0)) + Lam8*DBLE(R&
  &R32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*RR31*RR32 + 2.D0*Lam7*RR13*RR3&
  &1*RR33 + RR11*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR32**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1&
  &.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR31*RR32 + 2.D0*Lam8*RR13*RR32*RR33 + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS1S1S1S1f2211 = -1.D0*RR23*(2.D0*Lam7*RR11*RR13*RR21 + 2.D0*Lam8*RR12*RR13*RR22 + RR23*(Lam7*DBLE(RR11**INT(2.D0)) + Lam8*DBLE(R&
  &R12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR22 + 2.D0*Lam7*RR11*RR1&
  &3*RR23 + RR21*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR12**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1&
  &.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR21 + 2.D0*Lam8*RR12*RR13*RR23 + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS1S1S1S1f2212 = -1.D0*RR23*(Lam7*RR21*(RR13*RR21 + RR11*RR23) + Lam8*RR22*(RR13*RR22 + RR12*RR23) + RR23*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR21 + RR11*RR22) + Lam7*RR23*(RR13*RR21 + RR11&
  &*RR23) + RR21*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR23*(RR13*RR22 + RR12*RR23) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f2213 = -1.D0*RR23*(Lam7*RR21*(RR13*RR31 + RR11*RR33) + Lam8*RR22*(RR13*RR32 + RR12*RR33) + RR23*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR31 + RR11*RR32) + Lam7*RR23*(RR13*RR31 + RR11&
  &*RR33) + RR21*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR23*(RR13*RR32 + RR12*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f2221 = -1.D0*RR23*(Lam7*RR21*(RR13*RR21 + RR11*RR23) + Lam8*RR22*(RR13*RR22 + RR12*RR23) + RR23*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR21 + RR11*RR22) + Lam7*RR23*(RR13*RR21 + RR11&
  &*RR23) + RR21*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR23*(RR13*RR22 + RR12*RR23) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f2222 = -1.D0*RR23*(2.D0*Lam7*RR23*DBLE(RR21**INT(2.D0)) + 2.D0*Lam8*RR23*DBLE(RR22**INT(2.D0)) + RR23*(Lam7*DBLE(RR21**&
  &INT(2.D0)) + Lam8*DBLE(RR22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*DBLE(R&
  &R22**INT(2.D0)) + 2.D0*Lam7*RR21*DBLE(RR23**INT(2.D0)) + RR21*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR2&
  &2**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR22*DBLE(RR21**INT(2.D0)) + 2.D0*Lam8*RR2&
  &2*DBLE(RR23**INT(2.D0)) + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23*&
  &*INT(2.D0))))
CS1S1S1S1f2223 = -1.D0*RR23*(Lam7*RR21*(RR23*RR31 + RR21*RR33) + Lam8*RR22*(RR23*RR32 + RR22*RR33) + RR23*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*(RR22*RR31 + RR21*RR32) + Lam7*RR23*(RR23*RR31 + RR21&
  &*RR33) + RR21*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR23*(RR23*RR32 + RR22*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f2231 = -1.D0*RR23*(Lam7*RR21*(RR13*RR31 + RR11*RR33) + Lam8*RR22*(RR13*RR32 + RR12*RR33) + RR23*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR31 + RR11*RR32) + Lam7*RR23*(RR13*RR31 + RR11&
  &*RR33) + RR21*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR23*(RR13*RR32 + RR12*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f2232 = -1.D0*RR23*(Lam7*RR21*(RR23*RR31 + RR21*RR33) + Lam8*RR22*(RR23*RR32 + RR22*RR33) + RR23*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*(RR22*RR31 + RR21*RR32) + Lam7*RR23*(RR23*RR31 + RR21&
  &*RR33) + RR21*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR23*(RR23*RR32 + RR22*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f2233 = -1.D0*RR23*(2.D0*Lam7*RR21*RR31*RR33 + 2.D0*Lam8*RR22*RR32*RR33 + RR23*(Lam7*DBLE(RR31**INT(2.D0)) + Lam8*DBLE(R&
  &R32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR22*RR31*RR32 + 2.D0*Lam7*RR23*RR3&
  &1*RR33 + RR21*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR32**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1&
  &.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR31*RR32 + 2.D0*Lam8*RR23*RR32*RR33 + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS1S1S1S1f2311 = -1.D0*RR23*(2.D0*Lam7*RR11*RR13*RR31 + 2.D0*Lam8*RR12*RR13*RR32 + RR33*(Lam7*DBLE(RR11**INT(2.D0)) + Lam8*DBLE(R&
  &R12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR32 + 2.D0*Lam7*RR11*RR1&
  &3*RR33 + RR31*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR12**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1&
  &.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR31 + 2.D0*Lam8*RR12*RR13*RR33 + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS1S1S1S1f2312 = -1.D0*RR21*((3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)*RR31 + (Lam3 + Lam4 + Lam5)*&
  &(RR12*RR21 + RR11*RR22)*RR32 + Lam7*(RR13*RR21 + RR11*RR23)*RR33) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*(RR12*RR21 + RR11*RR22)*RR&
  &31 + ((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23)*RR32 + Lam8*(RR13*RR22 + RR12*RR23)*RR33) - 1.D0*&
  &RR23*(Lam7*(RR13*RR21 + RR11*RR23)*RR31 + Lam8*(RR13*RR22 + RR12*RR23)*RR32 + (Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23)*RR33)
CS1S1S1S1f2313 = -1.D0*RR23*(Lam7*RR31*(RR13*RR31 + RR11*RR33) + Lam8*RR32*(RR13*RR32 + RR12*RR33) + RR33*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR32*(RR12*RR31 + RR11*RR32) + Lam7*RR33*(RR13*RR31 + RR11&
  &*RR33) + RR31*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR33*(RR13*RR32 + RR12*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f2321 = -1.D0*RR21*((3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)*RR31 + (Lam3 + Lam4 + Lam5)*&
  &(RR12*RR21 + RR11*RR22)*RR32 + Lam7*(RR13*RR21 + RR11*RR23)*RR33) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*(RR12*RR21 + RR11*RR22)*RR&
  &31 + ((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23)*RR32 + Lam8*(RR13*RR22 + RR12*RR23)*RR33) - 1.D0*&
  &RR23*(Lam7*(RR13*RR21 + RR11*RR23)*RR31 + Lam8*(RR13*RR22 + RR12*RR23)*RR32 + (Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23)*RR33)
CS1S1S1S1f2322 = -1.D0*RR23*(2.D0*Lam7*RR21*RR23*RR31 + 2.D0*Lam8*RR22*RR23*RR32 + RR33*(Lam7*DBLE(RR21**INT(2.D0)) + Lam8*DBLE(R&
  &R22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR22*RR32 + 2.D0*Lam7*RR21*RR2&
  &3*RR33 + RR31*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR22**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1&
  &.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR22*RR31 + 2.D0*Lam8*RR22*RR23*RR33 + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS1S1S1S1f2323 = -1.D0*RR23*(Lam7*RR31*(RR23*RR31 + RR21*RR33) + Lam8*RR32*(RR23*RR32 + RR22*RR33) + RR33*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR32*(RR22*RR31 + RR21*RR32) + Lam7*RR33*(RR23*RR31 + RR21&
  &*RR33) + RR31*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR33*(RR23*RR32 + RR22*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f2331 = -1.D0*RR23*(Lam7*RR31*(RR13*RR31 + RR11*RR33) + Lam8*RR32*(RR13*RR32 + RR12*RR33) + RR33*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR32*(RR12*RR31 + RR11*RR32) + Lam7*RR33*(RR13*RR31 + RR11&
  &*RR33) + RR31*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR33*(RR13*RR32 + RR12*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f2332 = -1.D0*RR23*(Lam7*RR31*(RR23*RR31 + RR21*RR33) + Lam8*RR32*(RR23*RR32 + RR22*RR33) + RR33*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR32*(RR22*RR31 + RR21*RR32) + Lam7*RR33*(RR23*RR31 + RR21&
  &*RR33) + RR31*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR33*(RR23*RR32 + RR22*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f2333 = -1.D0*RR23*(2.D0*Lam7*RR33*DBLE(RR31**INT(2.D0)) + 2.D0*Lam8*RR33*DBLE(RR32**INT(2.D0)) + RR33*(Lam7*DBLE(RR31**&
  &INT(2.D0)) + Lam8*DBLE(RR32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR31*DBLE(R&
  &R32**INT(2.D0)) + 2.D0*Lam7*RR31*DBLE(RR33**INT(2.D0)) + RR31*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR3&
  &2**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR32*DBLE(RR31**INT(2.D0)) + 2.D0*Lam8*RR3&
  &2*DBLE(RR33**INT(2.D0)) + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33*&
  &*INT(2.D0))))
CS1S1S1S1f3111 = -1.D0*RR33*(2.D0*Lam7*RR13*DBLE(RR11**INT(2.D0)) + 2.D0*Lam8*RR13*DBLE(RR12**INT(2.D0)) + RR13*(Lam7*DBLE(RR11**&
  &INT(2.D0)) + Lam8*DBLE(RR12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*DBLE(R&
  &R12**INT(2.D0)) + 2.D0*Lam7*RR11*DBLE(RR13**INT(2.D0)) + RR11*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR1&
  &2**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*DBLE(RR11**INT(2.D0)) + 2.D0*Lam8*RR1&
  &2*DBLE(RR13**INT(2.D0)) + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13*&
  &*INT(2.D0))))
CS1S1S1S1f3112 = -1.D0*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR21 + RR11*RR22) + Lam7*RR13*(RR13*RR21 + RR11*RR23) + RR11*(3.D0*Lam1*R&
  &R11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23))*RR31 - 1.D0*((Lam3 + Lam4 + Lam5)*RR11*(RR12*RR21 + RR11*RR22) + &
  &Lam8*RR13*(RR13*RR22 + RR12*RR23) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23))*RR32 - 1.D0*&
  &(Lam7*RR11*(RR13*RR21 + RR11*RR23) + Lam8*RR12*(RR13*RR22 + RR12*RR23) + RR13*(Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23))*RR33
CS1S1S1S1f3113 = -1.D0*RR33*(Lam7*RR11*(RR13*RR31 + RR11*RR33) + Lam8*RR12*(RR13*RR32 + RR12*RR33) + RR13*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR31 + RR11*RR32) + Lam7*RR13*(RR13*RR31 + RR11&
  &*RR33) + RR11*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR13*(RR13*RR32 + RR12*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f3121 = -1.D0*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR21 + RR11*RR22) + Lam7*RR13*(RR13*RR21 + RR11*RR23) + RR11*(3.D0*Lam1*R&
  &R11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23))*RR31 - 1.D0*((Lam3 + Lam4 + Lam5)*RR11*(RR12*RR21 + RR11*RR22) + &
  &Lam8*RR13*(RR13*RR22 + RR12*RR23) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23))*RR32 - 1.D0*&
  &(Lam7*RR11*(RR13*RR21 + RR11*RR23) + Lam8*RR12*(RR13*RR22 + RR12*RR23) + RR13*(Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23))*RR33
CS1S1S1S1f3122 = -1.D0*RR33*(2.D0*Lam7*RR11*RR21*RR23 + 2.D0*Lam8*RR12*RR22*RR23 + RR13*(Lam7*DBLE(RR21**INT(2.D0)) + Lam8*DBLE(R&
  &R22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*RR21*RR22 + 2.D0*Lam7*RR13*RR2&
  &1*RR23 + RR11*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR22**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1&
  &.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR21*RR22 + 2.D0*Lam8*RR13*RR22*RR23 + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS1S1S1S1f3123 = -1.D0*RR33*(Lam7*RR11*(RR23*RR31 + RR21*RR33) + Lam8*RR12*(RR23*RR32 + RR22*RR33) + RR13*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR12*(RR22*RR31 + RR21*RR32) + Lam7*RR13*(RR23*RR31 + RR21&
  &*RR33) + RR11*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR13*(RR23*RR32 + RR22*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f3131 = -1.D0*RR33*(Lam7*RR11*(RR13*RR31 + RR11*RR33) + Lam8*RR12*(RR13*RR32 + RR12*RR33) + RR13*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR31 + RR11*RR32) + Lam7*RR13*(RR13*RR31 + RR11&
  &*RR33) + RR11*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR13*(RR13*RR32 + RR12*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f3132 = -1.D0*RR33*(Lam7*RR11*(RR23*RR31 + RR21*RR33) + Lam8*RR12*(RR23*RR32 + RR22*RR33) + RR13*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR12*(RR22*RR31 + RR21*RR32) + Lam7*RR13*(RR23*RR31 + RR21&
  &*RR33) + RR11*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR13*(RR23*RR32 + RR22*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f3133 = -1.D0*RR33*(2.D0*Lam7*RR11*RR31*RR33 + 2.D0*Lam8*RR12*RR32*RR33 + RR13*(Lam7*DBLE(RR31**INT(2.D0)) + Lam8*DBLE(R&
  &R32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*RR31*RR32 + 2.D0*Lam7*RR13*RR3&
  &1*RR33 + RR11*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR32**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1&
  &.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR31*RR32 + 2.D0*Lam8*RR13*RR32*RR33 + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS1S1S1S1f3211 = -1.D0*RR33*(2.D0*Lam7*RR11*RR13*RR21 + 2.D0*Lam8*RR12*RR13*RR22 + RR23*(Lam7*DBLE(RR11**INT(2.D0)) + Lam8*DBLE(R&
  &R12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR22 + 2.D0*Lam7*RR11*RR1&
  &3*RR23 + RR21*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR12**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1&
  &.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR21 + 2.D0*Lam8*RR12*RR13*RR23 + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS1S1S1S1f3212 = -1.D0*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR21 + RR11*RR22) + Lam7*RR23*(RR13*RR21 + RR11*RR23) + RR21*(3.D0*Lam1*R&
  &R11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23))*RR31 - 1.D0*((Lam3 + Lam4 + Lam5)*RR21*(RR12*RR21 + RR11*RR22) + &
  &Lam8*RR23*(RR13*RR22 + RR12*RR23) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23))*RR32 - 1.D0*&
  &(Lam7*RR21*(RR13*RR21 + RR11*RR23) + Lam8*RR22*(RR13*RR22 + RR12*RR23) + RR23*(Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23))*RR33
CS1S1S1S1f3213 = -1.D0*RR33*(Lam7*RR21*(RR13*RR31 + RR11*RR33) + Lam8*RR22*(RR13*RR32 + RR12*RR33) + RR23*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR31 + RR11*RR32) + Lam7*RR23*(RR13*RR31 + RR11&
  &*RR33) + RR21*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR23*(RR13*RR32 + RR12*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f3221 = -1.D0*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR21 + RR11*RR22) + Lam7*RR23*(RR13*RR21 + RR11*RR23) + RR21*(3.D0*Lam1*R&
  &R11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23))*RR31 - 1.D0*((Lam3 + Lam4 + Lam5)*RR21*(RR12*RR21 + RR11*RR22) + &
  &Lam8*RR23*(RR13*RR22 + RR12*RR23) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23))*RR32 - 1.D0*&
  &(Lam7*RR21*(RR13*RR21 + RR11*RR23) + Lam8*RR22*(RR13*RR22 + RR12*RR23) + RR23*(Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23))*RR33
CS1S1S1S1f3222 = -1.D0*RR33*(2.D0*Lam7*RR23*DBLE(RR21**INT(2.D0)) + 2.D0*Lam8*RR23*DBLE(RR22**INT(2.D0)) + RR23*(Lam7*DBLE(RR21**&
  &INT(2.D0)) + Lam8*DBLE(RR22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*DBLE(R&
  &R22**INT(2.D0)) + 2.D0*Lam7*RR21*DBLE(RR23**INT(2.D0)) + RR21*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR2&
  &2**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR22*DBLE(RR21**INT(2.D0)) + 2.D0*Lam8*RR2&
  &2*DBLE(RR23**INT(2.D0)) + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23*&
  &*INT(2.D0))))
CS1S1S1S1f3223 = -1.D0*RR33*(Lam7*RR21*(RR23*RR31 + RR21*RR33) + Lam8*RR22*(RR23*RR32 + RR22*RR33) + RR23*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR22*(RR22*RR31 + RR21*RR32) + Lam7*RR23*(RR23*RR31 + RR21&
  &*RR33) + RR21*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR23*(RR23*RR32 + RR22*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f3231 = -1.D0*RR33*(Lam7*RR21*(RR13*RR31 + RR11*RR33) + Lam8*RR22*(RR13*RR32 + RR12*RR33) + RR23*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR31 + RR11*RR32) + Lam7*RR23*(RR13*RR31 + RR11&
  &*RR33) + RR21*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR23*(RR13*RR32 + RR12*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f3232 = -1.D0*RR33*(Lam7*RR21*(RR23*RR31 + RR21*RR33) + Lam8*RR22*(RR23*RR32 + RR22*RR33) + RR23*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR22*(RR22*RR31 + RR21*RR32) + Lam7*RR23*(RR23*RR31 + RR21&
  &*RR33) + RR21*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR23*(RR23*RR32 + RR22*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f3233 = -1.D0*RR33*(2.D0*Lam7*RR21*RR31*RR33 + 2.D0*Lam8*RR22*RR32*RR33 + RR23*(Lam7*DBLE(RR31**INT(2.D0)) + Lam8*DBLE(R&
  &R32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR22*RR31*RR32 + 2.D0*Lam7*RR23*RR3&
  &1*RR33 + RR21*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR32**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1&
  &.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR31*RR32 + 2.D0*Lam8*RR23*RR32*RR33 + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS1S1S1S1f3311 = -1.D0*RR33*(2.D0*Lam7*RR11*RR13*RR31 + 2.D0*Lam8*RR12*RR13*RR32 + RR33*(Lam7*DBLE(RR11**INT(2.D0)) + Lam8*DBLE(R&
  &R12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR32 + 2.D0*Lam7*RR11*RR1&
  &3*RR33 + RR31*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR12**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1&
  &.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR31 + 2.D0*Lam8*RR12*RR13*RR33 + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS1S1S1S1f3312 = -1.D0*RR31*((3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)*RR31 + (Lam3 + Lam4 + Lam5)*&
  &(RR12*RR21 + RR11*RR22)*RR32 + Lam7*(RR13*RR21 + RR11*RR23)*RR33) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*(RR12*RR21 + RR11*RR22)*RR&
  &31 + ((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23)*RR32 + Lam8*(RR13*RR22 + RR12*RR23)*RR33) - 1.D0*&
  &RR33*(Lam7*(RR13*RR21 + RR11*RR23)*RR31 + Lam8*(RR13*RR22 + RR12*RR23)*RR32 + (Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23)*RR33)
CS1S1S1S1f3313 = -1.D0*RR33*(Lam7*RR31*(RR13*RR31 + RR11*RR33) + Lam8*RR32*(RR13*RR32 + RR12*RR33) + RR33*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR32*(RR12*RR31 + RR11*RR32) + Lam7*RR33*(RR13*RR31 + RR11&
  &*RR33) + RR31*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR33*(RR13*RR32 + RR12*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f3321 = -1.D0*RR31*((3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)*RR31 + (Lam3 + Lam4 + Lam5)*&
  &(RR12*RR21 + RR11*RR22)*RR32 + Lam7*(RR13*RR21 + RR11*RR23)*RR33) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*(RR12*RR21 + RR11*RR22)*RR&
  &31 + ((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23)*RR32 + Lam8*(RR13*RR22 + RR12*RR23)*RR33) - 1.D0*&
  &RR33*(Lam7*(RR13*RR21 + RR11*RR23)*RR31 + Lam8*(RR13*RR22 + RR12*RR23)*RR32 + (Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23)*RR33)
CS1S1S1S1f3322 = -1.D0*RR33*(2.D0*Lam7*RR21*RR23*RR31 + 2.D0*Lam8*RR22*RR23*RR32 + RR33*(Lam7*DBLE(RR21**INT(2.D0)) + Lam8*DBLE(R&
  &R22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR22*RR32 + 2.D0*Lam7*RR21*RR2&
  &3*RR33 + RR31*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR22**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1&
  &.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR22*RR31 + 2.D0*Lam8*RR22*RR23*RR33 + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS1S1S1S1f3323 = -1.D0*RR33*(Lam7*RR31*(RR23*RR31 + RR21*RR33) + Lam8*RR32*(RR23*RR32 + RR22*RR33) + RR33*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR32*(RR22*RR31 + RR21*RR32) + Lam7*RR33*(RR23*RR31 + RR21&
  &*RR33) + RR31*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR33*(RR23*RR32 + RR22*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f3331 = -1.D0*RR33*(Lam7*RR31*(RR13*RR31 + RR11*RR33) + Lam8*RR32*(RR13*RR32 + RR12*RR33) + RR33*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR32*(RR12*RR31 + RR11*RR32) + Lam7*RR33*(RR13*RR31 + RR11&
  &*RR33) + RR31*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR33*(RR13*RR32 + RR12*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f3332 = -1.D0*RR33*(Lam7*RR31*(RR23*RR31 + RR21*RR33) + Lam8*RR32*(RR23*RR32 + RR22*RR33) + RR33*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR32*(RR22*RR31 + RR21*RR32) + Lam7*RR33*(RR23*RR31 + RR21&
  &*RR33) + RR31*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR33*(RR23*RR32 + RR22*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f3333 = -1.D0*RR33*(2.D0*Lam7*RR33*DBLE(RR31**INT(2.D0)) + 2.D0*Lam8*RR33*DBLE(RR32**INT(2.D0)) + RR33*(Lam7*DBLE(RR31**&
  &INT(2.D0)) + Lam8*DBLE(RR32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR31*DBLE(R&
  &R32**INT(2.D0)) + 2.D0*Lam7*RR31*DBLE(RR33**INT(2.D0)) + RR31*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR3&
  &2**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR32*DBLE(RR31**INT(2.D0)) + 2.D0*Lam8*RR3&
  &2*DBLE(RR33**INT(2.D0)) + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33*&
  &*INT(2.D0))))

CS2S2S2S2f1111 = -1.D0*SB*(2.D0*CB2*(Lam3 + Lam4 + Lam5)*SB + SB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam2*SB2)) - 1.D0*CB*(2.D0*CB*(&
  &Lam3 + Lam4 + Lam5)*SB2 + CB*(3.D0*CB2*Lam1 + (Lam3 + Lam4 + Lam5)*SB2))
CS2S2S2S2f1112 = -1.D0*SB*(SB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1&
  &.D0*CB*(CB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + (Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f1121 = -1.D0*SB*(SB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1&
  &.D0*CB*(CB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + (Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f1122 = -1.D0*CB*(-2.D0*CB*(Lam3 + Lam4 + Lam5)*SB2 + CB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam1*SB2)) - 1.D0*SB*(-2.D0*CB&
  &2*(Lam3 + Lam4 + Lam5)*SB + SB*(3.D0*CB2*Lam2 + (Lam3 + Lam4 + Lam5)*SB2))
CS2S2S2S2f1211 = -1.D0*SB*(-2.D0*CB*(Lam3 + Lam4 + Lam5)*SB2 + CB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam2*SB2)) - 1.D0*CB*(2.D0*CB2&
  &*(Lam3 + Lam4 + Lam5)*SB - 1.D0*SB*(3.D0*CB2*Lam1 + (Lam3 + Lam4 + Lam5)*SB2))
CS2S2S2S2f1212 = -1.D0*CB*(-1.D0*SB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) -&
  & 1.D0*SB*(CB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) - 1.D0*(Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f1221 = -1.D0*CB*(-1.D0*SB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) -&
  & 1.D0*SB*(CB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) - 1.D0*(Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f1222 = -1.D0*CB*(-2.D0*CB2*(Lam3 + Lam4 + Lam5)*SB - 1.D0*SB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam1*SB2)) - 1.D0*SB*(2.D&
  &0*CB*(Lam3 + Lam4 + Lam5)*SB2 + CB*(3.D0*CB2*Lam2 + (Lam3 + Lam4 + Lam5)*SB2))
CS2S2S2S2f2111 = -1.D0*CB*(2.D0*CB2*(Lam3 + Lam4 + Lam5)*SB + SB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam2*SB2)) + SB*(2.D0*CB*(Lam3 &
  &+ Lam4 + Lam5)*SB2 + CB*(3.D0*CB2*Lam1 + (Lam3 + Lam4 + Lam5)*SB2))
CS2S2S2S2f2112 = -1.D0*CB*(SB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) + S&
  &B*(CB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + (Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f2121 = -1.D0*CB*(SB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) + S&
  &B*(CB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + (Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f2122 = SB*(-2.D0*CB*(Lam3 + Lam4 + Lam5)*SB2 + CB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam1*SB2)) - 1.D0*CB*(-2.D0*CB2*(Lam&
  &3 + Lam4 + Lam5)*SB + SB*(3.D0*CB2*Lam2 + (Lam3 + Lam4 + Lam5)*SB2))
CS2S2S2S2f2211 = -1.D0*CB*(-2.D0*CB*(Lam3 + Lam4 + Lam5)*SB2 + CB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam2*SB2)) + SB*(2.D0*CB2*(Lam&
  &3 + Lam4 + Lam5)*SB - 1.D0*SB*(3.D0*CB2*Lam1 + (Lam3 + Lam4 + Lam5)*SB2))
CS2S2S2S2f2212 = SB*(-1.D0*SB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*&
  &CB*(CB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) - 1.D0*(Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f2221 = SB*(-1.D0*SB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*&
  &CB*(CB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) - 1.D0*(Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f2222 = SB*(-2.D0*CB2*(Lam3 + Lam4 + Lam5)*SB - 1.D0*SB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam1*SB2)) - 1.D0*CB*(2.D0*CB*(&
  &Lam3 + Lam4 + Lam5)*SB2 + CB*(3.D0*CB2*Lam2 + (Lam3 + Lam4 + Lam5)*SB2))

CS3S3S3S3f1111 = -1.D0*SB*(2.D0*CB2*(Lam3 + Lam4)*SB + 2.D0*SB*(CB2*Lam5 + Lam2*SB2)) - 1.D0*CB*(2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D0&
  &*CB*(CB2*Lam1 + Lam5*SB2))
CS3S3S3S3f1112 = -1.D0*SB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*(-1.D0*&
  &CB*Lam1*SB + CB*Lam5*SB) + (Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f1121 = -1.D0*SB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*(-1.D0*&
  &CB*Lam1*SB + CB*Lam5*SB) + (Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f1122 = -1.D0*CB*(-2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D0*CB*(CB2*Lam5 + Lam1*SB2)) - 1.D0*SB*(-2.D0*CB2*(Lam3 + Lam4)*SB + 2.&
  &D0*SB*(CB2*Lam2 + Lam5*SB2))
CS3S3S3S3f1211 = -1.D0*SB*(-2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D0*CB*(CB2*Lam5 + Lam2*SB2)) - 1.D0*CB*(2.D0*CB2*(Lam3 + Lam4)*SB - 2.D&
  &0*SB*(CB2*Lam1 + Lam5*SB2))
CS3S3S3S3f1212 = -1.D0*CB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) - 1.D0*SB*(2.D0*CB*(CB*L&
  &am2*SB - 1.D0*CB*Lam5*SB) - 1.D0*(Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f1221 = -1.D0*CB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) - 1.D0*SB*(2.D0*CB*(CB*L&
  &am2*SB - 1.D0*CB*Lam5*SB) - 1.D0*(Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f1222 = -1.D0*CB*(-2.D0*CB2*(Lam3 + Lam4)*SB - 2.D0*SB*(CB2*Lam5 + Lam1*SB2)) - 1.D0*SB*(2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D&
  &0*CB*(CB2*Lam2 + Lam5*SB2))
CS3S3S3S3f2111 = -1.D0*CB*(2.D0*CB2*(Lam3 + Lam4)*SB + 2.D0*SB*(CB2*Lam5 + Lam2*SB2)) + SB*(2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D0*CB*(&
  &CB2*Lam1 + Lam5*SB2))
CS3S3S3S3f2112 = -1.D0*CB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) + SB*(2.D0*CB*(-1.D0*CB*La&
  &m1*SB + CB*Lam5*SB) + (Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f2121 = -1.D0*CB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) + SB*(2.D0*CB*(-1.D0*CB*La&
  &m1*SB + CB*Lam5*SB) + (Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f2122 = SB*(-2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D0*CB*(CB2*Lam5 + Lam1*SB2)) - 1.D0*CB*(-2.D0*CB2*(Lam3 + Lam4)*SB + 2.D0*SB*&
  &(CB2*Lam2 + Lam5*SB2))
CS3S3S3S3f2211 = -1.D0*CB*(-2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D0*CB*(CB2*Lam5 + Lam2*SB2)) + SB*(2.D0*CB2*(Lam3 + Lam4)*SB - 2.D0*SB*&
  &(CB2*Lam1 + Lam5*SB2))
CS3S3S3S3f2212 = SB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*(CB*Lam2*SB&
  & - 1.D0*CB*Lam5*SB) - 1.D0*(Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f2221 = SB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*(CB*Lam2*SB&
  & - 1.D0*CB*Lam5*SB) - 1.D0*(Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f2222 = SB*(-2.D0*CB2*(Lam3 + Lam4)*SB - 2.D0*SB*(CB2*Lam5 + Lam1*SB2)) - 1.D0*CB*(2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D0*CB*(&
  &CB2*Lam2 + Lam5*SB2))

CS2S2S1S1f1111 = -1.D0*CB*(2.D0*Lam5*RR11*RR12*SB + CB*(Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR12**INT(2.D&
  &0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1.D0*SB*(2.D0*CB*Lam5*RR11*RR12 + SB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR11**INT(2.D0)) + L&
  &am2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS2S2S1S1f1112 = -1.D0*CB*(CB*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR13*RR23) + Lam5*(RR12*RR21 + RR11*RR&
  &22)*SB) - 1.D0*SB*(CB*Lam5*(RR12*RR21 + RR11*RR22) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23)*S&
  &B)
CS2S2S1S1f1113 = -1.D0*CB*(CB*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR13*RR33) + Lam5*(RR12*RR31 + RR11*RR&
  &32)*SB) - 1.D0*SB*(CB*Lam5*(RR12*RR31 + RR11*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33)*S&
  &B)
CS2S2S1S1f1121 = -1.D0*CB*(CB*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR13*RR23) + Lam5*(RR12*RR21 + RR11*RR&
  &22)*SB) - 1.D0*SB*(CB*Lam5*(RR12*RR21 + RR11*RR22) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23)*S&
  &B)
CS2S2S1S1f1122 = -1.D0*CB*(2.D0*Lam5*RR21*RR22*SB + CB*(Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR22**INT(2.D&
  &0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1.D0*SB*(2.D0*CB*Lam5*RR21*RR22 + SB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR21**INT(2.D0)) + L&
  &am2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS2S2S1S1f1123 = -1.D0*CB*(CB*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR23*RR33) + Lam5*(RR22*RR31 + RR21*RR&
  &32)*SB) - 1.D0*SB*(CB*Lam5*(RR22*RR31 + RR21*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33)*S&
  &B)
CS2S2S1S1f1131 = -1.D0*CB*(CB*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR13*RR33) + Lam5*(RR12*RR31 + RR11*RR&
  &32)*SB) - 1.D0*SB*(CB*Lam5*(RR12*RR31 + RR11*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33)*S&
  &B)
CS2S2S1S1f1132 = -1.D0*CB*(CB*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR23*RR33) + Lam5*(RR22*RR31 + RR21*RR&
  &32)*SB) - 1.D0*SB*(CB*Lam5*(RR22*RR31 + RR21*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33)*S&
  &B)
CS2S2S1S1f1133 = -1.D0*CB*(2.D0*Lam5*RR31*RR32*SB + CB*(Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR32**INT(2.D&
  &0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1.D0*SB*(2.D0*CB*Lam5*RR31*RR32 + SB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR31**INT(2.D0)) + L&
  &am2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS2S2S1S1f1211 = -1.D0*CB*(2.D0*CB*Lam5*RR11*RR12 - 1.D0*SB*(Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR12**IN&
  &T(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1.D0*SB*(-2.D0*Lam5*RR11*RR12*SB + CB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR11**INT(2.D0&
  &)) + Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS2S2S1S1f1212 = -1.D0*SB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23) - 1.D0*Lam5*(RR12*RR21 + RR&
  &11*RR22)*SB) - 1.D0*CB*(CB*Lam5*(RR12*RR21 + RR11*RR22) - 1.D0*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR&
  &13*RR23)*SB)
CS2S2S1S1f1213 = -1.D0*SB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33) - 1.D0*Lam5*(RR12*RR31 + RR&
  &11*RR32)*SB) - 1.D0*CB*(CB*Lam5*(RR12*RR31 + RR11*RR32) - 1.D0*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR&
  &13*RR33)*SB)
CS2S2S1S1f1221 = -1.D0*SB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23) - 1.D0*Lam5*(RR12*RR21 + RR&
  &11*RR22)*SB) - 1.D0*CB*(CB*Lam5*(RR12*RR21 + RR11*RR22) - 1.D0*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR&
  &13*RR23)*SB)
CS2S2S1S1f1222 = -1.D0*CB*(2.D0*CB*Lam5*RR21*RR22 - 1.D0*SB*(Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR22**IN&
  &T(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1.D0*SB*(-2.D0*Lam5*RR21*RR22*SB + CB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR21**INT(2.D0&
  &)) + Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS2S2S1S1f1223 = -1.D0*SB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33) - 1.D0*Lam5*(RR22*RR31 + RR&
  &21*RR32)*SB) - 1.D0*CB*(CB*Lam5*(RR22*RR31 + RR21*RR32) - 1.D0*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR&
  &23*RR33)*SB)
CS2S2S1S1f1231 = -1.D0*SB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33) - 1.D0*Lam5*(RR12*RR31 + RR&
  &11*RR32)*SB) - 1.D0*CB*(CB*Lam5*(RR12*RR31 + RR11*RR32) - 1.D0*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR&
  &13*RR33)*SB)
CS2S2S1S1f1232 = -1.D0*SB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33) - 1.D0*Lam5*(RR22*RR31 + RR&
  &21*RR32)*SB) - 1.D0*CB*(CB*Lam5*(RR22*RR31 + RR21*RR32) - 1.D0*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR&
  &23*RR33)*SB)
CS2S2S1S1f1233 = -1.D0*CB*(2.D0*CB*Lam5*RR31*RR32 - 1.D0*SB*(Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR32**IN&
  &T(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1.D0*SB*(-2.D0*Lam5*RR31*RR32*SB + CB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR31**INT(2.D0&
  &)) + Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS2S2S1S1f2111 = SB*(2.D0*Lam5*RR11*RR12*SB + CB*(Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR12**INT(2.D0)) + &
  &Lam7*DBLE(RR13**INT(2.D0)))) - 1.D0*CB*(2.D0*CB*Lam5*RR11*RR12 + SB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR11**INT(2.D0)) + Lam2*DB&
  &LE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS2S2S1S1f2112 = SB*(CB*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR13*RR23) + Lam5*(RR12*RR21 + RR11*RR22)*SB&
  &) - 1.D0*CB*(CB*Lam5*(RR12*RR21 + RR11*RR22) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23)*SB)
CS2S2S1S1f2113 = SB*(CB*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR13*RR33) + Lam5*(RR12*RR31 + RR11*RR32)*SB&
  &) - 1.D0*CB*(CB*Lam5*(RR12*RR31 + RR11*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33)*SB)
CS2S2S1S1f2121 = SB*(CB*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR13*RR23) + Lam5*(RR12*RR21 + RR11*RR22)*SB&
  &) - 1.D0*CB*(CB*Lam5*(RR12*RR21 + RR11*RR22) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23)*SB)
CS2S2S1S1f2122 = SB*(2.D0*Lam5*RR21*RR22*SB + CB*(Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR22**INT(2.D0)) + &
  &Lam7*DBLE(RR23**INT(2.D0)))) - 1.D0*CB*(2.D0*CB*Lam5*RR21*RR22 + SB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR21**INT(2.D0)) + Lam2*DB&
  &LE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS2S2S1S1f2123 = SB*(CB*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR23*RR33) + Lam5*(RR22*RR31 + RR21*RR32)*SB&
  &) - 1.D0*CB*(CB*Lam5*(RR22*RR31 + RR21*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33)*SB)
CS2S2S1S1f2131 = SB*(CB*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR13*RR33) + Lam5*(RR12*RR31 + RR11*RR32)*SB&
  &) - 1.D0*CB*(CB*Lam5*(RR12*RR31 + RR11*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33)*SB)
CS2S2S1S1f2132 = SB*(CB*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR23*RR33) + Lam5*(RR22*RR31 + RR21*RR32)*SB&
  &) - 1.D0*CB*(CB*Lam5*(RR22*RR31 + RR21*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33)*SB)
CS2S2S1S1f2133 = SB*(2.D0*Lam5*RR31*RR32*SB + CB*(Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR32**INT(2.D0)) + &
  &Lam7*DBLE(RR33**INT(2.D0)))) - 1.D0*CB*(2.D0*CB*Lam5*RR31*RR32 + SB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR31**INT(2.D0)) + Lam2*DB&
  &LE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS2S2S1S1f2211 = SB*(2.D0*CB*Lam5*RR11*RR12 - 1.D0*SB*(Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR12**INT(2.D0&
  &)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1.D0*CB*(-2.D0*Lam5*RR11*RR12*SB + CB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR11**INT(2.D0)) + L&
  &am2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS2S2S1S1f2212 = -1.D0*CB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23) - 1.D0*Lam5*(RR12*RR21 + RR&
  &11*RR22)*SB) + SB*(CB*Lam5*(RR12*RR21 + RR11*RR22) - 1.D0*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR13*RR&
  &23)*SB)
CS2S2S1S1f2213 = -1.D0*CB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33) - 1.D0*Lam5*(RR12*RR31 + RR&
  &11*RR32)*SB) + SB*(CB*Lam5*(RR12*RR31 + RR11*RR32) - 1.D0*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR13*RR&
  &33)*SB)
CS2S2S1S1f2221 = -1.D0*CB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23) - 1.D0*Lam5*(RR12*RR21 + RR&
  &11*RR22)*SB) + SB*(CB*Lam5*(RR12*RR21 + RR11*RR22) - 1.D0*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR13*RR&
  &23)*SB)
CS2S2S1S1f2222 = SB*(2.D0*CB*Lam5*RR21*RR22 - 1.D0*SB*(Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR22**INT(2.D0&
  &)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1.D0*CB*(-2.D0*Lam5*RR21*RR22*SB + CB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR21**INT(2.D0)) + L&
  &am2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS2S2S1S1f2223 = -1.D0*CB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33) - 1.D0*Lam5*(RR22*RR31 + RR&
  &21*RR32)*SB) + SB*(CB*Lam5*(RR22*RR31 + RR21*RR32) - 1.D0*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR23*RR&
  &33)*SB)
CS2S2S1S1f2231 = -1.D0*CB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33) - 1.D0*Lam5*(RR12*RR31 + RR&
  &11*RR32)*SB) + SB*(CB*Lam5*(RR12*RR31 + RR11*RR32) - 1.D0*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR13*RR&
  &33)*SB)
CS2S2S1S1f2232 = -1.D0*CB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33) - 1.D0*Lam5*(RR22*RR31 + RR&
  &21*RR32)*SB) + SB*(CB*Lam5*(RR22*RR31 + RR21*RR32) - 1.D0*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR23*RR&
  &33)*SB)
CS2S2S1S1f2233 = SB*(2.D0*CB*Lam5*RR31*RR32 - 1.D0*SB*(Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR32**INT(2.D0&
  &)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1.D0*CB*(-2.D0*Lam5*RR31*RR32*SB + CB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR31**INT(2.D0)) + L&
  &am2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))

CS2S2S3S3f1111 = 0.5D0*(-1.D0*SB*(2.D0*CB2*(Lam4 + Lam5)*SB + 2.D0*SB*(CB2*Lam3 + Lam2*SB2)) - 1.D0*CB*(2.D0*CB*(Lam4 + Lam5)*SB2&
  & + 2.D0*CB*(CB2*Lam1 + Lam3*SB2)))
CS2S2S3S3f1112 = 0.5D0*(-1.D0*SB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*&
  &(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f1121 = 0.5D0*(-1.D0*SB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*&
  &(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f1122 = 0.5D0*(-1.D0*CB*(-2.D0*CB*(Lam4 + Lam5)*SB2 + 2.D0*CB*(CB2*Lam3 + Lam1*SB2)) - 1.D0*SB*(-2.D0*CB2*(Lam4 + Lam5)*&
  &SB + 2.D0*SB*(CB2*Lam2 + Lam3*SB2)))
CS2S2S3S3f1211 = 0.5D0*(-1.D0*SB*(-2.D0*CB*(Lam4 + Lam5)*SB2 + 2.D0*CB*(CB2*Lam3 + Lam2*SB2)) - 1.D0*CB*(2.D0*CB2*(Lam4 + Lam5)*S&
  &B - 2.D0*SB*(CB2*Lam1 + Lam3*SB2)))
CS2S2S3S3f1212 = 0.5D0*(-1.D0*CB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*SB*(2.D0*C&
  &B*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) - 1.D0*(Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f1221 = 0.5D0*(-1.D0*CB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*SB*(2.D0*C&
  &B*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) - 1.D0*(Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f1222 = 0.5D0*(-1.D0*CB*(-2.D0*CB2*(Lam4 + Lam5)*SB - 2.D0*SB*(CB2*Lam3 + Lam1*SB2)) - 1.D0*SB*(2.D0*CB*(Lam4 + Lam5)*SB&
  &2 + 2.D0*CB*(CB2*Lam2 + Lam3*SB2)))
CS2S2S3S3f2111 = 0.5D0*(-1.D0*CB*(2.D0*CB2*(Lam4 + Lam5)*SB + 2.D0*SB*(CB2*Lam3 + Lam2*SB2)) + SB*(2.D0*CB*(Lam4 + Lam5)*SB2 + 2.&
  &D0*CB*(CB2*Lam1 + Lam3*SB2)))
CS2S2S3S3f2112 = 0.5D0*(-1.D0*CB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) + SB*(2.D0*CB*(-1.D&
  &0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f2121 = 0.5D0*(-1.D0*CB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) + SB*(2.D0*CB*(-1.D&
  &0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f2122 = 0.5D0*(SB*(-2.D0*CB*(Lam4 + Lam5)*SB2 + 2.D0*CB*(CB2*Lam3 + Lam1*SB2)) - 1.D0*CB*(-2.D0*CB2*(Lam4 + Lam5)*SB + 2&
  &.D0*SB*(CB2*Lam2 + Lam3*SB2)))
CS2S2S3S3f2211 = 0.5D0*(-1.D0*CB*(-2.D0*CB*(Lam4 + Lam5)*SB2 + 2.D0*CB*(CB2*Lam3 + Lam2*SB2)) + SB*(2.D0*CB2*(Lam4 + Lam5)*SB - 2&
  &.D0*SB*(CB2*Lam1 + Lam3*SB2)))
CS2S2S3S3f2212 = 0.5D0*(SB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*(CB*&
  &Lam2*SB - 1.D0*CB*Lam3*SB) - 1.D0*(Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f2221 = 0.5D0*(SB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*(CB*&
  &Lam2*SB - 1.D0*CB*Lam3*SB) - 1.D0*(Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f2222 = 0.5D0*(SB*(-2.D0*CB2*(Lam4 + Lam5)*SB - 2.D0*SB*(CB2*Lam3 + Lam1*SB2)) - 1.D0*CB*(2.D0*CB*(Lam4 + Lam5)*SB2 + 2.&
  &D0*CB*(CB2*Lam2 + Lam3*SB2)))

CS1S1S3S3f1111 = 0.5D0*(-1.D0*RR12*(2.D0*CB*(Lam4 + Lam5)*RR11*SB + 2.D0*RR12*(CB2*Lam3 + Lam2*SB2)) - 1.D0*RR11*(2.D0*CB*(Lam4 +&
  & Lam5)*RR12*SB + 2.D0*RR11*(CB2*Lam1 + Lam3*SB2)) - 2.D0*(CB2*Lam7 + Lam8*SB2)*DBLE(RR13**INT(2.D0)))
CS1S1S3S3f1112 = 0.5D0*(-1.D0*RR12*(2.D0*RR12*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (Lam4 + Lam5)*RR11*(CB2 - 1.D0*SB2)) - 1.D0*RR11*(&
  &2.D0*RR11*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR12*(CB2 - 1.D0*SB2)) - 2.D0*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*DBLE(R&
  &R13**INT(2.D0)))
CS1S1S3S3f1121 = 0.5D0*(-1.D0*RR12*(2.D0*RR12*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (Lam4 + Lam5)*RR11*(CB2 - 1.D0*SB2)) - 1.D0*RR11*(&
  &2.D0*RR11*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR12*(CB2 - 1.D0*SB2)) - 2.D0*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*DBLE(R&
  &R13**INT(2.D0)))
CS1S1S3S3f1122 = 0.5D0*(-1.D0*RR11*(-2.D0*CB*(Lam4 + Lam5)*RR12*SB + 2.D0*RR11*(CB2*Lam3 + Lam1*SB2)) - 1.D0*RR12*(-2.D0*CB*(Lam4&
  & + Lam5)*RR11*SB + 2.D0*RR12*(CB2*Lam2 + Lam3*SB2)) - 2.D0*(CB2*Lam8 + Lam7*SB2)*DBLE(RR13**INT(2.D0)))
CS1S1S3S3f1211 = 0.5D0*(-2.D0*RR13*RR23*(CB2*Lam7 + Lam8*SB2) - 1.D0*RR12*(2.D0*CB*(Lam4 + Lam5)*RR21*SB + 2.D0*RR22*(CB2*Lam3 + &
  &Lam2*SB2)) - 1.D0*RR11*(2.D0*CB*(Lam4 + Lam5)*RR22*SB + 2.D0*RR21*(CB2*Lam1 + Lam3*SB2)))
CS1S1S3S3f1212 = 0.5D0*(-2.D0*RR13*RR23*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR12*(2.D0*RR22*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR21*(CB2 - 1.D0*SB2)) - 1.D0*RR11*(2.D0*RR21*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR22*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f1221 = 0.5D0*(-2.D0*RR13*RR23*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR12*(2.D0*RR22*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR21*(CB2 - 1.D0*SB2)) - 1.D0*RR11*(2.D0*RR21*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR22*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f1222 = 0.5D0*(-2.D0*RR13*RR23*(CB2*Lam8 + Lam7*SB2) - 1.D0*RR11*(-2.D0*CB*(Lam4 + Lam5)*RR22*SB + 2.D0*RR21*(CB2*Lam3 +&
  & Lam1*SB2)) - 1.D0*RR12*(-2.D0*CB*(Lam4 + Lam5)*RR21*SB + 2.D0*RR22*(CB2*Lam2 + Lam3*SB2)))
CS1S1S3S3f1311 = 0.5D0*(-2.D0*RR13*RR33*(CB2*Lam7 + Lam8*SB2) - 1.D0*RR12*(2.D0*CB*(Lam4 + Lam5)*RR31*SB + 2.D0*RR32*(CB2*Lam3 + &
  &Lam2*SB2)) - 1.D0*RR11*(2.D0*CB*(Lam4 + Lam5)*RR32*SB + 2.D0*RR31*(CB2*Lam1 + Lam3*SB2)))
CS1S1S3S3f1312 = 0.5D0*(-2.D0*RR13*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR12*(2.D0*RR32*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR31*(CB2 - 1.D0*SB2)) - 1.D0*RR11*(2.D0*RR31*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR32*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f1321 = 0.5D0*(-2.D0*RR13*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR12*(2.D0*RR32*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR31*(CB2 - 1.D0*SB2)) - 1.D0*RR11*(2.D0*RR31*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR32*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f1322 = 0.5D0*(-2.D0*RR13*RR33*(CB2*Lam8 + Lam7*SB2) - 1.D0*RR11*(-2.D0*CB*(Lam4 + Lam5)*RR32*SB + 2.D0*RR31*(CB2*Lam3 +&
  & Lam1*SB2)) - 1.D0*RR12*(-2.D0*CB*(Lam4 + Lam5)*RR31*SB + 2.D0*RR32*(CB2*Lam2 + Lam3*SB2)))
CS1S1S3S3f2111 = 0.5D0*(-2.D0*RR13*RR23*(CB2*Lam7 + Lam8*SB2) - 1.D0*RR22*(2.D0*CB*(Lam4 + Lam5)*RR11*SB + 2.D0*RR12*(CB2*Lam3 + &
  &Lam2*SB2)) - 1.D0*RR21*(2.D0*CB*(Lam4 + Lam5)*RR12*SB + 2.D0*RR11*(CB2*Lam1 + Lam3*SB2)))
CS1S1S3S3f2112 = 0.5D0*(-2.D0*RR13*RR23*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR22*(2.D0*RR12*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR11*(CB2 - 1.D0*SB2)) - 1.D0*RR21*(2.D0*RR11*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR12*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f2121 = 0.5D0*(-2.D0*RR13*RR23*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR22*(2.D0*RR12*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR11*(CB2 - 1.D0*SB2)) - 1.D0*RR21*(2.D0*RR11*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR12*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f2122 = 0.5D0*(-2.D0*RR13*RR23*(CB2*Lam8 + Lam7*SB2) - 1.D0*RR21*(-2.D0*CB*(Lam4 + Lam5)*RR12*SB + 2.D0*RR11*(CB2*Lam3 +&
  & Lam1*SB2)) - 1.D0*RR22*(-2.D0*CB*(Lam4 + Lam5)*RR11*SB + 2.D0*RR12*(CB2*Lam2 + Lam3*SB2)))
CS1S1S3S3f2211 = 0.5D0*(-1.D0*RR22*(2.D0*CB*(Lam4 + Lam5)*RR21*SB + 2.D0*RR22*(CB2*Lam3 + Lam2*SB2)) - 1.D0*RR21*(2.D0*CB*(Lam4 +&
  & Lam5)*RR22*SB + 2.D0*RR21*(CB2*Lam1 + Lam3*SB2)) - 2.D0*(CB2*Lam7 + Lam8*SB2)*DBLE(RR23**INT(2.D0)))
CS1S1S3S3f2212 = 0.5D0*(-1.D0*RR22*(2.D0*RR22*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (Lam4 + Lam5)*RR21*(CB2 - 1.D0*SB2)) - 1.D0*RR21*(&
  &2.D0*RR21*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR22*(CB2 - 1.D0*SB2)) - 2.D0*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*DBLE(R&
  &R23**INT(2.D0)))
CS1S1S3S3f2221 = 0.5D0*(-1.D0*RR22*(2.D0*RR22*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (Lam4 + Lam5)*RR21*(CB2 - 1.D0*SB2)) - 1.D0*RR21*(&
  &2.D0*RR21*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR22*(CB2 - 1.D0*SB2)) - 2.D0*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*DBLE(R&
  &R23**INT(2.D0)))
CS1S1S3S3f2222 = 0.5D0*(-1.D0*RR21*(-2.D0*CB*(Lam4 + Lam5)*RR22*SB + 2.D0*RR21*(CB2*Lam3 + Lam1*SB2)) - 1.D0*RR22*(-2.D0*CB*(Lam4&
  & + Lam5)*RR21*SB + 2.D0*RR22*(CB2*Lam2 + Lam3*SB2)) - 2.D0*(CB2*Lam8 + Lam7*SB2)*DBLE(RR23**INT(2.D0)))
CS1S1S3S3f2311 = 0.5D0*(-2.D0*RR23*RR33*(CB2*Lam7 + Lam8*SB2) - 1.D0*RR22*(2.D0*CB*(Lam4 + Lam5)*RR31*SB + 2.D0*RR32*(CB2*Lam3 + &
  &Lam2*SB2)) - 1.D0*RR21*(2.D0*CB*(Lam4 + Lam5)*RR32*SB + 2.D0*RR31*(CB2*Lam1 + Lam3*SB2)))
CS1S1S3S3f2312 = 0.5D0*(-2.D0*RR23*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR22*(2.D0*RR32*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR31*(CB2 - 1.D0*SB2)) - 1.D0*RR21*(2.D0*RR31*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR32*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f2321 = 0.5D0*(-2.D0*RR23*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR22*(2.D0*RR32*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR31*(CB2 - 1.D0*SB2)) - 1.D0*RR21*(2.D0*RR31*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR32*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f2322 = 0.5D0*(-2.D0*RR23*RR33*(CB2*Lam8 + Lam7*SB2) - 1.D0*RR21*(-2.D0*CB*(Lam4 + Lam5)*RR32*SB + 2.D0*RR31*(CB2*Lam3 +&
  & Lam1*SB2)) - 1.D0*RR22*(-2.D0*CB*(Lam4 + Lam5)*RR31*SB + 2.D0*RR32*(CB2*Lam2 + Lam3*SB2)))
CS1S1S3S3f3111 = 0.5D0*(-2.D0*RR13*RR33*(CB2*Lam7 + Lam8*SB2) - 1.D0*RR32*(2.D0*CB*(Lam4 + Lam5)*RR11*SB + 2.D0*RR12*(CB2*Lam3 + &
  &Lam2*SB2)) - 1.D0*RR31*(2.D0*CB*(Lam4 + Lam5)*RR12*SB + 2.D0*RR11*(CB2*Lam1 + Lam3*SB2)))
CS1S1S3S3f3112 = 0.5D0*(-2.D0*RR13*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR32*(2.D0*RR12*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR11*(CB2 - 1.D0*SB2)) - 1.D0*RR31*(2.D0*RR11*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR12*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f3121 = 0.5D0*(-2.D0*RR13*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR32*(2.D0*RR12*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR11*(CB2 - 1.D0*SB2)) - 1.D0*RR31*(2.D0*RR11*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR12*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f3122 = 0.5D0*(-2.D0*RR13*RR33*(CB2*Lam8 + Lam7*SB2) - 1.D0*RR31*(-2.D0*CB*(Lam4 + Lam5)*RR12*SB + 2.D0*RR11*(CB2*Lam3 +&
  & Lam1*SB2)) - 1.D0*RR32*(-2.D0*CB*(Lam4 + Lam5)*RR11*SB + 2.D0*RR12*(CB2*Lam2 + Lam3*SB2)))
CS1S1S3S3f3211 = 0.5D0*(-2.D0*RR23*RR33*(CB2*Lam7 + Lam8*SB2) - 1.D0*RR32*(2.D0*CB*(Lam4 + Lam5)*RR21*SB + 2.D0*RR22*(CB2*Lam3 + &
  &Lam2*SB2)) - 1.D0*RR31*(2.D0*CB*(Lam4 + Lam5)*RR22*SB + 2.D0*RR21*(CB2*Lam1 + Lam3*SB2)))
CS1S1S3S3f3212 = 0.5D0*(-2.D0*RR23*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR32*(2.D0*RR22*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR21*(CB2 - 1.D0*SB2)) - 1.D0*RR31*(2.D0*RR21*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR22*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f3221 = 0.5D0*(-2.D0*RR23*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR32*(2.D0*RR22*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR21*(CB2 - 1.D0*SB2)) - 1.D0*RR31*(2.D0*RR21*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR22*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f3222 = 0.5D0*(-2.D0*RR23*RR33*(CB2*Lam8 + Lam7*SB2) - 1.D0*RR31*(-2.D0*CB*(Lam4 + Lam5)*RR22*SB + 2.D0*RR21*(CB2*Lam3 +&
  & Lam1*SB2)) - 1.D0*RR32*(-2.D0*CB*(Lam4 + Lam5)*RR21*SB + 2.D0*RR22*(CB2*Lam2 + Lam3*SB2)))
CS1S1S3S3f3311 = 0.5D0*(-1.D0*RR32*(2.D0*CB*(Lam4 + Lam5)*RR31*SB + 2.D0*RR32*(CB2*Lam3 + Lam2*SB2)) - 1.D0*RR31*(2.D0*CB*(Lam4 +&
  & Lam5)*RR32*SB + 2.D0*RR31*(CB2*Lam1 + Lam3*SB2)) - 2.D0*(CB2*Lam7 + Lam8*SB2)*DBLE(RR33**INT(2.D0)))
CS1S1S3S3f3312 = 0.5D0*(-1.D0*RR32*(2.D0*RR32*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (Lam4 + Lam5)*RR31*(CB2 - 1.D0*SB2)) - 1.D0*RR31*(&
  &2.D0*RR31*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR32*(CB2 - 1.D0*SB2)) - 2.D0*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*DBLE(R&
  &R33**INT(2.D0)))
CS1S1S3S3f3321 = 0.5D0*(-1.D0*RR32*(2.D0*RR32*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (Lam4 + Lam5)*RR31*(CB2 - 1.D0*SB2)) - 1.D0*RR31*(&
  &2.D0*RR31*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR32*(CB2 - 1.D0*SB2)) - 2.D0*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*DBLE(R&
  &R33**INT(2.D0)))
CS1S1S3S3f3322 = 0.5D0*(-1.D0*RR31*(-2.D0*CB*(Lam4 + Lam5)*RR32*SB + 2.D0*RR31*(CB2*Lam3 + Lam1*SB2)) - 1.D0*RR32*(-2.D0*CB*(Lam4&
  & + Lam5)*RR31*SB + 2.D0*RR32*(CB2*Lam2 + Lam3*SB2)) - 2.D0*(CB2*Lam8 + Lam7*SB2)*DBLE(RR33**INT(2.D0)))

    else
        vevCalc = 2D0*MW*SW/EL

        CB = dcos(beta)
        SB = dsin(beta)
        TB = dtan(beta)

        CA1 = dcos(alpha1)
        CA2 = dcos(alpha2)
        CA3 = dcos(alpha3)
        SA1 = dsin(alpha1)
        SA2 = dsin(alpha2)
        SA3 = dsin(alpha3)

        ! hdecayLam1 = 1D0/vevCalc**2/CB**2*( -SB**2*(2D0*m12squared/S2B) + SA**2*Mh0**2 + CA**2*MHH**2 )
        ! hdecayLam2 = 1D0/vevCalc**2/SB**2*( -CB**2*(2D0*m12squared/S2B) + CA**2*Mh0**2 + SA**2*MHH**2 )
        ! hdecayLam3 = 1D0/vevCalc**2*( -(2D0*m12squared/S2B) + 2D0*MHp**2 + S2A/S2B*(MHH**2 - Mh0**2) )
        ! hdecayLam4 = 1D0/vevCalc**2*( (2D0*m12squared/S2B) + MA0**2 - 2D0*MHp**2 )
        ! hdecayLam5 = 1D0/vevCalc**2*( (2D0*m12squared/S2B) - MA0**2 )
        
        ! Lambda5 = EL2*m12squared/(2D0*SW2*MW2*SB*CB)
        if (TypeOf2HDM == 1) then
            YukS1Lep1 = CA2*SA1/SB
            YukS1Lep2 = ( CA1*CA3 - SA1*SA2*SA3 )/SB
            YukS1Lep3 = -( CA1*SA3 + CA3*SA1*SA2 )/SB
            YukS2Lep1 = 1D0
            YukS2Lep2 = CB/SB
            YukS3Lep1 = 1D0
            YukS3Lep2 = CB/SB
            YukS1Quark1 = CA2*SA1/SB
            YukS1Quark2 = ( CA1*CA3 - SA1*SA2*SA3 )/SB
            YukS1Quark3 = -( CA1*SA3 + CA3*SA1*SA2 )/SB
            YukS2Quark1 = 1D0
            YukS2Quark2 = CB/SB
            YukS3Quark1 = 1D0
            YukS3Quark2 = CB/SB
        else if (TypeOf2HDM == 2) then
            YukS1Lep1 = CA1*CA2/CB
            YukS1Lep2 = -( CA3*SA1 + CA1*SA2*SA3 )/CB
            YukS1Lep3 = ( SA1*SA3 - CA1*CA3*SA2 )/CB
            YukS2Lep1 = 1D0
            YukS2Lep2 = -SB/CB
            YukS3Lep1 = 1D0
            YukS3Lep2 = -SB/CB
            YukS1Quark1 = CA1*CA2/CB
            YukS1Quark2 = -( CA3*SA1 + CA1*SA2*SA3 )/CB
            YukS1Quark3 = ( SA1*SA3 - CA1*CA3*SA2 )/CB
            YukS2Quark1 = 1D0
            YukS2Quark2 = -SB/CB
            YukS3Quark1 = 1D0
            YukS3Quark2 = -SB/CB
        else if (TypeOf2HDM == 3) then
            YukS1Lep1 = CA1*CA2/CB
            YukS1Lep2 = -( CA3*SA1 + CA1*SA2*SA3 )/CB
            YukS1Lep3 = ( SA1*SA3 - CA1*CA3*SA2 )/CB
            YukS2Lep1 = 1D0
            YukS2Lep2 = -SB/CB
            YukS3Lep1 = 1D0
            YukS3Lep2 = -SB/CB
            YukS1Quark1 = CA2*SA1/SB
            YukS1Quark2 = ( CA1*CA3 - SA1*SA2*SA3 )/SB
            YukS1Quark3 = -( CA1*SA3 + CA3*SA1*SA2 )/SB
            YukS2Quark1 = 1D0
            YukS2Quark2 = CB/SB
            YukS3Quark1 = 1D0
            YukS3Quark2 = CB/SB
        else
            YukS1Lep1 = CA2*SA1/SB
            YukS1Lep2 = ( CA1*CA3 - SA1*SA2*SA3 )/SB
            YukS1Lep3 = -( CA1*SA3 + CA3*SA1*SA2 )/SB
            YukS2Lep1 = 1D0
            YukS2Lep2 = CB/SB
            YukS3Lep1 = 1D0
            YukS3Lep2 = CB/SB
            YukS1Quark1 = CA1*CA2/CB
            YukS1Quark2 = -( CA3*SA1 + CA1*SA2*SA3 )/CB
            YukS1Quark3 = ( SA1*SA3 - CA1*CA3*SA2 )/CB
            YukS2Quark1 = 1D0
            YukS2Quark2 = -SB/CB
            YukS3Quark1 = 1D0
            YukS3Quark2 = -SB/CB
        end if

        CA12 = CA1**2
        CA22 = CA2**2
        CA32 = CA3**2
        SA12 = SA1**2
        SA22 = SA2**2
        SA32 = SA3**2
        TB2 = TB**2
        SB2 = SB**2
        CB2 = CB**2

        ! Set the scalar couplings
RR11 = CA1*CA2
RR12 = CA2*SA1
RR13 = SA2
RR21 = -1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3
RR22 = CA1*CA3 - 1.D0*SA1*SA2*SA3
RR23 = CA2*SA3
RR31 = -1.D0*CA1*CA3*SA2 + SA1*SA3
RR32 = -1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3
RR33 = CA2*CA3

Lam1 = (0.25D0*EL2*((-1.D0*m12squared*SB)/CB + MH12*DBLE(RR11**INT(2.D0)) + MH22*DBLE(RR21**INT(2.D0)) + MH32*DBLE(RR31**INT(2.D0&
  &))))/(CB2*MW2*SW2)
Lam2 = (0.25D0*EL2*((-1.D0*CB*m12squared)/SB + MH12*DBLE(RR12**INT(2.D0)) + MH22*DBLE(RR22**INT(2.D0)) + MH32*DBLE(RR32**INT(2.D0&
  &))))/(MW2*SB2*SW2)
Lam3 = (0.25D0*EL2*(2.D0*MHp2 - (1.D0*m12squared)/(CB*SB) + (MH12*RR11*RR12 + MH22*RR21*RR22 + MH32*RR31*RR32)/(CB*SB)))/(MW2*SW2&
  &)
Lam4 = (0.25D0*EL2*(MA02 - 2.D0*MHp2 + m12squared/(CB*SB)))/(MW2*SW2)
Lam5 = (0.25D0*EL2*(-1.D0*MA02 + m12squared/(CB*SB)))/(MW2*SW2)
Lam6 = (MH12*DBLE(RR13**INT(2.D0)) + MH22*DBLE(RR23**INT(2.D0)) + MH32*DBLE(RR33**INT(2.D0)))*DBLE(vS**INT(-2.D0))
Lam7 = (0.5D0*EL*(MH12*RR11*RR13 + MH22*RR21*RR23 + MH32*RR31*RR33))/(CB*MW*SW*vS)
Lam8 = (0.5D0*EL*(MH12*RR12*RR13 + MH22*RR22*RR23 + MH32*RR32*RR33))/(MW*SB*SW*vS)

CS1S1S1f111 = -1.D0*RR13*(Lam7*RR11*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR12*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR11*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f112 = -1.D0*RR13*(Lam7*RR11*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR12*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR11*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f113 = -1.D0*RR13*(Lam7*RR11*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR12*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR11*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f121 = -1.D0*RR13*(Lam7*RR21*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR22*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR21*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f122 = -1.D0*RR13*(Lam7*RR21*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR22*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR21*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f123 = -1.D0*RR13*(Lam7*RR21*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR22*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR21*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f131 = -1.D0*RR13*(Lam7*RR31*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR32*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR31*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f132 = -1.D0*RR13*(Lam7*RR31*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR32*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR31*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f133 = -1.D0*RR13*(Lam7*RR31*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR32*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR31*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f211 = -1.D0*RR23*(Lam7*RR11*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR12*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR11*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f212 = -1.D0*RR23*(Lam7*RR11*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR12*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR11*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f213 = -1.D0*RR23*(Lam7*RR11*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR12*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR11*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f221 = -1.D0*RR23*(Lam7*RR21*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR22*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR21*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f222 = -1.D0*RR23*(Lam7*RR21*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR22*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR21*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f223 = -1.D0*RR23*(Lam7*RR21*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR22*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR21*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f231 = -1.D0*RR23*(Lam7*RR31*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR32*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR31*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f232 = -1.D0*RR23*(Lam7*RR31*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR32*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR31*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f233 = -1.D0*RR23*(Lam7*RR31*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR32*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR31*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f311 = -1.D0*RR33*(Lam7*RR11*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR12*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR11*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f312 = -1.D0*RR33*(Lam7*RR11*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR12*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR11*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f313 = -1.D0*RR33*(Lam7*RR11*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR12*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR13*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR12*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR13*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR11*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR11*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR13*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR12*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f321 = -1.D0*RR33*(Lam7*RR21*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR22*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR21*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f322 = -1.D0*RR33*(Lam7*RR21*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR22*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR21*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f323 = -1.D0*RR33*(Lam7*RR21*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR22*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR23*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR22*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR23*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR21*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR21*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR23*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR22*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS1S1S1f331 = -1.D0*RR33*(Lam7*RR31*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + Lam8*RR32*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR11*SW)/EL + (2.D0*Lam8*MW*RR12*SB*SW)/EL + 3.D0*Lam6*RR13*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR13*SW)/EL + RR11*vs) + RR31*((6.D0*CB*Lam1*MW*RR11*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR12*&
  &SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR13*SB*SW)/EL + RR12*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR1&
  &1*SW)/EL + (6.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS1S1S1f332 = -1.D0*RR33*(Lam7*RR31*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + Lam8*RR32*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR21*SW)/EL + (2.D0*Lam8*MW*RR22*SB*SW)/EL + 3.D0*Lam6*RR23*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR23*SW)/EL + RR21*vs) + RR31*((6.D0*CB*Lam1*MW*RR21*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR22*&
  &SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR23*SB*SW)/EL + RR22*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR2&
  &1*SW)/EL + (6.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS1S1S1f333 = -1.D0*RR33*(Lam7*RR31*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + Lam8*RR32*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR33*((&
  &2.D0*CB*Lam7*MW*RR31*SW)/EL + (2.D0*Lam8*MW*RR32*SB*SW)/EL + 3.D0*Lam6*RR33*vs)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR32*((2.D0&
  &*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam7*RR33*((2.D0*CB*MW*RR33*SW)/EL + RR31*vs) + RR31*((6.D0*CB*Lam1*MW*RR31*SW&
  &)/EL + (2.D0*(Lam3 + Lam4 + Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR31*((2.D0*CB*MW*RR32*&
  &SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + Lam8*RR33*((2.D0*MW*RR33*SB*SW)/EL + RR32*vs) + RR32*((2.D0*CB*(Lam3 + Lam4 + Lam5)*MW*RR3&
  &1*SW)/EL + (6.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))

CS2S2S1f111 = -1.D0*CB*(Lam5*SB*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + CB*((2.D0*CB*Lam1*MW*RR11*SW)/EL + (2.D0*(L&
  &am3 + Lam4 - 1.D0*Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*SB*(CB*Lam5*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/&
  &EL) + SB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR11*SW)/EL + (2.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS2S2S1f112 = -1.D0*CB*(Lam5*SB*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + CB*((2.D0*CB*Lam1*MW*RR21*SW)/EL + (2.D0*(L&
  &am3 + Lam4 - 1.D0*Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*SB*(CB*Lam5*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/&
  &EL) + SB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR21*SW)/EL + (2.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS2S2S1f113 = -1.D0*CB*(Lam5*SB*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + CB*((2.D0*CB*Lam1*MW*RR31*SW)/EL + (2.D0*(L&
  &am3 + Lam4 - 1.D0*Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*SB*(CB*Lam5*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/&
  &EL) + SB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR31*SW)/EL + (2.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS2S2S1f121 = -1.D0*CB*(CB*Lam5*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) - 1.D0*SB*((2.D0*CB*Lam1*MW*RR11*SW)/EL + (2.&
  &D0*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*SB*(-1.D0*Lam5*SB*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*R&
  &R11*SB*SW)/EL) + CB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR11*SW)/EL + (2.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS2S2S1f122 = -1.D0*CB*(CB*Lam5*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) - 1.D0*SB*((2.D0*CB*Lam1*MW*RR21*SW)/EL + (2.&
  &D0*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*SB*(-1.D0*Lam5*SB*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*R&
  &R21*SB*SW)/EL) + CB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR21*SW)/EL + (2.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS2S2S1f123 = -1.D0*CB*(CB*Lam5*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) - 1.D0*SB*((2.D0*CB*Lam1*MW*RR31*SW)/EL + (2.&
  &D0*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*SB*(-1.D0*Lam5*SB*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*R&
  &R31*SB*SW)/EL) + CB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR31*SW)/EL + (2.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS2S2S1f211 = SB*(Lam5*SB*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + CB*((2.D0*CB*Lam1*MW*RR11*SW)/EL + (2.D0*(Lam3 + &
  &Lam4 - 1.D0*Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*CB*(CB*Lam5*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) + &
  &SB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR11*SW)/EL + (2.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS2S2S1f212 = SB*(Lam5*SB*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + CB*((2.D0*CB*Lam1*MW*RR21*SW)/EL + (2.D0*(Lam3 + &
  &Lam4 - 1.D0*Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*CB*(CB*Lam5*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) + &
  &SB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR21*SW)/EL + (2.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS2S2S1f213 = SB*(Lam5*SB*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + CB*((2.D0*CB*Lam1*MW*RR31*SW)/EL + (2.D0*(Lam3 + &
  &Lam4 - 1.D0*Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*CB*(CB*Lam5*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) + &
  &SB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR31*SW)/EL + (2.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))
CS2S2S1f221 = SB*(CB*Lam5*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB*SW)/EL) - 1.D0*SB*((2.D0*CB*Lam1*MW*RR11*SW)/EL + (2.D0*(La&
  &m3 + Lam4 - 1.D0*Lam5)*MW*RR12*SB*SW)/EL + Lam7*RR13*vs)) - 1.D0*CB*(-1.D0*Lam5*SB*((2.D0*CB*MW*RR12*SW)/EL + (2.D0*MW*RR11*SB&
  &*SW)/EL) + CB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR11*SW)/EL + (2.D0*Lam2*MW*RR12*SB*SW)/EL + Lam8*RR13*vs))
CS2S2S1f222 = SB*(CB*Lam5*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB*SW)/EL) - 1.D0*SB*((2.D0*CB*Lam1*MW*RR21*SW)/EL + (2.D0*(La&
  &m3 + Lam4 - 1.D0*Lam5)*MW*RR22*SB*SW)/EL + Lam7*RR23*vs)) - 1.D0*CB*(-1.D0*Lam5*SB*((2.D0*CB*MW*RR22*SW)/EL + (2.D0*MW*RR21*SB&
  &*SW)/EL) + CB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR21*SW)/EL + (2.D0*Lam2*MW*RR22*SB*SW)/EL + Lam8*RR23*vs))
CS2S2S1f223 = SB*(CB*Lam5*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB*SW)/EL) - 1.D0*SB*((2.D0*CB*Lam1*MW*RR31*SW)/EL + (2.D0*(La&
  &m3 + Lam4 - 1.D0*Lam5)*MW*RR32*SB*SW)/EL + Lam7*RR33*vs)) - 1.D0*CB*(-1.D0*Lam5*SB*((2.D0*CB*MW*RR32*SW)/EL + (2.D0*MW*RR31*SB&
  &*SW)/EL) + CB*((2.D0*CB*(Lam3 + Lam4 - 1.D0*Lam5)*MW*RR31*SW)/EL + (2.D0*Lam2*MW*RR32*SB*SW)/EL + Lam8*RR33*vs))

CS1S3S3f111 = 0.5D0*(-1.D0*RR12*(CB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + SB*((2.D0*CB2*(Lam4 + La&
  &m5)*MW*SW)/EL + (4.D0*Lam2*MW*SB2*SW)/EL)) - 1.D0*RR11*(SB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) &
  &+ CB*((4.D0*CB2*Lam1*MW*SW)/EL + (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR13*(CB2*Lam7 + Lam8*SB2)*vs)
CS1S3S3f112 = 0.5D0*(-1.D0*RR12*(SB*((4.D0*CB*Lam2*MW*SB*SW)/EL - (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + CB*((2.D0*CB2*(Lam4 + La&
  &m5)*MW*SW)/EL - (4.D0*Lam3*MW*SB2*SW)/EL)) - 1.D0*RR11*(CB*((-4.D0*CB*Lam1*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL)&
  & + SB*((4.D0*CB2*Lam3*MW*SW)/EL - (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR13*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*vs)
CS1S3S3f121 = 0.5D0*(-1.D0*RR12*(-1.D0*SB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + CB*((2.D0*CB2*(Lam&
  &4 + Lam5)*MW*SW)/EL + (4.D0*Lam2*MW*SB2*SW)/EL)) - 1.D0*RR11*(CB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW&
  &)/EL) - 1.D0*SB*((4.D0*CB2*Lam1*MW*SW)/EL + (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR13*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*vs&
  &)
CS1S3S3f122 = 0.5D0*(-1.D0*RR12*(CB*((4.D0*CB*Lam2*MW*SB*SW)/EL - (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) - 1.D0*SB*((2.D0*CB2*(Lam4&
  & + Lam5)*MW*SW)/EL - (4.D0*Lam3*MW*SB2*SW)/EL)) - 1.D0*RR11*(-1.D0*SB*((-4.D0*CB*Lam1*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW&
  &*SB*SW)/EL) + CB*((4.D0*CB2*Lam3*MW*SW)/EL - (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR13*(CB2*Lam8 + Lam7*SB2)*vs)
CS1S3S3f211 = 0.5D0*(-1.D0*RR22*(CB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + SB*((2.D0*CB2*(Lam4 + La&
  &m5)*MW*SW)/EL + (4.D0*Lam2*MW*SB2*SW)/EL)) - 1.D0*RR21*(SB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) &
  &+ CB*((4.D0*CB2*Lam1*MW*SW)/EL + (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR23*(CB2*Lam7 + Lam8*SB2)*vs)
CS1S3S3f212 = 0.5D0*(-1.D0*RR22*(SB*((4.D0*CB*Lam2*MW*SB*SW)/EL - (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + CB*((2.D0*CB2*(Lam4 + La&
  &m5)*MW*SW)/EL - (4.D0*Lam3*MW*SB2*SW)/EL)) - 1.D0*RR21*(CB*((-4.D0*CB*Lam1*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL)&
  & + SB*((4.D0*CB2*Lam3*MW*SW)/EL - (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR23*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*vs)
CS1S3S3f221 = 0.5D0*(-1.D0*RR22*(-1.D0*SB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + CB*((2.D0*CB2*(Lam&
  &4 + Lam5)*MW*SW)/EL + (4.D0*Lam2*MW*SB2*SW)/EL)) - 1.D0*RR21*(CB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW&
  &)/EL) - 1.D0*SB*((4.D0*CB2*Lam1*MW*SW)/EL + (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR23*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*vs&
  &)
CS1S3S3f222 = 0.5D0*(-1.D0*RR22*(CB*((4.D0*CB*Lam2*MW*SB*SW)/EL - (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) - 1.D0*SB*((2.D0*CB2*(Lam4&
  & + Lam5)*MW*SW)/EL - (4.D0*Lam3*MW*SB2*SW)/EL)) - 1.D0*RR21*(-1.D0*SB*((-4.D0*CB*Lam1*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW&
  &*SB*SW)/EL) + CB*((4.D0*CB2*Lam3*MW*SW)/EL - (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR23*(CB2*Lam8 + Lam7*SB2)*vs)
CS1S3S3f311 = 0.5D0*(-1.D0*RR32*(CB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + SB*((2.D0*CB2*(Lam4 + La&
  &m5)*MW*SW)/EL + (4.D0*Lam2*MW*SB2*SW)/EL)) - 1.D0*RR31*(SB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) &
  &+ CB*((4.D0*CB2*Lam1*MW*SW)/EL + (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR33*(CB2*Lam7 + Lam8*SB2)*vs)
CS1S3S3f312 = 0.5D0*(-1.D0*RR32*(SB*((4.D0*CB*Lam2*MW*SB*SW)/EL - (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + CB*((2.D0*CB2*(Lam4 + La&
  &m5)*MW*SW)/EL - (4.D0*Lam3*MW*SB2*SW)/EL)) - 1.D0*RR31*(CB*((-4.D0*CB*Lam1*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL)&
  & + SB*((4.D0*CB2*Lam3*MW*SW)/EL - (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*vs)
CS1S3S3f321 = 0.5D0*(-1.D0*RR32*(-1.D0*SB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) + CB*((2.D0*CB2*(Lam&
  &4 + Lam5)*MW*SW)/EL + (4.D0*Lam2*MW*SB2*SW)/EL)) - 1.D0*RR31*(CB*((4.D0*CB*Lam3*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW&
  &)/EL) - 1.D0*SB*((4.D0*CB2*Lam1*MW*SW)/EL + (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*vs&
  &)
CS1S3S3f322 = 0.5D0*(-1.D0*RR32*(CB*((4.D0*CB*Lam2*MW*SB*SW)/EL - (2.D0*CB*(Lam4 + Lam5)*MW*SB*SW)/EL) - 1.D0*SB*((2.D0*CB2*(Lam4&
  & + Lam5)*MW*SW)/EL - (4.D0*Lam3*MW*SB2*SW)/EL)) - 1.D0*RR31*(-1.D0*SB*((-4.D0*CB*Lam1*MW*SB*SW)/EL + (2.D0*CB*(Lam4 + Lam5)*MW&
  &*SB*SW)/EL) + CB*((4.D0*CB2*Lam3*MW*SW)/EL - (2.D0*(Lam4 + Lam5)*MW*SB2*SW)/EL)) - 2.D0*RR33*(CB2*Lam8 + Lam7*SB2)*vs)

CS1S1S1S1f1111 = -1.D0*RR13*(2.D0*Lam7*RR13*DBLE(RR11**INT(2.D0)) + 2.D0*Lam8*RR13*DBLE(RR12**INT(2.D0)) + RR13*(Lam7*DBLE(RR11**&
  &INT(2.D0)) + Lam8*DBLE(RR12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*DBLE(R&
  &R12**INT(2.D0)) + 2.D0*Lam7*RR11*DBLE(RR13**INT(2.D0)) + RR11*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR1&
  &2**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*DBLE(RR11**INT(2.D0)) + 2.D0*Lam8*RR1&
  &2*DBLE(RR13**INT(2.D0)) + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13*&
  &*INT(2.D0))))
CS1S1S1S1f1112 = -1.D0*RR13*(Lam7*RR11*(RR13*RR21 + RR11*RR23) + Lam8*RR12*(RR13*RR22 + RR12*RR23) + RR13*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR21 + RR11*RR22) + Lam7*RR13*(RR13*RR21 + RR11&
  &*RR23) + RR11*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR13*(RR13*RR22 + RR12*RR23) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f1113 = -1.D0*RR13*(Lam7*RR11*(RR13*RR31 + RR11*RR33) + Lam8*RR12*(RR13*RR32 + RR12*RR33) + RR13*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR31 + RR11*RR32) + Lam7*RR13*(RR13*RR31 + RR11&
  &*RR33) + RR11*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR13*(RR13*RR32 + RR12*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f1121 = -1.D0*RR13*(Lam7*RR11*(RR13*RR21 + RR11*RR23) + Lam8*RR12*(RR13*RR22 + RR12*RR23) + RR13*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR21 + RR11*RR22) + Lam7*RR13*(RR13*RR21 + RR11&
  &*RR23) + RR11*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR13*(RR13*RR22 + RR12*RR23) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f1122 = -1.D0*RR13*(2.D0*Lam7*RR11*RR21*RR23 + 2.D0*Lam8*RR12*RR22*RR23 + RR13*(Lam7*DBLE(RR21**INT(2.D0)) + Lam8*DBLE(R&
  &R22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*RR21*RR22 + 2.D0*Lam7*RR13*RR2&
  &1*RR23 + RR11*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR22**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1&
  &.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR21*RR22 + 2.D0*Lam8*RR13*RR22*RR23 + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS1S1S1S1f1123 = -1.D0*RR13*(Lam7*RR11*(RR23*RR31 + RR21*RR33) + Lam8*RR12*(RR23*RR32 + RR22*RR33) + RR13*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*(RR22*RR31 + RR21*RR32) + Lam7*RR13*(RR23*RR31 + RR21&
  &*RR33) + RR11*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR13*(RR23*RR32 + RR22*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f1131 = -1.D0*RR13*(Lam7*RR11*(RR13*RR31 + RR11*RR33) + Lam8*RR12*(RR13*RR32 + RR12*RR33) + RR13*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR31 + RR11*RR32) + Lam7*RR13*(RR13*RR31 + RR11&
  &*RR33) + RR11*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR13*(RR13*RR32 + RR12*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f1132 = -1.D0*RR13*(Lam7*RR11*(RR23*RR31 + RR21*RR33) + Lam8*RR12*(RR23*RR32 + RR22*RR33) + RR13*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR12*(RR22*RR31 + RR21*RR32) + Lam7*RR13*(RR23*RR31 + RR21&
  &*RR33) + RR11*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR13*(RR23*RR32 + RR22*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f1133 = -1.D0*RR13*(2.D0*Lam7*RR11*RR31*RR33 + 2.D0*Lam8*RR12*RR32*RR33 + RR13*(Lam7*DBLE(RR31**INT(2.D0)) + Lam8*DBLE(R&
  &R32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*RR31*RR32 + 2.D0*Lam7*RR13*RR3&
  &1*RR33 + RR11*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR32**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1&
  &.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR31*RR32 + 2.D0*Lam8*RR13*RR32*RR33 + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS1S1S1S1f1211 = -1.D0*RR13*(2.D0*Lam7*RR11*RR13*RR21 + 2.D0*Lam8*RR12*RR13*RR22 + RR23*(Lam7*DBLE(RR11**INT(2.D0)) + Lam8*DBLE(R&
  &R12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR22 + 2.D0*Lam7*RR11*RR1&
  &3*RR23 + RR21*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR12**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1&
  &.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR21 + 2.D0*Lam8*RR12*RR13*RR23 + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS1S1S1S1f1212 = -1.D0*RR13*(Lam7*RR21*(RR13*RR21 + RR11*RR23) + Lam8*RR22*(RR13*RR22 + RR12*RR23) + RR23*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR21 + RR11*RR22) + Lam7*RR23*(RR13*RR21 + RR11&
  &*RR23) + RR21*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR23*(RR13*RR22 + RR12*RR23) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f1213 = -1.D0*RR13*(Lam7*RR21*(RR13*RR31 + RR11*RR33) + Lam8*RR22*(RR13*RR32 + RR12*RR33) + RR23*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR31 + RR11*RR32) + Lam7*RR23*(RR13*RR31 + RR11&
  &*RR33) + RR21*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR23*(RR13*RR32 + RR12*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f1221 = -1.D0*RR13*(Lam7*RR21*(RR13*RR21 + RR11*RR23) + Lam8*RR22*(RR13*RR22 + RR12*RR23) + RR23*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR21 + RR11*RR22) + Lam7*RR23*(RR13*RR21 + RR11&
  &*RR23) + RR21*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR23*(RR13*RR22 + RR12*RR23) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f1222 = -1.D0*RR13*(2.D0*Lam7*RR23*DBLE(RR21**INT(2.D0)) + 2.D0*Lam8*RR23*DBLE(RR22**INT(2.D0)) + RR23*(Lam7*DBLE(RR21**&
  &INT(2.D0)) + Lam8*DBLE(RR22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*DBLE(R&
  &R22**INT(2.D0)) + 2.D0*Lam7*RR21*DBLE(RR23**INT(2.D0)) + RR21*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR2&
  &2**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR22*DBLE(RR21**INT(2.D0)) + 2.D0*Lam8*RR2&
  &2*DBLE(RR23**INT(2.D0)) + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23*&
  &*INT(2.D0))))
CS1S1S1S1f1223 = -1.D0*RR13*(Lam7*RR21*(RR23*RR31 + RR21*RR33) + Lam8*RR22*(RR23*RR32 + RR22*RR33) + RR23*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*(RR22*RR31 + RR21*RR32) + Lam7*RR23*(RR23*RR31 + RR21&
  &*RR33) + RR21*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR23*(RR23*RR32 + RR22*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f1231 = -1.D0*RR13*(Lam7*RR21*(RR13*RR31 + RR11*RR33) + Lam8*RR22*(RR13*RR32 + RR12*RR33) + RR23*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR31 + RR11*RR32) + Lam7*RR23*(RR13*RR31 + RR11&
  &*RR33) + RR21*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR23*(RR13*RR32 + RR12*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f1232 = -1.D0*RR13*(Lam7*RR21*(RR23*RR31 + RR21*RR33) + Lam8*RR22*(RR23*RR32 + RR22*RR33) + RR23*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR22*(RR22*RR31 + RR21*RR32) + Lam7*RR23*(RR23*RR31 + RR21&
  &*RR33) + RR21*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR23*(RR23*RR32 + RR22*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f1233 = -1.D0*RR13*(2.D0*Lam7*RR21*RR31*RR33 + 2.D0*Lam8*RR22*RR32*RR33 + RR23*(Lam7*DBLE(RR31**INT(2.D0)) + Lam8*DBLE(R&
  &R32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR22*RR31*RR32 + 2.D0*Lam7*RR23*RR3&
  &1*RR33 + RR21*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR32**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1&
  &.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR31*RR32 + 2.D0*Lam8*RR23*RR32*RR33 + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS1S1S1S1f1311 = -1.D0*RR13*(2.D0*Lam7*RR11*RR13*RR31 + 2.D0*Lam8*RR12*RR13*RR32 + RR33*(Lam7*DBLE(RR11**INT(2.D0)) + Lam8*DBLE(R&
  &R12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR32 + 2.D0*Lam7*RR11*RR1&
  &3*RR33 + RR31*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR12**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1&
  &.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR31 + 2.D0*Lam8*RR12*RR13*RR33 + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS1S1S1S1f1312 = -1.D0*RR11*((3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)*RR31 + (Lam3 + Lam4 + Lam5)*&
  &(RR12*RR21 + RR11*RR22)*RR32 + Lam7*(RR13*RR21 + RR11*RR23)*RR33) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*(RR12*RR21 + RR11*RR22)*RR&
  &31 + ((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23)*RR32 + Lam8*(RR13*RR22 + RR12*RR23)*RR33) - 1.D0*&
  &RR13*(Lam7*(RR13*RR21 + RR11*RR23)*RR31 + Lam8*(RR13*RR22 + RR12*RR23)*RR32 + (Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23)*RR33)
CS1S1S1S1f1313 = -1.D0*RR13*(Lam7*RR31*(RR13*RR31 + RR11*RR33) + Lam8*RR32*(RR13*RR32 + RR12*RR33) + RR33*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR32*(RR12*RR31 + RR11*RR32) + Lam7*RR33*(RR13*RR31 + RR11&
  &*RR33) + RR31*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR33*(RR13*RR32 + RR12*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f1321 = -1.D0*RR11*((3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)*RR31 + (Lam3 + Lam4 + Lam5)*&
  &(RR12*RR21 + RR11*RR22)*RR32 + Lam7*(RR13*RR21 + RR11*RR23)*RR33) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*(RR12*RR21 + RR11*RR22)*RR&
  &31 + ((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23)*RR32 + Lam8*(RR13*RR22 + RR12*RR23)*RR33) - 1.D0*&
  &RR13*(Lam7*(RR13*RR21 + RR11*RR23)*RR31 + Lam8*(RR13*RR22 + RR12*RR23)*RR32 + (Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23)*RR33)
CS1S1S1S1f1322 = -1.D0*RR13*(2.D0*Lam7*RR21*RR23*RR31 + 2.D0*Lam8*RR22*RR23*RR32 + RR33*(Lam7*DBLE(RR21**INT(2.D0)) + Lam8*DBLE(R&
  &R22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR22*RR32 + 2.D0*Lam7*RR21*RR2&
  &3*RR33 + RR31*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR22**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1&
  &.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR22*RR31 + 2.D0*Lam8*RR22*RR23*RR33 + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS1S1S1S1f1323 = -1.D0*RR13*(Lam7*RR31*(RR23*RR31 + RR21*RR33) + Lam8*RR32*(RR23*RR32 + RR22*RR33) + RR33*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR32*(RR22*RR31 + RR21*RR32) + Lam7*RR33*(RR23*RR31 + RR21&
  &*RR33) + RR31*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR33*(RR23*RR32 + RR22*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f1331 = -1.D0*RR13*(Lam7*RR31*(RR13*RR31 + RR11*RR33) + Lam8*RR32*(RR13*RR32 + RR12*RR33) + RR33*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR32*(RR12*RR31 + RR11*RR32) + Lam7*RR33*(RR13*RR31 + RR11&
  &*RR33) + RR31*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR33*(RR13*RR32 + RR12*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f1332 = -1.D0*RR13*(Lam7*RR31*(RR23*RR31 + RR21*RR33) + Lam8*RR32*(RR23*RR32 + RR22*RR33) + RR33*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR11*((Lam3 + Lam4 + Lam5)*RR32*(RR22*RR31 + RR21*RR32) + Lam7*RR33*(RR23*RR31 + RR21&
  &*RR33) + RR31*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR12*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR33*(RR23*RR32 + RR22*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f1333 = -1.D0*RR13*(2.D0*Lam7*RR33*DBLE(RR31**INT(2.D0)) + 2.D0*Lam8*RR33*DBLE(RR32**INT(2.D0)) + RR33*(Lam7*DBLE(RR31**&
  &INT(2.D0)) + Lam8*DBLE(RR32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR11*(2.D0*(Lam3 + Lam4 + Lam5)*RR31*DBLE(R&
  &R32**INT(2.D0)) + 2.D0*Lam7*RR31*DBLE(RR33**INT(2.D0)) + RR31*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR3&
  &2**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1.D0*RR12*(2.D0*(Lam3 + Lam4 + Lam5)*RR32*DBLE(RR31**INT(2.D0)) + 2.D0*Lam8*RR3&
  &2*DBLE(RR33**INT(2.D0)) + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33*&
  &*INT(2.D0))))
CS1S1S1S1f2111 = -1.D0*RR23*(2.D0*Lam7*RR13*DBLE(RR11**INT(2.D0)) + 2.D0*Lam8*RR13*DBLE(RR12**INT(2.D0)) + RR13*(Lam7*DBLE(RR11**&
  &INT(2.D0)) + Lam8*DBLE(RR12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*DBLE(R&
  &R12**INT(2.D0)) + 2.D0*Lam7*RR11*DBLE(RR13**INT(2.D0)) + RR11*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR1&
  &2**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*DBLE(RR11**INT(2.D0)) + 2.D0*Lam8*RR1&
  &2*DBLE(RR13**INT(2.D0)) + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13*&
  &*INT(2.D0))))
CS1S1S1S1f2112 = -1.D0*RR23*(Lam7*RR11*(RR13*RR21 + RR11*RR23) + Lam8*RR12*(RR13*RR22 + RR12*RR23) + RR13*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR21 + RR11*RR22) + Lam7*RR13*(RR13*RR21 + RR11&
  &*RR23) + RR11*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR13*(RR13*RR22 + RR12*RR23) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f2113 = -1.D0*RR23*(Lam7*RR11*(RR13*RR31 + RR11*RR33) + Lam8*RR12*(RR13*RR32 + RR12*RR33) + RR13*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR31 + RR11*RR32) + Lam7*RR13*(RR13*RR31 + RR11&
  &*RR33) + RR11*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR13*(RR13*RR32 + RR12*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f2121 = -1.D0*RR23*(Lam7*RR11*(RR13*RR21 + RR11*RR23) + Lam8*RR12*(RR13*RR22 + RR12*RR23) + RR13*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR21 + RR11*RR22) + Lam7*RR13*(RR13*RR21 + RR11&
  &*RR23) + RR11*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR13*(RR13*RR22 + RR12*RR23) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f2122 = -1.D0*RR23*(2.D0*Lam7*RR11*RR21*RR23 + 2.D0*Lam8*RR12*RR22*RR23 + RR13*(Lam7*DBLE(RR21**INT(2.D0)) + Lam8*DBLE(R&
  &R22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*RR21*RR22 + 2.D0*Lam7*RR13*RR2&
  &1*RR23 + RR11*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR22**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1&
  &.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR21*RR22 + 2.D0*Lam8*RR13*RR22*RR23 + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS1S1S1S1f2123 = -1.D0*RR23*(Lam7*RR11*(RR23*RR31 + RR21*RR33) + Lam8*RR12*(RR23*RR32 + RR22*RR33) + RR13*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*(RR22*RR31 + RR21*RR32) + Lam7*RR13*(RR23*RR31 + RR21&
  &*RR33) + RR11*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR13*(RR23*RR32 + RR22*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f2131 = -1.D0*RR23*(Lam7*RR11*(RR13*RR31 + RR11*RR33) + Lam8*RR12*(RR13*RR32 + RR12*RR33) + RR13*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR31 + RR11*RR32) + Lam7*RR13*(RR13*RR31 + RR11&
  &*RR33) + RR11*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR13*(RR13*RR32 + RR12*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f2132 = -1.D0*RR23*(Lam7*RR11*(RR23*RR31 + RR21*RR33) + Lam8*RR12*(RR23*RR32 + RR22*RR33) + RR13*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR12*(RR22*RR31 + RR21*RR32) + Lam7*RR13*(RR23*RR31 + RR21&
  &*RR33) + RR11*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR13*(RR23*RR32 + RR22*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f2133 = -1.D0*RR23*(2.D0*Lam7*RR11*RR31*RR33 + 2.D0*Lam8*RR12*RR32*RR33 + RR13*(Lam7*DBLE(RR31**INT(2.D0)) + Lam8*DBLE(R&
  &R32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*RR31*RR32 + 2.D0*Lam7*RR13*RR3&
  &1*RR33 + RR11*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR32**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1&
  &.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR31*RR32 + 2.D0*Lam8*RR13*RR32*RR33 + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS1S1S1S1f2211 = -1.D0*RR23*(2.D0*Lam7*RR11*RR13*RR21 + 2.D0*Lam8*RR12*RR13*RR22 + RR23*(Lam7*DBLE(RR11**INT(2.D0)) + Lam8*DBLE(R&
  &R12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR22 + 2.D0*Lam7*RR11*RR1&
  &3*RR23 + RR21*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR12**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1&
  &.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR21 + 2.D0*Lam8*RR12*RR13*RR23 + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS1S1S1S1f2212 = -1.D0*RR23*(Lam7*RR21*(RR13*RR21 + RR11*RR23) + Lam8*RR22*(RR13*RR22 + RR12*RR23) + RR23*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR21 + RR11*RR22) + Lam7*RR23*(RR13*RR21 + RR11&
  &*RR23) + RR21*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR23*(RR13*RR22 + RR12*RR23) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f2213 = -1.D0*RR23*(Lam7*RR21*(RR13*RR31 + RR11*RR33) + Lam8*RR22*(RR13*RR32 + RR12*RR33) + RR23*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR31 + RR11*RR32) + Lam7*RR23*(RR13*RR31 + RR11&
  &*RR33) + RR21*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR23*(RR13*RR32 + RR12*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f2221 = -1.D0*RR23*(Lam7*RR21*(RR13*RR21 + RR11*RR23) + Lam8*RR22*(RR13*RR22 + RR12*RR23) + RR23*(Lam7*RR11*RR21 + Lam8*&
  &RR12*RR22 + 3.D0*Lam6*RR13*RR23)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR21 + RR11*RR22) + Lam7*RR23*(RR13*RR21 + RR11&
  &*RR23) + RR21*(3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR21 + RR11*RR22) + Lam8*RR23*(RR13*RR22 + RR12*RR23) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam&
  &8*RR13*RR23))
CS1S1S1S1f2222 = -1.D0*RR23*(2.D0*Lam7*RR23*DBLE(RR21**INT(2.D0)) + 2.D0*Lam8*RR23*DBLE(RR22**INT(2.D0)) + RR23*(Lam7*DBLE(RR21**&
  &INT(2.D0)) + Lam8*DBLE(RR22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*DBLE(R&
  &R22**INT(2.D0)) + 2.D0*Lam7*RR21*DBLE(RR23**INT(2.D0)) + RR21*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR2&
  &2**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR22*DBLE(RR21**INT(2.D0)) + 2.D0*Lam8*RR2&
  &2*DBLE(RR23**INT(2.D0)) + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23*&
  &*INT(2.D0))))
CS1S1S1S1f2223 = -1.D0*RR23*(Lam7*RR21*(RR23*RR31 + RR21*RR33) + Lam8*RR22*(RR23*RR32 + RR22*RR33) + RR23*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*(RR22*RR31 + RR21*RR32) + Lam7*RR23*(RR23*RR31 + RR21&
  &*RR33) + RR21*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR23*(RR23*RR32 + RR22*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f2231 = -1.D0*RR23*(Lam7*RR21*(RR13*RR31 + RR11*RR33) + Lam8*RR22*(RR13*RR32 + RR12*RR33) + RR23*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR31 + RR11*RR32) + Lam7*RR23*(RR13*RR31 + RR11&
  &*RR33) + RR21*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR23*(RR13*RR32 + RR12*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f2232 = -1.D0*RR23*(Lam7*RR21*(RR23*RR31 + RR21*RR33) + Lam8*RR22*(RR23*RR32 + RR22*RR33) + RR23*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR22*(RR22*RR31 + RR21*RR32) + Lam7*RR23*(RR23*RR31 + RR21&
  &*RR33) + RR21*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR23*(RR23*RR32 + RR22*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f2233 = -1.D0*RR23*(2.D0*Lam7*RR21*RR31*RR33 + 2.D0*Lam8*RR22*RR32*RR33 + RR23*(Lam7*DBLE(RR31**INT(2.D0)) + Lam8*DBLE(R&
  &R32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR22*RR31*RR32 + 2.D0*Lam7*RR23*RR3&
  &1*RR33 + RR21*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR32**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1&
  &.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR31*RR32 + 2.D0*Lam8*RR23*RR32*RR33 + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS1S1S1S1f2311 = -1.D0*RR23*(2.D0*Lam7*RR11*RR13*RR31 + 2.D0*Lam8*RR12*RR13*RR32 + RR33*(Lam7*DBLE(RR11**INT(2.D0)) + Lam8*DBLE(R&
  &R12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR32 + 2.D0*Lam7*RR11*RR1&
  &3*RR33 + RR31*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR12**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1&
  &.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR31 + 2.D0*Lam8*RR12*RR13*RR33 + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS1S1S1S1f2312 = -1.D0*RR21*((3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)*RR31 + (Lam3 + Lam4 + Lam5)*&
  &(RR12*RR21 + RR11*RR22)*RR32 + Lam7*(RR13*RR21 + RR11*RR23)*RR33) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*(RR12*RR21 + RR11*RR22)*RR&
  &31 + ((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23)*RR32 + Lam8*(RR13*RR22 + RR12*RR23)*RR33) - 1.D0*&
  &RR23*(Lam7*(RR13*RR21 + RR11*RR23)*RR31 + Lam8*(RR13*RR22 + RR12*RR23)*RR32 + (Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23)*RR33)
CS1S1S1S1f2313 = -1.D0*RR23*(Lam7*RR31*(RR13*RR31 + RR11*RR33) + Lam8*RR32*(RR13*RR32 + RR12*RR33) + RR33*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR32*(RR12*RR31 + RR11*RR32) + Lam7*RR33*(RR13*RR31 + RR11&
  &*RR33) + RR31*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR33*(RR13*RR32 + RR12*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f2321 = -1.D0*RR21*((3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)*RR31 + (Lam3 + Lam4 + Lam5)*&
  &(RR12*RR21 + RR11*RR22)*RR32 + Lam7*(RR13*RR21 + RR11*RR23)*RR33) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*(RR12*RR21 + RR11*RR22)*RR&
  &31 + ((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23)*RR32 + Lam8*(RR13*RR22 + RR12*RR23)*RR33) - 1.D0*&
  &RR23*(Lam7*(RR13*RR21 + RR11*RR23)*RR31 + Lam8*(RR13*RR22 + RR12*RR23)*RR32 + (Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23)*RR33)
CS1S1S1S1f2322 = -1.D0*RR23*(2.D0*Lam7*RR21*RR23*RR31 + 2.D0*Lam8*RR22*RR23*RR32 + RR33*(Lam7*DBLE(RR21**INT(2.D0)) + Lam8*DBLE(R&
  &R22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR22*RR32 + 2.D0*Lam7*RR21*RR2&
  &3*RR33 + RR31*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR22**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1&
  &.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR22*RR31 + 2.D0*Lam8*RR22*RR23*RR33 + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS1S1S1S1f2323 = -1.D0*RR23*(Lam7*RR31*(RR23*RR31 + RR21*RR33) + Lam8*RR32*(RR23*RR32 + RR22*RR33) + RR33*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR32*(RR22*RR31 + RR21*RR32) + Lam7*RR33*(RR23*RR31 + RR21&
  &*RR33) + RR31*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR33*(RR23*RR32 + RR22*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f2331 = -1.D0*RR23*(Lam7*RR31*(RR13*RR31 + RR11*RR33) + Lam8*RR32*(RR13*RR32 + RR12*RR33) + RR33*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR32*(RR12*RR31 + RR11*RR32) + Lam7*RR33*(RR13*RR31 + RR11&
  &*RR33) + RR31*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR33*(RR13*RR32 + RR12*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f2332 = -1.D0*RR23*(Lam7*RR31*(RR23*RR31 + RR21*RR33) + Lam8*RR32*(RR23*RR32 + RR22*RR33) + RR33*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR21*((Lam3 + Lam4 + Lam5)*RR32*(RR22*RR31 + RR21*RR32) + Lam7*RR33*(RR23*RR31 + RR21&
  &*RR33) + RR31*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR22*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR33*(RR23*RR32 + RR22*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f2333 = -1.D0*RR23*(2.D0*Lam7*RR33*DBLE(RR31**INT(2.D0)) + 2.D0*Lam8*RR33*DBLE(RR32**INT(2.D0)) + RR33*(Lam7*DBLE(RR31**&
  &INT(2.D0)) + Lam8*DBLE(RR32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR21*(2.D0*(Lam3 + Lam4 + Lam5)*RR31*DBLE(R&
  &R32**INT(2.D0)) + 2.D0*Lam7*RR31*DBLE(RR33**INT(2.D0)) + RR31*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR3&
  &2**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1.D0*RR22*(2.D0*(Lam3 + Lam4 + Lam5)*RR32*DBLE(RR31**INT(2.D0)) + 2.D0*Lam8*RR3&
  &2*DBLE(RR33**INT(2.D0)) + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33*&
  &*INT(2.D0))))
CS1S1S1S1f3111 = -1.D0*RR33*(2.D0*Lam7*RR13*DBLE(RR11**INT(2.D0)) + 2.D0*Lam8*RR13*DBLE(RR12**INT(2.D0)) + RR13*(Lam7*DBLE(RR11**&
  &INT(2.D0)) + Lam8*DBLE(RR12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*DBLE(R&
  &R12**INT(2.D0)) + 2.D0*Lam7*RR11*DBLE(RR13**INT(2.D0)) + RR11*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR1&
  &2**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*DBLE(RR11**INT(2.D0)) + 2.D0*Lam8*RR1&
  &2*DBLE(RR13**INT(2.D0)) + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13*&
  &*INT(2.D0))))
CS1S1S1S1f3112 = -1.D0*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR21 + RR11*RR22) + Lam7*RR13*(RR13*RR21 + RR11*RR23) + RR11*(3.D0*Lam1*R&
  &R11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23))*RR31 - 1.D0*((Lam3 + Lam4 + Lam5)*RR11*(RR12*RR21 + RR11*RR22) + &
  &Lam8*RR13*(RR13*RR22 + RR12*RR23) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23))*RR32 - 1.D0*&
  &(Lam7*RR11*(RR13*RR21 + RR11*RR23) + Lam8*RR12*(RR13*RR22 + RR12*RR23) + RR13*(Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23))*RR33
CS1S1S1S1f3113 = -1.D0*RR33*(Lam7*RR11*(RR13*RR31 + RR11*RR33) + Lam8*RR12*(RR13*RR32 + RR12*RR33) + RR13*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR31 + RR11*RR32) + Lam7*RR13*(RR13*RR31 + RR11&
  &*RR33) + RR11*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR13*(RR13*RR32 + RR12*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f3121 = -1.D0*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR21 + RR11*RR22) + Lam7*RR13*(RR13*RR21 + RR11*RR23) + RR11*(3.D0*Lam1*R&
  &R11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23))*RR31 - 1.D0*((Lam3 + Lam4 + Lam5)*RR11*(RR12*RR21 + RR11*RR22) + &
  &Lam8*RR13*(RR13*RR22 + RR12*RR23) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23))*RR32 - 1.D0*&
  &(Lam7*RR11*(RR13*RR21 + RR11*RR23) + Lam8*RR12*(RR13*RR22 + RR12*RR23) + RR13*(Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23))*RR33
CS1S1S1S1f3122 = -1.D0*RR33*(2.D0*Lam7*RR11*RR21*RR23 + 2.D0*Lam8*RR12*RR22*RR23 + RR13*(Lam7*DBLE(RR21**INT(2.D0)) + Lam8*DBLE(R&
  &R22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*RR21*RR22 + 2.D0*Lam7*RR13*RR2&
  &1*RR23 + RR11*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR22**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1&
  &.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR21*RR22 + 2.D0*Lam8*RR13*RR22*RR23 + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS1S1S1S1f3123 = -1.D0*RR33*(Lam7*RR11*(RR23*RR31 + RR21*RR33) + Lam8*RR12*(RR23*RR32 + RR22*RR33) + RR13*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR12*(RR22*RR31 + RR21*RR32) + Lam7*RR13*(RR23*RR31 + RR21&
  &*RR33) + RR11*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR13*(RR23*RR32 + RR22*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f3131 = -1.D0*RR33*(Lam7*RR11*(RR13*RR31 + RR11*RR33) + Lam8*RR12*(RR13*RR32 + RR12*RR33) + RR13*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR12*(RR12*RR31 + RR11*RR32) + Lam7*RR13*(RR13*RR31 + RR11&
  &*RR33) + RR11*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR13*(RR13*RR32 + RR12*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f3132 = -1.D0*RR33*(Lam7*RR11*(RR23*RR31 + RR21*RR33) + Lam8*RR12*(RR23*RR32 + RR22*RR33) + RR13*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR12*(RR22*RR31 + RR21*RR32) + Lam7*RR13*(RR23*RR31 + RR21&
  &*RR33) + RR11*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR11*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR13*(RR23*RR32 + RR22*RR33) + RR12*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f3133 = -1.D0*RR33*(2.D0*Lam7*RR11*RR31*RR33 + 2.D0*Lam8*RR12*RR32*RR33 + RR13*(Lam7*DBLE(RR31**INT(2.D0)) + Lam8*DBLE(R&
  &R32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR12*RR31*RR32 + 2.D0*Lam7*RR13*RR3&
  &1*RR33 + RR11*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR32**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1&
  &.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR31*RR32 + 2.D0*Lam8*RR13*RR32*RR33 + RR12*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS1S1S1S1f3211 = -1.D0*RR33*(2.D0*Lam7*RR11*RR13*RR21 + 2.D0*Lam8*RR12*RR13*RR22 + RR23*(Lam7*DBLE(RR11**INT(2.D0)) + Lam8*DBLE(R&
  &R12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR22 + 2.D0*Lam7*RR11*RR1&
  &3*RR23 + RR21*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR12**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1&
  &.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR21 + 2.D0*Lam8*RR12*RR13*RR23 + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS1S1S1S1f3212 = -1.D0*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR21 + RR11*RR22) + Lam7*RR23*(RR13*RR21 + RR11*RR23) + RR21*(3.D0*Lam1*R&
  &R11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23))*RR31 - 1.D0*((Lam3 + Lam4 + Lam5)*RR21*(RR12*RR21 + RR11*RR22) + &
  &Lam8*RR23*(RR13*RR22 + RR12*RR23) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23))*RR32 - 1.D0*&
  &(Lam7*RR21*(RR13*RR21 + RR11*RR23) + Lam8*RR22*(RR13*RR22 + RR12*RR23) + RR23*(Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23))*RR33
CS1S1S1S1f3213 = -1.D0*RR33*(Lam7*RR21*(RR13*RR31 + RR11*RR33) + Lam8*RR22*(RR13*RR32 + RR12*RR33) + RR23*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR31 + RR11*RR32) + Lam7*RR23*(RR13*RR31 + RR11&
  &*RR33) + RR21*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR23*(RR13*RR32 + RR12*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f3221 = -1.D0*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR21 + RR11*RR22) + Lam7*RR23*(RR13*RR21 + RR11*RR23) + RR21*(3.D0*Lam1*R&
  &R11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23))*RR31 - 1.D0*((Lam3 + Lam4 + Lam5)*RR21*(RR12*RR21 + RR11*RR22) + &
  &Lam8*RR23*(RR13*RR22 + RR12*RR23) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23))*RR32 - 1.D0*&
  &(Lam7*RR21*(RR13*RR21 + RR11*RR23) + Lam8*RR22*(RR13*RR22 + RR12*RR23) + RR23*(Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23))*RR33
CS1S1S1S1f3222 = -1.D0*RR33*(2.D0*Lam7*RR23*DBLE(RR21**INT(2.D0)) + 2.D0*Lam8*RR23*DBLE(RR22**INT(2.D0)) + RR23*(Lam7*DBLE(RR21**&
  &INT(2.D0)) + Lam8*DBLE(RR22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*DBLE(R&
  &R22**INT(2.D0)) + 2.D0*Lam7*RR21*DBLE(RR23**INT(2.D0)) + RR21*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR2&
  &2**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR22*DBLE(RR21**INT(2.D0)) + 2.D0*Lam8*RR2&
  &2*DBLE(RR23**INT(2.D0)) + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23*&
  &*INT(2.D0))))
CS1S1S1S1f3223 = -1.D0*RR33*(Lam7*RR21*(RR23*RR31 + RR21*RR33) + Lam8*RR22*(RR23*RR32 + RR22*RR33) + RR23*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR22*(RR22*RR31 + RR21*RR32) + Lam7*RR23*(RR23*RR31 + RR21&
  &*RR33) + RR21*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR23*(RR23*RR32 + RR22*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f3231 = -1.D0*RR33*(Lam7*RR21*(RR13*RR31 + RR11*RR33) + Lam8*RR22*(RR13*RR32 + RR12*RR33) + RR23*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR22*(RR12*RR31 + RR11*RR32) + Lam7*RR23*(RR13*RR31 + RR11&
  &*RR33) + RR21*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR23*(RR13*RR32 + RR12*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f3232 = -1.D0*RR33*(Lam7*RR21*(RR23*RR31 + RR21*RR33) + Lam8*RR22*(RR23*RR32 + RR22*RR33) + RR23*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR22*(RR22*RR31 + RR21*RR32) + Lam7*RR23*(RR23*RR31 + RR21&
  &*RR33) + RR21*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR21*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR23*(RR23*RR32 + RR22*RR33) + RR22*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f3233 = -1.D0*RR33*(2.D0*Lam7*RR21*RR31*RR33 + 2.D0*Lam8*RR22*RR32*RR33 + RR23*(Lam7*DBLE(RR31**INT(2.D0)) + Lam8*DBLE(R&
  &R32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR22*RR31*RR32 + 2.D0*Lam7*RR23*RR3&
  &1*RR33 + RR21*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR32**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1&
  &.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR31*RR32 + 2.D0*Lam8*RR23*RR32*RR33 + RR22*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS1S1S1S1f3311 = -1.D0*RR33*(2.D0*Lam7*RR11*RR13*RR31 + 2.D0*Lam8*RR12*RR13*RR32 + RR33*(Lam7*DBLE(RR11**INT(2.D0)) + Lam8*DBLE(R&
  &R12**INT(2.D0)) + 3.D0*Lam6*DBLE(RR13**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR32 + 2.D0*Lam7*RR11*RR1&
  &3*RR33 + RR31*(3.D0*Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR12**INT(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1&
  &.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR11*RR12*RR31 + 2.D0*Lam8*RR12*RR13*RR33 + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR11**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS1S1S1S1f3312 = -1.D0*RR31*((3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)*RR31 + (Lam3 + Lam4 + Lam5)*&
  &(RR12*RR21 + RR11*RR22)*RR32 + Lam7*(RR13*RR21 + RR11*RR23)*RR33) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*(RR12*RR21 + RR11*RR22)*RR&
  &31 + ((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23)*RR32 + Lam8*(RR13*RR22 + RR12*RR23)*RR33) - 1.D0*&
  &RR33*(Lam7*(RR13*RR21 + RR11*RR23)*RR31 + Lam8*(RR13*RR22 + RR12*RR23)*RR32 + (Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23)*RR33)
CS1S1S1S1f3313 = -1.D0*RR33*(Lam7*RR31*(RR13*RR31 + RR11*RR33) + Lam8*RR32*(RR13*RR32 + RR12*RR33) + RR33*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR32*(RR12*RR31 + RR11*RR32) + Lam7*RR33*(RR13*RR31 + RR11&
  &*RR33) + RR31*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR33*(RR13*RR32 + RR12*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f3321 = -1.D0*RR31*((3.D0*Lam1*RR11*RR21 + (Lam3 + Lam4 + Lam5)*RR12*RR22 + Lam7*RR13*RR23)*RR31 + (Lam3 + Lam4 + Lam5)*&
  &(RR12*RR21 + RR11*RR22)*RR32 + Lam7*(RR13*RR21 + RR11*RR23)*RR33) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*(RR12*RR21 + RR11*RR22)*RR&
  &31 + ((Lam3 + Lam4 + Lam5)*RR11*RR21 + 3.D0*Lam2*RR12*RR22 + Lam8*RR13*RR23)*RR32 + Lam8*(RR13*RR22 + RR12*RR23)*RR33) - 1.D0*&
  &RR33*(Lam7*(RR13*RR21 + RR11*RR23)*RR31 + Lam8*(RR13*RR22 + RR12*RR23)*RR32 + (Lam7*RR11*RR21 + Lam8*RR12*RR22 + 3.D0*Lam6*RR1&
  &3*RR23)*RR33)
CS1S1S1S1f3322 = -1.D0*RR33*(2.D0*Lam7*RR21*RR23*RR31 + 2.D0*Lam8*RR22*RR23*RR32 + RR33*(Lam7*DBLE(RR21**INT(2.D0)) + Lam8*DBLE(R&
  &R22**INT(2.D0)) + 3.D0*Lam6*DBLE(RR23**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR22*RR32 + 2.D0*Lam7*RR21*RR2&
  &3*RR33 + RR31*(3.D0*Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR22**INT(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1&
  &.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR21*RR22*RR31 + 2.D0*Lam8*RR22*RR23*RR33 + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR21**INT(2.D0&
  &)) + 3.D0*Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS1S1S1S1f3323 = -1.D0*RR33*(Lam7*RR31*(RR23*RR31 + RR21*RR33) + Lam8*RR32*(RR23*RR32 + RR22*RR33) + RR33*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR32*(RR22*RR31 + RR21*RR32) + Lam7*RR33*(RR23*RR31 + RR21&
  &*RR33) + RR31*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR33*(RR23*RR32 + RR22*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f3331 = -1.D0*RR33*(Lam7*RR31*(RR13*RR31 + RR11*RR33) + Lam8*RR32*(RR13*RR32 + RR12*RR33) + RR33*(Lam7*RR11*RR31 + Lam8*&
  &RR12*RR32 + 3.D0*Lam6*RR13*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR32*(RR12*RR31 + RR11*RR32) + Lam7*RR33*(RR13*RR31 + RR11&
  &*RR33) + RR31*(3.D0*Lam1*RR11*RR31 + (Lam3 + Lam4 + Lam5)*RR12*RR32 + Lam7*RR13*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR12*RR31 + RR11*RR32) + Lam8*RR33*(RR13*RR32 + RR12*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR11*RR31 + 3.D0*Lam2*RR12*RR32 + Lam&
  &8*RR13*RR33))
CS1S1S1S1f3332 = -1.D0*RR33*(Lam7*RR31*(RR23*RR31 + RR21*RR33) + Lam8*RR32*(RR23*RR32 + RR22*RR33) + RR33*(Lam7*RR21*RR31 + Lam8*&
  &RR22*RR32 + 3.D0*Lam6*RR23*RR33)) - 1.D0*RR31*((Lam3 + Lam4 + Lam5)*RR32*(RR22*RR31 + RR21*RR32) + Lam7*RR33*(RR23*RR31 + RR21&
  &*RR33) + RR31*(3.D0*Lam1*RR21*RR31 + (Lam3 + Lam4 + Lam5)*RR22*RR32 + Lam7*RR23*RR33)) - 1.D0*RR32*((Lam3 + Lam4 + Lam5)*RR31*&
  &(RR22*RR31 + RR21*RR32) + Lam8*RR33*(RR23*RR32 + RR22*RR33) + RR32*((Lam3 + Lam4 + Lam5)*RR21*RR31 + 3.D0*Lam2*RR22*RR32 + Lam&
  &8*RR23*RR33))
CS1S1S1S1f3333 = -1.D0*RR33*(2.D0*Lam7*RR33*DBLE(RR31**INT(2.D0)) + 2.D0*Lam8*RR33*DBLE(RR32**INT(2.D0)) + RR33*(Lam7*DBLE(RR31**&
  &INT(2.D0)) + Lam8*DBLE(RR32**INT(2.D0)) + 3.D0*Lam6*DBLE(RR33**INT(2.D0)))) - 1.D0*RR31*(2.D0*(Lam3 + Lam4 + Lam5)*RR31*DBLE(R&
  &R32**INT(2.D0)) + 2.D0*Lam7*RR31*DBLE(RR33**INT(2.D0)) + RR31*(3.D0*Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 + Lam5)*DBLE(RR3&
  &2**INT(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1.D0*RR32*(2.D0*(Lam3 + Lam4 + Lam5)*RR32*DBLE(RR31**INT(2.D0)) + 2.D0*Lam8*RR3&
  &2*DBLE(RR33**INT(2.D0)) + RR32*((Lam3 + Lam4 + Lam5)*DBLE(RR31**INT(2.D0)) + 3.D0*Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33*&
  &*INT(2.D0))))

CS2S2S2S2f1111 = -1.D0*SB*(2.D0*CB2*(Lam3 + Lam4 + Lam5)*SB + SB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam2*SB2)) - 1.D0*CB*(2.D0*CB*(&
  &Lam3 + Lam4 + Lam5)*SB2 + CB*(3.D0*CB2*Lam1 + (Lam3 + Lam4 + Lam5)*SB2))
CS2S2S2S2f1112 = -1.D0*SB*(SB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1&
  &.D0*CB*(CB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + (Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f1121 = -1.D0*SB*(SB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1&
  &.D0*CB*(CB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + (Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f1122 = -1.D0*CB*(-2.D0*CB*(Lam3 + Lam4 + Lam5)*SB2 + CB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam1*SB2)) - 1.D0*SB*(-2.D0*CB&
  &2*(Lam3 + Lam4 + Lam5)*SB + SB*(3.D0*CB2*Lam2 + (Lam3 + Lam4 + Lam5)*SB2))
CS2S2S2S2f1211 = -1.D0*SB*(-2.D0*CB*(Lam3 + Lam4 + Lam5)*SB2 + CB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam2*SB2)) - 1.D0*CB*(2.D0*CB2&
  &*(Lam3 + Lam4 + Lam5)*SB - 1.D0*SB*(3.D0*CB2*Lam1 + (Lam3 + Lam4 + Lam5)*SB2))
CS2S2S2S2f1212 = -1.D0*CB*(-1.D0*SB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) -&
  & 1.D0*SB*(CB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) - 1.D0*(Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f1221 = -1.D0*CB*(-1.D0*SB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) -&
  & 1.D0*SB*(CB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) - 1.D0*(Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f1222 = -1.D0*CB*(-2.D0*CB2*(Lam3 + Lam4 + Lam5)*SB - 1.D0*SB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam1*SB2)) - 1.D0*SB*(2.D&
  &0*CB*(Lam3 + Lam4 + Lam5)*SB2 + CB*(3.D0*CB2*Lam2 + (Lam3 + Lam4 + Lam5)*SB2))
CS2S2S2S2f2111 = -1.D0*CB*(2.D0*CB2*(Lam3 + Lam4 + Lam5)*SB + SB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam2*SB2)) + SB*(2.D0*CB*(Lam3 &
  &+ Lam4 + Lam5)*SB2 + CB*(3.D0*CB2*Lam1 + (Lam3 + Lam4 + Lam5)*SB2))
CS2S2S2S2f2112 = -1.D0*CB*(SB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) + S&
  &B*(CB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + (Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f2121 = -1.D0*CB*(SB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) + S&
  &B*(CB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + (Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f2122 = SB*(-2.D0*CB*(Lam3 + Lam4 + Lam5)*SB2 + CB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam1*SB2)) - 1.D0*CB*(-2.D0*CB2*(Lam&
  &3 + Lam4 + Lam5)*SB + SB*(3.D0*CB2*Lam2 + (Lam3 + Lam4 + Lam5)*SB2))
CS2S2S2S2f2211 = -1.D0*CB*(-2.D0*CB*(Lam3 + Lam4 + Lam5)*SB2 + CB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam2*SB2)) + SB*(2.D0*CB2*(Lam&
  &3 + Lam4 + Lam5)*SB - 1.D0*SB*(3.D0*CB2*Lam1 + (Lam3 + Lam4 + Lam5)*SB2))
CS2S2S2S2f2212 = SB*(-1.D0*SB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*&
  &CB*(CB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) - 1.D0*(Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f2221 = SB*(-1.D0*SB*(-3.D0*CB*Lam1*SB + CB*(Lam3 + Lam4 + Lam5)*SB) + CB*(Lam3 + Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*&
  &CB*(CB*(3.D0*CB*Lam2*SB - 1.D0*CB*(Lam3 + Lam4 + Lam5)*SB) - 1.D0*(Lam3 + Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2))
CS2S2S2S2f2222 = SB*(-2.D0*CB2*(Lam3 + Lam4 + Lam5)*SB - 1.D0*SB*(CB2*(Lam3 + Lam4 + Lam5) + 3.D0*Lam1*SB2)) - 1.D0*CB*(2.D0*CB*(&
  &Lam3 + Lam4 + Lam5)*SB2 + CB*(3.D0*CB2*Lam2 + (Lam3 + Lam4 + Lam5)*SB2))

CS3S3S3S3f1111 = -1.D0*SB*(2.D0*CB2*(Lam3 + Lam4)*SB + 2.D0*SB*(CB2*Lam5 + Lam2*SB2)) - 1.D0*CB*(2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D0&
  &*CB*(CB2*Lam1 + Lam5*SB2))
CS3S3S3S3f1112 = -1.D0*SB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*(-1.D0*&
  &CB*Lam1*SB + CB*Lam5*SB) + (Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f1121 = -1.D0*SB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*(-1.D0*&
  &CB*Lam1*SB + CB*Lam5*SB) + (Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f1122 = -1.D0*CB*(-2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D0*CB*(CB2*Lam5 + Lam1*SB2)) - 1.D0*SB*(-2.D0*CB2*(Lam3 + Lam4)*SB + 2.&
  &D0*SB*(CB2*Lam2 + Lam5*SB2))
CS3S3S3S3f1211 = -1.D0*SB*(-2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D0*CB*(CB2*Lam5 + Lam2*SB2)) - 1.D0*CB*(2.D0*CB2*(Lam3 + Lam4)*SB - 2.D&
  &0*SB*(CB2*Lam1 + Lam5*SB2))
CS3S3S3S3f1212 = -1.D0*CB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) - 1.D0*SB*(2.D0*CB*(CB*L&
  &am2*SB - 1.D0*CB*Lam5*SB) - 1.D0*(Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f1221 = -1.D0*CB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) - 1.D0*SB*(2.D0*CB*(CB*L&
  &am2*SB - 1.D0*CB*Lam5*SB) - 1.D0*(Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f1222 = -1.D0*CB*(-2.D0*CB2*(Lam3 + Lam4)*SB - 2.D0*SB*(CB2*Lam5 + Lam1*SB2)) - 1.D0*SB*(2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D&
  &0*CB*(CB2*Lam2 + Lam5*SB2))
CS3S3S3S3f2111 = -1.D0*CB*(2.D0*CB2*(Lam3 + Lam4)*SB + 2.D0*SB*(CB2*Lam5 + Lam2*SB2)) + SB*(2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D0*CB*(&
  &CB2*Lam1 + Lam5*SB2))
CS3S3S3S3f2112 = -1.D0*CB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) + SB*(2.D0*CB*(-1.D0*CB*La&
  &m1*SB + CB*Lam5*SB) + (Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f2121 = -1.D0*CB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) + SB*(2.D0*CB*(-1.D0*CB*La&
  &m1*SB + CB*Lam5*SB) + (Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f2122 = SB*(-2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D0*CB*(CB2*Lam5 + Lam1*SB2)) - 1.D0*CB*(-2.D0*CB2*(Lam3 + Lam4)*SB + 2.D0*SB*&
  &(CB2*Lam2 + Lam5*SB2))
CS3S3S3S3f2211 = -1.D0*CB*(-2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D0*CB*(CB2*Lam5 + Lam2*SB2)) + SB*(2.D0*CB2*(Lam3 + Lam4)*SB - 2.D0*SB*&
  &(CB2*Lam1 + Lam5*SB2))
CS3S3S3S3f2212 = SB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*(CB*Lam2*SB&
  & - 1.D0*CB*Lam5*SB) - 1.D0*(Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f2221 = SB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam5*SB) + CB*(Lam3 + Lam4)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*(CB*Lam2*SB&
  & - 1.D0*CB*Lam5*SB) - 1.D0*(Lam3 + Lam4)*SB*(CB2 - 1.D0*SB2))
CS3S3S3S3f2222 = SB*(-2.D0*CB2*(Lam3 + Lam4)*SB - 2.D0*SB*(CB2*Lam5 + Lam1*SB2)) - 1.D0*CB*(2.D0*CB*(Lam3 + Lam4)*SB2 + 2.D0*CB*(&
  &CB2*Lam2 + Lam5*SB2))

CS2S2S1S1f1111 = -1.D0*CB*(2.D0*Lam5*RR11*RR12*SB + CB*(Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR12**INT(2.D&
  &0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1.D0*SB*(2.D0*CB*Lam5*RR11*RR12 + SB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR11**INT(2.D0)) + L&
  &am2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS2S2S1S1f1112 = -1.D0*CB*(CB*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR13*RR23) + Lam5*(RR12*RR21 + RR11*RR&
  &22)*SB) - 1.D0*SB*(CB*Lam5*(RR12*RR21 + RR11*RR22) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23)*S&
  &B)
CS2S2S1S1f1113 = -1.D0*CB*(CB*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR13*RR33) + Lam5*(RR12*RR31 + RR11*RR&
  &32)*SB) - 1.D0*SB*(CB*Lam5*(RR12*RR31 + RR11*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33)*S&
  &B)
CS2S2S1S1f1121 = -1.D0*CB*(CB*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR13*RR23) + Lam5*(RR12*RR21 + RR11*RR&
  &22)*SB) - 1.D0*SB*(CB*Lam5*(RR12*RR21 + RR11*RR22) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23)*S&
  &B)
CS2S2S1S1f1122 = -1.D0*CB*(2.D0*Lam5*RR21*RR22*SB + CB*(Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR22**INT(2.D&
  &0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1.D0*SB*(2.D0*CB*Lam5*RR21*RR22 + SB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR21**INT(2.D0)) + L&
  &am2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS2S2S1S1f1123 = -1.D0*CB*(CB*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR23*RR33) + Lam5*(RR22*RR31 + RR21*RR&
  &32)*SB) - 1.D0*SB*(CB*Lam5*(RR22*RR31 + RR21*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33)*S&
  &B)
CS2S2S1S1f1131 = -1.D0*CB*(CB*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR13*RR33) + Lam5*(RR12*RR31 + RR11*RR&
  &32)*SB) - 1.D0*SB*(CB*Lam5*(RR12*RR31 + RR11*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33)*S&
  &B)
CS2S2S1S1f1132 = -1.D0*CB*(CB*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR23*RR33) + Lam5*(RR22*RR31 + RR21*RR&
  &32)*SB) - 1.D0*SB*(CB*Lam5*(RR22*RR31 + RR21*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33)*S&
  &B)
CS2S2S1S1f1133 = -1.D0*CB*(2.D0*Lam5*RR31*RR32*SB + CB*(Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR32**INT(2.D&
  &0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1.D0*SB*(2.D0*CB*Lam5*RR31*RR32 + SB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR31**INT(2.D0)) + L&
  &am2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS2S2S1S1f1211 = -1.D0*CB*(2.D0*CB*Lam5*RR11*RR12 - 1.D0*SB*(Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR12**IN&
  &T(2.D0)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1.D0*SB*(-2.D0*Lam5*RR11*RR12*SB + CB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR11**INT(2.D0&
  &)) + Lam2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS2S2S1S1f1212 = -1.D0*SB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23) - 1.D0*Lam5*(RR12*RR21 + RR&
  &11*RR22)*SB) - 1.D0*CB*(CB*Lam5*(RR12*RR21 + RR11*RR22) - 1.D0*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR&
  &13*RR23)*SB)
CS2S2S1S1f1213 = -1.D0*SB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33) - 1.D0*Lam5*(RR12*RR31 + RR&
  &11*RR32)*SB) - 1.D0*CB*(CB*Lam5*(RR12*RR31 + RR11*RR32) - 1.D0*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR&
  &13*RR33)*SB)
CS2S2S1S1f1221 = -1.D0*SB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23) - 1.D0*Lam5*(RR12*RR21 + RR&
  &11*RR22)*SB) - 1.D0*CB*(CB*Lam5*(RR12*RR21 + RR11*RR22) - 1.D0*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR&
  &13*RR23)*SB)
CS2S2S1S1f1222 = -1.D0*CB*(2.D0*CB*Lam5*RR21*RR22 - 1.D0*SB*(Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR22**IN&
  &T(2.D0)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1.D0*SB*(-2.D0*Lam5*RR21*RR22*SB + CB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR21**INT(2.D0&
  &)) + Lam2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS2S2S1S1f1223 = -1.D0*SB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33) - 1.D0*Lam5*(RR22*RR31 + RR&
  &21*RR32)*SB) - 1.D0*CB*(CB*Lam5*(RR22*RR31 + RR21*RR32) - 1.D0*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR&
  &23*RR33)*SB)
CS2S2S1S1f1231 = -1.D0*SB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33) - 1.D0*Lam5*(RR12*RR31 + RR&
  &11*RR32)*SB) - 1.D0*CB*(CB*Lam5*(RR12*RR31 + RR11*RR32) - 1.D0*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR&
  &13*RR33)*SB)
CS2S2S1S1f1232 = -1.D0*SB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33) - 1.D0*Lam5*(RR22*RR31 + RR&
  &21*RR32)*SB) - 1.D0*CB*(CB*Lam5*(RR22*RR31 + RR21*RR32) - 1.D0*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR&
  &23*RR33)*SB)
CS2S2S1S1f1233 = -1.D0*CB*(2.D0*CB*Lam5*RR31*RR32 - 1.D0*SB*(Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR32**IN&
  &T(2.D0)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1.D0*SB*(-2.D0*Lam5*RR31*RR32*SB + CB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR31**INT(2.D0&
  &)) + Lam2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS2S2S1S1f2111 = SB*(2.D0*Lam5*RR11*RR12*SB + CB*(Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR12**INT(2.D0)) + &
  &Lam7*DBLE(RR13**INT(2.D0)))) - 1.D0*CB*(2.D0*CB*Lam5*RR11*RR12 + SB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR11**INT(2.D0)) + Lam2*DB&
  &LE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS2S2S1S1f2112 = SB*(CB*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR13*RR23) + Lam5*(RR12*RR21 + RR11*RR22)*SB&
  &) - 1.D0*CB*(CB*Lam5*(RR12*RR21 + RR11*RR22) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23)*SB)
CS2S2S1S1f2113 = SB*(CB*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR13*RR33) + Lam5*(RR12*RR31 + RR11*RR32)*SB&
  &) - 1.D0*CB*(CB*Lam5*(RR12*RR31 + RR11*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33)*SB)
CS2S2S1S1f2121 = SB*(CB*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR13*RR23) + Lam5*(RR12*RR21 + RR11*RR22)*SB&
  &) - 1.D0*CB*(CB*Lam5*(RR12*RR21 + RR11*RR22) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23)*SB)
CS2S2S1S1f2122 = SB*(2.D0*Lam5*RR21*RR22*SB + CB*(Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR22**INT(2.D0)) + &
  &Lam7*DBLE(RR23**INT(2.D0)))) - 1.D0*CB*(2.D0*CB*Lam5*RR21*RR22 + SB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR21**INT(2.D0)) + Lam2*DB&
  &LE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS2S2S1S1f2123 = SB*(CB*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR23*RR33) + Lam5*(RR22*RR31 + RR21*RR32)*SB&
  &) - 1.D0*CB*(CB*Lam5*(RR22*RR31 + RR21*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33)*SB)
CS2S2S1S1f2131 = SB*(CB*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR13*RR33) + Lam5*(RR12*RR31 + RR11*RR32)*SB&
  &) - 1.D0*CB*(CB*Lam5*(RR12*RR31 + RR11*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33)*SB)
CS2S2S1S1f2132 = SB*(CB*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR23*RR33) + Lam5*(RR22*RR31 + RR21*RR32)*SB&
  &) - 1.D0*CB*(CB*Lam5*(RR22*RR31 + RR21*RR32) + ((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33)*SB)
CS2S2S1S1f2133 = SB*(2.D0*Lam5*RR31*RR32*SB + CB*(Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR32**INT(2.D0)) + &
  &Lam7*DBLE(RR33**INT(2.D0)))) - 1.D0*CB*(2.D0*CB*Lam5*RR31*RR32 + SB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR31**INT(2.D0)) + Lam2*DB&
  &LE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))
CS2S2S1S1f2211 = SB*(2.D0*CB*Lam5*RR11*RR12 - 1.D0*SB*(Lam1*DBLE(RR11**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR12**INT(2.D0&
  &)) + Lam7*DBLE(RR13**INT(2.D0)))) - 1.D0*CB*(-2.D0*Lam5*RR11*RR12*SB + CB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR11**INT(2.D0)) + L&
  &am2*DBLE(RR12**INT(2.D0)) + Lam8*DBLE(RR13**INT(2.D0))))
CS2S2S1S1f2212 = -1.D0*CB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23) - 1.D0*Lam5*(RR12*RR21 + RR&
  &11*RR22)*SB) + SB*(CB*Lam5*(RR12*RR21 + RR11*RR22) - 1.D0*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR13*RR&
  &23)*SB)
CS2S2S1S1f2213 = -1.D0*CB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33) - 1.D0*Lam5*(RR12*RR31 + RR&
  &11*RR32)*SB) + SB*(CB*Lam5*(RR12*RR31 + RR11*RR32) - 1.D0*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR13*RR&
  &33)*SB)
CS2S2S1S1f2221 = -1.D0*CB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR21 + Lam2*RR12*RR22 + Lam8*RR13*RR23) - 1.D0*Lam5*(RR12*RR21 + RR&
  &11*RR22)*SB) + SB*(CB*Lam5*(RR12*RR21 + RR11*RR22) - 1.D0*(Lam1*RR11*RR21 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR22 + Lam7*RR13*RR&
  &23)*SB)
CS2S2S1S1f2222 = SB*(2.D0*CB*Lam5*RR21*RR22 - 1.D0*SB*(Lam1*DBLE(RR21**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR22**INT(2.D0&
  &)) + Lam7*DBLE(RR23**INT(2.D0)))) - 1.D0*CB*(-2.D0*Lam5*RR21*RR22*SB + CB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR21**INT(2.D0)) + L&
  &am2*DBLE(RR22**INT(2.D0)) + Lam8*DBLE(RR23**INT(2.D0))))
CS2S2S1S1f2223 = -1.D0*CB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33) - 1.D0*Lam5*(RR22*RR31 + RR&
  &21*RR32)*SB) + SB*(CB*Lam5*(RR22*RR31 + RR21*RR32) - 1.D0*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR23*RR&
  &33)*SB)
CS2S2S1S1f2231 = -1.D0*CB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR11*RR31 + Lam2*RR12*RR32 + Lam8*RR13*RR33) - 1.D0*Lam5*(RR12*RR31 + RR&
  &11*RR32)*SB) + SB*(CB*Lam5*(RR12*RR31 + RR11*RR32) - 1.D0*(Lam1*RR11*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR12*RR32 + Lam7*RR13*RR&
  &33)*SB)
CS2S2S1S1f2232 = -1.D0*CB*(CB*((Lam3 + Lam4 - 1.D0*Lam5)*RR21*RR31 + Lam2*RR22*RR32 + Lam8*RR23*RR33) - 1.D0*Lam5*(RR22*RR31 + RR&
  &21*RR32)*SB) + SB*(CB*Lam5*(RR22*RR31 + RR21*RR32) - 1.D0*(Lam1*RR21*RR31 + (Lam3 + Lam4 - 1.D0*Lam5)*RR22*RR32 + Lam7*RR23*RR&
  &33)*SB)
CS2S2S1S1f2233 = SB*(2.D0*CB*Lam5*RR31*RR32 - 1.D0*SB*(Lam1*DBLE(RR31**INT(2.D0)) + (Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR32**INT(2.D0&
  &)) + Lam7*DBLE(RR33**INT(2.D0)))) - 1.D0*CB*(-2.D0*Lam5*RR31*RR32*SB + CB*((Lam3 + Lam4 - 1.D0*Lam5)*DBLE(RR31**INT(2.D0)) + L&
  &am2*DBLE(RR32**INT(2.D0)) + Lam8*DBLE(RR33**INT(2.D0))))

CS2S2S3S3f1111 = 0.5D0*(-1.D0*SB*(2.D0*CB2*(Lam4 + Lam5)*SB + 2.D0*SB*(CB2*Lam3 + Lam2*SB2)) - 1.D0*CB*(2.D0*CB*(Lam4 + Lam5)*SB2&
  & + 2.D0*CB*(CB2*Lam1 + Lam3*SB2)))
CS2S2S3S3f1112 = 0.5D0*(-1.D0*SB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*&
  &(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f1121 = 0.5D0*(-1.D0*SB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*&
  &(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f1122 = 0.5D0*(-1.D0*CB*(-2.D0*CB*(Lam4 + Lam5)*SB2 + 2.D0*CB*(CB2*Lam3 + Lam1*SB2)) - 1.D0*SB*(-2.D0*CB2*(Lam4 + Lam5)*&
  &SB + 2.D0*SB*(CB2*Lam2 + Lam3*SB2)))
CS2S2S3S3f1211 = 0.5D0*(-1.D0*SB*(-2.D0*CB*(Lam4 + Lam5)*SB2 + 2.D0*CB*(CB2*Lam3 + Lam2*SB2)) - 1.D0*CB*(2.D0*CB2*(Lam4 + Lam5)*S&
  &B - 2.D0*SB*(CB2*Lam1 + Lam3*SB2)))
CS2S2S3S3f1212 = 0.5D0*(-1.D0*CB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*SB*(2.D0*C&
  &B*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) - 1.D0*(Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f1221 = 0.5D0*(-1.D0*CB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*SB*(2.D0*C&
  &B*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) - 1.D0*(Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f1222 = 0.5D0*(-1.D0*CB*(-2.D0*CB2*(Lam4 + Lam5)*SB - 2.D0*SB*(CB2*Lam3 + Lam1*SB2)) - 1.D0*SB*(2.D0*CB*(Lam4 + Lam5)*SB&
  &2 + 2.D0*CB*(CB2*Lam2 + Lam3*SB2)))
CS2S2S3S3f2111 = 0.5D0*(-1.D0*CB*(2.D0*CB2*(Lam4 + Lam5)*SB + 2.D0*SB*(CB2*Lam3 + Lam2*SB2)) + SB*(2.D0*CB*(Lam4 + Lam5)*SB2 + 2.&
  &D0*CB*(CB2*Lam1 + Lam3*SB2)))
CS2S2S3S3f2112 = 0.5D0*(-1.D0*CB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) + SB*(2.D0*CB*(-1.D&
  &0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f2121 = 0.5D0*(-1.D0*CB*(2.D0*SB*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) + SB*(2.D0*CB*(-1.D&
  &0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f2122 = 0.5D0*(SB*(-2.D0*CB*(Lam4 + Lam5)*SB2 + 2.D0*CB*(CB2*Lam3 + Lam1*SB2)) - 1.D0*CB*(-2.D0*CB2*(Lam4 + Lam5)*SB + 2&
  &.D0*SB*(CB2*Lam2 + Lam3*SB2)))
CS2S2S3S3f2211 = 0.5D0*(-1.D0*CB*(-2.D0*CB*(Lam4 + Lam5)*SB2 + 2.D0*CB*(CB2*Lam3 + Lam2*SB2)) + SB*(2.D0*CB2*(Lam4 + Lam5)*SB - 2&
  &.D0*SB*(CB2*Lam1 + Lam3*SB2)))
CS2S2S3S3f2212 = 0.5D0*(SB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*(CB*&
  &Lam2*SB - 1.D0*CB*Lam3*SB) - 1.D0*(Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f2221 = 0.5D0*(SB*(-2.D0*SB*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + CB*(Lam4 + Lam5)*(CB2 - 1.D0*SB2)) - 1.D0*CB*(2.D0*CB*(CB*&
  &Lam2*SB - 1.D0*CB*Lam3*SB) - 1.D0*(Lam4 + Lam5)*SB*(CB2 - 1.D0*SB2)))
CS2S2S3S3f2222 = 0.5D0*(SB*(-2.D0*CB2*(Lam4 + Lam5)*SB - 2.D0*SB*(CB2*Lam3 + Lam1*SB2)) - 1.D0*CB*(2.D0*CB*(Lam4 + Lam5)*SB2 + 2.&
  &D0*CB*(CB2*Lam2 + Lam3*SB2)))

CS1S1S3S3f1111 = 0.5D0*(-1.D0*RR12*(2.D0*CB*(Lam4 + Lam5)*RR11*SB + 2.D0*RR12*(CB2*Lam3 + Lam2*SB2)) - 1.D0*RR11*(2.D0*CB*(Lam4 +&
  & Lam5)*RR12*SB + 2.D0*RR11*(CB2*Lam1 + Lam3*SB2)) - 2.D0*(CB2*Lam7 + Lam8*SB2)*DBLE(RR13**INT(2.D0)))
CS1S1S3S3f1112 = 0.5D0*(-1.D0*RR12*(2.D0*RR12*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (Lam4 + Lam5)*RR11*(CB2 - 1.D0*SB2)) - 1.D0*RR11*(&
  &2.D0*RR11*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR12*(CB2 - 1.D0*SB2)) - 2.D0*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*DBLE(R&
  &R13**INT(2.D0)))
CS1S1S3S3f1121 = 0.5D0*(-1.D0*RR12*(2.D0*RR12*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (Lam4 + Lam5)*RR11*(CB2 - 1.D0*SB2)) - 1.D0*RR11*(&
  &2.D0*RR11*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR12*(CB2 - 1.D0*SB2)) - 2.D0*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*DBLE(R&
  &R13**INT(2.D0)))
CS1S1S3S3f1122 = 0.5D0*(-1.D0*RR11*(-2.D0*CB*(Lam4 + Lam5)*RR12*SB + 2.D0*RR11*(CB2*Lam3 + Lam1*SB2)) - 1.D0*RR12*(-2.D0*CB*(Lam4&
  & + Lam5)*RR11*SB + 2.D0*RR12*(CB2*Lam2 + Lam3*SB2)) - 2.D0*(CB2*Lam8 + Lam7*SB2)*DBLE(RR13**INT(2.D0)))
CS1S1S3S3f1211 = 0.5D0*(-2.D0*RR13*RR23*(CB2*Lam7 + Lam8*SB2) - 1.D0*RR12*(2.D0*CB*(Lam4 + Lam5)*RR21*SB + 2.D0*RR22*(CB2*Lam3 + &
  &Lam2*SB2)) - 1.D0*RR11*(2.D0*CB*(Lam4 + Lam5)*RR22*SB + 2.D0*RR21*(CB2*Lam1 + Lam3*SB2)))
CS1S1S3S3f1212 = 0.5D0*(-2.D0*RR13*RR23*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR12*(2.D0*RR22*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR21*(CB2 - 1.D0*SB2)) - 1.D0*RR11*(2.D0*RR21*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR22*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f1221 = 0.5D0*(-2.D0*RR13*RR23*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR12*(2.D0*RR22*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR21*(CB2 - 1.D0*SB2)) - 1.D0*RR11*(2.D0*RR21*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR22*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f1222 = 0.5D0*(-2.D0*RR13*RR23*(CB2*Lam8 + Lam7*SB2) - 1.D0*RR11*(-2.D0*CB*(Lam4 + Lam5)*RR22*SB + 2.D0*RR21*(CB2*Lam3 +&
  & Lam1*SB2)) - 1.D0*RR12*(-2.D0*CB*(Lam4 + Lam5)*RR21*SB + 2.D0*RR22*(CB2*Lam2 + Lam3*SB2)))
CS1S1S3S3f1311 = 0.5D0*(-2.D0*RR13*RR33*(CB2*Lam7 + Lam8*SB2) - 1.D0*RR12*(2.D0*CB*(Lam4 + Lam5)*RR31*SB + 2.D0*RR32*(CB2*Lam3 + &
  &Lam2*SB2)) - 1.D0*RR11*(2.D0*CB*(Lam4 + Lam5)*RR32*SB + 2.D0*RR31*(CB2*Lam1 + Lam3*SB2)))
CS1S1S3S3f1312 = 0.5D0*(-2.D0*RR13*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR12*(2.D0*RR32*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR31*(CB2 - 1.D0*SB2)) - 1.D0*RR11*(2.D0*RR31*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR32*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f1321 = 0.5D0*(-2.D0*RR13*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR12*(2.D0*RR32*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR31*(CB2 - 1.D0*SB2)) - 1.D0*RR11*(2.D0*RR31*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR32*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f1322 = 0.5D0*(-2.D0*RR13*RR33*(CB2*Lam8 + Lam7*SB2) - 1.D0*RR11*(-2.D0*CB*(Lam4 + Lam5)*RR32*SB + 2.D0*RR31*(CB2*Lam3 +&
  & Lam1*SB2)) - 1.D0*RR12*(-2.D0*CB*(Lam4 + Lam5)*RR31*SB + 2.D0*RR32*(CB2*Lam2 + Lam3*SB2)))
CS1S1S3S3f2111 = 0.5D0*(-2.D0*RR13*RR23*(CB2*Lam7 + Lam8*SB2) - 1.D0*RR22*(2.D0*CB*(Lam4 + Lam5)*RR11*SB + 2.D0*RR12*(CB2*Lam3 + &
  &Lam2*SB2)) - 1.D0*RR21*(2.D0*CB*(Lam4 + Lam5)*RR12*SB + 2.D0*RR11*(CB2*Lam1 + Lam3*SB2)))
CS1S1S3S3f2112 = 0.5D0*(-2.D0*RR13*RR23*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR22*(2.D0*RR12*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR11*(CB2 - 1.D0*SB2)) - 1.D0*RR21*(2.D0*RR11*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR12*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f2121 = 0.5D0*(-2.D0*RR13*RR23*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR22*(2.D0*RR12*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR11*(CB2 - 1.D0*SB2)) - 1.D0*RR21*(2.D0*RR11*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR12*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f2122 = 0.5D0*(-2.D0*RR13*RR23*(CB2*Lam8 + Lam7*SB2) - 1.D0*RR21*(-2.D0*CB*(Lam4 + Lam5)*RR12*SB + 2.D0*RR11*(CB2*Lam3 +&
  & Lam1*SB2)) - 1.D0*RR22*(-2.D0*CB*(Lam4 + Lam5)*RR11*SB + 2.D0*RR12*(CB2*Lam2 + Lam3*SB2)))
CS1S1S3S3f2211 = 0.5D0*(-1.D0*RR22*(2.D0*CB*(Lam4 + Lam5)*RR21*SB + 2.D0*RR22*(CB2*Lam3 + Lam2*SB2)) - 1.D0*RR21*(2.D0*CB*(Lam4 +&
  & Lam5)*RR22*SB + 2.D0*RR21*(CB2*Lam1 + Lam3*SB2)) - 2.D0*(CB2*Lam7 + Lam8*SB2)*DBLE(RR23**INT(2.D0)))
CS1S1S3S3f2212 = 0.5D0*(-1.D0*RR22*(2.D0*RR22*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (Lam4 + Lam5)*RR21*(CB2 - 1.D0*SB2)) - 1.D0*RR21*(&
  &2.D0*RR21*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR22*(CB2 - 1.D0*SB2)) - 2.D0*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*DBLE(R&
  &R23**INT(2.D0)))
CS1S1S3S3f2221 = 0.5D0*(-1.D0*RR22*(2.D0*RR22*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (Lam4 + Lam5)*RR21*(CB2 - 1.D0*SB2)) - 1.D0*RR21*(&
  &2.D0*RR21*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR22*(CB2 - 1.D0*SB2)) - 2.D0*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*DBLE(R&
  &R23**INT(2.D0)))
CS1S1S3S3f2222 = 0.5D0*(-1.D0*RR21*(-2.D0*CB*(Lam4 + Lam5)*RR22*SB + 2.D0*RR21*(CB2*Lam3 + Lam1*SB2)) - 1.D0*RR22*(-2.D0*CB*(Lam4&
  & + Lam5)*RR21*SB + 2.D0*RR22*(CB2*Lam2 + Lam3*SB2)) - 2.D0*(CB2*Lam8 + Lam7*SB2)*DBLE(RR23**INT(2.D0)))
CS1S1S3S3f2311 = 0.5D0*(-2.D0*RR23*RR33*(CB2*Lam7 + Lam8*SB2) - 1.D0*RR22*(2.D0*CB*(Lam4 + Lam5)*RR31*SB + 2.D0*RR32*(CB2*Lam3 + &
  &Lam2*SB2)) - 1.D0*RR21*(2.D0*CB*(Lam4 + Lam5)*RR32*SB + 2.D0*RR31*(CB2*Lam1 + Lam3*SB2)))
CS1S1S3S3f2312 = 0.5D0*(-2.D0*RR23*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR22*(2.D0*RR32*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR31*(CB2 - 1.D0*SB2)) - 1.D0*RR21*(2.D0*RR31*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR32*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f2321 = 0.5D0*(-2.D0*RR23*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR22*(2.D0*RR32*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR31*(CB2 - 1.D0*SB2)) - 1.D0*RR21*(2.D0*RR31*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR32*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f2322 = 0.5D0*(-2.D0*RR23*RR33*(CB2*Lam8 + Lam7*SB2) - 1.D0*RR21*(-2.D0*CB*(Lam4 + Lam5)*RR32*SB + 2.D0*RR31*(CB2*Lam3 +&
  & Lam1*SB2)) - 1.D0*RR22*(-2.D0*CB*(Lam4 + Lam5)*RR31*SB + 2.D0*RR32*(CB2*Lam2 + Lam3*SB2)))
CS1S1S3S3f3111 = 0.5D0*(-2.D0*RR13*RR33*(CB2*Lam7 + Lam8*SB2) - 1.D0*RR32*(2.D0*CB*(Lam4 + Lam5)*RR11*SB + 2.D0*RR12*(CB2*Lam3 + &
  &Lam2*SB2)) - 1.D0*RR31*(2.D0*CB*(Lam4 + Lam5)*RR12*SB + 2.D0*RR11*(CB2*Lam1 + Lam3*SB2)))
CS1S1S3S3f3112 = 0.5D0*(-2.D0*RR13*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR32*(2.D0*RR12*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR11*(CB2 - 1.D0*SB2)) - 1.D0*RR31*(2.D0*RR11*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR12*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f3121 = 0.5D0*(-2.D0*RR13*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR32*(2.D0*RR12*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR11*(CB2 - 1.D0*SB2)) - 1.D0*RR31*(2.D0*RR11*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR12*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f3122 = 0.5D0*(-2.D0*RR13*RR33*(CB2*Lam8 + Lam7*SB2) - 1.D0*RR31*(-2.D0*CB*(Lam4 + Lam5)*RR12*SB + 2.D0*RR11*(CB2*Lam3 +&
  & Lam1*SB2)) - 1.D0*RR32*(-2.D0*CB*(Lam4 + Lam5)*RR11*SB + 2.D0*RR12*(CB2*Lam2 + Lam3*SB2)))
CS1S1S3S3f3211 = 0.5D0*(-2.D0*RR23*RR33*(CB2*Lam7 + Lam8*SB2) - 1.D0*RR32*(2.D0*CB*(Lam4 + Lam5)*RR21*SB + 2.D0*RR22*(CB2*Lam3 + &
  &Lam2*SB2)) - 1.D0*RR31*(2.D0*CB*(Lam4 + Lam5)*RR22*SB + 2.D0*RR21*(CB2*Lam1 + Lam3*SB2)))
CS1S1S3S3f3212 = 0.5D0*(-2.D0*RR23*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR32*(2.D0*RR22*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR21*(CB2 - 1.D0*SB2)) - 1.D0*RR31*(2.D0*RR21*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR22*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f3221 = 0.5D0*(-2.D0*RR23*RR33*(-1.D0*CB*Lam7*SB + CB*Lam8*SB) - 1.D0*RR32*(2.D0*RR22*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (&
  &Lam4 + Lam5)*RR21*(CB2 - 1.D0*SB2)) - 1.D0*RR31*(2.D0*RR21*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR22*(CB2 - 1.D0*SB&
  &2)))
CS1S1S3S3f3222 = 0.5D0*(-2.D0*RR23*RR33*(CB2*Lam8 + Lam7*SB2) - 1.D0*RR31*(-2.D0*CB*(Lam4 + Lam5)*RR22*SB + 2.D0*RR21*(CB2*Lam3 +&
  & Lam1*SB2)) - 1.D0*RR32*(-2.D0*CB*(Lam4 + Lam5)*RR21*SB + 2.D0*RR22*(CB2*Lam2 + Lam3*SB2)))
CS1S1S3S3f3311 = 0.5D0*(-1.D0*RR32*(2.D0*CB*(Lam4 + Lam5)*RR31*SB + 2.D0*RR32*(CB2*Lam3 + Lam2*SB2)) - 1.D0*RR31*(2.D0*CB*(Lam4 +&
  & Lam5)*RR32*SB + 2.D0*RR31*(CB2*Lam1 + Lam3*SB2)) - 2.D0*(CB2*Lam7 + Lam8*SB2)*DBLE(RR33**INT(2.D0)))
CS1S1S3S3f3312 = 0.5D0*(-1.D0*RR32*(2.D0*RR32*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (Lam4 + Lam5)*RR31*(CB2 - 1.D0*SB2)) - 1.D0*RR31*(&
  &2.D0*RR31*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR32*(CB2 - 1.D0*SB2)) - 2.D0*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*DBLE(R&
  &R33**INT(2.D0)))
CS1S1S3S3f3321 = 0.5D0*(-1.D0*RR32*(2.D0*RR32*(CB*Lam2*SB - 1.D0*CB*Lam3*SB) + (Lam4 + Lam5)*RR31*(CB2 - 1.D0*SB2)) - 1.D0*RR31*(&
  &2.D0*RR31*(-1.D0*CB*Lam1*SB + CB*Lam3*SB) + (Lam4 + Lam5)*RR32*(CB2 - 1.D0*SB2)) - 2.D0*(-1.D0*CB*Lam7*SB + CB*Lam8*SB)*DBLE(R&
  &R33**INT(2.D0)))
CS1S1S3S3f3322 = 0.5D0*(-1.D0*RR31*(-2.D0*CB*(Lam4 + Lam5)*RR32*SB + 2.D0*RR31*(CB2*Lam3 + Lam1*SB2)) - 1.D0*RR32*(-2.D0*CB*(Lam4&
  & + Lam5)*RR31*SB + 2.D0*RR32*(CB2*Lam2 + Lam3*SB2)) - 2.D0*(CB2*Lam8 + Lam7*SB2)*DBLE(RR33**INT(2.D0)))

    end if

end subroutine getParameters
