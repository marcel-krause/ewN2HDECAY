module counterterms
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

    contains
        ! Tadpole counterterms
            ! Alternative tadpole based counterterms
                ! Tadpole counterterms are not needed in the alternative tadpole approach

            ! Usual tadpole based counterterms
                double precision function dTH1H1()
                    use constants
                    implicit none
                    double complex TadH1, TadH2, TadH3, TadInt1, TadInt2, TadInt3
                    TadInt1 = CA1*CA2*TadH1() - (CA3*SA1+CA1*SA2*SA3)*TadH2() + (SA1*SA3-CA1*CA3*SA2)*TadH3()
                    TadInt2 = CA2*SA1*TadH1() + (CA1*CA3 - SA1*SA2*SA3)*TadH2() - (CA3*SA1*SA2+CA1*SA3)*TadH3()
                    TadInt3 = SA2*TadH1() + CA2*SA3*TadH2() + CA2*CA3*TadH3()
                    dTH1H1 = (CA1**2*CA2**2*EL*TadInt1)/(2*CB*MW*SW) + (CA2**2*EL*SA1**2*TadInt2)/(2*MW*SB*SW) + (SA2**2*TadInt3)/vS
                end function dTH1H1

                double precision function dTH1H2()
                    use constants
                    implicit none
                    double complex TadH1, TadH2, TadH3, TadInt1, TadInt2, TadInt3
                    TadInt1 = CA1*CA2*TadH1() - (CA3*SA1+CA1*SA2*SA3)*TadH2() + (SA1*SA3-CA1*CA3*SA2)*TadH3()
                    TadInt2 = CA2*SA1*TadH1() + (CA1*CA3 - SA1*SA2*SA3)*TadH2() - (CA3*SA1*SA2+CA1*SA3)*TadH3()
                    TadInt3 = SA2*TadH1() + CA2*SA3*TadH2() + CA2*CA3*TadH3()
                    dTH1H2 = (CA1*CA2*EL*(-(CA3*SA1) - CA1*SA2*SA3)*TadInt1)/(2*CB*MW*SW) + (CA2*EL*SA1*(CA1*CA3 - SA1*SA2*SA3)*&
                                &TadInt2)/(2*MW*SB*SW) + (CA2*SA2*SA3*TadInt3)/vS
                end function dTH1H2

                double precision function dTH1H3()
                    use constants
                    implicit none
                    double complex TadH1, TadH2, TadH3, TadInt1, TadInt2, TadInt3
                    TadInt1 = CA1*CA2*TadH1() - (CA3*SA1+CA1*SA2*SA3)*TadH2() + (SA1*SA3-CA1*CA3*SA2)*TadH3()
                    TadInt2 = CA2*SA1*TadH1() + (CA1*CA3 - SA1*SA2*SA3)*TadH2() - (CA3*SA1*SA2+CA1*SA3)*TadH3()
                    TadInt3 = SA2*TadH1() + CA2*SA3*TadH2() + CA2*CA3*TadH3()
                    dTH1H3 = (CA1*CA2*EL*(-(CA1*CA3*SA2) + SA1*SA3)*TadInt1)/(2*CB*MW*SW) + (CA2*EL*SA1*(-(CA3*SA1*SA2) - CA1*SA3)*&
                                &TadInt2)/(2*MW*SB*SW) + (CA2*CA3*SA2*TadInt3)/vS
                end function dTH1H3

                double precision function dTH2H2()
                    use constants
                    implicit none
                    double complex TadH1, TadH2, TadH3, TadInt1, TadInt2, TadInt3
                    TadInt1 = CA1*CA2*TadH1() - (CA3*SA1+CA1*SA2*SA3)*TadH2() + (SA1*SA3-CA1*CA3*SA2)*TadH3()
                    TadInt2 = CA2*SA1*TadH1() + (CA1*CA3 - SA1*SA2*SA3)*TadH2() - (CA3*SA1*SA2+CA1*SA3)*TadH3()
                    TadInt3 = SA2*TadH1() + CA2*SA3*TadH2() + CA2*CA3*TadH3()
                    dTH2H2 = (EL*(-(CA3*SA1) - CA1*SA2*SA3)**2*TadInt1)/(2*CB*MW*SW) + (EL*(CA1*CA3 - SA1*SA2*SA3)**2*TadInt2)/(2*&
                                &MW*SB*SW) + (CA2**2*SA3**2*TadInt3)/vS
                end function dTH2H2

                double precision function dTH2H3()
                    use constants
                    implicit none
                    double complex TadH1, TadH2, TadH3, TadInt1, TadInt2, TadInt3
                    TadInt1 = CA1*CA2*TadH1() - (CA3*SA1+CA1*SA2*SA3)*TadH2() + (SA1*SA3-CA1*CA3*SA2)*TadH3()
                    TadInt2 = CA2*SA1*TadH1() + (CA1*CA3 - SA1*SA2*SA3)*TadH2() - (CA3*SA1*SA2+CA1*SA3)*TadH3()
                    TadInt3 = SA2*TadH1() + CA2*SA3*TadH2() + CA2*CA3*TadH3()
                    dTH2H3 = (EL*(-(CA1*CA3*SA2) + SA1*SA3)*(-(CA3*SA1) - CA1*SA2*SA3)*TadInt1)/(2*CB*MW*SW) + (EL*(-(CA3*SA1*SA2)&
                                & - CA1*SA3)*(CA1*CA3 - SA1*SA2*SA3)*TadInt2)/(2*MW*SB*SW) + (CA2**2*CA3*SA3*TadInt3)/vS
                end function dTH2H3

                double precision function dTH3H3()
                    use constants
                    implicit none
                    double complex TadH1, TadH2, TadH3, TadInt1, TadInt2, TadInt3
                    TadInt1 = CA1*CA2*TadH1() - (CA3*SA1+CA1*SA2*SA3)*TadH2() + (SA1*SA3-CA1*CA3*SA2)*TadH3()
                    TadInt2 = CA2*SA1*TadH1() + (CA1*CA3 - SA1*SA2*SA3)*TadH2() - (CA3*SA1*SA2+CA1*SA3)*TadH3()
                    TadInt3 = SA2*TadH1() + CA2*SA3*TadH2() + CA2*CA3*TadH3()
                    dTH3H3 = (EL*(-(CA1*CA3*SA2) + SA1*SA3)**2*TadInt1)/(2*CB*MW*SW) + (EL*(-(CA3*SA1*SA2) - CA1*SA3)**2*TadInt2)/&
                                &(2*MW*SB*SW) + (CA2**2*CA3**2*TadInt3)/vS
                end function dTH3H3

                double precision function dTGpGp()
                    use constants
                    implicit none
                    double complex TadH1, TadH2, TadH3, TadInt1, TadInt2
                    TadInt1 = CA1*CA2*TadH1() - (CA3*SA1+CA1*SA2*SA3)*TadH2() + (SA1*SA3-CA1*CA3*SA2)*TadH3()
                    TadInt2 = CA2*SA1*TadH1() + (CA1*CA3 - SA1*SA2*SA3)*TadH2() - (CA3*SA1*SA2+CA1*SA3)*TadH3()
                    dTGpGp = (CB*EL*TadInt1)/(2*MW*SW) + (EL*SB*TadInt2)/(2*MW*SW)
                end function dTGpGp

                double precision function dTGpHp()
                    use constants
                    implicit none
                    double complex TadH1, TadH2, TadH3, TadInt1, TadInt2
                    TadInt1 = CA1*CA2*TadH1() - (CA3*SA1+CA1*SA2*SA3)*TadH2() + (SA1*SA3-CA1*CA3*SA2)*TadH3()
                    TadInt2 = CA2*SA1*TadH1() + (CA1*CA3 - SA1*SA2*SA3)*TadH2() - (CA3*SA1*SA2+CA1*SA3)*TadH3()
                    dTGpHp = -(EL*SB*TadInt1)/(2*MW*SW) + (CB*EL*TadInt2)/(2*MW*SW)
                end function dTGpHp

                double precision function dTHpHp()
                    use constants
                    implicit none
                    double complex TadH1, TadH2, TadH3, TadInt1, TadInt2
                    TadInt1 = CA1*CA2*TadH1() - (CA3*SA1+CA1*SA2*SA3)*TadH2() + (SA1*SA3-CA1*CA3*SA2)*TadH3()
                    TadInt2 = CA2*SA1*TadH1() + (CA1*CA3 - SA1*SA2*SA3)*TadH2() - (CA3*SA1*SA2+CA1*SA3)*TadH3()
                    dTHpHp = (EL*SB**2*TadInt1)/(2*CB*MW*SW) + (CB**2*EL*TadInt2)/(2*MW*SB*SW)
                end function dTHpHp

                double precision function dTG0G0()
                    use constants
                    implicit none
                    double complex TadH1, TadH2, TadH3, TadInt1, TadInt2
                    TadInt1 = CA1*CA2*TadH1() - (CA3*SA1+CA1*SA2*SA3)*TadH2() + (SA1*SA3-CA1*CA3*SA2)*TadH3()
                    TadInt2 = CA2*SA1*TadH1() + (CA1*CA3 - SA1*SA2*SA3)*TadH2() - (CA3*SA1*SA2+CA1*SA3)*TadH3()
                    dTG0G0 = (CB*EL*TadInt1)/(2*MW*SW) + (EL*SB*TadInt2)/(2*MW*SW)
                end function dTG0G0

                double precision function dTG0A0()
                    use constants
                    implicit none
                    double complex TadH1, TadH2, TadH3, TadInt1, TadInt2
                    TadInt1 = CA1*CA2*TadH1() - (CA3*SA1+CA1*SA2*SA3)*TadH2() + (SA1*SA3-CA1*CA3*SA2)*TadH3()
                    TadInt2 = CA2*SA1*TadH1() + (CA1*CA3 - SA1*SA2*SA3)*TadH2() - (CA3*SA1*SA2+CA1*SA3)*TadH3()
                    dTG0A0 = -(EL*SB*TadInt1)/(2*MW*SW) + (CB*EL*TadInt2)/(2*MW*SW)
                end function dTG0A0

                double precision function dTA0A0()
                    use constants
                    implicit none
                    double complex TadH1, TadH2, TadH3, TadInt1, TadInt2
                    TadInt1 = CA1*CA2*TadH1() - (CA3*SA1+CA1*SA2*SA3)*TadH2() + (SA1*SA3-CA1*CA3*SA2)*TadH3()
                    TadInt2 = CA2*SA1*TadH1() + (CA1*CA3 - SA1*SA2*SA3)*TadH2() - (CA3*SA1*SA2+CA1*SA3)*TadH3()
                    dTA0A0 = (EL*SB**2*TadInt1)/(2*CB*MW*SW) + (CB**2*EL*TadInt2)/(2*MW*SB*SW)
                end function dTA0A0

        ! Counterterms of the electroweak sector
            ! Alternative tadpole based counterterms
                double precision function dMZ2Alter()
                    use constants
                    implicit none
                    double complex SelfZ0Z0Alter
                    dMZ2Alter = DBLE(SelfZ0Z0Alter(MZ2))
                end function dMZ2Alter

                double precision function dMW2Alter()
                    use constants
                    implicit none
                    double complex SelfWpWpAlter
                    dMW2Alter = DBLE(SelfWpWpAlter(MW2))
                end function dMW2Alter

            ! Usual tadpole based counterterms
                double precision function dMZ2Usual()
                    use constants
                    implicit none
                    double complex SelfZ0Z0Usual
                    dMZ2Usual = DBLE(SelfZ0Z0Usual(MZ2))
                end function dMZ2Usual

                double precision function dMW2Usual()
                    use constants
                    implicit none
                    double complex SelfWpWpUsual
                    dMW2Usual = DBLE(SelfWpWpUsual(MW2))
                end function dMW2Usual

            ! Tadpole invariant counterterms
                double precision function dZWpWpOS()
                    use constants
                    implicit none
                    double complex DSelfWpWp
                    dZWpWpOS = -DBLE(DSelfWpWp(MW2))
                end function dZWpWpOS

                double precision function dZZ0Z0OS()
                    use constants
                    implicit none
                    double complex DSelfZ0Z0
                    dZZ0Z0OS = -DBLE(DSelfZ0Z0(MZ2))
                end function dZZ0Z0OS

                double precision function dZAAOS()
                    use constants
                    implicit none
                    double complex DSelfAA
                    dZAAOS = -DBLE(DSelfAA(0.D0))
                end function dZAAOS

                double precision function dZZ0AOS()
                    use constants
                    implicit none
                    double complex SelfAZ0UsualZeroMom
                    dZZ0AOS = 2.D0*DBLE(SelfAZ0UsualZeroMom(0.D0))/MZ2
                end function dZZ0AOS

                double precision function dZAZ0OS()
                    use constants
                    implicit none
                    double complex SelfAZ0Usual
                    dZAZ0OS = -2.D0*DBLE(SelfAZ0Usual(MZ2))/MZ2
                end function dZAZ0OS

                double precision function dZe()
                    use constants
                    implicit none
                    double complex SelfAZ0UsualZeroMom, DSelfAA
                    dZe = (SW*DBLE(SelfAZ0UsualZeroMom(0.D0)))/(CW*MZ2) + DBLE(DSelfAA(0.D0))/2.D0
                end function dZe

                double precision function dg()
                    use constants
                    implicit none
                    dg = EL*( dZe() + (dMW2Usual() - MW2*dMZ2Usual()/MZ2)/(2.D0*(MZ2 - MW2)) )/SW
                end function dg

                double precision function dZeAtMZ()
                    use constants
                    implicit none
                    double complex SelfAALight, DSelfAALight
                    dZeAtMZ = dZe() - 0.5D0*( DBLE(DSelfAALight(0.D0)) - DBLE(SelfAALight(MZ2))/MZ2 )
                end function dZeAtMZ

                double precision function dgAtMZ()
                    use constants
                    implicit none
                    dgAtMZ = EL*( dZeAtMZ() + (dMW2Usual() - MW2*dMZ2Usual()/MZ2)/(2.D0*(MZ2 - MW2)) )/SW
                end function dgAtMZ

        ! Counterterms of the scalar sector
            ! Alternative tadpole based counterterms
                double precision function dZH1H2OSAlter()
                    use constants
                    implicit none
                    double complex SelfH1H2Alter
                    dZH1H2OSAlter = 2.D0*(DBLE(SelfH1H2Alter(MH22)))/(MH12 - MH22)
                end function dZH1H2OSAlter

                double precision function dZH2H1OSAlter()
                    use constants
                    implicit none
                    double complex SelfH1H2Alter
                    dZH2H1OSAlter = 2.D0*(DBLE(SelfH1H2Alter(MH12)))/(MH22 - MH12)
                end function dZH2H1OSAlter

                double precision function dZH1H3OSAlter()
                    use constants
                    implicit none
                    double complex SelfH1H3Alter
                    dZH1H3OSAlter = 2.D0*(DBLE(SelfH1H3Alter(MH32)))/(MH12 - MH32)
                end function dZH1H3OSAlter

                double precision function dZH3H1OSAlter()
                    use constants
                    implicit none
                    double complex SelfH1H3Alter
                    dZH3H1OSAlter = 2.D0*(DBLE(SelfH1H3Alter(MH12)))/(MH32 - MH12)
                end function dZH3H1OSAlter

                double precision function dZH2H3OSAlter()
                    use constants
                    implicit none
                    double complex SelfH2H3Alter
                    dZH2H3OSAlter = 2.D0*(DBLE(SelfH2H3Alter(MH32)))/(MH22 - MH32)
                end function dZH2H3OSAlter

                double precision function dZH3H2OSAlter()
                    use constants
                    implicit none
                    double complex SelfH2H3Alter
                    dZH3H2OSAlter = 2.D0*(DBLE(SelfH2H3Alter(MH22)))/(MH32 - MH22)
                end function dZH3H2OSAlter

                double precision function dZG0A0OSAlter()
                    use constants
                    implicit none
                    double complex SelfG0A0Alter
                    dZG0A0OSAlter = -2.D0*(DBLE(SelfG0A0Alter(MA02)))/(MA02)
                end function dZG0A0OSAlter

                double precision function dZA0G0OSAlter()
                    use constants
                    implicit none
                    double complex SelfG0A0Alter
                    dZA0G0OSAlter = 2.D0*(DBLE(SelfG0A0Alter(0.D0)))/(MA02)
                end function dZA0G0OSAlter

                double precision function dZGpHpOSAlter()
                    use constants
                    implicit none
                    double complex SelfGpHpAlter
                    dZGpHpOSAlter = -2.D0*(DBLE(SelfGpHpAlter(MHp2)))/(MHp2)
                end function dZGpHpOSAlter

                double precision function dZHpGpOSAlter()
                    use constants
                    implicit none
                    double complex SelfGpHpAlter
                    dZHpGpOSAlter = 2.D0*(DBLE(SelfGpHpAlter(0.D0)))/(MHp2)
                end function dZHpGpOSAlter

                double precision function dMH12OSAlter()
                    use constants
                    implicit none
                    double complex SelfH1H1Alter
                    dMH12OSAlter = DBLE(SelfH1H1Alter(MH12))
                end function dMH12OSAlter

                double precision function dMH22OSAlter()
                    use constants
                    implicit none
                    double complex SelfH2H2Alter
                    dMH22OSAlter = DBLE(SelfH2H2Alter(MH22))
                end function dMH22OSAlter

                double precision function dMH32OSAlter()
                    use constants
                    implicit none
                    double complex SelfH3H3Alter
                    dMH32OSAlter = DBLE(SelfH3H3Alter(MH32))
                end function dMH32OSAlter

                double precision function dMA02OSAlter()
                    use constants
                    implicit none
                    double complex SelfA0A0Alter
                    dMA02OSAlter = DBLE(SelfA0A0Alter(MA02))
                end function dMA02OSAlter

                double precision function dMHp2OSAlter()
                    use constants
                    implicit none
                    double complex SelfHpHpAlter
                    dMHp2OSAlter = DBLE(SelfHpHpAlter(MHp2))
                end function dMHp2OSAlter

            ! Usual tadpole based counterterms
                double precision function dZH1H2OSUsual()
                    use constants
                    implicit none
                    double complex SelfH1H2Usual
                    dZH1H2OSUsual = 2.D0*(DBLE(SelfH1H2Usual(MH22)) - DBLE(dTH1H2()))/(MH12 - MH22)
                end function dZH1H2OSUsual

                double precision function dZH2H1OSUsual()
                    use constants
                    implicit none
                    double complex SelfH1H2Usual
                    dZH2H1OSUsual = 2.D0*(DBLE(SelfH1H2Usual(MH12)) - DBLE(dTH1H2()))/(MH22 - MH12)
                end function dZH2H1OSUsual

                double precision function dZH1H3OSUsual()
                    use constants
                    implicit none
                    double complex SelfH1H3Usual
                    dZH1H3OSUsual = 2.D0*(DBLE(SelfH1H3Usual(MH32)) - DBLE(dTH1H3()))/(MH12 - MH32)
                end function dZH1H3OSUsual

                double precision function dZH3H1OSUsual()
                    use constants
                    implicit none
                    double complex SelfH1H3Usual
                    dZH3H1OSUsual = 2.D0*(DBLE(SelfH1H3Usual(MH12)) - DBLE(dTH1H3()))/(MH32 - MH12)
                end function dZH3H1OSUsual

                double precision function dZH2H3OSUsual()
                    use constants
                    implicit none
                    double complex SelfH2H3Usual
                    dZH2H3OSUsual = 2.D0*(DBLE(SelfH2H3Usual(MH32)) - DBLE(dTH2H3()))/(MH22 - MH32)
                end function dZH2H3OSUsual

                double precision function dZH3H2OSUsual()
                    use constants
                    implicit none
                    double complex SelfH2H3Usual
                    dZH3H2OSUsual = 2.D0*(DBLE(SelfH2H3Usual(MH22)) - DBLE(dTH2H3()))/(MH32 - MH22)
                end function dZH3H2OSUsual

                double precision function dZG0A0OSUsual()
                    use constants
                    implicit none
                    double complex SelfG0A0Usual
                    dZG0A0OSUsual = -2.D0*(DBLE(SelfG0A0Usual(MA02)) - DBLE(dTG0A0()))/(MA02)
                end function dZG0A0OSUsual

                double precision function dZA0G0OSUsual()
                    use constants
                    implicit none
                    double complex SelfG0A0Usual
                    dZA0G0OSUsual = 2.D0*(DBLE(SelfG0A0Usual(0.D0)) - DBLE(dTG0A0()))/(MA02)
                end function dZA0G0OSUsual

                double precision function dZGpHpOSUsual()
                    use constants
                    implicit none
                    double complex SelfGpHpUsual
                    dZGpHpOSUsual = -2.D0*(DBLE(SelfGpHpUsual(MHp2)) - DBLE(dTGpHp()))/(MHp2)
                end function dZGpHpOSUsual

                double precision function dZHpGpOSUsual()
                    use constants
                    implicit none
                    double complex SelfGpHpUsual
                    dZHpGpOSUsual = 2.D0*(DBLE(SelfGpHpUsual(0.D0)) - DBLE(dTGpHp()))/(MHp2)
                end function dZHpGpOSUsual

                double precision function dMH12OSUsual()
                    use constants
                    implicit none
                    double complex SelfH1H1Usual
                    dMH12OSUsual = DBLE(SelfH1H1Usual(MH12)) - DBLE(dTH1H1())
                end function dMH12OSUsual

                double precision function dMH22OSUsual()
                    use constants
                    implicit none
                    double complex SelfH2H2Usual
                    dMH22OSUsual = DBLE(SelfH2H2Usual(MH22)) - DBLE(dTH2H2())
                end function dMH22OSUsual

                double precision function dMH32OSUsual()
                    use constants
                    implicit none
                    double complex SelfH3H3Usual
                    dMH32OSUsual = DBLE(SelfH3H3Usual(MH32)) - DBLE(dTH3H3())
                end function dMH32OSUsual
                
                double precision function dMA02OSUsual()
                    use constants
                    implicit none
                    double complex SelfA0A0Usual
                    dMA02OSUsual = DBLE(SelfA0A0Usual(MA02)) - DBLE(dTA0A0())
                end function dMA02OSUsual

                double precision function dMHp2OSUsual()
                    use constants
                    implicit none
                    double complex SelfHpHpUsual
                    dMHp2OSUsual = DBLE(SelfHpHpUsual(MHp2)) - DBLE(dTHpHp())
                end function dMHp2OSUsual

            ! Tadpole invariant counterterms
                double precision function dZH1H1OS()
                    use constants
                    implicit none
                    double complex DSelfH1H1
                    dZH1H1OS = -DBLE(DSelfH1H1(MH12))
                end function dZH1H1OS

                double precision function dZH2H2OS()
                    use constants
                    implicit none
                    double complex DSelfH2H2
                    dZH2H2OS = -DBLE(DSelfH2H2(MH22))
                end function dZH2H2OS

                double precision function dZH3H3OS()
                    use constants
                    implicit none
                    double complex DSelfH3H3
                    dZH3H3OS = -DBLE(DSelfH3H3(MH32))
                end function dZH3H3OS

                double precision function dZG0G0OS()
                    use constants
                    implicit none
                    double complex DSelfG0G0
                    dZG0G0OS = -DBLE(DSelfG0G0(0.D0))
                end function dZG0G0OS

                double precision function dZA0A0OS()
                    use constants
                    implicit none
                    double complex DSelfA0A0
                    dZA0A0OS = -DBLE(DSelfA0A0(MA02))
                end function dZA0A0OS

                double precision function dZGpGpOS()
                    use constants
                    implicit none
                    double complex DSelfGpGp
                    dZGpGpOS = -DBLE(DSelfGpGp(0.D0))
                end function dZGpGpOS

                double precision function dZHpHpOS()
                    use constants
                    implicit none
                    double complex DSelfHpHp
                    dZHpHpOS = -DBLE(DSelfHpHp(MHp2))
                end function dZHpHpOS

        ! Counterterms of the fermion sector
            ! Alternative tadpole based counterterms
                double precision function dMEOSAlter()
                    use constants
                    implicit none
                    double complex SelfElElLeftAlter, SelfElElRightAlter, SelfElElScalarAlter
                    dMEOSAlter = ME/2D0*DBLE( SelfElElLeftAlter(ME2) + SelfElElRightAlter(ME2) + &
                        & 2D0*SelfElElScalarAlter(ME2) )
                end function dMEOSAlter

                double precision function dMMOSAlter()
                    use constants
                    implicit none
                    double complex SelfMuMuLeftAlter, SelfMuMuRightAlter, SelfMuMuScalarAlter
                    dMMOSAlter = MM/2D0*DBLE( SelfMuMuLeftAlter(MM2) + SelfMuMuRightAlter(MM2) + &
                        & 2D0*SelfMuMuScalarAlter(MM2) )
                end function dMMOSAlter

                double precision function dMLOSAlter()
                    use constants
                    implicit none
                    double complex SelfTauTauLeftAlter, SelfTauTauRightAlter, SelfTauTauScalarAlter
                    dMLOSAlter = ML/2D0*DBLE( SelfTauTauLeftAlter(ML2) + SelfTauTauRightAlter(ML2) + &
                        & 2D0*SelfTauTauScalarAlter(ML2) )
                end function dMLOSAlter

                ! double precision function dMLOSAlterWeak()
                !     use constants
                !     implicit none
                !     double complex SelfTauTauLeftWeakAlter, SelfTauTauRightWeakAlter, SelfTauTauScalarWeakAlter
                !     dMLOSAlterWeak = ML/2D0*DBLE( SelfTauTauLeftWeakAlter(ML2) + SelfTauTauRightWeakAlter(ML2) + &
                !         & 2D0*SelfTauTauScalarWeakAlter(ML2) )
                ! end function dMLOSAlterWeak

                double precision function dMDOSAlter()
                    use constants
                    implicit none
                    double complex SelfDDLeftAlter, SelfDDRightAlter, SelfDDScalarAlter
                    dMDOSAlter = MD/2D0*DBLE( SelfDDLeftAlter(MD2) + SelfDDRightAlter(MD2) + &
                        & 2D0*SelfDDScalarAlter(MD2) )
                end function dMDOSAlter

                double precision function dMUOSAlter()
                    use constants
                    implicit none
                    double complex SelfUULeftAlter, SelfUURightAlter, SelfUUScalarAlter
                    dMUOSAlter = MU/2D0*DBLE( SelfUULeftAlter(MU2) + SelfUURightAlter(MU2) + &
                        & 2D0*SelfUUScalarAlter(MU2) )
                end function dMUOSAlter

                double precision function dMSOSAlter()
                    use constants
                    implicit none
                    double complex SelfSSLeftAlter, SelfSSRightAlter, SelfSSScalarAlter
                    dMSOSAlter = MS/2D0*DBLE( SelfSSLeftAlter(MS2) + SelfSSRightAlter(MS2) + &
                        & 2D0*SelfSSScalarAlter(MS2) )
                end function dMSOSAlter

                double precision function dMCOSAlter()
                    use constants
                    implicit none
                    double complex SelfCCLeftAlter, SelfCCRightAlter, SelfCCScalarAlter
                    dMCOSAlter = MC/2D0*DBLE( SelfCCLeftAlter(MC2) + SelfCCRightAlter(MC2) + &
                        & 2D0*SelfCCScalarAlter(MC2) )
                end function dMCOSAlter

                double precision function dMBOSAlter()
                    use constants
                    implicit none
                    double complex SelfBBLeftAlter, SelfBBRightAlter, SelfBBScalarAlter
                    dMBOSAlter = MB/2D0*DBLE( SelfBBLeftAlter(MB2) + SelfBBRightAlter(MB2) + &
                        & 2D0*SelfBBScalarAlter(MB2) )
                end function dMBOSAlter

                double precision function dMTOSAlter()
                    use constants
                    implicit none
                    double complex SelfTTLeftAlter, SelfTTRightAlter, SelfTTScalarAlter
                    dMTOSAlter = MT/2D0*DBLE( SelfTTLeftAlter(MT2) + SelfTTRightAlter(MT2) + &
                        & 2D0*SelfTTScalarAlter(MT2) )
                end function dMTOSAlter

            ! Usual tadpole based counterterms
                double precision function dMEOSUsual()
                    use constants
                    implicit none
                    double complex SelfElElLeftUsual, SelfElElRightUsual, SelfElElScalarUsual
                    dMEOSUsual = ME/2D0*DBLE( SelfElElLeftUsual(ME2) + SelfElElRightUsual(ME2) + &
                        & 2D0*SelfElElScalarUsual(ME2) )
                end function dMEOSUsual

                double precision function dMMOSUsual()
                    use constants
                    implicit none
                    double complex SelfMuMuLeftUsual, SelfMuMuRightUsual, SelfMuMuScalarUsual
                    dMMOSUsual = MM/2D0*DBLE( SelfMuMuLeftUsual(MM2) + SelfMuMuRightUsual(MM2) + &
                        & 2D0*SelfMuMuScalarUsual(MM2) )
                end function dMMOSUsual

                double precision function dMLOSUsual()
                    use constants
                    implicit none
                    double complex SelfTauTauLeftUsual, SelfTauTauRightUsual, SelfTauTauScalarUsual
                    dMLOSUsual = ML/2D0*DBLE( SelfTauTauLeftUsual(ML2) + SelfTauTauRightUsual(ML2) + &
                        & 2D0*SelfTauTauScalarUsual(ML2) )
                end function dMLOSUsual

                ! double precision function dMLOSUsualWeak()
                !     use constants
                !     implicit none
                !     double complex SelfTauTauLeftWeakUsual, SelfTauTauRightWeakUsual, SelfTauTauScalarWeakUsual
                !     dMLOSUsualWeak = ML/2D0*DBLE( SelfTauTauLeftWeakUsual(ML2) + SelfTauTauRightWeakUsual(ML2) + &
                !         & 2D0*SelfTauTauScalarWeakUsual(ML2) )
                ! end function dMLOSUsualWeak

                double precision function dMDOSUsual()
                    use constants
                    implicit none
                    double complex SelfDDLeftUsual, SelfDDRightUsual, SelfDDScalarUsual
                    dMDOSUsual = MD/2D0*DBLE( SelfDDLeftUsual(MD2) + SelfDDRightUsual(MD2) + &
                        & 2D0*SelfDDScalarUsual(MD2) )
                end function dMDOSUsual

                double precision function dMUOSUsual()
                    use constants
                    implicit none
                    double complex SelfUULeftUsual, SelfUURightUsual, SelfUUScalarUsual
                    dMUOSUsual = MU/2D0*DBLE( SelfUULeftUsual(MU2) + SelfUURightUsual(MU2) + &
                        & 2D0*SelfUUScalarUsual(MU2) )
                end function dMUOSUsual

                double precision function dMSOSUsual()
                    use constants
                    implicit none
                    double complex SelfSSLeftUsual, SelfSSRightUsual, SelfSSScalarUsual
                    dMSOSUsual = MS/2D0*DBLE( SelfSSLeftUsual(MS2) + SelfSSRightUsual(MS2) + &
                        & 2D0*SelfSSScalarUsual(MS2) )
                end function dMSOSUsual

                double precision function dMCOSUsual()
                    use constants
                    implicit none
                    double complex SelfCCLeftUsual, SelfCCRightUsual, SelfCCScalarUsual
                    dMCOSUsual = MC/2D0*DBLE( SelfCCLeftUsual(MC2) + SelfCCRightUsual(MC2) + &
                        & 2D0*SelfCCScalarUsual(MC2) )
                end function dMCOSUsual

                double precision function dMBOSUsual()
                    use constants
                    implicit none
                    double complex SelfBBLeftUsual, SelfBBRightUsual, SelfBBScalarUsual
                    dMBOSUsual = MB/2D0*DBLE( SelfBBLeftUsual(MB2) + SelfBBRightUsual(MB2) + &
                        & 2D0*SelfBBScalarUsual(MB2) )
                end function dMBOSUsual

                double precision function dMTOSUsual()
                    use constants
                    implicit none
                    double complex SelfTTLeftUsual, SelfTTRightUsual, SelfTTScalarUsual
                    dMTOSUsual = MT/2D0*DBLE( SelfTTLeftUsual(MT2) + SelfTTRightUsual(MT2) + &
                        & 2D0*SelfTTScalarUsual(MT2) )
                end function dMTOSUsual

            ! Tadpole invariant counterterms
                double precision function dZNeuENeuEOSRight()
                    use constants
                    implicit none
                    double complex SelfNeuENeuERightUsual, DSelfNeuENeuELeft, DSelfNeuENeuERight
                    double complex DSelfNeuENeuEScalar
                    dZNeuENeuEOSRight = -DBLE( SelfNeuENeuERightUsual(0D0) ) - 0D0*DBLE( DSelfNeuENeuELeft(0D0) + &
                        & DSelfNeuENeuERight(0D0) + 2D0*DSelfNeuENeuEScalar(0D0) )
                end function dZNeuENeuEOSRight

                double precision function dZNeuENeuEOSLeft()
                    use constants
                    implicit none
                    double complex SelfNeuENeuELeftUsual, DSelfNeuENeuELeft, DSelfNeuENeuERight
                    double complex DSelfNeuENeuEScalar
                    dZNeuENeuEOSLeft = -DBLE( SelfNeuENeuELeftUsual(0D0) ) - 0D0*DBLE( DSelfNeuENeuELeft(0D0) + &
                        & DSelfNeuENeuERight(0D0) + 2D0*DSelfNeuENeuEScalar(0D0) )
                end function dZNeuENeuEOSLeft

                double precision function dZNeuMNeuMOSRight()
                    use constants
                    implicit none
                    double complex SelfNeuMNeuMRightUsual, DSelfNeuMNeuMLeft, DSelfNeuMNeuMRight
                    double complex DSelfNeuMNeuMScalar
                    dZNeuMNeuMOSRight = -DBLE( SelfNeuMNeuMRightUsual(0D0) ) - 0D0*DBLE( DSelfNeuMNeuMLeft(0D0) + &
                        & DSelfNeuMNeuMRight(0D0) + 2D0*DSelfNeuMNeuMScalar(0D0) )
                end function dZNeuMNeuMOSRight

                double precision function dZNeuMNeuMOSLeft()
                    use constants
                    implicit none
                    double complex SelfNeuMNeuMLeftUsual, DSelfNeuMNeuMLeft, DSelfNeuMNeuMRight
                    double complex DSelfNeuMNeuMScalar
                    dZNeuMNeuMOSLeft = -DBLE( SelfNeuMNeuMLeftUsual(0D0) ) - 0D0*DBLE( DSelfNeuMNeuMLeft(0D0) + &
                        & DSelfNeuMNeuMRight(0D0) + 2D0*DSelfNeuMNeuMScalar(0D0) )
                end function dZNeuMNeuMOSLeft

                double precision function dZNeuTNeuTOSRight()
                    use constants
                    implicit none
                    double complex SelfNeuTNeuTRightUsual, DSelfNeuTNeuTLeft, DSelfNeuTNeuTRight
                    double complex DSelfNeuTNeuTScalar
                    dZNeuTNeuTOSRight = -DBLE( SelfNeuTNeuTRightUsual(0D0) ) - 0D0*DBLE( DSelfNeuTNeuTLeft(0D0) + &
                        & DSelfNeuTNeuTRight(0D0) + 2D0*DSelfNeuTNeuTScalar(0D0) )
                end function dZNeuTNeuTOSRight

                double precision function dZNeuTNeuTOSLeft()
                    use constants
                    implicit none
                    double complex SelfNeuTNeuTLeftUsual, DSelfNeuTNeuTLeft, DSelfNeuTNeuTRight
                    double complex DSelfNeuTNeuTScalar
                    dZNeuTNeuTOSLeft = -DBLE( SelfNeuTNeuTLeftUsual(0D0) ) - 0D0*DBLE( DSelfNeuTNeuTLeft(0D0) + &
                        & DSelfNeuTNeuTRight(0D0) + 2D0*DSelfNeuTNeuTScalar(0D0) )
                end function dZNeuTNeuTOSLeft

                double precision function dZElElOSLeft()
                    use constants
                    implicit none
                    double complex SelfElElLeftUsual, DSelfElElLeft, DSelfElElRight, DSelfElElScalar
                    dZElElOSLeft = -DBLE( SelfElElLeftUsual(ME2) ) - ME2*DBLE( DSelfElElLeft(ME2) + &
                        & DSelfElElRight(ME2) + 2D0*DSelfElElScalar(ME2) )
                end function dZElElOSLeft

                double precision function dZElElOSRight()
                    use constants
                    implicit none
                    double complex SelfElElRightUsual, DSelfElElLeft, DSelfElElRight, DSelfElElScalar
                    dZElElOSRight = -DBLE( SelfElElRightUsual(ME2) ) - ME2*DBLE( DSelfElElLeft(ME2) + &
                        & DSelfElElRight(ME2) + 2D0*DSelfElElScalar(ME2) )
                end function dZElElOSRight

                double precision function dZMuMuOSLeft()
                    use constants
                    implicit none
                    double complex SelfMuMuLeftUsual, DSelfMuMuLeft, DSelfMuMuRight, DSelfMuMuScalar
                    dZMuMuOSLeft = -DBLE( SelfMuMuLeftUsual(MM2) ) - MM2*DBLE( DSelfMuMuLeft(MM2) + &
                        & DSelfMuMuRight(MM2) + 2D0*DSelfMuMuScalar(MM2) )
                end function dZMuMuOSLeft

                double precision function dZMuMuOSRight()
                    use constants
                    implicit none
                    double complex SelfMuMuRightUsual, DSelfMuMuLeft, DSelfMuMuRight, DSelfMuMuScalar
                    dZMuMuOSRight = -DBLE( SelfMuMuRightUsual(MM2) ) - MM2*DBLE( DSelfMuMuLeft(MM2) + &
                        & DSelfMuMuRight(MM2) + 2D0*DSelfMuMuScalar(MM2) )
                end function dZMuMuOSRight

                double precision function dZTauTauOSLeft()
                    use constants
                    implicit none
                    double complex SelfTauTauLeftUsual, DSelfTauTauLeft, DSelfTauTauRight, DSelfTauTauScalar
                    dZTauTauOSLeft = -DBLE( SelfTauTauLeftUsual(ML2) ) - ML2*DBLE( DSelfTauTauLeft(ML2) + &
                        & DSelfTauTauRight(ML2) + 2D0*DSelfTauTauScalar(ML2) )
                end function dZTauTauOSLeft

                double precision function dZTauTauOSRight()
                    use constants
                    implicit none
                    double complex SelfTauTauRightUsual, DSelfTauTauLeft, DSelfTauTauRight, DSelfTauTauScalar
                    dZTauTauOSRight = -DBLE( SelfTauTauRightUsual(ML2) ) - ML2*DBLE( DSelfTauTauLeft(ML2) + &
                        & DSelfTauTauRight(ML2) + 2D0*DSelfTauTauScalar(ML2) )
                end function dZTauTauOSRight
                
                ! double precision function dZTauTauOSLeftWeak()
                !     use constants
                !     implicit none
                !     double complex SelfTauTauLeftWeakUsual, DSelfTauTauLeftWeak, DSelfTauTauRightWeak, DSelfTauTauScalarWeak
                !     dZTauTauOSLeftWeak = -DBLE( SelfTauTauLeftWeakUsual(ML2) ) - ML2*DBLE( DSelfTauTauLeftWeak(ML2) + &
                !         & DSelfTauTauRightWeak(ML2) + 2D0*DSelfTauTauScalarWeak(ML2) )
                ! end function dZTauTauOSLeftWeak

                ! double precision function dZTauTauOSRightWeak()
                !     use constants
                !     implicit none
                !     double complex SelfTauTauRightWeakUsual, DSelfTauTauLeftWeak, DSelfTauTauRightWeak, DSelfTauTauScalarWeak
                !     dZTauTauOSRightWeak = -DBLE( SelfTauTauRightWeakUsual(ML2) ) - ML2*DBLE( DSelfTauTauLeftWeak(ML2) + &
                !         & DSelfTauTauRightWeak(ML2) + 2D0*DSelfTauTauScalarWeak(ML2) )
                ! end function dZTauTauOSRightWeak

                double precision function dZDDOSLeft()
                    use constants
                    implicit none
                    double complex SelfDDLeftUsual, DSelfDDLeft, DSelfDDRight, DSelfDDScalar
                    dZDDOSLeft = -DBLE( SelfDDLeftUsual(MD2) ) - MD2*DBLE( DSelfDDLeft(MD2) + &
                        & DSelfDDRight(MD2) + 2D0*DSelfDDScalar(MD2) )
                end function dZDDOSLeft

                double precision function dZDDOSRight()
                    use constants
                    implicit none
                    double complex SelfDDRightUsual, DSelfDDLeft, DSelfDDRight, DSelfDDScalar
                    dZDDOSRight = -DBLE( SelfDDRightUsual(MD2) ) - MD2*DBLE( DSelfDDLeft(MD2) + &
                        & DSelfDDRight(MD2) + 2D0*DSelfDDScalar(MD2) )
                end function dZDDOSRight

                double precision function dZUUOSLeft()
                    use constants
                    implicit none
                    double complex SelfUULeftUsual, DSelfUULeft, DSelfUURight, DSelfUUScalar
                    dZUUOSLeft = -DBLE( SelfUULeftUsual(MU2) ) - MU2*DBLE( DSelfUULeft(MU2) + &
                        & DSelfUURight(MU2) + 2D0*DSelfUUScalar(MU2) )
                end function dZUUOSLeft

                double precision function dZUUOSRight()
                    use constants
                    implicit none
                    double complex SelfUURightUsual, DSelfUULeft, DSelfUURight, DSelfUUScalar
                    dZUUOSRight = -DBLE( SelfUURightUsual(MU2) ) - MU2*DBLE( DSelfUULeft(MU2) + &
                        & DSelfUURight(MU2) + 2D0*DSelfUUScalar(MU2) )
                end function dZUUOSRight

                double precision function dZSSOSLeft()
                    use constants
                    implicit none
                    double complex SelfSSLeftUsual, DSelfSSLeft, DSelfSSRight, DSelfSSScalar
                    dZSSOSLeft = -DBLE( SelfSSLeftUsual(MS2) ) - MS2*DBLE( DSelfSSLeft(MS2) + &
                        & DSelfSSRight(MS2) + 2D0*DSelfSSScalar(MS2) )
                end function dZSSOSLeft

                double precision function dZSSOSRight()
                    use constants
                    implicit none
                    double complex SelfSSRightUsual, DSelfSSLeft, DSelfSSRight, DSelfSSScalar
                    dZSSOSRight = -DBLE( SelfSSRightUsual(MS2) ) - MS2*DBLE( DSelfSSLeft(MS2) + &
                        & DSelfSSRight(MS2) + 2D0*DSelfSSScalar(MS2) )
                end function dZSSOSRight

                double precision function dZCCOSLeft()
                    use constants
                    implicit none
                    double complex SelfCCLeftUsual, DSelfCCLeft, DSelfCCRight, DSelfCCScalar
                    dZCCOSLeft = -DBLE( SelfCCLeftUsual(MC2) ) - MC2*DBLE( DSelfCCLeft(MC2) + &
                        & DSelfCCRight(MC2) + 2D0*DSelfCCScalar(MC2) )
                end function dZCCOSLeft

                double precision function dZCCOSRight()
                    use constants
                    implicit none
                    double complex SelfCCRightUsual, DSelfCCLeft, DSelfCCRight, DSelfCCScalar
                    dZCCOSRight = -DBLE( SelfCCRightUsual(MC2) ) - MC2*DBLE( DSelfCCLeft(MC2) + &
                        & DSelfCCRight(MC2) + 2D0*DSelfCCScalar(MC2) )
                end function dZCCOSRight

                double precision function dZBBOSLeft()
                    use constants
                    implicit none
                    double complex SelfBBLeftUsual, DSelfBBLeft, DSelfBBRight, DSelfBBScalar
                    dZBBOSLeft = -DBLE( SelfBBLeftUsual(MB2) ) - MB2*DBLE( DSelfBBLeft(MB2) + &
                        & DSelfBBRight(MB2) + 2D0*DSelfBBScalar(MB2) )
                end function dZBBOSLeft

                double precision function dZBBOSRight()
                    use constants
                    implicit none
                    double complex SelfBBRightUsual, DSelfBBLeft, DSelfBBRight, DSelfBBScalar
                    dZBBOSRight = -DBLE( SelfBBRightUsual(MB2) ) - MB2*DBLE( DSelfBBLeft(MB2) + &
                        & DSelfBBRight(MB2) + 2D0*DSelfBBScalar(MB2) )
                end function dZBBOSRight

                double precision function dZTTOSLeft()
                    use constants
                    implicit none
                    double complex SelfTTLeftUsual, DSelfTTLeft, DSelfTTRight, DSelfTTScalar
                    dZTTOSLeft = -DBLE( SelfTTLeftUsual(MT2) ) - MT2*DBLE( DSelfTTLeft(MT2) + &
                        & DSelfTTRight(MT2) + 2D0*DSelfTTScalar(MT2) )
                end function dZTTOSLeft

                double precision function dZTTOSRight()
                    use constants
                    implicit none
                    double complex SelfTTRightUsual, DSelfTTLeft, DSelfTTRight, DSelfTTScalar
                    dZTTOSRight = -DBLE( SelfTTRightUsual(MT2) ) - MT2*DBLE( DSelfTTLeft(MT2) + &
                        & DSelfTTRight(MT2) + 2D0*DSelfTTScalar(MT2) )
                end function dZTTOSRight

                double precision function dZCTOSLeft()
                    use constants
                    implicit none
                    double complex SelfCTLeftUsual, SelfCTRightUsual, SelfCTScalarUsual
                    dZCTOSLeft = 2D0/( MC2 - MT2 )*DBLE( &
                            & MT2*SelfCTLeftUsual(MT2) + MC*MT*SelfCTRightUsual(MT2) + &
                            & (MC2 + MT2)*SelfCTScalarUsual(MT2) &
                        & )
                end function dZCTOSLeft

                double precision function dZCTOSRight()
                    use constants
                    implicit none
                    double complex SelfCTLeftUsual, SelfCTRightUsual, SelfCTScalarUsual
                    dZCTOSRight = 2D0/( MC2 - MT2 )*DBLE( &
                            & MT2*SelfCTRightUsual(MT2) + MC*MT*SelfCTLeftUsual(MT2) + &
                            & (2D0*MC*MT)*SelfCTScalarUsual(MT2) &
                        & )
                end function dZCTOSRight

                double precision function dZTCOSLeft()
                    use constants
                    implicit none
                    double complex SelfCTLeftUsual, SelfCTRightUsual, SelfCTScalarUsual
                    dZTCOSLeft = 2D0/( MT2 - MC2 )*DBLE( &
                            & MC2*SelfCTLeftUsual(MC2) + MC*MT*SelfCTRightUsual(MC2) + &
                            & (MC2 + MT2)*SelfCTScalarUsual(MC2) &
                        & )
                end function dZTCOSLeft

                double precision function dZTCOSRight()
                    use constants
                    implicit none
                    double complex SelfCTLeftUsual, SelfCTRightUsual, SelfCTScalarUsual
                    dZTCOSRight = 2D0/( MT2 - MC2 )*DBLE( &
                            & MC2*SelfCTRightUsual(MC2) + MC*MT*SelfCTLeftUsual(MC2) + &
                            & (2D0*MC*MT)*SelfCTScalarUsual(MC2) &
                        & )
                end function dZTCOSRight

                double precision function dZUTOSLeft()
                    use constants
                    implicit none
                    double complex SelfUTLeftUsual, SelfUTRightUsual, SelfUTScalarUsual
                    dZUTOSLeft = 2D0/( MU2 - MT2 )*DBLE( &
                            & MT2*SelfUTLeftUsual(MT2) + MU*MT*SelfUTRightUsual(MT2) + &
                            & (MU2 + MT2)*SelfUTScalarUsual(MT2) &
                        & )
                end function dZUTOSLeft

                double precision function dZUTOSRight()
                    use constants
                    implicit none
                    double complex SelfUTLeftUsual, SelfUTRightUsual, SelfUTScalarUsual
                    dZUTOSRight = 2D0/( MU2 - MT2 )*DBLE( &
                            & MT2*SelfUTRightUsual(MT2) + MU*MT*SelfUTLeftUsual(MT2) + &
                            & (2D0*MU*MT)*SelfUTScalarUsual(MT2) &
                        & )
                end function dZUTOSRight

                double precision function dZTUOSLeft()
                    use constants
                    implicit none
                    double complex SelfUTLeftUsual, SelfUTRightUsual, SelfUTScalarUsual
                    dZTUOSLeft = 2D0/( MT2 - MU2 )*DBLE( &
                            & MU2*SelfUTLeftUsual(MU2) + MU*MT*SelfUTRightUsual(MU2) + &
                            & (MU2 + MT2)*SelfUTScalarUsual(MU2) &
                        & )
                end function dZTUOSLeft

                double precision function dZTUOSRight()
                    use constants
                    implicit none
                    double complex SelfUTLeftUsual, SelfUTRightUsual, SelfUTScalarUsual
                    dZTUOSRight = 2D0/( MT2 - MU2 )*DBLE( &
                            & MU2*SelfUTRightUsual(MU2) + MU*MT*SelfUTLeftUsual(MU2) + &
                            & (2D0*MU*MT)*SelfUTScalarUsual(MU2) &
                        & )
                end function dZTUOSRight

                double precision function dZSBOSLeft()
                    use constants
                    implicit none
                    double complex SelfSBLeftUsual, SelfSBRightUsual, SelfSBScalarUsual
                    dZSBOSLeft = 2D0/( MS2 - MB2 )*DBLE( &
                            & MB2*SelfSBLeftUsual(MB2) + MS*MB*SelfSBRightUsual(MB2) + &
                            & (MS2 + MB2)*SelfSBScalarUsual(MB2) &
                        & )
                end function dZSBOSLeft

                double precision function dZSBOSRight()
                    use constants
                    implicit none
                    double complex SelfSBLeftUsual, SelfSBRightUsual, SelfSBScalarUsual
                    dZSBOSRight = 2D0/( MS2 - MB2 )*DBLE( &
                            & MB2*SelfSBRightUsual(MB2) + MS*MB*SelfSBLeftUsual(MB2) + &
                            & (2D0*MS*MB)*SelfSBScalarUsual(MB2) &
                        & )
                end function dZSBOSRight

                double precision function dZBSOSLeft()
                    use constants
                    implicit none
                    double complex SelfSBLeftUsual, SelfSBRightUsual, SelfSBScalarUsual
                    dZBSOSLeft = 2D0/( MB2 - MS2 )*DBLE( &
                            & MS2*SelfSBLeftUsual(MS2) + MS*MB*SelfSBRightUsual(MS2) + &
                            & (MS2 + MB2)*SelfSBScalarUsual(MS2) &
                        & )
                end function dZBSOSLeft

                double precision function dZBSOSRight()
                    use constants
                    implicit none
                    double complex SelfSBLeftUsual, SelfSBRightUsual, SelfSBScalarUsual
                    dZBSOSRight = 2D0/( MB2 - MS2 )*DBLE( &
                            & MS2*SelfSBRightUsual(MS2) + MS*MB*SelfSBLeftUsual(MS2) + &
                            & (2D0*MS*MB)*SelfSBScalarUsual(MS2) &
                        & )
                end function dZBSOSRight

                double precision function dZDBOSLeft()
                    use constants
                    implicit none
                    double complex SelfDBLeftUsual, SelfDBRightUsual, SelfDBScalarUsual
                    dZDBOSLeft = 2D0/( MD2 - MB2 )*DBLE( &
                            & MB2*SelfDBLeftUsual(MB2) + MD*MB*SelfDBRightUsual(MB2) + &
                            & (MD2 + MB2)*SelfDBScalarUsual(MB2) &
                        & )
                end function dZDBOSLeft

                double precision function dZDBOSRight()
                    use constants
                    implicit none
                    double complex SelfDBLeftUsual, SelfDBRightUsual, SelfDBScalarUsual
                    dZDBOSRight = 2D0/( MD2 - MB2 )*DBLE( &
                            & MB2*SelfDBRightUsual(MB2) + MD*MB*SelfDBLeftUsual(MB2) + &
                            & (2D0*MD*MB)*SelfDBScalarUsual(MB2) &
                        & )
                end function dZDBOSRight

                double precision function dZBDOSLeft()
                    use constants
                    implicit none
                    double complex SelfDBLeftUsual, SelfDBRightUsual, SelfDBScalarUsual
                    dZBDOSLeft = 2D0/( MB2 - MD2 )*DBLE( &
                            & MD2*SelfDBLeftUsual(MD2) + MD*MB*SelfDBRightUsual(MD2) + &
                            & (MD2 + MB2)*SelfDBScalarUsual(MD2) &
                        & )
                end function dZBDOSLeft

                double precision function dZBDOSRight()
                    use constants
                    implicit none
                    double complex SelfDBLeftUsual, SelfDBRightUsual, SelfDBScalarUsual
                    dZBDOSRight = 2D0/( MB2 - MD2 )*DBLE( &
                            & MD2*SelfDBRightUsual(MD2) + MD*MB*SelfDBLeftUsual(MD2) + &
                            & (2D0*MD*MB)*SelfDBScalarUsual(MD2) &
                        & )
                end function dZBDOSRight

                double precision function dZUCOSLeft()
                    use constants
                    implicit none
                    double complex SelfUCLeftUsual, SelfUCRightUsual, SelfUCScalarUsual
                    dZUCOSLeft = 2D0/( MU2 - MC2 )*DBLE( &
                            & MC2*SelfUCLeftUsual(MC2) + MC*MU*SelfUCRightUsual(MC2) + &
                            & (MC2 + MU2)*SelfUCScalarUsual(MC2) &
                        & )
                end function dZUCOSLeft

                double precision function dZUCOSRight()
                    use constants
                    implicit none
                    double complex SelfUCLeftUsual, SelfUCRightUsual, SelfUCScalarUsual
                    dZUCOSRight = 2D0/( MU2 - MC2 )*DBLE( &
                            & MC2*SelfUCRightUsual(MC2) + MC*MU*SelfUCLeftUsual(MC2) + &
                            & (2D0*MC*MU)*SelfUCScalarUsual(MC2) &
                        & )
                end function dZUCOSRight

                double precision function dZCUOSLeft()
                    use constants
                    implicit none
                    double complex SelfUCLeftUsual, SelfUCRightUsual, SelfUCScalarUsual
                    dZCUOSLeft = 2D0/( MC2 - MU2 )*DBLE( &
                            & MU2*SelfUCLeftUsual(MU2) + MC*MU*SelfUCRightUsual(MU2) + &
                            & (MC2 + MU2)*SelfUCScalarUsual(MU2) &
                        & )
                end function dZCUOSLeft

                double precision function dZCUOSRight()
                    use constants
                    implicit none
                    double complex SelfUCLeftUsual, SelfUCRightUsual, SelfUCScalarUsual
                    dZCUOSRight = 2D0/( MC2 - MU2 )*DBLE( &
                            & MU2*SelfUCRightUsual(MU2) + MC*MU*SelfUCLeftUsual(MU2) + &
                            & (2D0*MC*MU)*SelfUCScalarUsual(MU2) &
                        & )
                end function dZCUOSRight

                double precision function dZDSOSLeft()
                    use constants
                    implicit none
                    double complex SelfDSLeftUsual, SelfDSRightUsual, SelfDSScalarUsual
                    dZDSOSLeft = 2D0/( MD2 - MS2 )*DBLE( &
                            & MS2*SelfDSLeftUsual(MS2) + MS*MD*SelfDSRightUsual(MS2) + &
                            & (MS2 + MD2)*SelfDSScalarUsual(MS2) &
                        & )
                end function dZDSOSLeft

                double precision function dZDSOSRight()
                    use constants
                    implicit none
                    double complex SelfDSLeftUsual, SelfDSRightUsual, SelfDSScalarUsual
                    dZDSOSRight = 2D0/( MD2 - MS2 )*DBLE( &
                            & MS2*SelfDSRightUsual(MS2) + MS*MD*SelfDSLeftUsual(MS2) + &
                            & (2D0*MS*MD)*SelfDSScalarUsual(MS2) &
                        & )
                end function dZDSOSRight

                double precision function dZSDOSLeft()
                    use constants
                    implicit none
                    double complex SelfDSLeftUsual, SelfDSRightUsual, SelfDSScalarUsual
                    dZSDOSLeft = 2D0/( MS2 - MD2 )*DBLE( &
                            & MD2*SelfDSLeftUsual(MD2) + MS*MD*SelfDSRightUsual(MD2) + &
                            & (MS2 + MD2)*SelfDSScalarUsual(MD2) &
                        & )
                end function dZSDOSLeft

                double precision function dZSDOSRight()
                    use constants
                    implicit none
                    double complex SelfDSLeftUsual, SelfDSRightUsual, SelfDSScalarUsual
                    dZSDOSRight = 2D0/( MS2 - MD2 )*DBLE( &
                            & MD2*SelfDSRightUsual(MD2) + MS*MD*SelfDSLeftUsual(MD2) + &
                            & (2D0*MS*MD)*SelfDSScalarUsual(MD2) &
                        & )
                end function dZSDOSRight



        ! CKM renormalization
            ! Yamada's scheme (hep-ph/0103046)
                double precision function dCKM11Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM11Yamada = 1D0/4D0*( &
                        & ( dZUTOSLeft() - dZTUOSLeft() )*CKM31 + &
                        & ( dZUCOSLeft() - dZCUOSLeft() )*CKM21 - &
                        & ( dZBDOSLeft() - dZDBOSLeft() )*CKM13 - &
                        & ( dZSDOSLeft() - dZDSOSLeft() )*CKM12 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM11Yamada

                double precision function dCKM12Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM12Yamada = 1D0/4D0*( &
                        & ( dZUTOSLeft() - dZTUOSLeft() )*CKM32 + &
                        & ( dZUCOSLeft() - dZCUOSLeft() )*CKM22 - &
                        & ( dZBSOSLeft() - dZSBOSLeft() )*CKM13 - &
                        & ( dZDSOSLeft() - dZSDOSLeft() )*CKM11 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM12Yamada

                double precision function dCKM13Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM13Yamada = 1D0/4D0*( &
                        & ( dZUTOSLeft() - dZTUOSLeft() )*CKM33 + &
                        & ( dZUCOSLeft() - dZCUOSLeft() )*CKM23 - &
                        & ( dZSBOSLeft() - dZBSOSLeft() )*CKM12 - &
                        & ( dZDBOSLeft() - dZBDOSLeft() )*CKM11 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM13Yamada

                double precision function dCKM21Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM21Yamada = 1D0/4D0*( &
                        & ( dZCTOSLeft() - dZTCOSLeft() )*CKM31 + &
                        & ( dZCUOSLeft() - dZUCOSLeft() )*CKM11 - &
                        & ( dZBDOSLeft() - dZDBOSLeft() )*CKM23 - &
                        & ( dZSDOSLeft() - dZDSOSLeft() )*CKM22 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM21Yamada

                double precision function dCKM22Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM22Yamada = 1D0/4D0*( &
                        & ( dZCUOSLeft() - dZUCOSLeft() )*CKM12 + &
                        & ( dZCTOSLeft() - dZCTOSLeft() )*CKM32 - &
                        & ( dZDSOSLeft() - dZSDOSLeft() )*CKM21 - &
                        & ( dZBSOSLeft() - dZSBOSLeft() )*CKM23 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM22Yamada

                double precision function dCKM23Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM23Yamada = 1D0/4D0*( &
                        & ( dZCTOSLeft() - dZTCOSLeft() )*CKM33 + &
                        & ( dZCUOSLeft() - dZUCOSLeft() )*CKM13 - &
                        & ( dZSBOSLeft() - dZBSOSLeft() )*CKM22 - &
                        & ( dZDBOSLeft() - dZBDOSLeft() )*CKM21 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM23Yamada

                double precision function dCKM31Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM31Yamada = 1D0/4D0*( &
                        & ( dZTCOSLeft() - dZCTOSLeft() )*CKM21 + &
                        & ( dZTUOSLeft() - dZUTOSLeft() )*CKM11 - &
                        & ( dZBDOSLeft() - dZDBOSLeft() )*CKM33 - &
                        & ( dZSDOSLeft() - dZDSOSLeft() )*CKM32 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM31Yamada

                double precision function dCKM32Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM32Yamada = 1D0/4D0*( &
                        & ( dZTCOSLeft() - dZCTOSLeft() )*CKM22 + &
                        & ( dZTUOSLeft() - dZUTOSLeft() )*CKM12 - &
                        & ( dZBSOSLeft() - dZSBOSLeft() )*CKM33 - &
                        & ( dZDSOSLeft() - dZSDOSLeft() )*CKM31 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM32Yamada

                double precision function dCKM33Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM33Yamada = 1D0/4D0*( &
                        & ( dZTUOSLeft() - dZUTOSLeft() )*CKM13 + &
                        & ( dZTCOSLeft() - dZCTOSLeft() )*CKM23 - &
                        & ( dZDBOSLeft() - dZBDOSLeft() )*CKM31 - &
                        & ( dZSBOSLeft() - dZBSOSLeft() )*CKM32 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM33Yamada

        ! Mixing angle counterterms
            ! Alternative tadpole based counterterms
                double precision function dAlpha1KanAlter()
                    use constants
                    implicit none
                    dAlpha1KanAlter = CA3*( dZH1H2OSAlter() - dZH2H1OSAlter() )/(4.D0*CA2) - &
                                    & SA3*( dZH1H3OSAlter() - dZH3H1OSAlter() )/(4.D0*CA2)
                end function dAlpha1KanAlter

                double precision function dAlpha2KanAlter()
                    use constants
                    implicit none
                    dAlpha2KanAlter = CA3*( dZH1H3OSAlter() - dZH3H1OSAlter() )/4.D0 + &
                                    & SA3*( dZH1H2OSAlter() - dZH2H1OSAlter() )/4.D0
                end function dAlpha2KanAlter

                double precision function dAlpha3KanAlter()
                    use constants
                    implicit none
                    dAlpha3KanAlter = ( dZH2H3OSAlter() - dZH3H2OSAlter() )/4.D0 + &
                                    & SA2*SA3*( dZH1H3OSAlter() - dZH3H1OSAlter() )/(4.D0*CA2) - &
                                    & SA2*CA3*( dZH1H2OSAlter() - dZH2H1OSAlter() )/(4.D0*CA2)
                end function dAlpha3KanAlter

                double precision function dBeta1KanAlter()
                    use constants
                    implicit none
                    dBeta1KanAlter = ( dZG0A0OSAlter() - dZA0G0OSAlter() )/4.D0
                end function dBeta1KanAlter

                double precision function dBeta2KanAlter()
                    use constants
                    implicit none
                    dBeta2KanAlter = ( dZGpHpOSAlter() - dZHpGpOSAlter() )/4.D0
                end function dBeta2KanAlter

                double precision function dAlpha1PinchOS()
                    use constants
                    implicit none
                    double complex SelfH1H2Alter, SelfH1H2Add, SelfH1H3Alter, SelfH1H3Add
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dAlpha1PinchOS = CA3*( DBLE(SelfH1H2Alter(MH12)) + DBLE(SelfH1H2Alter(MH22)) + DBLE(SelfH1H2Add(MH12)) + &
                                   & DBLE(SelfH1H2Add(MH22)) )/( 2.D0*CA2*(MH12 - MH22) ) - &
                                   & SA3*( DBLE(SelfH1H3Alter(MH12)) + DBLE(SelfH1H3Alter(MH32)) + DBLE(SelfH1H3Add(MH12)) + &
                                   & DBLE(SelfH1H3Add(MH32)) )/( 2.D0*CA2*(MH12 - MH32) )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dAlpha1PinchOS

                double precision function dAlpha2PinchOS()
                    use constants
                    implicit none
                    double complex SelfH1H2Alter, SelfH1H2Add, SelfH1H3Alter, SelfH1H3Add
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dAlpha2PinchOS = CA3*( DBLE(SelfH1H3Alter(MH12)) + DBLE(SelfH1H3Alter(MH32)) + DBLE(SelfH1H3Add(MH12)) + &
                                   & DBLE(SelfH1H3Add(MH32)) )/( 2.D0*(MH12 - MH32) ) + &
                                   & SA3*( DBLE(SelfH1H2Alter(MH12)) + DBLE(SelfH1H2Alter(MH22)) + DBLE(SelfH1H2Add(MH12)) + &
                                   & DBLE(SelfH1H2Add(MH22)) )/( 2.D0*(MH12 - MH22) )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dAlpha2PinchOS

                double precision function dAlpha3PinchOS()
                    use constants
                    implicit none
                    double complex SelfH1H2Alter, SelfH1H2Add, SelfH1H3Alter, SelfH1H3Add, SelfH2H3Alter, SelfH2H3Add
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dAlpha3PinchOS = ( DBLE(SelfH2H3Alter(MH22)) + DBLE(SelfH2H3Alter(MH32)) + DBLE(SelfH2H3Add(MH22)) + &
                                   & DBLE(SelfH2H3Add(MH32)) )/( 2.D0*(MH22 - MH32) ) + &
                                   & SA2*SA3*( DBLE(SelfH1H3Alter(MH12)) + DBLE(SelfH1H3Alter(MH32)) + DBLE(SelfH1H3Add(MH12)) + &
                                   & DBLE(SelfH1H3Add(MH32)) )/( 2.D0*CA2*(MH12 - MH32) ) - &
                                   & SA2*CA3*( DBLE(SelfH1H2Alter(MH12)) + DBLE(SelfH1H2Alter(MH22)) + DBLE(SelfH1H2Add(MH12)) + &
                                   & DBLE(SelfH1H2Add(MH22)) )/( 2.D0*CA2*(MH12 - MH22) )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dAlpha3PinchOS

                double precision function dBeta1PinchOS()
                    use constants
                    implicit none
                    double complex SelfG0A0Alter, SelfG0A0Add
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dBeta1PinchOS = -( DBLE(SelfG0A0Alter(MA02)) + DBLE(SelfG0A0Alter(0.D0)) + DBLE(SelfG0A0Add(MA02)) + &
                                    & DBLE(SelfG0A0Add(0.D0)) )/( 2.D0*MA02 )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dBeta1PinchOS

                double precision function dBeta2PinchOS()
                    use constants
                    implicit none
                    double complex SelfGpHpAlter, SelfGpHpAdd
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dBeta2PinchOS = -( DBLE(SelfGpHpAlter(MHp2)) + DBLE(SelfGpHpAlter(0.D0)) + DBLE(SelfGpHpAdd(MHp2)) + &
                                    & DBLE(SelfGpHpAdd(0.D0)) )/( 2.D0*MHp2 )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dBeta2PinchOS

                double precision function dAlpha1PinchPStar()
                    use constants
                    implicit none
                    double complex SelfH1H2Alter, SelfH1H3Alter
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dAlpha1PinchPStar = CA3*( DBLE(SelfH1H2Alter((MH12 + MH22)/2.D0)) )/(CA2*( MH12 - MH22 )) - &
                                      & SA3*( DBLE(SelfH1H3Alter((MH12 + MH32)/2.D0)) )/(CA2*( MH12 - MH32 ))
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dAlpha1PinchPStar

                double precision function dAlpha2PinchPStar()
                    use constants
                    implicit none
                    double complex SelfH1H2Alter, SelfH1H3Alter
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dAlpha2PinchPStar = CA3*( DBLE(SelfH1H3Alter((MH12 + MH32)/2.D0)) )/( MH12 - MH32 ) + &
                                      & SA3*( DBLE(SelfH1H2Alter((MH12 + MH22)/2.D0)) )/( MH12 - MH22 )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dAlpha2PinchPStar

                double precision function dAlpha3PinchPStar()
                    use constants
                    implicit none
                    double complex SelfH1H2Alter, SelfH1H3Alter, SelfH2H3Alter
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dAlpha3PinchPStar = ( DBLE(SelfH2H3Alter((MH22 + MH32)/2.D0)) )/( MH22 - MH32 ) + &
                                      & SA2*SA3*( DBLE(SelfH1H3Alter((MH12 + MH32)/2.D0)) )/(CA2*( MH12 - MH32 )) - &
                                      & SA2*CA3*( DBLE(SelfH1H2Alter((MH12 + MH22)/2.D0)) )/(CA2*( MH12 - MH22 ))
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dAlpha3PinchPStar

                double precision function dBeta1PinchPStar()
                    use constants
                    implicit none
                    double complex SelfG0A0Alter
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dBeta1PinchPStar = -( DBLE(SelfG0A0Alter(MA02/2.D0)) )/( MA02 )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dBeta1PinchPStar

                double precision function dBeta2PinchPStar()
                    use constants
                    implicit none
                    double complex SelfGpHpAlter
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dBeta2PinchPStar = -( DBLE(SelfGpHpAlter(MHp2/2.D0)) )/( MHp2 )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dBeta2PinchPStar

                double precision function dAlpha1MSBarAlter()
                    use constants
                    implicit none
                    dAlpha1MSBarAlter = ( &
&(-0.5D0*SA3*((-0.0625D0*CS1S1S1f111*CS1S1S1f131)/PI2 - (0.0625D0*CS1S1S1f132*CS1S1S1f222)/PI2 + (0.0625D0*CS1S1S1f111*CS1S1S1f311&
  &)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f313)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f322)/PI2&
  & + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f311)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + (0.&
  &125D0*CS1S3S3f112*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f322)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.125D0&
  &*CS2S2S1f121*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f223)/PI2 - (0.0625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.125D0*CS1S1S3&
  &S3f1322*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1311*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1113*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3&
  &*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)))/(PI2*SW2) - (0.0625D0*CS1S1S1f131*CS2S2S1f221*MA02*DBLE(MH1&
  &**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f122*MHp2*DBLE(MH1**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f111*MW2*D&
  &BLE(MH1**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f131*CS2S2S1f111*MZ2*DBLE(MH1**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S1f131*EL2*MW2*(&
  &(2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE(MH1**INT(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1&
  &*DBLE(MB**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MC**INT(4.D0))*DBLE(MH1**INT(-2.&
  &D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*DBLE(MD**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1&
  &S1S1f131*EL*YukS1Lep1*DBLE(ME**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark3*(6.D&
  &0*MB2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MC2 - &
  &1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark3*(6.D0*MD2 - 1.D0*DBLE(MH1**INT(2.D0&
  &))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0&
  &*EL2*ML2*YukS1Lep1*YukS1Lep3*(6.D0*ML2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep3*(6&
  &.D0*MM2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark3*(6.D0*MS2 - 1.D0*DBLE(MH1**IN&
  &T(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH1**INT(2.D0))&
  &))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH1**INT(2.D0))))/(&
  &MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA&
  &1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH1**INT(2.D0))))/(PI2*SW2) - (0.03125D0*E&
  &L2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MHp2 &
  &+ MW2 + 2.D0*(MHp2 + DBLE(MH1**INT(2.D0)))))/(PI2*SW2) - (0.0625D0*CS1S1S1f132*CS2S2S1f222*MA02*DBLE(MH2**INT(-2.D0)))/PI2 - (&
  &0.125D0*CS1S1S1f132*CS1S3S3f222*MHp2*DBLE(MH2**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f132*CS1S3S3f211*MW2*DBLE(MH2**INT(-2.D0)))/&
  &PI2 - (0.0625D0*CS1S1S1f132*CS2S2S1f112*MZ2*DBLE(MH2**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S1f132*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3&
  &*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(MH2**INT(-2.D0)))/(PI2*SW2) - (0.75D0*&
  &CS1S1S1f132*EL*YukS1Quark2*DBLE(MB**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3)*DBLE(MC**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MD**INT(4.D0))*D&
  &BLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(ME**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - &
  &(0.0625D0*CS1S1S1f132*CS1S1S1f211*DBLE(MH1**INT(2.D0))*DBLE(MH2**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1S1f1322*DBLE(MH2**INT(2.D&
  &0)))/PI2 - (0.0625D0*CS1S1S1f122*CS1S1S1f131*DBLE(MH1**INT(-2.D0))*DBLE(MH2**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f133*CS2S2S1f2&
  &23*MA02*DBLE(MH3**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f133*CS1S3S3f322*MHp2*DBLE(MH3**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f133*C&
  &S1S3S3f311*MW2*DBLE(MH3**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f133*CS2S2S1f113*MZ2*DBLE(MH3**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1&
  &S1f133*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE&
  &(MH3**INT(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f133*EL*YukS1Quark3*DBLE(MB**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.&
  &75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1&
  &S1S1f133*EL*YukS1Quark3*DBLE(MD**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(ME**INT&
  &(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.0625D0*CS1S1S1f133*CS1S1S1f311*DBLE(MH1**INT(2.D0))*DBLE(MH3**INT(-2.D0)))/PI2&
  & - (0.0625D0*CS1S1S1f133*CS1S1S1f322*DBLE(MH2**INT(2.D0))*DBLE(MH3**INT(-2.D0)))/PI2 - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Qua&
  &rk3*(6.D0*MB2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D&
  &0*MC2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark3*(6.D0*MD2 - 1.D0*DBLE(MH3**&
  &INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0&
  &.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep3*(6.D0*ML2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS&
  &1Lep3*(6.D0*MM2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark3*(6.D0*MS2 - 1.D0*DBLE&
  &(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH3**IN&
  &T(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH3**INT(2.&
  &D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1333*DBLE(MH3**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f131*CS1S1S1f133*DBLE(MH1**IN&
  &T(-2.D0))*DBLE(MH3**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f132*CS1S1S1f233*DBLE(MH2**INT(-2.D0))*DBLE(MH3**INT(2.D0)))/PI2 - (0.0&
  &3125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MW2 + 2.&
  &D0*DBLE(MH3**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1&
  &.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH3**INT(2.D0)))))/(PI2*SW2) - (0.25D0*CS1S1S1f131&
  &*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(MH2**INT(-2.D0))*&
  &DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(MH3**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (&
  &0.25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE&
  &(MH2**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(MH3**INT(-2.D0))*DBLE(MM**INT(4.D0)&
  &))/(MW*PI2*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*DBLE(MH1**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f1&
  &32*EL*YukS1Quark2*DBLE(MH2**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f133*EL*YukS1Quark3*DBLE(MH3**INT(-2&
  &.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*&
  &SW) - (0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MH2**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*&
  &CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MH3**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CA2*CS1S&
  &1S1f131*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)&
  &*DBLE(MH2**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MH&
  &3**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2&
  &.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DB&
  &LE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)&
  &)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f131*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*&
  &SB*SW)/EL)*DBLE(MH1**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(&
  &-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH1**INT(2.D0)))*DBLE((CW2 + SW2)*&
  &*INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.&
  &D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH1**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*S&
  &W2) + (0.09375D0*CS1S1S1f132*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2&
  &*SA3)*SB*SW)/EL)*DBLE(MH2**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f133*EL2*MZ2*((2.D0*CB*&
  &MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(MH3**INT(-2.D0))*DBLE((C&
  &W2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA&
  &3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH3**INT(2.D0)))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0&
  &*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MA0&
  &2 + MZ2 + 2.D0*(MA02 + DBLE(MH3**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*&
  &SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA&
  &1*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(CA2*(DBLE&
  &(MH1**INT(2.D0)) - 1.D0*DBLE(MH3**INT(2.D0)))) + (0.5D0*CA3*((-0.0625D0*CS1S1S1f111*CS1S1S1f121)/PI2 + (0.0625D0*CS1S1S1f111*C&
  &S1S1S1f211)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f212)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f213)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1&
  &f223)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f233)/PI2 - (0.0625D0*CS1S1S1f123*CS1S1S1f333)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f211&
  &)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f212)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f221)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f222)/PI2 &
  &+ (0.0625D0*CS2S2S1f111*CS2S2S1f112)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f122)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f222)/PI2 - (0&
  &.0625D0*CS2S2S1S1f2212*MA02)/PI2 - (0.125D0*CS1S1S3S3f1222*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1211*MW2)/PI2 - (0.0625D0*CS2S2S1S1f&
  &1112*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2)&
  & - (0.0625D0*CS1S1S1f121*CS2S2S1f221*MA02*DBLE(MH1**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f121*CS1S3S3f122*MHp2*DBLE(MH1**INT(-2.&
  &D0)))/PI2 - (0.125D0*CS1S1S1f121*CS1S3S3f111*MW2*DBLE(MH1**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f121*CS2S2S1f111*MZ2*DBLE(MH1**&
  &INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S1f121*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE(MH1**INT(-2.D0&
  &)))/(PI2*SW2) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MB**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CA2*CS1S1S&
  &1f121*EL*SA1*DBLE(MC**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MD**INT(4.D0)&
  &)*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(ME**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW)&
  & - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark2*(6.D0*MB2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*S&
  &A1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*Yuk&
  &S1Quark2*(6.D0*MD2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep2*(6.D0*ME2 - 1.D0*DBLE(&
  &MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep2*(6.D0*ML2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2)&
  & - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep2*(6.D0*MM2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quar&
  &k1*YukS1Quark2*(6.D0*MS2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)&
  &*(6.D0*MT2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MU2&
  & - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA&
  &2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH1**INT(2.&
  &D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0&
  &*CA1*SA2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH1**INT(2.D0)))))/(PI2*SW2) - (0.0625D0*CS1S1S1f122*CS2S2S1f222*MA02*&
  &DBLE(MH2**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f122*CS1S3S3f222*MHp2*DBLE(MH2**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f122*CS1S3S3f2&
  &11*MW2*DBLE(MH2**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f122*CS2S2S1f112*MZ2*DBLE(MH2**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S1f122*E&
  &L2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(MH2**INT&
  &(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MB**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CS1S&
  &1S1f122*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*Yuk&
  &S1Quark2*DBLE(MD**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(ME**INT(4.D0))*DBLE(MH&
  &2**INT(-2.D0)))/(MW*PI2*SW) - (0.0625D0*CS1S1S1f122*CS1S1S1f211*DBLE(MH1**INT(2.D0))*DBLE(MH2**INT(-2.D0)))/PI2 - (0.09375D0*E&
  &L2*MB2*YukS1Quark1*YukS1Quark2*(6.D0*MB2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(CA1*CA3 - 1&
  &.D0*SA1*SA2*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark2*(6.D0*&
  &MD2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep2*(6.D0*ME2 - 1.D0*DBLE(MH2**INT(2.D0))&
  &))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep2*(6.D0*ML2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*E&
  &L2*MM2*YukS1Lep1*YukS1Lep2*(6.D0*MM2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark2*&
  &(6.D0*MS2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MT2 - 1.&
  &D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH&
  &2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1222*DBLE(MH2**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f122*DBL&
  &E(MH1**INT(-2.D0))*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)&
  & + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH2**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*&
  &CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(M&
  &H2**INT(2.D0)))))/(PI2*SW2) - (0.0625D0*CS1S1S1f123*CS2S2S1f223*MA02*DBLE(MH3**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f123*CS1S3S3&
  &f322*MHp2*DBLE(MH3**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f123*CS1S3S3f311*MW2*DBLE(MH3**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f123&
  &*CS2S2S1f113*MZ2*DBLE(MH3**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S1f123*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL &
  &+ (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(MH3**INT(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3&
  &*DBLE(MB**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**I&
  &NT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3*DBLE(MD**INT(4.D0))*DBLE(MH3**INT(-2.D0))&
  &)/(MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(ME**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.0625D0*CS1S1S1f12&
  &3*CS1S1S1f311*DBLE(MH1**INT(2.D0))*DBLE(MH3**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f123*CS1S1S1f322*DBLE(MH2**INT(2.D0))*DBLE(MH&
  &3**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1S1f1233*DBLE(MH3**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f133*DBLE(MH1**INT(-2.&
  &D0))*DBLE(MH3**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f122*CS1S1S1f233*DBLE(MH2**INT(-2.D0))*DBLE(MH3**INT(2.D0)))/PI2 - (0.25D0*C&
  &S1S1S1f121*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(MH2**IN&
  &T(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(MH3**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*P&
  &I2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS&
  &1Lep2*DBLE(MH2**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(MH3**INT(-2.D0))*DBLE(MM*&
  &*INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MH1**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0&
  &*CS1S1S1f122*EL*YukS1Quark2*DBLE(MH2**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3*DBLE(M&
  &H3**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(&
  &MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MH2**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) &
  &- (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MH3**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D&
  &0*CA2*CS1S1S1f121*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*(CA1*CA3 - 1.D0*SA&
  &1*SA2*SA3)*DBLE(MH2**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA&
  &3)*DBLE(MH3**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*S&
  &W)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(&
  &4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*&
  &SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f121*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2&
  &*MW*SA1*SB*SW)/EL)*DBLE(MH1**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*S&
  &B)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH1**INT(2.D0)))*DBLE((CW2 &
  &+ SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1&
  &.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH1**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2&
  &*PI2*SW2) + (0.09375D0*CS1S1S1f122*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*SB*SW)/EL)*DBLE(MH2**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*S&
  &A1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH2**INT(2.D0)))*DBLE((&
  &CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0&
  &*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH2**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/&
  &(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f123*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2&
  & - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(MH3**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.0546875D0*((2.D0*CA1*CA2*CB&
  &*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(CA2*(D&
  &BLE(MH1**INT(2.D0)) - 1.D0*DBLE(MH2**INT(2.D0))))&
                    & )*DLOG(1D0/EvalScale**2)
                end function dAlpha1MSBarAlter

                double precision function dAlpha2MSBarAlter()
                    use constants
                    implicit none
                    dAlpha2MSBarAlter = ( &
&(0.5D0*CA3*((-0.0625D0*CS1S1S1f111*CS1S1S1f131)/PI2 - (0.0625D0*CS1S1S1f132*CS1S1S1f222)/PI2 + (0.0625D0*CS1S1S1f111*CS1S1S1f311)&
  &/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f313)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f322)/PI2 &
  &+ (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f311)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + (0.1&
  &25D0*CS1S3S3f112*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f322)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.125D0*&
  &CS2S2S1f121*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f223)/PI2 - (0.0625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.125D0*CS1S1S3S&
  &3f1322*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1311*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1113*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3*&
  &SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)))/(PI2*SW2) - (0.0625D0*CS1S1S1f131*CS2S2S1f221*MA02*DBLE(MH1*&
  &*INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f122*MHp2*DBLE(MH1**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f111*MW2*DB&
  &LE(MH1**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f131*CS2S2S1f111*MZ2*DBLE(MH1**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S1f131*EL2*MW2*((&
  &2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE(MH1**INT(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*&
  &DBLE(MB**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MC**INT(4.D0))*DBLE(MH1**INT(-2.D&
  &0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*DBLE(MD**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S&
  &1S1f131*EL*YukS1Lep1*DBLE(ME**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark3*(6.D0&
  &*MB2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MC2 - 1&
  &.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark3*(6.D0*MD2 - 1.D0*DBLE(MH1**INT(2.D0)&
  &)))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*&
  &EL2*ML2*YukS1Lep1*YukS1Lep3*(6.D0*ML2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep3*(6.&
  &D0*MM2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark3*(6.D0*MS2 - 1.D0*DBLE(MH1**INT&
  &(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH1**INT(2.D0)))&
  &)/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH1**INT(2.D0))))/(M&
  &W2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1&
  &*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH1**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL&
  &2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MHp2 +&
  & MW2 + 2.D0*(MHp2 + DBLE(MH1**INT(2.D0)))))/(PI2*SW2) - (0.0625D0*CS1S1S1f132*CS2S2S1f222*MA02*DBLE(MH2**INT(-2.D0)))/PI2 - (0&
  &.125D0*CS1S1S1f132*CS1S3S3f222*MHp2*DBLE(MH2**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f132*CS1S3S3f211*MW2*DBLE(MH2**INT(-2.D0)))/P&
  &I2 - (0.0625D0*CS1S1S1f132*CS2S2S1f112*MZ2*DBLE(MH2**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S1f132*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*&
  &SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(MH2**INT(-2.D0)))/(PI2*SW2) - (0.75D0*C&
  &S1S1S1f132*EL*YukS1Quark2*DBLE(MB**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*&
  &SA2*SA3)*DBLE(MC**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MD**INT(4.D0))*DB&
  &LE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(ME**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (&
  &0.0625D0*CS1S1S1f132*CS1S1S1f211*DBLE(MH1**INT(2.D0))*DBLE(MH2**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1S1f1322*DBLE(MH2**INT(2.D0&
  &)))/PI2 - (0.0625D0*CS1S1S1f122*CS1S1S1f131*DBLE(MH1**INT(-2.D0))*DBLE(MH2**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f133*CS2S2S1f22&
  &3*MA02*DBLE(MH3**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f133*CS1S3S3f322*MHp2*DBLE(MH3**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f133*CS&
  &1S3S3f311*MW2*DBLE(MH3**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f133*CS2S2S1f113*MZ2*DBLE(MH3**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S&
  &1f133*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(&
  &MH3**INT(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f133*EL*YukS1Quark3*DBLE(MB**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.7&
  &5D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S&
  &1S1f133*EL*YukS1Quark3*DBLE(MD**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(ME**INT(&
  &4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.0625D0*CS1S1S1f133*CS1S1S1f311*DBLE(MH1**INT(2.D0))*DBLE(MH3**INT(-2.D0)))/PI2 &
  &- (0.0625D0*CS1S1S1f133*CS1S1S1f322*DBLE(MH2**INT(2.D0))*DBLE(MH3**INT(-2.D0)))/PI2 - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quar&
  &k3*(6.D0*MB2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0&
  &*MC2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark3*(6.D0*MD2 - 1.D0*DBLE(MH3**I&
  &NT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.&
  &03125D0*EL2*ML2*YukS1Lep1*YukS1Lep3*(6.D0*ML2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1&
  &Lep3*(6.D0*MM2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark3*(6.D0*MS2 - 1.D0*DBLE(&
  &MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH3**INT&
  &(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH3**INT(2.D&
  &0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1333*DBLE(MH3**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f131*CS1S1S1f133*DBLE(MH1**INT&
  &(-2.D0))*DBLE(MH3**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f132*CS1S1S1f233*DBLE(MH2**INT(-2.D0))*DBLE(MH3**INT(2.D0)))/PI2 - (0.03&
  &125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MW2 + 2.D&
  &0*DBLE(MH3**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.&
  &D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH3**INT(2.D0)))))/(PI2*SW2) - (0.25D0*CS1S1S1f131*&
  &EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(MH2**INT(-2.D0))*D&
  &BLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(MH3**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0&
  &.25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(&
  &MH2**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(MH3**INT(-2.D0))*DBLE(MM**INT(4.D0))&
  &)/(MW*PI2*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*DBLE(MH1**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f13&
  &2*EL*YukS1Quark2*DBLE(MH2**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f133*EL*YukS1Quark3*DBLE(MH3**INT(-2.&
  &D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*S&
  &W) - (0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MH2**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*C&
  &S1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MH3**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CA2*CS1S1&
  &S1f131*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*&
  &DBLE(MH2**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MH3&
  &**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.&
  &D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBL&
  &E(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))&
  &*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f131*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*S&
  &B*SW)/EL)*DBLE(MH1**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-&
  &1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH1**INT(2.D0)))*DBLE((CW2 + SW2)**&
  &INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D&
  &0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH1**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW&
  &2) + (0.09375D0*CS1S1S1f132*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*&
  &SA3)*SB*SW)/EL)*DBLE(MH2**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f133*EL2*MZ2*((2.D0*CB*M&
  &W*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(MH3**INT(-2.D0))*DBLE((CW&
  &2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3&
  &*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH3**INT(2.D0)))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*&
  &EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MA02&
  & + MZ2 + 2.D0*(MA02 + DBLE(MH3**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*S&
  &W)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1&
  &*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(DBLE(MH1**&
  &INT(2.D0)) - 1.D0*DBLE(MH3**INT(2.D0))) + (0.5D0*SA3*((-0.0625D0*CS1S1S1f111*CS1S1S1f121)/PI2 + (0.0625D0*CS1S1S1f111*CS1S1S1f&
  &211)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f212)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f213)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f223)/P&
  &I2 + (0.0625D0*CS1S1S1f133*CS1S1S1f233)/PI2 - (0.0625D0*CS1S1S1f123*CS1S1S1f333)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f211)/PI2 +&
  & (0.125D0*CS1S3S3f121*CS1S3S3f212)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f221)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f222)/PI2 + (0.06&
  &25D0*CS2S2S1f111*CS2S2S1f112)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f122)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f222)/PI2 - (0.0625D0&
  &*CS2S2S1S1f2212*MA02)/PI2 - (0.125D0*CS1S1S3S3f1222*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1211*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1112*MZ&
  &2)/PI2 + (0.25D0*EL2*MW2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.0&
  &625D0*CS1S1S1f121*CS2S2S1f221*MA02*DBLE(MH1**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f121*CS1S3S3f122*MHp2*DBLE(MH1**INT(-2.D0)))/P&
  &I2 - (0.125D0*CS1S1S1f121*CS1S3S3f111*MW2*DBLE(MH1**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f121*CS2S2S1f111*MZ2*DBLE(MH1**INT(-2.&
  &D0)))/PI2 + (0.1875D0*CS1S1S1f121*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE(MH1**INT(-2.D0)))/(PI&
  &2*SW2) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MB**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f121*E&
  &L*SA1*DBLE(MC**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MD**INT(4.D0))*DBLE(&
  &MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(ME**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.0&
  &9375D0*EL2*MB2*YukS1Quark1*YukS1Quark2*(6.D0*MB2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(CA1&
  &*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark&
  &2*(6.D0*MD2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep2*(6.D0*ME2 - 1.D0*DBLE(MH1**IN&
  &T(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep2*(6.D0*ML2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.0&
  &3125D0*EL2*MM2*YukS1Lep1*YukS1Lep2*(6.D0*MM2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS&
  &1Quark2*(6.D0*MS2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*&
  &MT2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MU2 - 1.D0&
  &*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + &
  &CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH1**INT(2.D0))))/&
  &(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA&
  &2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH1**INT(2.D0)))))/(PI2*SW2) - (0.0625D0*CS1S1S1f122*CS2S2S1f222*MA02*DBLE(MH&
  &2**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f122*CS1S3S3f222*MHp2*DBLE(MH2**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f122*CS1S3S3f211*MW2*&
  &DBLE(MH2**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f122*CS2S2S1f112*MZ2*DBLE(MH2**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S1f122*EL2*MW2*&
  &((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(MH2**INT(-2.D0)&
  &))/(PI2*SW2) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MB**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f122&
  &*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*YukS1Quark&
  &2*DBLE(MD**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(ME**INT(4.D0))*DBLE(MH2**INT(&
  &-2.D0)))/(MW*PI2*SW) - (0.0625D0*CS1S1S1f122*CS1S1S1f211*DBLE(MH1**INT(2.D0))*DBLE(MH2**INT(-2.D0)))/PI2 - (0.09375D0*EL2*MB2*&
  &YukS1Quark1*YukS1Quark2*(6.D0*MB2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark2*(6.D0*MD2 - 1&
  &.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep2*(6.D0*ME2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2&
  &*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep2*(6.D0*ML2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*&
  &YukS1Lep1*YukS1Lep2*(6.D0*MM2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark2*(6.D0*M&
  &S2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MT2 - 1.D0*DBLE&
  &(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH2**INT(&
  &2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1222*DBLE(MH2**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f122*DBLE(MH1**&
  &INT(-2.D0))*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1&
  &*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH2**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)&
  &*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH2**INT&
  &(2.D0)))))/(PI2*SW2) - (0.0625D0*CS1S1S1f123*CS2S2S1f223*MA02*DBLE(MH3**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f123*CS1S3S3f322*MH&
  &p2*DBLE(MH3**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f123*CS1S3S3f311*MW2*DBLE(MH3**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f123*CS2S2S&
  &1f113*MZ2*DBLE(MH3**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S1f123*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0&
  &*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(MH3**INT(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3*DBLE(M&
  &B**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0&
  &))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3*DBLE(MD**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*P&
  &I2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(ME**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.0625D0*CS1S1S1f123*CS1S1&
  &S1f311*DBLE(MH1**INT(2.D0))*DBLE(MH3**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f123*CS1S1S1f322*DBLE(MH2**INT(2.D0))*DBLE(MH3**INT(&
  &-2.D0)))/PI2 - (0.0625D0*CS1S1S1S1f1233*DBLE(MH3**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f133*DBLE(MH1**INT(-2.D0))*DB&
  &LE(MH3**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f122*CS1S1S1f233*DBLE(MH2**INT(-2.D0))*DBLE(MH3**INT(2.D0)))/PI2 - (0.25D0*CS1S1S1f&
  &121*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(MH2**INT(-2.D0&
  &))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(MH3**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*D&
  &BLE(MH2**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(MH3**INT(-2.D0))*DBLE(MM**INT(4.&
  &D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MH1**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S&
  &1f122*EL*YukS1Quark2*DBLE(MH2**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3*DBLE(MH3**INT&
  &(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*&
  &SB*SW) - (0.75D0*CS1S1S1f122*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MH2**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75&
  &D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MH3**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CA2*C&
  &S1S1S1f121*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*(CA1*CA3 - 1.D0*SA1*SA2*S&
  &A3)*DBLE(MH2**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE&
  &(MH3**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*&
  &((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*&
  &DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3&
  &))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f121*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1&
  &*SB*SW)/EL)*DBLE(MH1**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*&
  &(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH1**INT(2.D0)))*DBLE((CW2 + SW2)*&
  &*INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3&
  &*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH1**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW&
  &2) + (0.09375D0*CS1S1S1f122*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*&
  &SA3)*SB*SW)/EL)*DBLE(MH2**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*&
  &(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH2**INT(2.D0)))*DBLE((CW2 + S&
  &W2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0&
  &*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH2**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI&
  &2*SW2) + (0.09375D0*CS1S1S1f123*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0&
  &*CA1*SA3)*SB*SW)/EL)*DBLE(MH3**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)&
  &/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA&
  &3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(DBLE(MH1**INT&
  &(2.D0)) - 1.D0*DBLE(MH2**INT(2.D0)))&
                    & )*DLOG(1D0/EvalScale**2)
                end function dAlpha2MSBarAlter

                double precision function dAlpha3MSBarAlter()
                    use constants
                    implicit none
                    dAlpha3MSBarAlter = ( &
&(0.5D0*SA2*SA3*((-0.0625D0*CS1S1S1f111*CS1S1S1f131)/PI2 - (0.0625D0*CS1S1S1f132*CS1S1S1f222)/PI2 + (0.0625D0*CS1S1S1f111*CS1S1S1f&
  &311)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f313)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f322)/&
  &PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f311)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + &
  &(0.125D0*CS1S3S3f112*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f322)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.12&
  &5D0*CS2S2S1f121*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f223)/PI2 - (0.0625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.125D0*CS1S&
  &1S3S3f1322*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1311*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1113*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*&
  &CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)))/(PI2*SW2) - (0.0625D0*CS1S1S1f131*CS2S2S1f221*MA02*DBLE(&
  &MH1**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f122*MHp2*DBLE(MH1**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f111*MW&
  &2*DBLE(MH1**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f131*CS2S2S1f111*MZ2*DBLE(MH1**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S1f131*EL2*MW&
  &2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE(MH1**INT(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f131*EL*YukS1Qua&
  &rk1*DBLE(MB**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MC**INT(4.D0))*DBLE(MH1**INT(&
  &-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*DBLE(MD**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*&
  &CS1S1S1f131*EL*YukS1Lep1*DBLE(ME**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark3*(&
  &6.D0*MB2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MC2&
  & - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark3*(6.D0*MD2 - 1.D0*DBLE(MH1**INT(2&
  &.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.0312&
  &5D0*EL2*ML2*YukS1Lep1*YukS1Lep3*(6.D0*ML2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep3&
  &*(6.D0*MM2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark3*(6.D0*MS2 - 1.D0*DBLE(MH1*&
  &*INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH1**INT(2.D&
  &0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH1**INT(2.D0)))&
  &)/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0&
  &*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH1**INT(2.D0))))/(PI2*SW2) - (0.03125D&
  &0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MH&
  &p2 + MW2 + 2.D0*(MHp2 + DBLE(MH1**INT(2.D0)))))/(PI2*SW2) - (0.0625D0*CS1S1S1f132*CS2S2S1f222*MA02*DBLE(MH2**INT(-2.D0)))/PI2 &
  &- (0.125D0*CS1S1S1f132*CS1S3S3f222*MHp2*DBLE(MH2**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f132*CS1S3S3f211*MW2*DBLE(MH2**INT(-2.D0)&
  &))/PI2 - (0.0625D0*CS1S1S1f132*CS2S2S1f112*MZ2*DBLE(MH2**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S1f132*EL2*MW2*((2.D0*CB*MW*(-1.D0*&
  &CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(MH2**INT(-2.D0)))/(PI2*SW2) - (0.75&
  &D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MB**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*&
  &SA1*SA2*SA3)*DBLE(MC**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MD**INT(4.D0)&
  &)*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(ME**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW)&
  & - (0.0625D0*CS1S1S1f132*CS1S1S1f211*DBLE(MH1**INT(2.D0))*DBLE(MH2**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1S1f1322*DBLE(MH2**INT(&
  &2.D0)))/PI2 - (0.0625D0*CS1S1S1f122*CS1S1S1f131*DBLE(MH1**INT(-2.D0))*DBLE(MH2**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f133*CS2S2S&
  &1f223*MA02*DBLE(MH3**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f133*CS1S3S3f322*MHp2*DBLE(MH3**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f13&
  &3*CS1S3S3f311*MW2*DBLE(MH3**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f133*CS2S2S1f113*MZ2*DBLE(MH3**INT(-2.D0)))/PI2 + (0.1875D0*CS&
  &1S1S1f133*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*D&
  &BLE(MH3**INT(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f133*EL*YukS1Quark3*DBLE(MB**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - &
  &(0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*&
  &CS1S1S1f133*EL*YukS1Quark3*DBLE(MD**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(ME**&
  &INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.0625D0*CS1S1S1f133*CS1S1S1f311*DBLE(MH1**INT(2.D0))*DBLE(MH3**INT(-2.D0)))/&
  &PI2 - (0.0625D0*CS1S1S1f133*CS1S1S1f322*DBLE(MH2**INT(2.D0))*DBLE(MH3**INT(-2.D0)))/PI2 - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1&
  &Quark3*(6.D0*MB2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(&
  &6.D0*MC2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark3*(6.D0*MD2 - 1.D0*DBLE(MH&
  &3**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) -&
  & (0.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep3*(6.D0*ML2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*Y&
  &ukS1Lep3*(6.D0*MM2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark3*(6.D0*MS2 - 1.D0*D&
  &BLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH3*&
  &*INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH3**INT&
  &(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1333*DBLE(MH3**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f131*CS1S1S1f133*DBLE(MH1*&
  &*INT(-2.D0))*DBLE(MH3**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f132*CS1S1S1f233*DBLE(MH2**INT(-2.D0))*DBLE(MH3**INT(2.D0)))/PI2 - (&
  &0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MW2 +&
  & 2.D0*DBLE(MH3**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) &
  &- 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH3**INT(2.D0)))))/(PI2*SW2) - (0.25D0*CS1S1S1f&
  &131*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(MH2**INT(-2.D0&
  &))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(MH3**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*D&
  &BLE(MH2**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(MH3**INT(-2.D0))*DBLE(MM**INT(4.&
  &D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*DBLE(MH1**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S&
  &1f132*EL*YukS1Quark2*DBLE(MH2**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f133*EL*YukS1Quark3*DBLE(MH3**INT&
  &(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*&
  &SB*SW) - (0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MH2**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75&
  &D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MH3**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CA2*C&
  &S1S1S1f131*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*S&
  &A3)*DBLE(MH2**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE&
  &(MH3**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*&
  &((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))&
  &*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*S&
  &A3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f131*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*S&
  &A1*SB*SW)/EL)*DBLE(MH1**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(C&
  &B*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH1**INT(2.D0)))*DBLE((CW2 + SW&
  &2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(&
  &-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH1**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI&
  &2*SW2) + (0.09375D0*CS1S1S1f132*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*&
  &SA2*SA3)*SB*SW)/EL)*DBLE(MH2**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f133*EL2*MZ2*((2.D0*&
  &CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(MH3**INT(-2.D0))*DBLE&
  &((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0&
  &*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH3**INT(2.D0)))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.01562&
  &5D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*&
  &MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH3**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.0546875D0*((2.D0*CA1*CA2*CB*&
  &MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0&
  &*CA1*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(CA2*(D&
  &BLE(MH1**INT(2.D0)) - 1.D0*DBLE(MH3**INT(2.D0)))) - (0.5D0*CA3*SA2*((-0.0625D0*CS1S1S1f111*CS1S1S1f121)/PI2 + (0.0625D0*CS1S1S&
  &1f111*CS1S1S1f211)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f212)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f213)/PI2 + (0.125D0*CS1S1S1f123*&
  &CS1S1S1f223)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f233)/PI2 - (0.0625D0*CS1S1S1f123*CS1S1S1f333)/PI2 + (0.125D0*CS1S3S3f111*CS1S&
  &3S3f211)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f212)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f221)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f22&
  &2)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f112)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f122)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f222)/P&
  &I2 - (0.0625D0*CS2S2S1S1f2212*MA02)/PI2 - (0.125D0*CS1S1S3S3f1222*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1211*MW2)/PI2 - (0.0625D0*CS2&
  &S2S1S1f1112*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(P&
  &I2*SW2) - (0.0625D0*CS1S1S1f121*CS2S2S1f221*MA02*DBLE(MH1**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f121*CS1S3S3f122*MHp2*DBLE(MH1**&
  &INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f121*CS1S3S3f111*MW2*DBLE(MH1**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f121*CS2S2S1f111*MZ2*DBL&
  &E(MH1**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S1f121*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE(MH1**IN&
  &T(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MB**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CA2&
  &*CS1S1S1f121*EL*SA1*DBLE(MC**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MD**IN&
  &T(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(ME**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*&
  &PI2*SW) - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark2*(6.D0*MB2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL&
  &2*MC2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Qua&
  &rk1*YukS1Quark2*(6.D0*MD2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep2*(6.D0*ME2 - 1.D&
  &0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep2*(6.D0*ML2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*P&
  &I2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep2*(6.D0*MM2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*Yu&
  &kS1Quark1*YukS1Quark2*(6.D0*MS2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(CA1*CA3 - 1.D0*SA1*S&
  &A2*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6&
  &.D0*MU2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*EL2*&
  &(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH1*&
  &*INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1&
  & - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH1**INT(2.D0)))))/(PI2*SW2) - (0.0625D0*CS1S1S1f122*CS2S2S1f22&
  &2*MA02*DBLE(MH2**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f122*CS1S3S3f222*MHp2*DBLE(MH2**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f122*CS&
  &1S3S3f211*MW2*DBLE(MH2**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f122*CS2S2S1f112*MZ2*DBLE(MH2**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S&
  &1f122*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(M&
  &H2**INT(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MB**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.75&
  &D0*CS1S1S1f122*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122&
  &*EL*YukS1Quark2*DBLE(MD**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(ME**INT(4.D0))*&
  &DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.0625D0*CS1S1S1f122*CS1S1S1f211*DBLE(MH1**INT(2.D0))*DBLE(MH2**INT(-2.D0)))/PI2 - (0.09&
  &375D0*EL2*MB2*YukS1Quark1*YukS1Quark2*(6.D0*MB2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(CA1*&
  &CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark2&
  &*(6.D0*MD2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep2*(6.D0*ME2 - 1.D0*DBLE(MH2**INT&
  &(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep2*(6.D0*ML2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03&
  &125D0*EL2*MM2*YukS1Lep1*YukS1Lep2*(6.D0*MM2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1&
  &Quark2*(6.D0*MS2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*M&
  &T2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MU2 - 1.D0*&
  &DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1222*DBLE(MH2**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f&
  &122*DBLE(MH1**INT(-2.D0))*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*S&
  &A2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH2**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.&
  &D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 +&
  & DBLE(MH2**INT(2.D0)))))/(PI2*SW2) - (0.0625D0*CS1S1S1f123*CS2S2S1f223*MA02*DBLE(MH3**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f123*&
  &CS1S3S3f322*MHp2*DBLE(MH3**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f123*CS1S3S3f311*MW2*DBLE(MH3**INT(-2.D0)))/PI2 - (0.0625D0*CS1S&
  &1S1f123*CS2S2S1f113*MZ2*DBLE(MH3**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S1f123*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*&
  &SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(MH3**INT(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f123*EL*YukS&
  &1Quark3*DBLE(MB**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBL&
  &E(MC**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3*DBLE(MD**INT(4.D0))*DBLE(MH3**INT(&
  &-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(ME**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.0625D0*CS1&
  &S1S1f123*CS1S1S1f311*DBLE(MH1**INT(2.D0))*DBLE(MH3**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f123*CS1S1S1f322*DBLE(MH2**INT(2.D0))*&
  &DBLE(MH3**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1S1f1233*DBLE(MH3**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f133*DBLE(MH1**&
  &INT(-2.D0))*DBLE(MH3**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f122*CS1S1S1f233*DBLE(MH2**INT(-2.D0))*DBLE(MH3**INT(2.D0)))/PI2 - (0&
  &.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(&
  &MH2**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(MH3**INT(-2.D0))*DBLE(ML**INT(4.D0))&
  &)/(MW*PI2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f122*&
  &EL*YukS1Lep2*DBLE(MH2**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(MH3**INT(-2.D0))*D&
  &BLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MH1**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - &
  &(0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MH2**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3&
  &*DBLE(MH3**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MT**INT(4.&
  &D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MH2**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*&
  &SB*SW) - (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MH3**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) -&
  & (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*(CA1*CA3 - &
  &1.D0*SA1*SA2*SA3)*DBLE(MH2**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0&
  &*CA1*SA3)*DBLE(MH3**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*S&
  &A1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(E&
  &L**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.&
  &D0*SA1*SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f121*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2&
  &.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE(MH1**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA&
  &2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH1**INT(2.D0)))*DBL&
  &E((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1&
  &.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH1**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)&
  &))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f122*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 -&
  & 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(MH2**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB &
  &+ CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH2**INT(2.D0)))&
  &*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3)&
  & - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH2**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2&
  &.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f123*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*&
  &SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(MH3**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.0546875D0*((2.D0*CA1&
  &*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1&
  &.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/&
  &(CA2*(DBLE(MH1**INT(2.D0)) - 1.D0*DBLE(MH2**INT(2.D0)))) + (0.5D0*((-0.0625D0*CS1S1S1f111*CS1S1S1f231)/PI2 - (0.0625D0*CS1S1S1&
  &f222*CS1S1S1f232)/PI2 + (0.0625D0*CS1S1S1f211*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f212*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f213*&
  &CS1S1S1f313)/PI2 + (0.0625D0*CS1S1S1f222*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f223*CS1S1S1f323)/PI2 + (0.125D0*CS1S3S3f211*CS1S3&
  &S3f311)/PI2 + (0.125D0*CS1S3S3f221*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f212*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f222*CS1S3S3f322&
  &)/PI2 + (0.0625D0*CS2S2S1f112*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f122*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f222*CS2S2S1f223)/PI&
  &2 - (0.0625D0*CS2S2S1S1f2223*MA02)/PI2 - (0.125D0*CS1S1S3S3f2322*MHp2)/PI2 - (0.125D0*CS1S1S3S3f2311*MW2)/PI2 - (0.0625D0*CS2S&
  &2S1S1f1123*MZ2)/PI2 + (0.25D0*EL2*MW2*((-1.D0*CA1*CA3*SA2 + SA1*SA3)*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (-1.D0*CA3*SA1*SA2 -&
  & 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.0625D0*CS1S1S1f231*CS2S2S1f221*MA02*DBLE(MH1**INT(-2.D0)))/PI2 - &
  &(0.125D0*CS1S1S1f231*CS1S3S3f122*MHp2*DBLE(MH1**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f231*CS1S3S3f111*MW2*DBLE(MH1**INT(-2.D0)))&
  &/PI2 - (0.0625D0*CS1S1S1f231*CS2S2S1f111*MZ2*DBLE(MH1**INT(-2.D0)))/PI2 + (0.1875D0*CS1S1S1f231*EL2*MW2*((2.D0*CA1*CA2*CB*MW*S&
  &W)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE(MH1**INT(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f231*EL*YukS1Quark1*DBLE(MB**INT(4.D0))*&
  &DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f231*EL*SA1*DBLE(MC**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SB*SW) &
  &- (0.75D0*CS1S1S1f231*EL*YukS1Quark1*DBLE(MD**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f231*EL*YukS1Lep1&
  &*DBLE(ME**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) - (0.0625D0*CS1S1S1S1f2311*DBLE(MH1**INT(2.D0)))/PI2 - (0.0625D0*CS1S1&
  &S1f232*CS2S2S1f222*MA02*DBLE(MH2**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f232*CS1S3S3f222*MHp2*DBLE(MH2**INT(-2.D0)))/PI2 - (0.125&
  &D0*CS1S1S1f232*CS1S3S3f211*MW2*DBLE(MH2**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f232*CS2S2S1f112*MZ2*DBLE(MH2**INT(-2.D0)))/PI2 +&
  & (0.1875D0*CS1S1S1f232*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*&
  &SB*SW)/EL)*DBLE(MH2**INT(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f232*EL*YukS1Quark2*DBLE(MB**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(M&
  &W*PI2*SW) - (0.75D0*CS1S1S1f232*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SB*SW) - (0&
  &.75D0*CS1S1S1f232*EL*YukS1Quark2*DBLE(MD**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f232*EL*YukS1Lep2*DBL&
  &E(ME**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) - (0.0625D0*CS1S1S1f211*CS1S1S1f232*DBLE(MH1**INT(2.D0))*DBLE(MH2**INT(-2.&
  &D0)))/PI2 - (0.09375D0*EL2*MB2*YukS1Quark2*YukS1Quark3*(6.D0*MB2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*&
  &MC2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) &
  &- (0.09375D0*EL2*MD2*YukS1Quark2*YukS1Quark3*(6.D0*MD2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1L&
  &ep2*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep2*YukS1Lep3*(6.D0*ML2 - 1.D0*&
  &DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep2*YukS1Lep3*(6.D0*MM2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2&
  &*SW2) - (0.09375D0*EL2*MS2*YukS1Quark2*YukS1Quark3*(6.D0*MS2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MT2*&
  &(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0&
  &.09375D0*EL2*MU2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*&
  &PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f2322*DBLE(MH2**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f122*CS1S1S1f231*DBLE(MH1**INT(-2.D0))*DB&
  &LE(MH2**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0&
  &*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH2**INT(2.D0))))/(PI2*SW2) - (0.03125D0&
  &*EL2*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(&
  &-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH2**INT(2.D0)))))/(PI2*SW2) - (0.0625D0*CS1S1S1f2&
  &33*CS2S2S1f223*MA02*DBLE(MH3**INT(-2.D0)))/PI2 - (0.125D0*CS1S1S1f233*CS1S3S3f322*MHp2*DBLE(MH3**INT(-2.D0)))/PI2 - (0.125D0*C&
  &S1S1S1f233*CS1S3S3f311*MW2*DBLE(MH3**INT(-2.D0)))/PI2 - (0.0625D0*CS1S1S1f233*CS2S2S1f113*MZ2*DBLE(MH3**INT(-2.D0)))/PI2 + (0.&
  &1875D0*CS1S1S1f233*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*&
  &SW)/EL)*DBLE(MH3**INT(-2.D0)))/(PI2*SW2) - (0.75D0*CS1S1S1f233*EL*YukS1Quark3*DBLE(MB**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*P&
  &I2*SW) - (0.75D0*CS1S1S1f233*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SB*SW) -&
  & (0.75D0*CS1S1S1f233*EL*YukS1Quark3*DBLE(MD**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f233*EL*YukS1Lep3*&
  &DBLE(ME**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.0625D0*CS1S1S1f233*CS1S1S1f311*DBLE(MH1**INT(2.D0))*DBLE(MH3**INT(&
  &-2.D0)))/PI2 - (0.0625D0*CS1S1S1f233*CS1S1S1f322*DBLE(MH2**INT(2.D0))*DBLE(MH3**INT(-2.D0)))/PI2 - (0.09375D0*EL2*MB2*YukS1Qua&
  &rk2*YukS1Quark3*(6.D0*MB2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MC2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*&
  &(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark2*YukS1Q&
  &uark3*(6.D0*MD2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep2*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH3&
  &**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep2*YukS1Lep3*(6.D0*ML2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - &
  &(0.03125D0*EL2*MM2*YukS1Lep2*YukS1Lep3*(6.D0*MM2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark2*&
  &YukS1Quark3*(6.D0*MS2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MT2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1&
  &*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MU2*(-1.D0*CA3*SA1*SA2 - 1&
  &.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f2333&
  &*DBLE(MH3**INT(2.D0)))/PI2 - (0.0625D0*CS1S1S1f133*CS1S1S1f231*DBLE(MH1**INT(-2.D0))*DBLE(MH3**INT(2.D0)))/PI2 - (0.0625D0*CS1&
  &S1S1f232*CS1S1S1f233*DBLE(MH2**INT(-2.D0))*DBLE(MH3**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.&
  &D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MW2 + 2.D0*&
  &DBLE(MH3**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*&
  &SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH3**&
  &INT(2.D0)))))/(PI2*SW2) - (0.25D0*CS1S1S1f231*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS&
  &1S1S1f232*EL*YukS1Lep2*DBLE(MH2**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f233*EL*YukS1Lep3*DBLE(MH3**INT&
  &(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f231*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI&
  &2*SW) - (0.25D0*CS1S1S1f232*EL*YukS1Lep2*DBLE(MH2**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.25D0*CS1S1S1f233*EL*YukS1&
  &Lep3*DBLE(MH3**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f231*EL*YukS1Quark1*DBLE(MH1**INT(-2.D0))*DBLE(MS&
  &**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CS1S1S1f232*EL*YukS1Quark2*DBLE(MH2**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D&
  &0*CS1S1S1f233*EL*YukS1Quark3*DBLE(MH3**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f231*EL*SA1*DBLE(MH1*&
  &*INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f232*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MH2**INT(-2.D0))*D&
  &BLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f233*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MH3**INT(-2.D0))*DBLE(MT**&
  &INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CA2*CS1S1S1f231*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D&
  &0*CS1S1S1f232*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MH2**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S1S1f233*&
  &EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MH3**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.109375D0*((2.D0*CB*MW*(-1&
  &.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*&
  &CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D&
  &0*EL2*MZ2*((-1.D0*CA1*CA3*SA2 + SA1*SA3)*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.&
  &D0*SA1*SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f231*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2&
  &.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE(MH1**INT(-2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f232*EL2*MZ2*(&
  &(2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(MH2**INT(-2.D0))&
  &*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*C&
  &A1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH2**INT(2.D0)))*D&
  &BLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 &
  &+ SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 +&
  & DBLE(MH2**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f233*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*C&
  &A3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(MH3**INT(-2.D0))*DBLE((CW2 + SW2)**INT(&
  &2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*&
  &CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH3**INT(2.D0)))*DBLE((CW2 + SW2)**INT(2.&
  &D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1&
  &*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH3**INT(2.D0))))&
  &*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.0546875D0*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D&
  &0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(DBLE(M&
  &H2**INT(2.D0)) - 1.D0*DBLE(MH3**INT(2.D0)))&
                    & )*DLOG(1D0/EvalScale**2)
                end function dAlpha3MSBarAlter

                double precision function dBetaMSBarAlter()
                    use constants
                    implicit none
                    dBetaMSBarAlter = ( &
&(0.5D0*((0.0625D0*CS1S1S1f111*CS1S3S3f112)/PI2 - (0.125D0*CS1S3S3f111*CS1S3S3f112)/PI2 - (0.125D0*CS1S3S3f112*CS1S3S3f122)/PI2 + &
  &(0.0625D0*CS1S1S1f222*CS1S3S3f212)/PI2 - (0.125D0*CS1S3S3f211*CS1S3S3f212)/PI2 - (0.125D0*CS1S3S3f212*CS1S3S3f222)/PI2 + (0.06&
  &25D0*CS1S1S1f333*CS1S3S3f312)/PI2 - (0.125D0*CS1S3S3f311*CS1S3S3f312)/PI2 - (0.125D0*CS1S3S3f312*CS1S3S3f322)/PI2 + (0.0625D0*&
  &CS2S2S3S3f2212*MA02)/PI2 + (0.125D0*CS3S3S3S3f1222*MHp2)/PI2 + (0.125D0*CS3S3S3S3f1121*MW2)/PI2 + (0.0625D0*CS2S2S3S3f1112*MZ2&
  &)/PI2 + (0.375D0*CB*CKM23*CKMC23*EL2*MB2*MC2)/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM21*CKMC21*EL2*MC2*MD2)/(MW2*PI2*SB*SW2) - (0.0&
  &9375D0*CB*CKM21*CKMC21*EL2*MC2*MHp2)/(MW2*PI2*SB*SW2) - (0.09375D0*CB*CKM22*CKMC22*EL2*MC2*MHp2)/(MW2*PI2*SB*SW2) - (0.09375D0&
  &*CB*CKM23*CKMC23*EL2*MC2*MHp2)/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM22*CKMC22*EL2*MC2*MS2)/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM33*C&
  &KMC33*EL2*MB2*MT2)/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM31*CKMC31*EL2*MD2*MT2)/(MW2*PI2*SB*SW2) - (0.09375D0*CB*CKM31*CKMC31*EL2*&
  &MHp2*MT2)/(MW2*PI2*SB*SW2) - (0.09375D0*CB*CKM32*CKMC32*EL2*MHp2*MT2)/(MW2*PI2*SB*SW2) - (0.09375D0*CB*CKM33*CKMC33*EL2*MHp2*M&
  &T2)/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM32*CKMC32*EL2*MS2*MT2)/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM13*CKMC13*EL2*MB2*MU2)/(MW2*PI2&
  &*SB*SW2) + (0.375D0*CB*CKM11*CKMC11*EL2*MD2*MU2)/(MW2*PI2*SB*SW2) - (0.09375D0*CB*CKM11*CKMC11*EL2*MHp2*MU2)/(MW2*PI2*SB*SW2) &
  &- (0.09375D0*CB*CKM12*CKMC12*EL2*MHp2*MU2)/(MW2*PI2*SB*SW2) - (0.09375D0*CB*CKM13*CKMC13*EL2*MHp2*MU2)/(MW2*PI2*SB*SW2) + (0.3&
  &75D0*CB*CKM12*CKMC12*EL2*MS2*MU2)/(MW2*PI2*SB*SW2) - (0.03125D0*EL2*ME2*MHp2*YukS3Lep1*YukS3Lep2)/(MW2*PI2*SW2) - (0.03125D0*E&
  &L2*MHp2*ML2*YukS3Lep1*YukS3Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MHp2*MM2*YukS3Lep1*YukS3Lep2)/(MW2*PI2*SW2) - (0.375D0*CB*CKM2&
  &3*CKMC23*EL2*MB2*MC2*YukS3Quark1)/(MW2*PI2*SB*SW2) - (0.375D0*CB*CKM21*CKMC21*EL2*MC2*MD2*YukS3Quark1)/(MW2*PI2*SB*SW2) - (0.3&
  &75D0*CB*CKM22*CKMC22*EL2*MC2*MS2*YukS3Quark1)/(MW2*PI2*SB*SW2) - (0.375D0*CB*CKM33*CKMC33*EL2*MB2*MT2*YukS3Quark1)/(MW2*PI2*SB&
  &*SW2) - (0.375D0*CB*CKM31*CKMC31*EL2*MD2*MT2*YukS3Quark1)/(MW2*PI2*SB*SW2) - (0.375D0*CB*CKM32*CKMC32*EL2*MS2*MT2*YukS3Quark1)&
  &/(MW2*PI2*SB*SW2) - (0.375D0*CB*CKM13*CKMC13*EL2*MB2*MU2*YukS3Quark1)/(MW2*PI2*SB*SW2) - (0.375D0*CB*CKM11*CKMC11*EL2*MD2*MU2*&
  &YukS3Quark1)/(MW2*PI2*SB*SW2) - (0.375D0*CB*CKM12*CKMC12*EL2*MS2*MU2*YukS3Quark1)/(MW2*PI2*SB*SW2) - (0.375D0*CKM23*CKMC23*EL2&
  &*MB2*MC2*YukS3Quark2)/(MW2*PI2*SW2) - (0.375D0*CKM21*CKMC21*EL2*MC2*MD2*YukS3Quark2)/(MW2*PI2*SW2) - (0.375D0*CKM22*CKMC22*EL2&
  &*MC2*MS2*YukS3Quark2)/(MW2*PI2*SW2) - (0.375D0*CKM33*CKMC33*EL2*MB2*MT2*YukS3Quark2)/(MW2*PI2*SW2) - (0.375D0*CKM31*CKMC31*EL2&
  &*MD2*MT2*YukS3Quark2)/(MW2*PI2*SW2) - (0.375D0*CKM32*CKMC32*EL2*MS2*MT2*YukS3Quark2)/(MW2*PI2*SW2) - (0.375D0*CKM13*CKMC13*EL2&
  &*MB2*MU2*YukS3Quark2)/(MW2*PI2*SW2) - (0.375D0*CKM11*CKMC11*EL2*MD2*MU2*YukS3Quark2)/(MW2*PI2*SW2) - (0.375D0*CKM12*CKMC12*EL2&
  &*MS2*MU2*YukS3Quark2)/(MW2*PI2*SW2) + (0.375D0*CKM23*CKMC23*EL2*MB2*MC2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) + (0.375D0*CKM2&
  &1*CKMC21*EL2*MC2*MD2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) - (0.09375D0*CKM13*CKMC13*EL2*MB2*MHp2*YukS3Quark1*YukS3Quark2)/(M&
  &W2*PI2*SW2) - (0.09375D0*CKM23*CKMC23*EL2*MB2*MHp2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) - (0.09375D0*CKM33*CKMC33*EL2*MB2*MH&
  &p2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) - (0.09375D0*CKM11*CKMC11*EL2*MD2*MHp2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) - (0.0&
  &9375D0*CKM21*CKMC21*EL2*MD2*MHp2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) - (0.09375D0*CKM31*CKMC31*EL2*MD2*MHp2*YukS3Quark1*Yuk&
  &S3Quark2)/(MW2*PI2*SW2) + (0.375D0*CKM22*CKMC22*EL2*MC2*MS2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) - (0.09375D0*CKM12*CKMC12*E&
  &L2*MHp2*MS2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) - (0.09375D0*CKM22*CKMC22*EL2*MHp2*MS2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW&
  &2) - (0.09375D0*CKM32*CKMC32*EL2*MHp2*MS2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) + (0.375D0*CKM33*CKMC33*EL2*MB2*MT2*YukS3Quar&
  &k1*YukS3Quark2)/(MW2*PI2*SW2) + (0.375D0*CKM31*CKMC31*EL2*MD2*MT2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) + (0.375D0*CKM32*CKMC&
  &32*EL2*MS2*MT2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) + (0.375D0*CKM13*CKMC13*EL2*MB2*MU2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW&
  &2) + (0.375D0*CKM11*CKMC11*EL2*MD2*MU2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) + (0.375D0*CKM12*CKMC12*EL2*MS2*MU2*YukS3Quark1*&
  &YukS3Quark2)/(MW2*PI2*SW2) + (0.03125D0*CA1*CB2*EL2*MHp2*SA1*DBLE(CA2**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*CB2*EL2*MW2*SA1*&
  &DBLE(CA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*MHp2*SA1*SB2*DBLE(CA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*MW2*SA1&
  &*SB2*DBLE(CA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*MHp2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA2**INT(2.D0)))/(PI2*SW2) - (0.031&
  &25D0*CB*EL2*MW2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*CB2*EL2*MHp2*SA1*DBLE(CA3**INT(2.D0))&
  &)/(PI2*SW2) - (0.03125D0*CA1*CB2*EL2*MW2*SA1*DBLE(CA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*EL2*MHp2*SA1*SB2*DBLE(CA3**INT(2&
  &.D0)))/(PI2*SW2) + (0.03125D0*CA1*EL2*MW2*SA1*SB2*DBLE(CA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*MHp2*SB*DBLE(CA1**INT(2.&
  &D0))*DBLE(CA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*MW2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA3**INT(2.D0)))/(PI2*SW2) + (0.375D&
  &0*CKM13*CKMC13*EL2*YukS3Quark1*YukS3Quark2*DBLE(MB**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CKM23*CKMC23*EL2*YukS3Quark1*YukS3Qua&
  &rk2*DBLE(MB**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CKM33*CKMC33*EL2*YukS3Quark1*YukS3Quark2*DBLE(MB**INT(4.D0)))/(MW2*PI2*SW2) &
  &+ (0.375D0*CB*CKM21*CKMC21*EL2*DBLE(MC**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM22*CKMC22*EL2*DBLE(MC**INT(4.D0)))/(MW2*&
  &PI2*SB*SW2) + (0.375D0*CB*CKM23*CKMC23*EL2*DBLE(MC**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CKM11*CKMC11*EL2*YukS3Quark1*YukS3&
  &Quark2*DBLE(MD**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CKM21*CKMC21*EL2*YukS3Quark1*YukS3Quark2*DBLE(MD**INT(4.D0)))/(MW2*PI2*SW&
  &2) + (0.375D0*CKM31*CKMC31*EL2*YukS3Quark1*YukS3Quark2*DBLE(MD**INT(4.D0)))/(MW2*PI2*SW2) + (0.125D0*EL2*YukS3Lep1*YukS3Lep2*D&
  &BLE(ME**INT(4.D0)))/(MW2*PI2*SW2) + (0.0625D0*CS1S3S3f112*CS2S2S1f221*MA02*DBLE(MH1**INT(-2.D0)))/PI2 + (0.125D0*CS1S3S3f112*C&
  &S1S3S3f122*MHp2*DBLE(MH1**INT(-2.D0)))/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f112*MW2*DBLE(MH1**INT(-2.D0)))/PI2 + (0.0625D0*CS1S3&
  &S3f112*CS2S2S1f111*MZ2*DBLE(MH1**INT(-2.D0)))/PI2 - (0.1875D0*CA1*CA2*CB*CS1S3S3f112*CW2*EL*MW*MZ2*DBLE(MH1**INT(-2.D0)))/(PI2&
  &*SW) - (0.1875D0*CA2*CS1S3S3f112*CW2*EL*MW*MZ2*SA1*SB*DBLE(MH1**INT(-2.D0)))/(PI2*SW) - (0.375D0*CA1*CA2*CB*CS1S3S3f112*EL*MW*&
  &MZ2*SW*DBLE(MH1**INT(-2.D0)))/PI2 - (0.375D0*CA2*CS1S3S3f112*EL*MW*MZ2*SA1*SB*SW*DBLE(MH1**INT(-2.D0)))/PI2 + (0.75D0*CS1S3S3f&
  &112*EL*YukS1Quark1*DBLE(MB**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) + (0.75D0*CA2*CS1S3S3f112*EL*SA1*DBLE(MC**INT(4.D0))&
  &*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SB*SW) + (0.75D0*CS1S3S3f112*EL*YukS1Quark1*DBLE(MD**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2&
  &*SW) + (0.25D0*CS1S3S3f112*EL*YukS1Lep1*DBLE(ME**INT(4.D0))*DBLE(MH1**INT(-2.D0)))/(MW*PI2*SW) + (0.0625D0*CS1S1S3S3f1112*DBLE&
  &(MH1**INT(2.D0)))/PI2 + (0.03125D0*CA1*CB2*EL2*SA1*DBLE(CA2**INT(2.D0))*DBLE(MH1**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*S&
  &A1*SB2*DBLE(CA2**INT(2.D0))*DBLE(MH1**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA2**INT(2.D0))*D&
  &BLE(MH1**INT(2.D0)))/(PI2*SW2) + (0.0625D0*CS1S3S3f212*CS2S2S1f222*MA02*DBLE(MH2**INT(-2.D0)))/PI2 + (0.125D0*CS1S3S3f212*CS1S&
  &3S3f222*MHp2*DBLE(MH2**INT(-2.D0)))/PI2 + (0.125D0*CS1S3S3f211*CS1S3S3f212*MW2*DBLE(MH2**INT(-2.D0)))/PI2 + (0.0625D0*CS1S3S3f&
  &212*CS2S2S1f112*MZ2*DBLE(MH2**INT(-2.D0)))/PI2 + (0.1875D0*CA3*CB*CS1S3S3f212*CW2*EL*MW*MZ2*SA1*DBLE(MH2**INT(-2.D0)))/(PI2*SW&
  &) + (0.1875D0*CA1*CB*CS1S3S3f212*CW2*EL*MW*MZ2*SA2*SA3*DBLE(MH2**INT(-2.D0)))/(PI2*SW) - (0.1875D0*CA1*CA3*CS1S3S3f212*CW2*EL*&
  &MW*MZ2*SB*DBLE(MH2**INT(-2.D0)))/(PI2*SW) + (0.1875D0*CS1S3S3f212*CW2*EL*MW*MZ2*SA1*SA2*SA3*SB*DBLE(MH2**INT(-2.D0)))/(PI2*SW)&
  & + (0.375D0*CA3*CB*CS1S3S3f212*EL*MW*MZ2*SA1*SW*DBLE(MH2**INT(-2.D0)))/PI2 + (0.375D0*CA1*CB*CS1S3S3f212*EL*MW*MZ2*SA2*SA3*SW*&
  &DBLE(MH2**INT(-2.D0)))/PI2 - (0.375D0*CA1*CA3*CS1S3S3f212*EL*MW*MZ2*SB*SW*DBLE(MH2**INT(-2.D0)))/PI2 + (0.375D0*CS1S3S3f212*EL&
  &*MW*MZ2*SA1*SA2*SA3*SB*SW*DBLE(MH2**INT(-2.D0)))/PI2 + (0.75D0*CS1S3S3f212*EL*YukS1Quark2*DBLE(MB**INT(4.D0))*DBLE(MH2**INT(-2&
  &.D0)))/(MW*PI2*SW) + (0.75D0*CA1*CA3*CS1S3S3f212*EL*DBLE(MC**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S3S&
  &3f212*EL*SA1*SA2*SA3*DBLE(MC**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SB*SW) + (0.75D0*CS1S3S3f212*EL*YukS1Quark2*DBLE(MD**I&
  &NT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW*PI2*SW) + (0.25D0*CS1S3S3f212*EL*YukS1Lep2*DBLE(ME**INT(4.D0))*DBLE(MH2**INT(-2.D0)))/(MW&
  &*PI2*SW) + (0.0625D0*CS1S1S1f211*CS1S3S3f212*DBLE(MH1**INT(2.D0))*DBLE(MH2**INT(-2.D0)))/PI2 + (0.0625D0*CS1S1S3S3f2212*DBLE(M&
  &H2**INT(2.D0)))/PI2 - (0.125D0*CA1*CA3*CB*EL2*SA1*SA2*SA3*SB*DBLE(MH2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA3*CB2*EL2*SA2*SA3*&
  &DBLE(CA1**INT(2.D0))*DBLE(MH2**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA3*EL2*SA2*SA3*SB2*DBLE(CA1**INT(2.D0))*DBLE(MH2**INT(2.D0)&
  &))/(PI2*SW2) - (0.03125D0*CA1*CB2*EL2*SA1*DBLE(CA3**INT(2.D0))*DBLE(MH2**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*EL2*SA1*SB2*DB&
  &LE(CA3**INT(2.D0))*DBLE(MH2**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA3**INT(2.D0))*DBLE(MH2**&
  &INT(2.D0)))/(PI2*SW2) + (0.0625D0*CS1S1S1f122*CS1S3S3f112*DBLE(MH1**INT(-2.D0))*DBLE(MH2**INT(2.D0)))/PI2 + (0.0625D0*CS1S3S3f&
  &312*CS2S2S1f223*MA02*DBLE(MH3**INT(-2.D0)))/PI2 + (0.125D0*CS1S3S3f312*CS1S3S3f322*MHp2*DBLE(MH3**INT(-2.D0)))/PI2 + (0.125D0*&
  &CS1S3S3f311*CS1S3S3f312*MW2*DBLE(MH3**INT(-2.D0)))/PI2 + (0.0625D0*CS1S3S3f312*CS2S2S1f113*MZ2*DBLE(MH3**INT(-2.D0)))/PI2 + (0&
  &.1875D0*CA1*CA3*CB*CS1S3S3f312*CW2*EL*MW*MZ2*SA2*DBLE(MH3**INT(-2.D0)))/(PI2*SW) - (0.1875D0*CB*CS1S3S3f312*CW2*EL*MW*MZ2*SA1*&
  &SA3*DBLE(MH3**INT(-2.D0)))/(PI2*SW) + (0.1875D0*CA3*CS1S3S3f312*CW2*EL*MW*MZ2*SA1*SA2*SB*DBLE(MH3**INT(-2.D0)))/(PI2*SW) + (0.&
  &1875D0*CA1*CS1S3S3f312*CW2*EL*MW*MZ2*SA3*SB*DBLE(MH3**INT(-2.D0)))/(PI2*SW) + (0.375D0*CA1*CA3*CB*CS1S3S3f312*EL*MW*MZ2*SA2*SW&
  &*DBLE(MH3**INT(-2.D0)))/PI2 - (0.375D0*CB*CS1S3S3f312*EL*MW*MZ2*SA1*SA3*SW*DBLE(MH3**INT(-2.D0)))/PI2 + (0.375D0*CA3*CS1S3S3f3&
  &12*EL*MW*MZ2*SA1*SA2*SB*SW*DBLE(MH3**INT(-2.D0)))/PI2 + (0.375D0*CA1*CS1S3S3f312*EL*MW*MZ2*SA3*SB*SW*DBLE(MH3**INT(-2.D0)))/PI&
  &2 + (0.75D0*CS1S3S3f312*EL*YukS1Quark3*DBLE(MB**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) - (0.75D0*CA3*CS1S3S3f312*EL*SA1&
  &*SA2*DBLE(MC**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SB*SW) - (0.75D0*CA1*CS1S3S3f312*EL*SA3*DBLE(MC**INT(4.D0))*DBLE(MH3**&
  &INT(-2.D0)))/(MW*PI2*SB*SW) + (0.75D0*CS1S3S3f312*EL*YukS1Quark3*DBLE(MD**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) + (0.2&
  &5D0*CS1S3S3f312*EL*YukS1Lep3*DBLE(ME**INT(4.D0))*DBLE(MH3**INT(-2.D0)))/(MW*PI2*SW) + (0.0625D0*CS1S1S1f311*CS1S3S3f312*DBLE(M&
  &H1**INT(2.D0))*DBLE(MH3**INT(-2.D0)))/PI2 + (0.0625D0*CS1S1S1f322*CS1S3S3f312*DBLE(MH2**INT(2.D0))*DBLE(MH3**INT(-2.D0)))/PI2 &
  &+ (0.0625D0*CS1S1S3S3f3312*DBLE(MH3**INT(2.D0)))/PI2 + (0.125D0*CA1*CA3*CB*EL2*SA1*SA2*SA3*SB*DBLE(MH3**INT(2.D0)))/(PI2*SW2) &
  &+ (0.03125D0*CA3*CB2*EL2*SA2*SA3*DBLE(CA1**INT(2.D0))*DBLE(MH3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA3*EL2*SA2*SA3*SB2*DBLE(CA&
  &1**INT(2.D0))*DBLE(MH3**INT(2.D0)))/(PI2*SW2) + (0.0625D0*CS1S1S1f133*CS1S3S3f112*DBLE(MH1**INT(-2.D0))*DBLE(MH3**INT(2.D0)))/&
  &PI2 + (0.0625D0*CS1S1S1f233*CS1S3S3f212*DBLE(MH2**INT(-2.D0))*DBLE(MH3**INT(2.D0)))/PI2 + (0.125D0*EL2*YukS3Lep1*YukS3Lep2*DBL&
  &E(ML**INT(4.D0)))/(MW2*PI2*SW2) + (0.25D0*CS1S3S3f112*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) + (0&
  &.25D0*CS1S3S3f212*EL*YukS1Lep2*DBLE(MH2**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) + (0.25D0*CS1S3S3f312*EL*YukS1Lep3*DBLE(&
  &MH3**INT(-2.D0))*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) + (0.125D0*EL2*YukS3Lep1*YukS3Lep2*DBLE(MM**INT(4.D0)))/(MW2*PI2*SW2) + (0.2&
  &5D0*CS1S3S3f112*EL*YukS1Lep1*DBLE(MH1**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) + (0.25D0*CS1S3S3f212*EL*YukS1Lep2*DBLE(MH&
  &2**INT(-2.D0))*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) + (0.25D0*CS1S3S3f312*EL*YukS1Lep3*DBLE(MH3**INT(-2.D0))*DBLE(MM**INT(4.D0)))/&
  &(MW*PI2*SW) + (0.375D0*CKM12*CKMC12*EL2*YukS3Quark1*YukS3Quark2*DBLE(MS**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CKM22*CKMC22*EL2&
  &*YukS3Quark1*YukS3Quark2*DBLE(MS**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CKM32*CKMC32*EL2*YukS3Quark1*YukS3Quark2*DBLE(MS**INT(4&
  &.D0)))/(MW2*PI2*SW2) + (0.75D0*CS1S3S3f112*EL*YukS1Quark1*DBLE(MH1**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) + (0.75D0*CS1&
  &S3S3f212*EL*YukS1Quark2*DBLE(MH2**INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) + (0.75D0*CS1S3S3f312*EL*YukS1Quark3*DBLE(MH3**&
  &INT(-2.D0))*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) + (0.375D0*CB*CKM31*CKMC31*EL2*DBLE(MT**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*C&
  &B*CKM32*CKMC32*EL2*DBLE(MT**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM33*CKMC33*EL2*DBLE(MT**INT(4.D0)))/(MW2*PI2*SB*SW2) &
  &+ (0.75D0*CA2*CS1S3S3f112*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) + (0.75D0*CA1*CA3*CS1S3S3f212*EL*DB&
  &LE(MH2**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S3S3f212*EL*SA1*SA2*SA3*DBLE(MH2**INT(-2.D0))*DBLE(MT**IN&
  &T(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CA3*CS1S3S3f312*EL*SA1*SA2*DBLE(MH3**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.7&
  &5D0*CA1*CS1S3S3f312*EL*SA3*DBLE(MH3**INT(-2.D0))*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) + (0.375D0*CB*CKM11*CKMC11*EL2*DBLE(MU**I&
  &NT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM12*CKMC12*EL2*DBLE(MU**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM13*CKMC13*EL&
  &2*DBLE(MU**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.75D0*CA2*CS1S3S3f112*EL*SA1*DBLE(MH1**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB&
  &*SW) + (0.75D0*CA1*CA3*CS1S3S3f212*EL*DBLE(MH2**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CS1S3S3f212*EL*SA1*S&
  &A2*SA3*DBLE(MH2**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CA3*CS1S3S3f312*EL*SA1*SA2*DBLE(MH3**INT(-2.D0))*DB&
  &LE(MU**INT(4.D0)))/(MW*PI2*SB*SW) - (0.75D0*CA1*CS1S3S3f312*EL*SA3*DBLE(MH3**INT(-2.D0))*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) -&
  & (0.375D0*CA1*CA2*CB*CS1S3S3f112*EL*DBLE(MH1**INT(-2.D0))*DBLE(MW**INT(3.D0)))/(PI2*SW) - (0.375D0*CA2*CS1S3S3f112*EL*SA1*SB*D&
  &BLE(MH1**INT(-2.D0))*DBLE(MW**INT(3.D0)))/(PI2*SW) + (0.375D0*CA3*CB*CS1S3S3f212*EL*SA1*DBLE(MH2**INT(-2.D0))*DBLE(MW**INT(3.D&
  &0)))/(PI2*SW) + (0.375D0*CA1*CB*CS1S3S3f212*EL*SA2*SA3*DBLE(MH2**INT(-2.D0))*DBLE(MW**INT(3.D0)))/(PI2*SW) - (0.375D0*CA1*CA3*&
  &CS1S3S3f212*EL*SB*DBLE(MH2**INT(-2.D0))*DBLE(MW**INT(3.D0)))/(PI2*SW) + (0.375D0*CS1S3S3f212*EL*SA1*SA2*SA3*SB*DBLE(MH2**INT(-&
  &2.D0))*DBLE(MW**INT(3.D0)))/(PI2*SW) + (0.375D0*CA1*CA3*CB*CS1S3S3f312*EL*SA2*DBLE(MH3**INT(-2.D0))*DBLE(MW**INT(3.D0)))/(PI2*&
  &SW) - (0.375D0*CB*CS1S3S3f312*EL*SA1*SA3*DBLE(MH3**INT(-2.D0))*DBLE(MW**INT(3.D0)))/(PI2*SW) + (0.375D0*CA3*CS1S3S3f312*EL*SA1&
  &*SA2*SB*DBLE(MH3**INT(-2.D0))*DBLE(MW**INT(3.D0)))/(PI2*SW) + (0.375D0*CA1*CS1S3S3f312*EL*SA3*SB*DBLE(MH3**INT(-2.D0))*DBLE(MW&
  &**INT(3.D0)))/(PI2*SW) + (0.03125D0*CB*EL2*MHp2*SB*DBLE(CA2**INT(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*MW&
  &2*SB*DBLE(CA2**INT(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*MHp2*SB*DBLE(CA3**INT(2.D0))*DBLE(SA1**INT(2.D0)&
  &))/(PI2*SW2) - (0.03125D0*CB*EL2*MW2*SB*DBLE(CA3**INT(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*SB*DBLE(CA2**&
  &INT(2.D0))*DBLE(MH1**INT(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA3*CB2*EL2*SA2*SA3*DBLE(MH2**INT(2.D0))*DBLE(SA1&
  &**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA3*EL2*SA2*SA3*SB2*DBLE(MH2**INT(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*&
  &EL2*SB*DBLE(CA3**INT(2.D0))*DBLE(MH2**INT(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA3*CB2*EL2*SA2*SA3*DBLE(MH3**IN&
  &T(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA3*EL2*SA2*SA3*SB2*DBLE(MH3**INT(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2)&
  & + (0.03125D0*CA1*CB2*EL2*MHp2*SA1*DBLE(CA3**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*CB2*EL2*MW2*SA1*DBLE(&
  &CA3**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*MHp2*SA1*SB2*DBLE(CA3**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(&
  &PI2*SW2) - (0.03125D0*CA1*EL2*MW2*SA1*SB2*DBLE(CA3**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*MHp2*SB*DBL&
  &E(CA1**INT(2.D0))*DBLE(CA3**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*MW2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA&
  &3**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*CB2*EL2*SA1*DBLE(CA3**INT(2.D0))*DBLE(MH3**INT(2.D0))*DBLE(SA2*&
  &*INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*SA1*SB2*DBLE(CA3**INT(2.D0))*DBLE(MH3**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) &
  &- (0.03125D0*CB*EL2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA3**INT(2.D0))*DBLE(MH3**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) + (0.031&
  &25D0*CB*EL2*MHp2*SB*DBLE(CA3**INT(2.D0))*DBLE(SA1**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*MW2*SB*DBLE(&
  &CA3**INT(2.D0))*DBLE(SA1**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*SB*DBLE(CA3**INT(2.D0))*DBLE(MH3**INT&
  &(2.D0))*DBLE(SA1**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*CB2*EL2*MHp2*SA1*DBLE(SA3**INT(2.D0)))/(PI2*SW2)&
  & - (0.03125D0*CA1*CB2*EL2*MW2*SA1*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*EL2*MHp2*SA1*SB2*DBLE(SA3**INT(2.D0)))/(PI2&
  &*SW2) + (0.03125D0*CA1*EL2*MW2*SA1*SB2*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*MHp2*SB*DBLE(CA1**INT(2.D0))*DBLE(S&
  &A3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*MW2*SB*DBLE(CA1**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*CB2&
  &*EL2*SA1*DBLE(MH3**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*EL2*SA1*SB2*DBLE(MH3**INT(2.D0))*DBLE(SA3**INT(&
  &2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*SB*DBLE(CA1**INT(2.D0))*DBLE(MH3**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D&
  &0*CB*EL2*MHp2*SB*DBLE(SA1**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*MW2*SB*DBLE(SA1**INT(2.D0))*DBLE(SA3&
  &**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*SB*DBLE(MH3**INT(2.D0))*DBLE(SA1**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.&
  &03125D0*CA1*CB2*EL2*MHp2*SA1*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*CB2*EL2*MW2*SA1*DBLE(SA2**I&
  &NT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*MHp2*SA1*SB2*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW&
  &2) - (0.03125D0*CA1*EL2*MW2*SA1*SB2*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*MHp2*SB*DBLE(CA1*&
  &*INT(2.D0))*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*MW2*SB*DBLE(CA1**INT(2.D0))*DBLE(SA2**INT&
  &(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*CB2*EL2*SA1*DBLE(MH2**INT(2.D0))*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2&
  &.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*SA1*SB2*DBLE(MH2**INT(2.D0))*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.0&
  &3125D0*CB*EL2*SB*DBLE(CA1**INT(2.D0))*DBLE(MH2**INT(2.D0))*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*C&
  &B*EL2*MHp2*SB*DBLE(SA1**INT(2.D0))*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*MW2*SB*DBLE(SA1**I&
  &NT(2.D0))*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*SB*DBLE(MH2**INT(2.D0))*DBLE(SA1**INT(2.D0)&
  &)*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.1875D0*CA1*CA2*CB*CS1S3S3f112*EL*MW*MZ2*DBLE(MH1**INT(-2.D0))*DBLE&
  &(SW**INT(3.D0)))/(CW2*PI2) - (0.1875D0*CA2*CS1S3S3f112*EL*MW*MZ2*SA1*SB*DBLE(MH1**INT(-2.D0))*DBLE(SW**INT(3.D0)))/(CW2*PI2) +&
  & (0.1875D0*CA3*CB*CS1S3S3f212*EL*MW*MZ2*SA1*DBLE(MH2**INT(-2.D0))*DBLE(SW**INT(3.D0)))/(CW2*PI2) + (0.1875D0*CA1*CB*CS1S3S3f21&
  &2*EL*MW*MZ2*SA2*SA3*DBLE(MH2**INT(-2.D0))*DBLE(SW**INT(3.D0)))/(CW2*PI2) - (0.1875D0*CA1*CA3*CS1S3S3f212*EL*MW*MZ2*SB*DBLE(MH2&
  &**INT(-2.D0))*DBLE(SW**INT(3.D0)))/(CW2*PI2) + (0.1875D0*CS1S3S3f212*EL*MW*MZ2*SA1*SA2*SA3*SB*DBLE(MH2**INT(-2.D0))*DBLE(SW**I&
  &NT(3.D0)))/(CW2*PI2) + (0.1875D0*CA1*CA3*CB*CS1S3S3f312*EL*MW*MZ2*SA2*DBLE(MH3**INT(-2.D0))*DBLE(SW**INT(3.D0)))/(CW2*PI2) - (&
  &0.1875D0*CB*CS1S3S3f312*EL*MW*MZ2*SA1*SA3*DBLE(MH3**INT(-2.D0))*DBLE(SW**INT(3.D0)))/(CW2*PI2) + (0.1875D0*CA3*CS1S3S3f312*EL*&
  &MW*MZ2*SA1*SA2*SB*DBLE(MH3**INT(-2.D0))*DBLE(SW**INT(3.D0)))/(CW2*PI2) + (0.1875D0*CA1*CS1S3S3f312*EL*MW*MZ2*SA3*SB*DBLE(MH3**&
  &INT(-2.D0))*DBLE(SW**INT(3.D0)))/(CW2*PI2)))/MHp2&
                    & )*DLOG(1D0/EvalScale**2)
                end function dBetaMSBarAlter

                ! double precision function dBetaProcDep1Alter()
                !     use constants
                !     implicit none
                !     double complex A0toTauPTauMProcDepVC
                !     dBetaProcDep1Alter = -Yuk6/(1 + Yuk6**2)*( A0toTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSAlterWeak()/ML -&
                !         & dMW2Alter()/(2D0*MW2) + dZA0A0OS()/2D0 - dZG0A0OSAlter()/(2D0*Yuk6) + dZTauTauOSLeftWeak()/2D0 + &
                !         & dZTauTauOSRightWeak()/2D0 )
                ! end function dBetaProcDep1Alter

                ! double precision function dAlphaProcDep1Alter()
                !     use constants
                !     implicit none
                !     double complex HHtoTauPTauMProcDepVC
                !     dAlphaProcDep1Alter = -Yuk5/Yuk4*( HHtoTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSAlterWeak()/ML - &
                !         & dMW2Alter()/(2D0*MW2) + Yuk6*dBetaProcDep1Alter() + dZHHHHOS()/2D0 + Yuk4*dZh0HHOSAlter()/(2D0*Yuk5) + &
                !         & dZTauTauOSLeftWeak()/2D0 + dZTauTauOSRightWeak()/2D0 )
                ! end function dAlphaProcDep1Alter

                ! double precision function dBetaProcDep2Alter()
                !     use constants
                !     implicit none
                !     double complex A0toTauPTauMProcDepVC
                !     dBetaProcDep2Alter = -Yuk6/(1 + Yuk6**2)*( A0toTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSAlterWeak()/ML -&
                !         & dMW2Alter()/(2D0*MW2) + dZA0A0OS()/2D0 - dZG0A0OSAlter()/(2D0*Yuk6) + dZTauTauOSLeftWeak()/2D0 + &
                !         & dZTauTauOSRightWeak()/2D0 )
                ! end function dBetaProcDep2Alter

                ! double precision function dAlphaProcDep2Alter()
                !     use constants
                !     implicit none
                !     double complex h0toTauPTauMProcDepVC
                !     dAlphaProcDep2Alter = Yuk4/Yuk5*( h0toTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSAlterWeak()/ML - &
                !         & dMW2Alter()/(2D0*MW2) + Yuk6*dBetaProcDep2Alter() + dZh0h0OS()/2D0 + Yuk5*dZHHh0OSAlter()/(2D0*Yuk4) + &
                !         & dZTauTauOSLeftWeak()/2D0 + dZTauTauOSRightWeak()/2D0 )
                ! end function dAlphaProcDep2Alter

                ! double precision function dBetaProcDep3Alter()
                !     use constants
                !     implicit none
                !     double complex h0toTauPTauMProcDepVC, HHtoTauPTauMProcDepVC
                !     dBetaProcDep3Alter = -1D0/(Yuk6*(Yuk4**2 + Yuk5**2))*( Yuk4*Yuk5*(dZHHh0OSAlter()/2D0 + dZh0HHOSAlter()/2D0) &
                !         & + Yuk4**2*( h0toTauPTauMProcDepVC() + dZh0h0OS()/2D0 ) &
                !         & + Yuk5**2*( HHtoTauPTauMProcDepVC() + dZHHHHOS()/2D0 ) &
                !         & + (Yuk4**2 + Yuk5**2)*( dgAtMZ()/(EL/SW) + dMLOSAlterWeak()/ML - dMW2Alter()/(2D0*MW2) + &
                !         & dZTauTauOSLeftWeak()/2D0 + dZTauTauOSRightWeak()/2D0 ) )
                ! end function dBetaProcDep3Alter

                ! double precision function dAlphaProcDep3Alter()
                !     use constants
                !     implicit none
                !     double complex h0toTauPTauMProcDepVC, HHtoTauPTauMProcDepVC
                !     dAlphaProcDep3Alter = Yuk4*Yuk5/(Yuk4**2 + Yuk5**2) * ( h0toTauPTauMProcDepVC() - HHtoTauPTauMProcDepVC() + &
                !         & dZh0h0OS()/2D0 - dZHHHHOS()/2D0 + Yuk5/Yuk4*dZHHh0OSAlter()/2D0 - Yuk4/Yuk5*dZh0HHOSAlter()/2D0 )
                ! end function dAlphaProcDep3Alter

            ! Usual tadpole based counterterms
                double precision function dAlpha1KanUsual()
                    use constants
                    implicit none
                    dAlpha1KanUsual = CA3*( dZH1H2OSUsual() - dZH2H1OSUsual() )/(4.D0*CA2) - &
                                    & SA3*( dZH1H3OSUsual() - dZH3H1OSUsual() )/(4.D0*CA2)
                end function dAlpha1KanUsual

                double precision function dAlpha2KanUsual()
                    use constants
                    implicit none
                    dAlpha2KanUsual = CA3*( dZH1H3OSUsual() - dZH3H1OSUsual() )/4.D0 + &
                                    & SA3*( dZH1H2OSUsual() - dZH2H1OSUsual() )/4.D0
                end function dAlpha2KanUsual

                double precision function dAlpha3KanUsual()
                    use constants
                    implicit none
                    dAlpha3KanUsual = ( dZH2H3OSUsual() - dZH3H2OSUsual() )/4.D0 + &
                                    & SA2*SA3*( dZH1H3OSUsual() - dZH3H1OSUsual() )/(4.D0*CA2) - &
                                    & SA2*CA3*( dZH1H2OSUsual() - dZH2H1OSUsual() )/(4.D0*CA2)
                end function dAlpha3KanUsual

                double precision function dBeta1KanUsual()
                    use constants
                    implicit none
                    dBeta1KanUsual = ( dZG0A0OSUsual() - dZA0G0OSUsual() )/4.D0
                end function dBeta1KanUsual

                double precision function dBeta2KanUsual()
                    use constants
                    implicit none
                    dBeta2KanUsual = ( dZGpHpOSUsual() - dZHpGpOSUsual() )/4.D0
                end function dBeta2KanUsual

                double precision function dAlpha1MSBarUsual()
                    use constants
                    implicit none
                    dAlpha1MSBarUsual = ( &
&(-0.5D0*SA3*((0.0625D0*CS1S1S1f111*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f313)/P&
  &I2 + (0.0625D0*CS1S1S1f122*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f333)/PI2 +&
  & (0.125D0*CS1S3S3f111*CS1S3S3f311)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f321)/PI2 + (0.12&
  &5D0*CS1S3S3f122*CS1S3S3f322)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f123)/PI2 + (0.0625D0*&
  &CS2S2S1f221*CS2S2S1f223)/PI2 - (0.0625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.125D0*CS1S1S3S3f1322*MHp2)/PI2 - (0.125D0*CS1S1S3S3f131&
  &1*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1113*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*&
  &CA1*CA3*SA2 + SA1*SA3)))/(PI2*SW2) - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark3*(6.D0*MB2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*P&
  &I2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*S&
  &W2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark3*(6.D0*MD2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*Yu&
  &kS1Lep1*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep3*(6.D0*ML2 - 1&
  &.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep3*(6.D0*MM2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2&
  &*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark3*(6.D0*MS2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*&
  &EL2*MT2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*&
  &MU2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f131&
  &1*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 &
  &- 1.D0*CA1*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH1**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.&
  &D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH1**INT(2.D0)))&
  &))/(PI2*SW2) - (0.0625D0*CS1S1S1S1f1322*DBLE(MH2**INT(2.D0)))/PI2 - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark3*(6.D0*MB2 - 1.D&
  &0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MC2 - 1.D0*DBLE(M&
  &H3**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark3*(6.D0*MD2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*P&
  &I2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*Yu&
  &kS1Lep1*YukS1Lep3*(6.D0*ML2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep3*(6.D0*MM2 - 1&
  &.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark3*(6.D0*MS2 - 1.D0*DBLE(MH3**INT(2.D0))))/&
  &(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2&
  &*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SB2&
  &*SW2) - (0.0625D0*CS1S1S1S1f1333*DBLE(MH3**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 +&
  & SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH3**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*&
  &SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D&
  &0*(MHp2 + DBLE(MH3**INT(2.D0)))))/(PI2*SW2) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*&
  &MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**&
  &INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*DBLE(&
  &(CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*&
  &CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH1**INT(2.D0)))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625&
  &D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*M&
  &A02 + MZ2 + 2.D0*(MA02 + DBLE(MH1**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA&
  &2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH3**INT(2.D0)))*DB&
  &LE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*S&
  &A3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH3**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D&
  &0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR31*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0&
  &.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW&
  &*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)&
  &))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*&
  &PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1&
  &S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**I&
  &NT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(&
  &MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2&
  &.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D0*C&
  &S1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.&
  &D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3&
  &*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.37&
  &5D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S&
  &1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT(2.&
  &D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - &
  &(0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))&
  &/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2&
  &.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(&
  &2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2&
  &)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW&
  &*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(&
  &CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (&
  &0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f&
  &222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/&
  &(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW&
  &) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DB&
  &LE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1&
  &*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR12*RR32*(RR12*((-0&
  &.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2&
  &)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE&
  &(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(&
  &4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI&
  &2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep&
  &1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS*&
  &*INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/&
  &(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(&
  &CW2*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (&
  &0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA&
  &1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA&
  &3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.&
  &125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f32&
  &2*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(M&
  &W*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*C&
  &A1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW&
  &*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA&
  &02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*&
  &MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.&
  &375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*&
  &SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (&
  &0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DB&
  &LE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/&
  &(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT&
  &(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*(&
  &(2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT&
  &(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR13*RR33*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 -&
  & (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2&
  &*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.&
  &D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(&
  &MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*&
  &CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM&
  &**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0))&
  &)/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL +&
  & (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D&
  &0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(&
  &-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Qua&
  &rk3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0&
  &.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*C&
  &S1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT&
  &(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW)&
  & - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0&
  &)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*&
  &((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**I&
  &NT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*&
  &MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0&
  &*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*E&
  &L*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1&
  &S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)&
  &))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2&
  &*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)&
  &*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(&
  &CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*&
  &SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA&
  &1*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(CA2*(DBLE&
  &(MH1**INT(2.D0)) - 1.D0*DBLE(MH3**INT(2.D0)))) + (0.5D0*CA3*((0.0625D0*CS1S1S1f111*CS1S1S1f211)/PI2 + (0.125D0*CS1S1S1f112*CS1&
  &S1S1f212)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f213)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f222)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f&
  &223)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f233)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f211)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f212)/&
  &PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f221)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f222)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f112)/PI2 +&
  & (0.125D0*CS2S2S1f121*CS2S2S1f122)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f222)/PI2 - (0.0625D0*CS2S2S1S1f2212*MA02)/PI2 - (0.125D&
  &0*CS1S1S3S3f1222*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1211*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1112*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA1*CA2*(&
  &-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Qua&
  &rk2*(6.D0*MB2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MC2 &
  &- 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark2*(6.D0*MD2 - 1.D0*DBLE(MH1**INT(2.&
  &D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep2*(6.D0*ME2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125&
  &D0*EL2*ML2*YukS1Lep1*YukS1Lep2*(6.D0*ML2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep2*&
  &(6.D0*MM2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark2*(6.D0*MS2 - 1.D0*DBLE(MH1**&
  &INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH1**INT(2.D0))))/(&
  &MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB&
  &2*SW2) - (0.0625D0*CS1S1S1S1f1211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.&
  &D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH1**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*&
  &SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0&
  &*(MHp2 + DBLE(MH1**INT(2.D0)))))/(PI2*SW2) - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark2*(6.D0*MB2 - 1.D0*DBLE(MH2**INT(2.D0)))&
  &)/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2&
  &*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark2*(6.D0*MD2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*&
  &YukS1Lep1*YukS1Lep2*(6.D0*ME2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep2*(6.D0*ML2 -&
  & 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep2*(6.D0*MM2 - 1.D0*DBLE(MH2**INT(2.D0))))/(M&
  &W2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark2*(6.D0*MS2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA&
  &2*EL2*MT2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*&
  &SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1222*DBLE(MH2&
  &**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*&
  &SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH2**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0&
  &*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH2**INT(2.D0)))))/(PI2*SW2)&
  & - (0.0625D0*CS1S1S1S1f1233*DBLE(MH3**INT(2.D0)))/PI2 + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*&
  &((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*&
  &DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3&
  &))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*S&
  &A3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH1**INT(2.D0)))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0&
  &.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-&
  &1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH1**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*C&
  &B + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH2**INT(2.D0)&
  &))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA&
  &3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH2**INT(2.D0))))*DBLE((CW2 + SW2)**INT&
  &(2.D0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR21*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 &
  &- (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA&
  &2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4&
  &.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/&
  &(MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0&
  &*CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(M&
  &M**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)&
  &))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL &
  &+ (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625&
  &D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*&
  &(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Qu&
  &ark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (&
  &0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*&
  &CS1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**IN&
  &T(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW&
  &) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D&
  &0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2&
  &*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**&
  &INT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211&
  &*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D&
  &0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*&
  &EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW)&
  & - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S&
  &1S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0&
  &)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI&
  &2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3&
  &)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*&
  &(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR12*RR22*(RR12*&
  &((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111&
  &*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*&
  &DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**&
  &INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0))&
  &)/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS&
  &1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE&
  &(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0&
  &)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)&
  &))/(CW2*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2&
  & - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA&
  &3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D&
  &0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) -&
  & (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S&
  &1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0))&
  &)/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*&
  &SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.&
  &D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D&
  &0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f22&
  &2*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*&
  &EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) -&
  & (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*&
  &PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW)&
  & - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f23&
  &3*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0&
  &)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT*&
  &*INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*M&
  &Z2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)*&
  &*INT(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR13*RR23*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/P&
  &I2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0&
  &*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**IN&
  &T(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)&
  &))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.0312&
  &5D0*CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBL&
  &E(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.&
  &D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/&
  &EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0&
  &625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*&
  &MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS&
  &1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) &
  &- (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125&
  &D0*CS1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3*&
  &*INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2&
  &*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(&
  &4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*&
  &MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2&
  &)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f&
  &211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (&
  &2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375&
  &D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*&
  &SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*C&
  &S1S1S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4&
  &.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW&
  &*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*&
  &SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*&
  &MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CA1*CA2*CB&
  &*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(CA2*(D&
  &BLE(MH1**INT(2.D0)) - 1.D0*DBLE(MH2**INT(2.D0)))) &
                    & )*DLOG(1D0/EvalScale**2)
                end function dAlpha1MSBarUsual

                double precision function dAlpha2MSBarUsual()
                    use constants
                    implicit none
                    dAlpha2MSBarUsual = ( &
&(0.5D0*CA3*((0.0625D0*CS1S1S1f111*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f313)/PI&
  &2 + (0.0625D0*CS1S1S1f122*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f333)/PI2 + &
  &(0.125D0*CS1S3S3f111*CS1S3S3f311)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f321)/PI2 + (0.125&
  &D0*CS1S3S3f122*CS1S3S3f322)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f123)/PI2 + (0.0625D0*C&
  &S2S2S1f221*CS2S2S1f223)/PI2 - (0.0625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.125D0*CS1S1S3S3f1322*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1311&
  &*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1113*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*C&
  &A1*CA3*SA2 + SA1*SA3)))/(PI2*SW2) - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark3*(6.D0*MB2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI&
  &2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW&
  &2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark3*(6.D0*MD2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*Yuk&
  &S1Lep1*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep3*(6.D0*ML2 - 1.&
  &D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep3*(6.D0*MM2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*&
  &PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark3*(6.D0*MS2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*E&
  &L2*MT2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*M&
  &U2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1311&
  &*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 -&
  & 1.D0*CA1*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH1**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D&
  &0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH1**INT(2.D0))))&
  &)/(PI2*SW2) - (0.0625D0*CS1S1S1S1f1322*DBLE(MH2**INT(2.D0)))/PI2 - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark3*(6.D0*MB2 - 1.D0&
  &*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH&
  &3**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark3*(6.D0*MD2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI&
  &2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*Yuk&
  &S1Lep1*YukS1Lep3*(6.D0*ML2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep3*(6.D0*MM2 - 1.&
  &D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark3*(6.D0*MS2 - 1.D0*DBLE(MH3**INT(2.D0))))/(&
  &MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*&
  &SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SB2*&
  &SW2) - (0.0625D0*CS1S1S1S1f1333*DBLE(MH3**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + &
  &SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH3**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*S&
  &A1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0&
  &*(MHp2 + DBLE(MH3**INT(2.D0)))))/(PI2*SW2) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*M&
  &W*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**I&
  &NT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*DBLE((&
  &CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*C&
  &A3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH1**INT(2.D0)))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D&
  &0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MA&
  &02 + MZ2 + 2.D0*(MA02 + DBLE(MH1**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2&
  &*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH3**INT(2.D0)))*DBL&
  &E((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA&
  &3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH3**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0&
  &)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR31*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.&
  &0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*&
  &SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0))&
  &)/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*P&
  &I2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S&
  &1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(M&
  &W*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.&
  &D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D0*CS&
  &1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D&
  &0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*&
  &DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375&
  &D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1&
  &S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT(2.D&
  &0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (&
  &0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/&
  &(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.&
  &D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2&
  &.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)&
  &/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*&
  &(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(C&
  &A1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0&
  &.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f2&
  &22*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(&
  &MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW)&
  & - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBL&
  &E(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*&
  &CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR12*RR32*(RR12*((-0.&
  &03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)&
  &/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(&
  &MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4&
  &.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2&
  & - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1&
  &*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**&
  &INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(&
  &MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(C&
  &W2*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0&
  &.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1&
  &*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3&
  &*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.1&
  &25D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322&
  &*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW&
  &*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) -&
  & (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA&
  &1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*&
  &(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA0&
  &2)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*M&
  &W2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.3&
  &75D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*S&
  &B*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0&
  &.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBL&
  &E(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(&
  &MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(&
  &4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((&
  &2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(&
  &2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR13*RR33*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - &
  &(0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*&
  &MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D&
  &0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(M&
  &W*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*C&
  &S1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM*&
  &*INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))&
  &/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + &
  &(2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D0&
  &*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-&
  &1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quar&
  &k3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.&
  &375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS&
  &1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT(&
  &2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)&
  &))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*(&
  &(2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**IN&
  &T(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*M&
  &W2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*&
  &MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL&
  &*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) -&
  & (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S&
  &1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0))&
  &)/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*&
  &SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*&
  &DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(C&
  &A1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*S&
  &W)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1&
  &*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(DBLE(MH1**&
  &INT(2.D0)) - 1.D0*DBLE(MH3**INT(2.D0))) + (0.5D0*SA3*((0.0625D0*CS1S1S1f111*CS1S1S1f211)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f21&
  &2)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f213)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f222)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f223)/PI&
  &2 + (0.0625D0*CS1S1S1f133*CS1S1S1f233)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f211)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f212)/PI2 + (&
  &0.125D0*CS1S3S3f112*CS1S3S3f221)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f222)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f112)/PI2 + (0.125&
  &D0*CS2S2S1f121*CS2S2S1f122)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f222)/PI2 - (0.0625D0*CS2S2S1S1f2212*MA02)/PI2 - (0.125D0*CS1S1&
  &S3S3f1222*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1211*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1112*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA1*CA2*(-1.D0*C&
  &A3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark2*(6.&
  &D0*MB2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MC2 - 1.D0*&
  &DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark2*(6.D0*MD2 - 1.D0*DBLE(MH1**INT(2.D0))))/&
  &(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep2*(6.D0*ME2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*&
  &ML2*YukS1Lep1*YukS1Lep2*(6.D0*ML2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep2*(6.D0*M&
  &M2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark2*(6.D0*MS2 - 1.D0*DBLE(MH1**INT(2.D&
  &0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2&
  &*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) &
  &- (0.0625D0*CS1S1S1S1f1211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*&
  &SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH1**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1&
  &.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 &
  &+ DBLE(MH1**INT(2.D0)))))/(PI2*SW2) - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark2*(6.D0*MB2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*&
  &PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) -&
  & (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark2*(6.D0*MD2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Le&
  &p1*YukS1Lep2*(6.D0*ME2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep2*(6.D0*ML2 - 1.D0*D&
  &BLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep2*(6.D0*MM2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*&
  &SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark2*(6.D0*MS2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*M&
  &T2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(CA&
  &1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1222*DBLE(MH2**INT(2&
  &.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB&
  &)*(2.D0*MW2 + 2.D0*DBLE(MH2**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA&
  &2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH2**INT(2.D0)))))/(PI2*SW2) - (0.0&
  &625D0*CS1S1S1S1f1233*DBLE(MH3**INT(2.D0)))/PI2 + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*&
  &CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW&
  &**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*DBLE&
  &((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (&
  &CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH1**INT(2.D0)))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625&
  &D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MA&
  &02 + MZ2 + 2.D0*(MA02 + DBLE(MH1**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2&
  &*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH2**INT(2.D0)))*DBLE&
  &((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.&
  &D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH2**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0))&
  &)/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR21*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.06&
  &25D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA&
  &1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/&
  &(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2&
  &*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S&
  &1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(&
  &4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*&
  &PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0&
  &*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D0*CS1S&
  &3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*&
  &CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DB&
  &LE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0&
  &*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1&
  &f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT(2.D0)&
  &))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.&
  &375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(M&
  &W*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0&
  &*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D&
  &0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/P&
  &I2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(C&
  &A1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1&
  &*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.1&
  &25D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f222&
  &*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW&
  &*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) -&
  & (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(&
  &MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA&
  &3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR12*RR22*(RR12*((-0.03&
  &125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/P&
  &I2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB&
  &**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D&
  &0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2 -&
  & (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1*D&
  &BLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW&
  &*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2&
  &*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.0&
  &3125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*S&
  &A2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*S&
  &A1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125&
  &D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322*D&
  &BLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*P&
  &I2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (&
  &0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*&
  &SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-&
  &1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA02)&
  &/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2&
  &*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375&
  &D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*&
  &SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.0&
  &3125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBLE(&
  &MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW&
  &*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.&
  &D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.&
  &D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.&
  &D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR13*RR23*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0&
  &.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW&
  &*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)&
  &))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*&
  &PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1&
  &S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**I&
  &NT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(&
  &MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2&
  &.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D0*C&
  &S1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.&
  &D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3&
  &*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.37&
  &5D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S&
  &1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT(2.&
  &D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - &
  &(0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))&
  &/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2&
  &.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(&
  &2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2&
  &)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW&
  &*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(&
  &CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (&
  &0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f&
  &222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/&
  &(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW&
  &) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DB&
  &LE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1&
  &*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)&
  &/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA&
  &3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(DBLE(MH1**INT&
  &(2.D0)) - 1.D0*DBLE(MH2**INT(2.D0))) &
                    & )*DLOG(1D0/EvalScale**2)
                end function dAlpha2MSBarUsual

                double precision function dAlpha3MSBarUsual()
                    use constants
                    implicit none
                    dAlpha3MSBarUsual = ( &
&(0.5D0*SA2*SA3*((0.0625D0*CS1S1S1f111*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f313&
  &)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f333)/PI&
  &2 + (0.125D0*CS1S3S3f111*CS1S3S3f311)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f321)/PI2 + (0&
  &.125D0*CS1S3S3f122*CS1S3S3f322)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f123)/PI2 + (0.0625&
  &D0*CS2S2S1f221*CS2S2S1f223)/PI2 - (0.0625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.125D0*CS1S1S3S3f1322*MHp2)/PI2 - (0.125D0*CS1S1S3S3f&
  &1311*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1113*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.&
  &D0*CA1*CA3*SA2 + SA1*SA3)))/(PI2*SW2) - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark3*(6.D0*MB2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW&
  &2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB&
  &2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark3*(6.D0*MD2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2&
  &*YukS1Lep1*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep3*(6.D0*ML2 &
  &- 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep3*(6.D0*MM2 - 1.D0*DBLE(MH1**INT(2.D0))))/(&
  &MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark3*(6.D0*MS2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*C&
  &A2*EL2*MT2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*E&
  &L2*MU2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f&
  &1311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*S&
  &A2 - 1.D0*CA1*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH1**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(&
  &-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH1**INT(2.D0&
  &)))))/(PI2*SW2) - (0.0625D0*CS1S1S1S1f1322*DBLE(MH2**INT(2.D0)))/PI2 - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark3*(6.D0*MB2 - &
  &1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MC2 - 1.D0*DBL&
  &E(MH3**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark3*(6.D0*MD2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW&
  &2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2&
  &*YukS1Lep1*YukS1Lep3*(6.D0*ML2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep3*(6.D0*MM2 &
  &- 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark3*(6.D0*MS2 - 1.D0*DBLE(MH3**INT(2.D0))&
  &))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*&
  &PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*&
  &SB2*SW2) - (0.0625D0*CS1S1S1S1f1333*DBLE(MH3**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA&
  &2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH3**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*&
  &CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MHp2 + MW2 + &
  &2.D0*(MHp2 + DBLE(MH3**INT(2.D0)))))/(PI2*SW2) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*&
  &CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(S&
  &W**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*DB&
  &LE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.&
  &D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH1**INT(2.D0)))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015&
  &625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D&
  &0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH1**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB +&
  & CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH3**INT(2.D0)))&
  &*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA&
  &1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH3**INT(2.D0))))*DBLE((CW2 + SW2)**INT(&
  &2.D0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR31*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 -&
  & (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2&
  &*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.&
  &D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(&
  &MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*&
  &CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM&
  &**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0))&
  &)/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL +&
  & (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D&
  &0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(&
  &-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Qua&
  &rk3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0&
  &.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*C&
  &S1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT&
  &(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW)&
  & - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0&
  &)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*&
  &((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**I&
  &NT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*&
  &MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0&
  &*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*E&
  &L*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1&
  &S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)&
  &))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2&
  &*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)&
  &*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(&
  &CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR12*RR32*(RR12*(&
  &(-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*&
  &MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*D&
  &BLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**I&
  &NT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))&
  &/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1&
  &Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(&
  &MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)&
  &))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0))&
  &)/(CW2*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 &
  &- (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3&
  &*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0&
  &*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - &
  &(0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1&
  &f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))&
  &/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*S&
  &W) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D&
  &0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0&
  &*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222&
  &*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*E&
  &L2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - &
  &(0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*P&
  &I2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233&
  &*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)&
  &))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**&
  &INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ&
  &2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**&
  &INT(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR13*RR33*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI&
  &2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*&
  &CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT&
  &(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0))&
  &)/(MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125&
  &D0*CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE&
  &(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D&
  &0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/E&
  &L + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.06&
  &25D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*M&
  &W*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1&
  &Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) -&
  & (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D&
  &0*CS1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**&
  &INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*&
  &SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4&
  &.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*M&
  &Z2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)&
  &**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f2&
  &11*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2&
  &.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D&
  &0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*S&
  &W) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS&
  &1S1S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.&
  &D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*&
  &PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*S&
  &A3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*M&
  &W*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CA1*CA2*CB*&
  &MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0&
  &*CA1*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(CA2*(D&
  &BLE(MH1**INT(2.D0)) - 1.D0*DBLE(MH3**INT(2.D0)))) - (0.5D0*CA3*SA2*((0.0625D0*CS1S1S1f111*CS1S1S1f211)/PI2 + (0.125D0*CS1S1S1f&
  &112*CS1S1S1f212)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f213)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f222)/PI2 + (0.125D0*CS1S1S1f123*C&
  &S1S1S1f223)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f233)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f211)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S&
  &3f212)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f221)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f222)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f112&
  &)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f122)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f222)/PI2 - (0.0625D0*CS2S2S1S1f2212*MA02)/PI2 - &
  &(0.125D0*CS1S1S3S3f1222*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1211*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1112*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA&
  &1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.09375D0*EL2*MB2*YukS1Quark1*Y&
  &ukS1Quark2*(6.D0*MB2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.&
  &D0*MC2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark2*(6.D0*MD2 - 1.D0*DBLE(MH1*&
  &*INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep1*YukS1Lep2*(6.D0*ME2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (&
  &0.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep2*(6.D0*ML2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*Yuk&
  &S1Lep2*(6.D0*MM2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark2*(6.D0*MS2 - 1.D0*DBL&
  &E(MH1**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MT2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH1**INT(2.D&
  &0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH1**INT(2.D0))))/(MW2&
  &*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*S&
  &A1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH1**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(&
  &CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MHp2 + MW2&
  & + 2.D0*(MHp2 + DBLE(MH1**INT(2.D0)))))/(PI2*SW2) - (0.09375D0*EL2*MB2*YukS1Quark1*YukS1Quark2*(6.D0*MB2 - 1.D0*DBLE(MH2**INT(&
  &2.D0))))/(MW2*PI2*SW2) - (0.09375D0*CA2*EL2*MC2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*&
  &PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark1*YukS1Quark2*(6.D0*MD2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*E&
  &L2*ME2*YukS1Lep1*YukS1Lep2*(6.D0*ME2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep1*YukS1Lep2*(6.D&
  &0*ML2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep1*YukS1Lep2*(6.D0*MM2 - 1.D0*DBLE(MH2**INT(2.D0&
  &))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark1*YukS1Quark2*(6.D0*MS2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.093&
  &75D0*CA2*EL2*MT2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*E&
  &L2*MU2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f1222*D&
  &BLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH2**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3&
  & - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH2**INT(2.D0)))))/(P&
  &I2*SW2) - (0.0625D0*CS1S1S1S1f1233*DBLE(MH3**INT(2.D0)))/PI2 + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*S&
  &W)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(&
  &4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*&
  &SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA&
  &1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH1**INT(2.D0)))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW&
  &2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)&
  &*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH1**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA&
  &1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH2**IN&
  &T(2.D0)))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH2**INT(2.D0))))*DBLE((CW2 + SW&
  &2)**INT(2.D0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR21*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp&
  &2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (&
  &2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC&
  &**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4&
  &.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.&
  &03125D0*CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1&
  &*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**IN&
  &T(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*&
  &SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - &
  &(0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0&
  &*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*&
  &YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*&
  &SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.0&
  &3125D0*CS1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(&
  &MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW&
  &*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**&
  &INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*&
  &EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 +&
  & SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S&
  &3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL&
  & + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0&
  &.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*&
  &PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125&
  &D0*CS1S1S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**I&
  &NT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))&
  &/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*&
  &SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2&
  &.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR12*RR22&
  &*(RR12*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S&
  &2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1&
  &Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DB&
  &LE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT&
  &(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*&
  &EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quar&
  &k1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**I&
  &NT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**IN&
  &T(2.D0)))/(CW2*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*M&
  &W2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-&
  &1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*E&
  &L*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI&
  &2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0&
  &*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT&
  &(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(&
  &MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*S&
  &A2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL&
  & + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2&
  &S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.0&
  &9375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2&
  &*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)&
  &))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*&
  &PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1&
  &S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**I&
  &NT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*D&
  &BLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D&
  &0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 &
  &+ SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR13*RR23*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*&
  &MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL &
  &+ (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE&
  &(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - &
  &(0.03125D0*CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1L&
  &ep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT*&
  &*INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*&
  &MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2&
  & - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2&
  &.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*&
  &EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*&
  &SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (&
  &0.03125D0*CS1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DB&
  &LE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/&
  &(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(M&
  &T**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875&
  &D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW&
  &2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*C&
  &S1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)&
  &/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) -&
  & (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(&
  &MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03&
  &125D0*CS1S1S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML&
  &**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0&
  &)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL +&
  & (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CA1&
  &*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1&
  &.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/&
  &(CA2*(DBLE(MH1**INT(2.D0)) - 1.D0*DBLE(MH2**INT(2.D0)))) + (0.5D0*((0.0625D0*CS1S1S1f211*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f2&
  &12*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f213*CS1S1S1f313)/PI2 + (0.0625D0*CS1S1S1f222*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f223*CS&
  &1S1S1f323)/PI2 + (0.0625D0*CS1S1S1f233*CS1S1S1f333)/PI2 + (0.125D0*CS1S3S3f211*CS1S3S3f311)/PI2 + (0.125D0*CS1S3S3f221*CS1S3S3&
  &f312)/PI2 + (0.125D0*CS1S3S3f212*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f222*CS1S3S3f322)/PI2 + (0.0625D0*CS2S2S1f112*CS2S2S1f113)&
  &/PI2 + (0.125D0*CS2S2S1f122*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f222*CS2S2S1f223)/PI2 - (0.0625D0*CS2S2S1S1f2223*MA02)/PI2 - (&
  &0.125D0*CS1S1S3S3f2322*MHp2)/PI2 - (0.125D0*CS1S1S3S3f2311*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1123*MZ2)/PI2 + (0.25D0*EL2*MW2*((-1&
  &.D0*CA1*CA3*SA2 + SA1*SA3)*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3&
  &)))/(PI2*SW2) - (0.0625D0*CS1S1S1S1f2311*DBLE(MH1**INT(2.D0)))/PI2 - (0.09375D0*EL2*MB2*YukS1Quark2*YukS1Quark3*(6.D0*MB2 - 1.&
  &D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MC2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(&
  &6.D0*MC2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark2*YukS1Quark3*(6.D0*MD2 - 1.D0*DBLE(MH&
  &2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep2*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) -&
  & (0.03125D0*EL2*ML2*YukS1Lep2*YukS1Lep3*(6.D0*ML2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*YukS1Lep2*Y&
  &ukS1Lep3*(6.D0*MM2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark2*YukS1Quark3*(6.D0*MS2 - 1.D0*D&
  &BLE(MH2**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MT2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(6.D0&
  &*MT2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MU2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH2**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f2322*DBLE(MH2**INT(2.D0)))/PI2 - (&
  &0.03125D0*EL2*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3&
  &) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH2**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CB*(-1.D0*CA3*SA1*SA2&
  & - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA&
  &2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH2**INT(2.D0)))))/(PI2*SW2) - (0.09375D0*EL2*MB2*YukS1Quark2*YukS1Quark3*(6.&
  &D0*MB2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MC2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3)*(6.D0*MC2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MD2*YukS1Quark2*YukS1Quark3*(6.D0*MD2 - 1&
  &.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*YukS1Lep2*YukS1Lep3*(6.D0*ME2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2&
  &*PI2*SW2) - (0.03125D0*EL2*ML2*YukS1Lep2*YukS1Lep3*(6.D0*ML2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*&
  &YukS1Lep2*YukS1Lep3*(6.D0*MM2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*YukS1Quark2*YukS1Quark3*(6.D0*M&
  &S2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SW2) - (0.09375D0*EL2*MT2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2&
  &*SA3)*(6.D0*MT2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MU2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*C&
  &A3 - 1.D0*SA1*SA2*SA3)*(6.D0*MU2 - 1.D0*DBLE(MH3**INT(2.D0))))/(MW2*PI2*SB2*SW2) - (0.0625D0*CS1S1S1S1f2333*DBLE(MH3**INT(2.D0&
  &)))/PI2 - (0.03125D0*EL2*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*&
  &CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MW2 + 2.D0*DBLE(MH3**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(CB*(-1.D0*&
  &CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - &
  &1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MHp2 + MW2 + 2.D0*(MHp2 + DBLE(MH3**INT(2.D0)))))/(PI2*SW2) + (0.109375D0*((2.D0*CB*MW*(-1.D0*CA1&
  &*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2&
  &*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*M&
  &Z2*((-1.D0*CA1*CA3*SA2 + SA1*SA3)*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*&
  &SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2&
  & - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ2 + 2.D0*DBLE(MH2**INT(&
  &2.D0)))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1&
  &*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*(-1.D0*MA02 + MZ2 + 2.D&
  &0*(MA02 + DBLE(MH2**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA&
  &3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(2.D0*MZ&
  &2 + 2.D0*DBLE(MH3**INT(2.D0)))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1&
  &*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*&
  &(-1.D0*MA02 + MZ2 + 2.D0*(MA02 + DBLE(MH3**INT(2.D0))))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR21*RR3&
  &1*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2&
  &S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS&
  &1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*D&
  &BLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**IN&
  &T(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0&
  &*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Qua&
  &rk1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**&
  &INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**I&
  &NT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*&
  &MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(&
  &-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*&
  &EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*P&
  &I2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D&
  &0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/&
  &(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*&
  &SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/E&
  &L + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS&
  &2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.&
  &09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI&
  &2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0&
  &)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW&
  &*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS&
  &1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**&
  &INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*&
  &DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875&
  &D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2&
  & + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR22*RR32*(RR12*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS&
  &1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*&
  &MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*E&
  &L*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*D&
  &BLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0&
  &)))/PI2 - (0.03125D0*CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0&
  &*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA&
  &1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*C&
  &A1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f223&
  &*MA02)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*E&
  &L2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) -&
  & (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0))&
  &)/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*P&
  &I2*SW) - (0.03125D0*CS1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S&
  &1S1f333*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*S&
  &A3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) +&
  & (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL&
  &)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (&
  &0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA&
  &2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW&
  &*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT&
  &(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/P&
  &I2 - (0.03125D0*CS1S1S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Le&
  &p2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS&
  &**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA&
  &3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3&
  &)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR23*&
  &RR33*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*&
  &CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*Y&
  &ukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark&
  &1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f111*DBLE(MH1*&
  &*INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f122*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f133*DBLE(MH3**INT(2.D0)))/PI2 - (0.12&
  &5D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1&
  &Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(M&
  &U**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)&
  &**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f3&
  &11*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*M&
  &W*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375&
  &D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(M&
  &W*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.03125D0*CS1S1S1f311*DBLE(MH1**INT(2.D0)))/PI2 - (0.031&
  &25D0*CS1S1S1f322*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f333*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep3*DBLE(ML*&
  &*INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)&
  &))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*S&
  &A1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW&
  &)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0&
  &*CS2S2S1f222*MA02)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + &
  &(0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/&
  &(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4&
  &.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/&
  &(MW*PI2*SW) - (0.03125D0*CS1S1S1f211*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CS1S1S1f222*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0&
  &*CS1S1S1f233*DBLE(MH3**INT(2.D0)))/PI2 - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(M&
  &M**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA&
  &3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046&
  &875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((&
  &CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D&
  &0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(DBLE(M&
  &H2**INT(2.D0)) - 1.D0*DBLE(MH3**INT(2.D0))) &
                    & )*DLOG(1D0/EvalScale**2)
                end function dAlpha3MSBarUsual

                double precision function dBetaMSBarUsual()
                    use constants
                    implicit none
                    dBetaMSBarUsual = ( &
&(0.5D0*((-0.125D0*CS1S3S3f111*CS1S3S3f112)/PI2 - (0.125D0*CS1S3S3f112*CS1S3S3f122)/PI2 - (0.125D0*CS1S3S3f211*CS1S3S3f212)/PI2 - &
  &(0.125D0*CS1S3S3f212*CS1S3S3f222)/PI2 - (0.125D0*CS1S3S3f311*CS1S3S3f312)/PI2 - (0.125D0*CS1S3S3f312*CS1S3S3f322)/PI2 + (0.062&
  &5D0*CS2S2S3S3f2212*MA02)/PI2 + (0.125D0*CS3S3S3S3f1222*MHp2)/PI2 + (0.125D0*CS3S3S3S3f1121*MW2)/PI2 + (0.0625D0*CS2S2S3S3f1112&
  &*MZ2)/PI2 + (0.1875D0*CA1*CA2*CB2*EL2*MZ2*RR12)/PI2 - (0.1875D0*CA3*CB2*EL2*MZ2*RR22*SA1)/PI2 - (0.1875D0*CA1*CA3*CB2*EL2*MZ2*&
  &RR32*SA2)/PI2 + (0.1875D0*CB2*EL2*MZ2*RR32*SA1*SA3)/PI2 - (0.1875D0*CA1*CB2*EL2*MZ2*RR22*SA2*SA3)/PI2 - (0.1875D0*CA1*CA2*CB*E&
  &L2*MZ2*RR11*SB)/PI2 + (0.1875D0*CA1*CA3*CB*EL2*MZ2*RR22*SB)/PI2 + (0.1875D0*CA2*CB*EL2*MZ2*RR12*SA1*SB)/PI2 + (0.1875D0*CA3*CB&
  &*EL2*MZ2*RR21*SA1*SB)/PI2 + (0.1875D0*CA1*CA3*CB*EL2*MZ2*RR31*SA2*SB)/PI2 - (0.1875D0*CA3*CB*EL2*MZ2*RR32*SA1*SA2*SB)/PI2 - (0&
  &.1875D0*CA1*CB*EL2*MZ2*RR32*SA3*SB)/PI2 - (0.1875D0*CB*EL2*MZ2*RR31*SA1*SA3*SB)/PI2 + (0.1875D0*CA1*CB*EL2*MZ2*RR21*SA2*SA3*SB&
  &)/PI2 - (0.1875D0*CB*EL2*MZ2*RR22*SA1*SA2*SA3*SB)/PI2 - (0.1875D0*CA1*CA3*EL2*MZ2*RR21*SB2)/PI2 - (0.1875D0*CA2*EL2*MZ2*RR11*S&
  &A1*SB2)/PI2 + (0.1875D0*CA3*EL2*MZ2*RR31*SA1*SA2*SB2)/PI2 + (0.1875D0*CA1*EL2*MZ2*RR31*SA3*SB2)/PI2 + (0.1875D0*EL2*MZ2*RR21*S&
  &A1*SA2*SA3*SB2)/PI2 - (0.03125D0*CB*CS2S2S1f221*EL*MA02*RR12)/(MW*PI2*SW) - (0.0625D0*CB*CS1S3S3f122*EL*MHp2*RR12)/(MW*PI2*SW)&
  & - (0.0625D0*CB*CS1S3S3f111*EL*MW*RR12)/(PI2*SW) - (0.03125D0*CB*CS2S2S1f111*EL*MZ2*RR12)/(MW*PI2*SW) - (0.03125D0*CB*CS2S2S1f&
  &222*EL*MA02*RR22)/(MW*PI2*SW) - (0.0625D0*CB*CS1S3S3f222*EL*MHp2*RR22)/(MW*PI2*SW) - (0.0625D0*CB*CS1S3S3f211*EL*MW*RR22)/(PI2&
  &*SW) - (0.03125D0*CB*CS2S2S1f112*EL*MZ2*RR22)/(MW*PI2*SW) - (0.03125D0*CB*CS2S2S1f223*EL*MA02*RR32)/(MW*PI2*SW) - (0.0625D0*CB&
  &*CS1S3S3f322*EL*MHp2*RR32)/(MW*PI2*SW) - (0.0625D0*CB*CS1S3S3f311*EL*MW*RR32)/(PI2*SW) - (0.03125D0*CB*CS2S2S1f113*EL*MZ2*RR32&
  &)/(MW*PI2*SW) + (0.03125D0*CS2S2S1f221*EL*MA02*RR11*SB)/(MW*PI2*SW) + (0.0625D0*CS1S3S3f122*EL*MHp2*RR11*SB)/(MW*PI2*SW) + (0.&
  &0625D0*CS1S3S3f111*EL*MW*RR11*SB)/(PI2*SW) + (0.03125D0*CS2S2S1f111*EL*MZ2*RR11*SB)/(MW*PI2*SW) + (0.03125D0*CS2S2S1f222*EL*MA&
  &02*RR21*SB)/(MW*PI2*SW) + (0.0625D0*CS1S3S3f222*EL*MHp2*RR21*SB)/(MW*PI2*SW) + (0.0625D0*CS1S3S3f211*EL*MW*RR21*SB)/(PI2*SW) +&
  & (0.03125D0*CS2S2S1f112*EL*MZ2*RR21*SB)/(MW*PI2*SW) + (0.03125D0*CS2S2S1f223*EL*MA02*RR31*SB)/(MW*PI2*SW) + (0.0625D0*CS1S3S3f&
  &322*EL*MHp2*RR31*SB)/(MW*PI2*SW) + (0.0625D0*CS1S3S3f311*EL*MW*RR31*SB)/(PI2*SW) + (0.03125D0*CS2S2S1f113*EL*MZ2*RR31*SB)/(MW*&
  &PI2*SW) + (0.1875D0*CA1*CA2*CB2*EL2*MW2*RR12)/(PI2*SW2) + (0.09375D0*CA1*CA2*CB2*CW2*EL2*MZ2*RR12)/(PI2*SW2) - (0.1875D0*CA3*C&
  &B2*EL2*MW2*RR22*SA1)/(PI2*SW2) - (0.09375D0*CA3*CB2*CW2*EL2*MZ2*RR22*SA1)/(PI2*SW2) - (0.1875D0*CA1*CA3*CB2*EL2*MW2*RR32*SA2)/&
  &(PI2*SW2) - (0.09375D0*CA1*CA3*CB2*CW2*EL2*MZ2*RR32*SA2)/(PI2*SW2) + (0.1875D0*CB2*EL2*MW2*RR32*SA1*SA3)/(PI2*SW2) + (0.09375D&
  &0*CB2*CW2*EL2*MZ2*RR32*SA1*SA3)/(PI2*SW2) - (0.1875D0*CA1*CB2*EL2*MW2*RR22*SA2*SA3)/(PI2*SW2) - (0.09375D0*CA1*CB2*CW2*EL2*MZ2&
  &*RR22*SA2*SA3)/(PI2*SW2) + (0.375D0*CB*CKM23*CKMC23*EL2*MB2*MC2)/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM21*CKMC21*EL2*MC2*MD2)/(MW2&
  &*PI2*SB*SW2) - (0.09375D0*CB*CKM21*CKMC21*EL2*MC2*MHp2)/(MW2*PI2*SB*SW2) - (0.09375D0*CB*CKM22*CKMC22*EL2*MC2*MHp2)/(MW2*PI2*S&
  &B*SW2) - (0.09375D0*CB*CKM23*CKMC23*EL2*MC2*MHp2)/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM22*CKMC22*EL2*MC2*MS2)/(MW2*PI2*SB*SW2) + &
  &(0.375D0*CB*CKM33*CKMC33*EL2*MB2*MT2)/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM31*CKMC31*EL2*MD2*MT2)/(MW2*PI2*SB*SW2) - (0.09375D0*C&
  &B*CKM31*CKMC31*EL2*MHp2*MT2)/(MW2*PI2*SB*SW2) - (0.09375D0*CB*CKM32*CKMC32*EL2*MHp2*MT2)/(MW2*PI2*SB*SW2) - (0.09375D0*CB*CKM3&
  &3*CKMC33*EL2*MHp2*MT2)/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM32*CKMC32*EL2*MS2*MT2)/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM13*CKMC13*EL&
  &2*MB2*MU2)/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM11*CKMC11*EL2*MD2*MU2)/(MW2*PI2*SB*SW2) - (0.09375D0*CB*CKM11*CKMC11*EL2*MHp2*MU2&
  &)/(MW2*PI2*SB*SW2) - (0.09375D0*CB*CKM12*CKMC12*EL2*MHp2*MU2)/(MW2*PI2*SB*SW2) - (0.09375D0*CB*CKM13*CKMC13*EL2*MHp2*MU2)/(MW2&
  &*PI2*SB*SW2) + (0.375D0*CB*CKM12*CKMC12*EL2*MS2*MU2)/(MW2*PI2*SB*SW2) - (0.1875D0*CA1*CA2*CB*EL2*MW2*RR11*SB)/(PI2*SW2) - (0.0&
  &9375D0*CA1*CA2*CB*CW2*EL2*MZ2*RR11*SB)/(PI2*SW2) + (0.1875D0*CA1*CA3*CB*EL2*MW2*RR22*SB)/(PI2*SW2) + (0.09375D0*CA1*CA3*CB*CW2&
  &*EL2*MZ2*RR22*SB)/(PI2*SW2) + (0.1875D0*CA2*CB*EL2*MW2*RR12*SA1*SB)/(PI2*SW2) + (0.09375D0*CA2*CB*CW2*EL2*MZ2*RR12*SA1*SB)/(PI&
  &2*SW2) + (0.1875D0*CA3*CB*EL2*MW2*RR21*SA1*SB)/(PI2*SW2) + (0.09375D0*CA3*CB*CW2*EL2*MZ2*RR21*SA1*SB)/(PI2*SW2) + (0.1875D0*CA&
  &1*CA3*CB*EL2*MW2*RR31*SA2*SB)/(PI2*SW2) + (0.09375D0*CA1*CA3*CB*CW2*EL2*MZ2*RR31*SA2*SB)/(PI2*SW2) - (0.1875D0*CA3*CB*EL2*MW2*&
  &RR32*SA1*SA2*SB)/(PI2*SW2) - (0.09375D0*CA3*CB*CW2*EL2*MZ2*RR32*SA1*SA2*SB)/(PI2*SW2) - (0.1875D0*CA1*CB*EL2*MW2*RR32*SA3*SB)/&
  &(PI2*SW2) - (0.09375D0*CA1*CB*CW2*EL2*MZ2*RR32*SA3*SB)/(PI2*SW2) - (0.1875D0*CB*EL2*MW2*RR31*SA1*SA3*SB)/(PI2*SW2) - (0.09375D&
  &0*CB*CW2*EL2*MZ2*RR31*SA1*SA3*SB)/(PI2*SW2) + (0.1875D0*CA1*CB*EL2*MW2*RR21*SA2*SA3*SB)/(PI2*SW2) + (0.09375D0*CA1*CB*CW2*EL2*&
  &MZ2*RR21*SA2*SA3*SB)/(PI2*SW2) - (0.1875D0*CB*EL2*MW2*RR22*SA1*SA2*SA3*SB)/(PI2*SW2) - (0.09375D0*CB*CW2*EL2*MZ2*RR22*SA1*SA2*&
  &SA3*SB)/(PI2*SW2) - (0.1875D0*CA1*CA3*EL2*MW2*RR21*SB2)/(PI2*SW2) - (0.09375D0*CA1*CA3*CW2*EL2*MZ2*RR21*SB2)/(PI2*SW2) - (0.18&
  &75D0*CA2*EL2*MW2*RR11*SA1*SB2)/(PI2*SW2) - (0.09375D0*CA2*CW2*EL2*MZ2*RR11*SA1*SB2)/(PI2*SW2) + (0.1875D0*CA3*EL2*MW2*RR31*SA1&
  &*SA2*SB2)/(PI2*SW2) + (0.09375D0*CA3*CW2*EL2*MZ2*RR31*SA1*SA2*SB2)/(PI2*SW2) + (0.1875D0*CA1*EL2*MW2*RR31*SA3*SB2)/(PI2*SW2) +&
  & (0.09375D0*CA1*CW2*EL2*MZ2*RR31*SA3*SB2)/(PI2*SW2) + (0.1875D0*EL2*MW2*RR21*SA1*SA2*SA3*SB2)/(PI2*SW2) + (0.09375D0*CW2*EL2*M&
  &Z2*RR21*SA1*SA2*SA3*SB2)/(PI2*SW2) + (0.09375D0*CA1*CA2*CB2*EL2*MZ2*RR12*SW2)/(CW2*PI2) - (0.09375D0*CA3*CB2*EL2*MZ2*RR22*SA1*&
  &SW2)/(CW2*PI2) - (0.09375D0*CA1*CA3*CB2*EL2*MZ2*RR32*SA2*SW2)/(CW2*PI2) + (0.09375D0*CB2*EL2*MZ2*RR32*SA1*SA3*SW2)/(CW2*PI2) -&
  & (0.09375D0*CA1*CB2*EL2*MZ2*RR22*SA2*SA3*SW2)/(CW2*PI2) - (0.09375D0*CA1*CA2*CB*EL2*MZ2*RR11*SB*SW2)/(CW2*PI2) + (0.09375D0*CA&
  &1*CA3*CB*EL2*MZ2*RR22*SB*SW2)/(CW2*PI2) + (0.09375D0*CA2*CB*EL2*MZ2*RR12*SA1*SB*SW2)/(CW2*PI2) + (0.09375D0*CA3*CB*EL2*MZ2*RR2&
  &1*SA1*SB*SW2)/(CW2*PI2) + (0.09375D0*CA1*CA3*CB*EL2*MZ2*RR31*SA2*SB*SW2)/(CW2*PI2) - (0.09375D0*CA3*CB*EL2*MZ2*RR32*SA1*SA2*SB&
  &*SW2)/(CW2*PI2) - (0.09375D0*CA1*CB*EL2*MZ2*RR32*SA3*SB*SW2)/(CW2*PI2) - (0.09375D0*CB*EL2*MZ2*RR31*SA1*SA3*SB*SW2)/(CW2*PI2) &
  &+ (0.09375D0*CA1*CB*EL2*MZ2*RR21*SA2*SA3*SB*SW2)/(CW2*PI2) - (0.09375D0*CB*EL2*MZ2*RR22*SA1*SA2*SA3*SB*SW2)/(CW2*PI2) - (0.093&
  &75D0*CA1*CA3*EL2*MZ2*RR21*SB2*SW2)/(CW2*PI2) - (0.09375D0*CA2*EL2*MZ2*RR11*SA1*SB2*SW2)/(CW2*PI2) + (0.09375D0*CA3*EL2*MZ2*RR3&
  &1*SA1*SA2*SB2*SW2)/(CW2*PI2) + (0.09375D0*CA1*EL2*MZ2*RR31*SA3*SB2*SW2)/(CW2*PI2) + (0.09375D0*EL2*MZ2*RR21*SA1*SA2*SA3*SB2*SW&
  &2)/(CW2*PI2) - (0.03125D0*EL2*ME2*MHp2*YukS3Lep1*YukS3Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MHp2*ML2*YukS3Lep1*YukS3Lep2)/(MW2*&
  &PI2*SW2) - (0.03125D0*EL2*MHp2*MM2*YukS3Lep1*YukS3Lep2)/(MW2*PI2*SW2) - (0.375D0*CB*CKM23*CKMC23*EL2*MB2*MC2*YukS3Quark1)/(MW2&
  &*PI2*SB*SW2) - (0.375D0*CB*CKM21*CKMC21*EL2*MC2*MD2*YukS3Quark1)/(MW2*PI2*SB*SW2) - (0.375D0*CB*CKM22*CKMC22*EL2*MC2*MS2*YukS3&
  &Quark1)/(MW2*PI2*SB*SW2) - (0.375D0*CB*CKM33*CKMC33*EL2*MB2*MT2*YukS3Quark1)/(MW2*PI2*SB*SW2) - (0.375D0*CB*CKM31*CKMC31*EL2*M&
  &D2*MT2*YukS3Quark1)/(MW2*PI2*SB*SW2) - (0.375D0*CB*CKM32*CKMC32*EL2*MS2*MT2*YukS3Quark1)/(MW2*PI2*SB*SW2) - (0.375D0*CB*CKM13*&
  &CKMC13*EL2*MB2*MU2*YukS3Quark1)/(MW2*PI2*SB*SW2) - (0.375D0*CB*CKM11*CKMC11*EL2*MD2*MU2*YukS3Quark1)/(MW2*PI2*SB*SW2) - (0.375&
  &D0*CB*CKM12*CKMC12*EL2*MS2*MU2*YukS3Quark1)/(MW2*PI2*SB*SW2) - (0.375D0*CKM23*CKMC23*EL2*MB2*MC2*YukS3Quark2)/(MW2*PI2*SW2) - &
  &(0.375D0*CKM21*CKMC21*EL2*MC2*MD2*YukS3Quark2)/(MW2*PI2*SW2) - (0.375D0*CKM22*CKMC22*EL2*MC2*MS2*YukS3Quark2)/(MW2*PI2*SW2) - &
  &(0.375D0*CKM33*CKMC33*EL2*MB2*MT2*YukS3Quark2)/(MW2*PI2*SW2) - (0.375D0*CKM31*CKMC31*EL2*MD2*MT2*YukS3Quark2)/(MW2*PI2*SW2) - &
  &(0.375D0*CKM32*CKMC32*EL2*MS2*MT2*YukS3Quark2)/(MW2*PI2*SW2) - (0.375D0*CKM13*CKMC13*EL2*MB2*MU2*YukS3Quark2)/(MW2*PI2*SW2) - &
  &(0.375D0*CKM11*CKMC11*EL2*MD2*MU2*YukS3Quark2)/(MW2*PI2*SW2) - (0.375D0*CKM12*CKMC12*EL2*MS2*MU2*YukS3Quark2)/(MW2*PI2*SW2) + &
  &(0.375D0*CKM23*CKMC23*EL2*MB2*MC2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) + (0.375D0*CKM21*CKMC21*EL2*MC2*MD2*YukS3Quark1*YukS3&
  &Quark2)/(MW2*PI2*SW2) - (0.09375D0*CKM13*CKMC13*EL2*MB2*MHp2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) - (0.09375D0*CKM23*CKMC23*&
  &EL2*MB2*MHp2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) - (0.09375D0*CKM33*CKMC33*EL2*MB2*MHp2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*S&
  &W2) - (0.09375D0*CKM11*CKMC11*EL2*MD2*MHp2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) - (0.09375D0*CKM21*CKMC21*EL2*MD2*MHp2*YukS3&
  &Quark1*YukS3Quark2)/(MW2*PI2*SW2) - (0.09375D0*CKM31*CKMC31*EL2*MD2*MHp2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) + (0.375D0*CKM&
  &22*CKMC22*EL2*MC2*MS2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) - (0.09375D0*CKM12*CKMC12*EL2*MHp2*MS2*YukS3Quark1*YukS3Quark2)/(&
  &MW2*PI2*SW2) - (0.09375D0*CKM22*CKMC22*EL2*MHp2*MS2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) - (0.09375D0*CKM32*CKMC32*EL2*MHp2*&
  &MS2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) + (0.375D0*CKM33*CKMC33*EL2*MB2*MT2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) + (0.375&
  &D0*CKM31*CKMC31*EL2*MD2*MT2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) + (0.375D0*CKM32*CKMC32*EL2*MS2*MT2*YukS3Quark1*YukS3Quark2&
  &)/(MW2*PI2*SW2) + (0.375D0*CKM13*CKMC13*EL2*MB2*MU2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) + (0.375D0*CKM11*CKMC11*EL2*MD2*MU2&
  &*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) + (0.375D0*CKM12*CKMC12*EL2*MS2*MU2*YukS3Quark1*YukS3Quark2)/(MW2*PI2*SW2) + (0.03125D&
  &0*CA1*CB2*EL2*MHp2*SA1*DBLE(CA2**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*CB2*EL2*MW2*SA1*DBLE(CA2**INT(2.D0)))/(PI2*SW2) - (0.0&
  &3125D0*CA1*EL2*MHp2*SA1*SB2*DBLE(CA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*MW2*SA1*SB2*DBLE(CA2**INT(2.D0)))/(PI2*SW2) -&
  & (0.03125D0*CB*EL2*MHp2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*MW2*SB*DBLE(CA1**INT(2.D0)&
  &)*DBLE(CA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*CB2*EL2*MHp2*SA1*DBLE(CA3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*CB2*EL2*M&
  &W2*SA1*DBLE(CA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*EL2*MHp2*SA1*SB2*DBLE(CA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*EL2*&
  &MW2*SA1*SB2*DBLE(CA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*MHp2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA3**INT(2.D0)))/(PI2*SW2) +&
  & (0.03125D0*CB*EL2*MW2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA3**INT(2.D0)))/(PI2*SW2) - (0.375D0*CB*EL2*RR12*YukS1Quark1*DBLE(MB**INT&
  &(4.D0)))/(MW2*PI2*SW2) + (0.375D0*EL2*RR11*SB*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW2*PI2*SW2) - (0.375D0*CB*EL2*RR22*YukS1Quark&
  &2*DBLE(MB**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*EL2*RR21*SB*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW2*PI2*SW2) - (0.375D0*CB*EL2*R&
  &R32*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*EL2*RR31*SB*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW2*PI2*SW2) + (0.&
  &375D0*CKM13*CKMC13*EL2*YukS3Quark1*YukS3Quark2*DBLE(MB**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CKM23*CKMC23*EL2*YukS3Quark1*YukS&
  &3Quark2*DBLE(MB**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CKM33*CKMC33*EL2*YukS3Quark1*YukS3Quark2*DBLE(MB**INT(4.D0)))/(MW2*PI2*S&
  &W2) + (0.375D0*CA1*CA3*EL2*RR21*DBLE(MC**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CA2*EL2*RR11*SA1*DBLE(MC**INT(4.D0)))/(MW2*PI2*S&
  &W2) - (0.375D0*CA3*EL2*RR31*SA1*SA2*DBLE(MC**INT(4.D0)))/(MW2*PI2*SW2) - (0.375D0*CA1*EL2*RR31*SA3*DBLE(MC**INT(4.D0)))/(MW2*P&
  &I2*SW2) - (0.375D0*EL2*RR21*SA1*SA2*SA3*DBLE(MC**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CB*CKM21*CKMC21*EL2*DBLE(MC**INT(4.D0)))&
  &/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM22*CKMC22*EL2*DBLE(MC**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM23*CKMC23*EL2*DBLE(MC*&
  &*INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.375D0*CA1*CA3*CB*EL2*RR22*DBLE(MC**INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.375D0*CA2*CB*EL2*RR12*&
  &SA1*DBLE(MC**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CA3*CB*EL2*RR32*SA1*SA2*DBLE(MC**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*&
  &CA1*CB*EL2*RR32*SA3*DBLE(MC**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CB*EL2*RR22*SA1*SA2*SA3*DBLE(MC**INT(4.D0)))/(MW2*PI2*SB*&
  &SW2) - (0.375D0*CB*EL2*RR12*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*EL2*RR11*SB*YukS1Quark1*DBLE(MD**INT(4.D&
  &0)))/(MW2*PI2*SW2) - (0.375D0*CB*EL2*RR22*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*EL2*RR21*SB*YukS1Quark2*DB&
  &LE(MD**INT(4.D0)))/(MW2*PI2*SW2) - (0.375D0*CB*EL2*RR32*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*EL2*RR31*SB*&
  &YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CKM11*CKMC11*EL2*YukS3Quark1*YukS3Quark2*DBLE(MD**INT(4.D0)))/(MW2*P&
  &I2*SW2) + (0.375D0*CKM21*CKMC21*EL2*YukS3Quark1*YukS3Quark2*DBLE(MD**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CKM31*CKMC31*EL2*Yuk&
  &S3Quark1*YukS3Quark2*DBLE(MD**INT(4.D0)))/(MW2*PI2*SW2) - (0.125D0*CB*EL2*RR12*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW2*PI2*SW2) + &
  &(0.125D0*EL2*RR11*SB*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW2*PI2*SW2) - (0.125D0*CB*EL2*RR22*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW2*P&
  &I2*SW2) + (0.125D0*EL2*RR21*SB*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW2*PI2*SW2) - (0.125D0*CB*EL2*RR32*YukS1Lep3*DBLE(ME**INT(4.D0&
  &)))/(MW2*PI2*SW2) + (0.125D0*EL2*RR31*SB*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW2*PI2*SW2) + (0.125D0*EL2*YukS3Lep1*YukS3Lep2*DBLE(&
  &ME**INT(4.D0)))/(MW2*PI2*SW2) + (0.0625D0*CS1S1S3S3f1112*DBLE(MH1**INT(2.D0)))/PI2 - (0.03125D0*CB*CS1S1S1f111*EL*RR12*DBLE(MH&
  &1**INT(2.D0)))/(MW*PI2*SW) - (0.03125D0*CB*CS1S1S1f211*EL*RR22*DBLE(MH1**INT(2.D0)))/(MW*PI2*SW) - (0.03125D0*CB*CS1S1S1f311*E&
  &L*RR32*DBLE(MH1**INT(2.D0)))/(MW*PI2*SW) + (0.03125D0*CS1S1S1f111*EL*RR11*SB*DBLE(MH1**INT(2.D0)))/(MW*PI2*SW) + (0.03125D0*CS&
  &1S1S1f211*EL*RR21*SB*DBLE(MH1**INT(2.D0)))/(MW*PI2*SW) + (0.03125D0*CS1S1S1f311*EL*RR31*SB*DBLE(MH1**INT(2.D0)))/(MW*PI2*SW) +&
  & (0.03125D0*CA1*CB2*EL2*SA1*DBLE(CA2**INT(2.D0))*DBLE(MH1**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*SA1*SB2*DBLE(CA2**INT(2.&
  &D0))*DBLE(MH1**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA2**INT(2.D0))*DBLE(MH1**INT(2.D0)))/(P&
  &I2*SW2) + (0.0625D0*CS1S1S3S3f2212*DBLE(MH2**INT(2.D0)))/PI2 - (0.03125D0*CB*CS1S1S1f122*EL*RR12*DBLE(MH2**INT(2.D0)))/(MW*PI2&
  &*SW) - (0.03125D0*CB*CS1S1S1f222*EL*RR22*DBLE(MH2**INT(2.D0)))/(MW*PI2*SW) - (0.03125D0*CB*CS1S1S1f322*EL*RR32*DBLE(MH2**INT(2&
  &.D0)))/(MW*PI2*SW) + (0.03125D0*CS1S1S1f122*EL*RR11*SB*DBLE(MH2**INT(2.D0)))/(MW*PI2*SW) + (0.03125D0*CS1S1S1f222*EL*RR21*SB*D&
  &BLE(MH2**INT(2.D0)))/(MW*PI2*SW) + (0.03125D0*CS1S1S1f322*EL*RR31*SB*DBLE(MH2**INT(2.D0)))/(MW*PI2*SW) - (0.125D0*CA1*CA3*CB*E&
  &L2*SA1*SA2*SA3*SB*DBLE(MH2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA3*CB2*EL2*SA2*SA3*DBLE(CA1**INT(2.D0))*DBLE(MH2**INT(2.D0)))/&
  &(PI2*SW2) + (0.03125D0*CA3*EL2*SA2*SA3*SB2*DBLE(CA1**INT(2.D0))*DBLE(MH2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*CB2*EL2*SA1*D&
  &BLE(CA3**INT(2.D0))*DBLE(MH2**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*EL2*SA1*SB2*DBLE(CA3**INT(2.D0))*DBLE(MH2**INT(2.D0)))/(P&
  &I2*SW2) + (0.03125D0*CB*EL2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA3**INT(2.D0))*DBLE(MH2**INT(2.D0)))/(PI2*SW2) + (0.0625D0*CS1S1S3S3&
  &f3312*DBLE(MH3**INT(2.D0)))/PI2 - (0.03125D0*CB*CS1S1S1f133*EL*RR12*DBLE(MH3**INT(2.D0)))/(MW*PI2*SW) - (0.03125D0*CB*CS1S1S1f&
  &233*EL*RR22*DBLE(MH3**INT(2.D0)))/(MW*PI2*SW) - (0.03125D0*CB*CS1S1S1f333*EL*RR32*DBLE(MH3**INT(2.D0)))/(MW*PI2*SW) + (0.03125&
  &D0*CS1S1S1f133*EL*RR11*SB*DBLE(MH3**INT(2.D0)))/(MW*PI2*SW) + (0.03125D0*CS1S1S1f233*EL*RR21*SB*DBLE(MH3**INT(2.D0)))/(MW*PI2*&
  &SW) + (0.03125D0*CS1S1S1f333*EL*RR31*SB*DBLE(MH3**INT(2.D0)))/(MW*PI2*SW) + (0.125D0*CA1*CA3*CB*EL2*SA1*SA2*SA3*SB*DBLE(MH3**I&
  &NT(2.D0)))/(PI2*SW2) + (0.03125D0*CA3*CB2*EL2*SA2*SA3*DBLE(CA1**INT(2.D0))*DBLE(MH3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA3*EL&
  &2*SA2*SA3*SB2*DBLE(CA1**INT(2.D0))*DBLE(MH3**INT(2.D0)))/(PI2*SW2) - (0.125D0*CB*EL2*RR12*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW2*&
  &PI2*SW2) + (0.125D0*EL2*RR11*SB*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW2*PI2*SW2) - (0.125D0*CB*EL2*RR22*YukS1Lep2*DBLE(ML**INT(4.D&
  &0)))/(MW2*PI2*SW2) + (0.125D0*EL2*RR21*SB*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW2*PI2*SW2) - (0.125D0*CB*EL2*RR32*YukS1Lep3*DBLE(M&
  &L**INT(4.D0)))/(MW2*PI2*SW2) + (0.125D0*EL2*RR31*SB*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW2*PI2*SW2) + (0.125D0*EL2*YukS3Lep1*YukS&
  &3Lep2*DBLE(ML**INT(4.D0)))/(MW2*PI2*SW2) - (0.125D0*CB*EL2*RR12*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW2*PI2*SW2) + (0.125D0*EL2*RR&
  &11*SB*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW2*PI2*SW2) - (0.125D0*CB*EL2*RR22*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW2*PI2*SW2) + (0.12&
  &5D0*EL2*RR21*SB*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW2*PI2*SW2) - (0.125D0*CB*EL2*RR32*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW2*PI2*SW&
  &2) + (0.125D0*EL2*RR31*SB*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW2*PI2*SW2) + (0.125D0*EL2*YukS3Lep1*YukS3Lep2*DBLE(MM**INT(4.D0)))&
  &/(MW2*PI2*SW2) - (0.375D0*CB*EL2*RR12*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*EL2*RR11*SB*YukS1Quark1*DBLE(M&
  &S**INT(4.D0)))/(MW2*PI2*SW2) - (0.375D0*CB*EL2*RR22*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*EL2*RR21*SB*YukS&
  &1Quark2*DBLE(MS**INT(4.D0)))/(MW2*PI2*SW2) - (0.375D0*CB*EL2*RR32*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*EL&
  &2*RR31*SB*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CKM12*CKMC12*EL2*YukS3Quark1*YukS3Quark2*DBLE(MS**INT(4.D0&
  &)))/(MW2*PI2*SW2) + (0.375D0*CKM22*CKMC22*EL2*YukS3Quark1*YukS3Quark2*DBLE(MS**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CKM32*CKMC&
  &32*EL2*YukS3Quark1*YukS3Quark2*DBLE(MS**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CA1*CA3*EL2*RR21*DBLE(MT**INT(4.D0)))/(MW2*PI2*SW&
  &2) + (0.375D0*CA2*EL2*RR11*SA1*DBLE(MT**INT(4.D0)))/(MW2*PI2*SW2) - (0.375D0*CA3*EL2*RR31*SA1*SA2*DBLE(MT**INT(4.D0)))/(MW2*PI&
  &2*SW2) - (0.375D0*CA1*EL2*RR31*SA3*DBLE(MT**INT(4.D0)))/(MW2*PI2*SW2) - (0.375D0*EL2*RR21*SA1*SA2*SA3*DBLE(MT**INT(4.D0)))/(MW&
  &2*PI2*SW2) + (0.375D0*CB*CKM31*CKMC31*EL2*DBLE(MT**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM32*CKMC32*EL2*DBLE(MT**INT(4.&
  &D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM33*CKMC33*EL2*DBLE(MT**INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.375D0*CA1*CA3*CB*EL2*RR22*DBL&
  &E(MT**INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.375D0*CA2*CB*EL2*RR12*SA1*DBLE(MT**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CA3*CB*EL2*&
  &RR32*SA1*SA2*DBLE(MT**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CA1*CB*EL2*RR32*SA3*DBLE(MT**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.3&
  &75D0*CB*EL2*RR22*SA1*SA2*SA3*DBLE(MT**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CA1*CA3*EL2*RR21*DBLE(MU**INT(4.D0)))/(MW2*PI2*S&
  &W2) + (0.375D0*CA2*EL2*RR11*SA1*DBLE(MU**INT(4.D0)))/(MW2*PI2*SW2) - (0.375D0*CA3*EL2*RR31*SA1*SA2*DBLE(MU**INT(4.D0)))/(MW2*P&
  &I2*SW2) - (0.375D0*CA1*EL2*RR31*SA3*DBLE(MU**INT(4.D0)))/(MW2*PI2*SW2) - (0.375D0*EL2*RR21*SA1*SA2*SA3*DBLE(MU**INT(4.D0)))/(M&
  &W2*PI2*SW2) + (0.375D0*CB*CKM11*CKMC11*EL2*DBLE(MU**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM12*CKMC12*EL2*DBLE(MU**INT(4&
  &.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CB*CKM13*CKMC13*EL2*DBLE(MU**INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.375D0*CA1*CA3*CB*EL2*RR22*DB&
  &LE(MU**INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.375D0*CA2*CB*EL2*RR12*SA1*DBLE(MU**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CA3*CB*EL2&
  &*RR32*SA1*SA2*DBLE(MU**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.375D0*CA1*CB*EL2*RR32*SA3*DBLE(MU**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.&
  &375D0*CB*EL2*RR22*SA1*SA2*SA3*DBLE(MU**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.03125D0*CB*EL2*MHp2*SB*DBLE(CA2**INT(2.D0))*DBLE(SA1*&
  &*INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*MW2*SB*DBLE(CA2**INT(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*MHp&
  &2*SB*DBLE(CA3**INT(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*MW2*SB*DBLE(CA3**INT(2.D0))*DBLE(SA1**INT(2.D0))&
  &)/(PI2*SW2) + (0.03125D0*CB*EL2*SB*DBLE(CA2**INT(2.D0))*DBLE(MH1**INT(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA3*&
  &CB2*EL2*SA2*SA3*DBLE(MH2**INT(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA3*EL2*SA2*SA3*SB2*DBLE(MH2**INT(2.D0))*DBL&
  &E(SA1**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*SB*DBLE(CA3**INT(2.D0))*DBLE(MH2**INT(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2) &
  &- (0.03125D0*CA3*CB2*EL2*SA2*SA3*DBLE(MH3**INT(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA3*EL2*SA2*SA3*SB2*DBLE(MH&
  &3**INT(2.D0))*DBLE(SA1**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*CB2*EL2*MHp2*SA1*DBLE(CA3**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI&
  &2*SW2) + (0.03125D0*CA1*CB2*EL2*MW2*SA1*DBLE(CA3**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*MHp2*SA1*SB2&
  &*DBLE(CA3**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*MW2*SA1*SB2*DBLE(CA3**INT(2.D0))*DBLE(SA2**INT(2.D0&
  &)))/(PI2*SW2) - (0.03125D0*CB*EL2*MHp2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA3**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) - (0.03125&
  &D0*CB*EL2*MW2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA3**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*CB2*EL2*SA1*DBLE(C&
  &A3**INT(2.D0))*DBLE(MH3**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*SA1*SB2*DBLE(CA3**INT(2.D0))*DBLE(MH3&
  &**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*SB*DBLE(CA1**INT(2.D0))*DBLE(CA3**INT(2.D0))*DBLE(MH3**INT(2.&
  &D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*MHp2*SB*DBLE(CA3**INT(2.D0))*DBLE(SA1**INT(2.D0))*DBLE(SA2**INT(2.D0)&
  &))/(PI2*SW2) + (0.03125D0*CB*EL2*MW2*SB*DBLE(CA3**INT(2.D0))*DBLE(SA1**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) + (0.03125D0&
  &*CB*EL2*SB*DBLE(CA3**INT(2.D0))*DBLE(MH3**INT(2.D0))*DBLE(SA1**INT(2.D0))*DBLE(SA2**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*CB2&
  &*EL2*MHp2*SA1*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*CB2*EL2*MW2*SA1*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA&
  &1*EL2*MHp2*SA1*SB2*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*EL2*MW2*SA1*SB2*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125&
  &D0*CB*EL2*MHp2*SB*DBLE(CA1**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*MW2*SB*DBLE(CA1**INT(2.D0))*DBLE(SA&
  &3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*CB2*EL2*SA1*DBLE(MH3**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*EL&
  &2*SA1*SB2*DBLE(MH3**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*SB*DBLE(CA1**INT(2.D0))*DBLE(MH3**INT(2.D0)&
  &)*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*MHp2*SB*DBLE(SA1**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D&
  &0*CB*EL2*MW2*SB*DBLE(SA1**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*SB*DBLE(MH3**INT(2.D0))*DBLE(SA1**INT&
  &(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*CB2*EL2*MHp2*SA1*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2)&
  & + (0.03125D0*CA1*CB2*EL2*MW2*SA1*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*MHp2*SA1*SB2*DBLE(&
  &SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*MW2*SA1*SB2*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(P&
  &I2*SW2) - (0.03125D0*CB*EL2*MHp2*SB*DBLE(CA1**INT(2.D0))*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*&
  &EL2*MW2*SB*DBLE(CA1**INT(2.D0))*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CA1*CB2*EL2*SA1*DBLE(MH2**IN&
  &T(2.D0))*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CA1*EL2*SA1*SB2*DBLE(MH2**INT(2.D0))*DBLE(SA2**INT(&
  &2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) - (0.03125D0*CB*EL2*SB*DBLE(CA1**INT(2.D0))*DBLE(MH2**INT(2.D0))*DBLE(SA2**INT(2.D0))*D&
  &BLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL2*MHp2*SB*DBLE(SA1**INT(2.D0))*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI&
  &2*SW2) + (0.03125D0*CB*EL2*MW2*SB*DBLE(SA1**INT(2.D0))*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2) + (0.03125D0*CB*EL&
  &2*SB*DBLE(MH2**INT(2.D0))*DBLE(SA1**INT(2.D0))*DBLE(SA2**INT(2.D0))*DBLE(SA3**INT(2.D0)))/(PI2*SW2)))/MHp2 &
                        & )*DLOG(1D0/EvalScale**2)
                end function dBetaMSBarUsual

                ! double precision function dBetaProcDep1Usual()
                !     use constants
                !     implicit none
                !     double complex A0toTauPTauMProcDepVC
                !     dBetaProcDep1Usual = -Yuk6/(1 + Yuk6**2)*( A0toTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSUsualWeak()/ML -&
                !         & dMW2Usual()/(2D0*MW2) + dZA0A0OS()/2D0 - dZG0A0OSUsual()/(2D0*Yuk6) + dZTauTauOSLeftWeak()/2D0 + &
                !         & dZTauTauOSRightWeak()/2D0 )
                ! end function dBetaProcDep1Usual

                ! double precision function dAlphaProcDep1Usual()
                !     use constants
                !     implicit none
                !     double complex HHtoTauPTauMProcDepVC
                !     dAlphaProcDep1Usual = -Yuk5/Yuk4*( HHtoTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSUsualWeak()/ML - &
                !         & dMW2Usual()/(2D0*MW2) + Yuk6*dBetaProcDep1Usual() + dZHHHHOS()/2D0 + Yuk4*dZh0HHOSUsual()/(2D0*Yuk5) + &
                !         & dZTauTauOSLeftWeak()/2D0 + dZTauTauOSRightWeak()/2D0 )
                ! end function dAlphaProcDep1Usual

                ! double precision function dBetaProcDep2Usual()
                !     use constants
                !     implicit none
                !     double complex A0toTauPTauMProcDepVC
                !     dBetaProcDep2Usual = -Yuk6/(1 + Yuk6**2)*( A0toTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSUsualWeak()/ML -&
                !         & dMW2Usual()/(2D0*MW2) + dZA0A0OS()/2D0 - dZG0A0OSUsual()/(2D0*Yuk6) + dZTauTauOSLeftWeak()/2D0 + &
                !         & dZTauTauOSRightWeak()/2D0 )
                ! end function dBetaProcDep2Usual

                ! double precision function dAlphaProcDep2Usual()
                !     use constants
                !     implicit none
                !     double complex h0toTauPTauMProcDepVC
                !     dAlphaProcDep2Usual = Yuk4/Yuk5*( h0toTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSUsualWeak()/ML - &
                !         & dMW2Usual()/(2D0*MW2) + Yuk6*dBetaProcDep2Usual() + dZh0h0OS()/2D0 + Yuk5*dZHHh0OSUsual()/(2D0*Yuk4) + &
                !         & dZTauTauOSLeftWeak()/2D0 + dZTauTauOSRightWeak()/2D0 )
                ! end function dAlphaProcDep2Usual

                ! double precision function dBetaProcDep3Usual()
                !     use constants
                !     implicit none
                !     double complex h0toTauPTauMProcDepVC, HHtoTauPTauMProcDepVC
                !     dBetaProcDep3Usual = - 1D0/(Yuk6*(Yuk4**2 + Yuk5**2))*( Yuk4*Yuk5*(dZHHh0OSUsual()/2D0 + dZh0HHOSUsual()/2D0)&
                !         & + Yuk4**2*( h0toTauPTauMProcDepVC() + dZh0h0OS()/2D0 ) &
                !         & + Yuk5**2*( HHtoTauPTauMProcDepVC() + dZHHHHOS()/2D0 ) &
                !         & + (Yuk4**2 + Yuk5**2)*( dgAtMZ()/(EL/SW) + dMLOSUsualWeak()/ML - dMW2Usual()/(2D0*MW2) + &
                !         & dZTauTauOSLeftWeak()/2D0 + dZTauTauOSRightWeak()/2D0 ) )
                ! end function dBetaProcDep3Usual

                ! double precision function dAlphaProcDep3Usual()
                !     use constants
                !     implicit none
                !     double complex h0toTauPTauMProcDepVC, HHtoTauPTauMProcDepVC
                !     dAlphaProcDep3Usual = Yuk4*Yuk5/(Yuk4**2 + Yuk5**2) * ( h0toTauPTauMProcDepVC() - HHtoTauPTauMProcDepVC() + &
                !         & dZh0h0OS()/2D0 - dZHHHHOS()/2D0 + Yuk5/Yuk4*dZHHh0OSUsual()/2D0 - Yuk4/Yuk5*dZh0HHOSUsual()/2D0 )
                ! end function dAlphaProcDep3Usual

        ! 2HDM Z2-soft-breaking parameter m_{12}**2
              ! Tadpole invariant counterterms
                double precision function dm122MSBarUsual()
                    use constants
                    implicit none
                    double precision S2B, C2B
                    S2B = 2D0*SB*CB 
                    C2B = CB**2 - SB**2
                    dm122MSBarUsual = m12squared/(16D0*PI**2*(4D0*MW2*SW2/EL2))*( &
                        & 2D0*3D0*MU2*(1D0/TB - C2B/S2B)*1D0/TB + 2D0*3D0*MC2*(1D0/TB - C2B/S2B)*1D0/TB + &
                        & 2D0*3D0*MT2*(1D0/TB - C2B/S2B)*1D0/TB + 2D0*3D0*MD2*(YukS2Quark2 - C2B/S2B)*YukS2Quark2 + &
                        & 2D0*3D0*MS2*(YukS2Quark2 - C2B/S2B)*YukS2Quark2 + 2D0*3D0*MB2*(YukS2Quark2-C2B/S2B)*YukS2Quark2+&
                        & 2D0*1D0*ME2*(YukS2Lep2 - C2B/S2B)*YukS2Lep2 + 2D0*1D0*MM2*(YukS2Lep2 - C2B/S2B)*YukS2Lep2 + &
                        & 2D0*1D0*ML2*(YukS2Lep2 - C2B/S2B)*YukS2Lep2 + 8D0*m12squared/S2B - 2D0*MHp2 - MA02 - &
                        & 3D0*(2D0*MW2 + MZ2) + &
                        & (CA1*CA2**2*SA1)/(CB*SB)*MH12 + &
                        & (-(CA1*CA3**2*SA1) - CA1**2*CA3*SA2*SA3 + CA3*SA1**2*SA2*SA3 + CA1*SA1*SA2**2*SA3**2)/(CB*SB)*MH22 + &
                        & (CA1*CA3**2*SA1*SA2**2 + CA1**2*CA3*SA2*SA3 - CA3*SA1**2*SA2*SA3 - CA1*SA1*SA3**2)/(CB*SB)*MH32 &
                    & )*DLOG(1D0/EvalScale**2)
                end function dm122MSBarUsual

                double precision function dm122MSBarAlter()
                    use constants
                    implicit none
                    double precision S2B, C2B
                    S2B = 2D0*SB*CB 
                    C2B = CB**2 - SB**2
                    dm122MSBarAlter = m12squared/(16D0*PI**2*(4D0*MW2*SW2/EL2))*( &
                        & 2D0*3D0*MU2*(1D0/TB - C2B/S2B)*1D0/TB + 2D0*3D0*MC2*(1D0/TB - C2B/S2B)*1D0/TB + &
                        & 2D0*3D0*MT2*(1D0/TB - C2B/S2B)*1D0/TB + 2D0*3D0*MD2*(YukS2Quark2 - C2B/S2B)*YukS2Quark2 + &
                        & 2D0*3D0*MS2*(YukS2Quark2 - C2B/S2B)*YukS2Quark2 + 2D0*3D0*MB2*(YukS2Quark2-C2B/S2B)*YukS2Quark2+&
                        & 2D0*1D0*ME2*(YukS2Lep2 - C2B/S2B)*YukS2Lep2 + 2D0*1D0*MM2*(YukS2Lep2 - C2B/S2B)*YukS2Lep2 + &
                        & 2D0*1D0*ML2*(YukS2Lep2 - C2B/S2B)*YukS2Lep2 + 8D0*m12squared/S2B - 2D0*MHp2 - MA02 - &
                        & 3D0*(2D0*MW2 + MZ2) + &
                        & (CA1*CA2**2*SA1)/(CB*SB)*MH12 + &
                        & (-(CA1*CA3**2*SA1) - CA1**2*CA3*SA2*SA3 + CA3*SA1**2*SA2*SA3 + CA1*SA1*SA2**2*SA3**2)/(CB*SB)*MH22 + &
                        & (CA1*CA3**2*SA1*SA2**2 + CA1**2*CA3*SA2*SA3 - CA3*SA1**2*SA2*SA3 - CA1*SA1*SA3**2)/(CB*SB)*MH32 &
                    & )*DLOG(1D0/EvalScale**2)
                end function dm122MSBarAlter

        ! Singlet vev vS
              ! Tadpole invariant counterterms
                double precision function dvSMSBarUsual()
                    use constants
                    implicit none
                    dvSMSBarUsual = 0D0
                end function dvSMSBarUsual

                double precision function dvSMSBarAlter()
                    use constants
                    implicit none
                    dvSMSBarAlter = ( &
&(0.5D0*vS*(2.D0*CA2*MH12*RR13*((0.5D0*CA3*((-0.0625D0*CS1S1S1f111*CS1S1S1f131)/PI2 - (0.0625D0*CS1S1S1f132*CS1S1S1f222)/PI2 + (0.&
  &0625D0*CS1S1S1f111*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f313)/PI2 + (0.0625D&
  &0*CS1S1S1f122*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f311)/PI2 + (0.125D0*CS1S&
  &3S3f121*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f322)/PI2 + (0.0625D0*CS2S2S1f1&
  &11*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f223)/PI2 - (0.0625D0*CS2S2S1S1f221&
  &3*MA02)/PI2 - (0.0625D0*CS1S1S1f131*CS2S2S1f221*MA02)/(MH12*PI2) - (0.0625D0*CS1S1S1S1f1311*MH12)/PI2 - (0.0625D0*CS1S1S1f132*&
  &CS2S2S1f222*MA02)/(MH22*PI2) - (0.0625D0*CS1S1S1f132*CS1S1S1f211*MH12)/(MH22*PI2) - (0.0625D0*CS1S1S1S1f1322*MH22)/PI2 - (0.06&
  &25D0*CS1S1S1f122*CS1S1S1f131*MH22)/(MH12*PI2) - (0.0625D0*CS1S1S1f133*CS2S2S1f223*MA02)/(MH32*PI2) - (0.0625D0*CS1S1S1f133*CS1&
  &S1S1f311*MH12)/(MH32*PI2) - (0.0625D0*CS1S1S1f133*CS1S1S1f322*MH22)/(MH32*PI2) - (0.0625D0*CS1S1S1S1f1333*MH32)/PI2 - (0.0625D&
  &0*CS1S1S1f131*CS1S1S1f133*MH32)/(MH12*PI2) - (0.0625D0*CS1S1S1f132*CS1S1S1f233*MH32)/(MH22*PI2) - (0.125D0*CS1S1S3S3f1322*MHp2&
  &)/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f122*MHp2)/(MH12*PI2) - (0.125D0*CS1S1S1f132*CS1S3S3f222*MHp2)/(MH22*PI2) - (0.125D0*CS1S1&
  &S1f133*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.125D0*CS1S1S3S3f1311*MW2)/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f111*MW2)/(MH12*PI2) - (0&
  &.125D0*CS1S1S1f132*CS1S3S3f211*MW2)/(MH22*PI2) - (0.125D0*CS1S1S1f133*CS1S3S3f311*MW2)/(MH32*PI2) - (0.0625D0*CS2S2S1S1f1113*M&
  &Z2)/PI2 - (0.0625D0*CS1S1S1f131*CS2S2S1f111*MZ2)/(MH12*PI2) - (0.0625D0*CS1S1S1f132*CS2S2S1f112*MZ2)/(MH22*PI2) - (0.0625D0*CS&
  &1S1S1f133*CS2S2S1f113*MZ2)/(MH32*PI2) + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*S&
  &A2 + SA1*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) &
  &+ (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH32 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.&
  &D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp2&
  &) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB))/(PI2*&
  &SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH32 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1&
  &*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH12)*SA1*(-1.D0*CA3*SA1*S&
  &A2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/&
  &(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) -&
  & (0.09375D0*CA2*EL2*MT2*(-1.D0*MH32 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2&
  &*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH32 + &
  &6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) + (0.1875D0*CS1S1S1f131*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/E&
  &L + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(MH12*PI2*SW2) + (0.1875D0*CS1S1S1f133*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)&
  &/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.1875D0*CS1S1S1f132*EL2*MW2*((2.D0*CB*MW*(-1.&
  &D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(MH22*PI2*SW2) - (0.03125D0*EL2*ME2*&
  &(6.D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32)*YukS1Lep1*YukS1Lep3)/(MW&
  &2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH32 + &
  &6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) -&
  & (0.03125D0*EL2*MM2*(-1.D0*MH32 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH12)*Yuk&
  &S1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09&
  &375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH32)*YukS1Q&
  &uark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.0937&
  &5D0*EL2*MS2*(-1.D0*MH32 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*DBLE(MB**INT(4&
  &.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f133*EL*Yu&
  &kS1Quark3*DBLE(MB**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MC**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0&
  &.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f132*EL*(CA1&
  &*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(M&
  &H12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f133*EL*YukS1Quark3&
  &*DBLE(MD**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S&
  &1S1f132*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MH32*MW*PI&
  &2*SW) - (0.25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(ML**INT&
  &(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f131*EL*Yu&
  &kS1Lep1*DBLE(MM**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25&
  &D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(M&
  &H12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f133*EL*YukS1Quark3&
  &*DBLE(MS**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MT**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.75D0*CS1&
  &S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.&
  &D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MU**INT(4.D0)))/(MH12*MW*PI2*SB&
  &*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f13&
  &2*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*C&
  &A2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL&
  &)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D&
  &0*CA1*CA3*SA2 + SA1*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + C&
  &A2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*S&
  &W2) - (0.015625D0*EL2*(2.D0*MH32 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 &
  &- 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12) + MZ2)*(CA2&
  &*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBLE((CW2 + SW2)**I&
  &NT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH32) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*C&
  &A3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*C&
  &S1S1S1f131*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*PI2*SW2) &
  &+ (0.09375D0*CS1S1S1f133*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA&
  &3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2) + (0.09375D0*CS1S1S1f132*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - &
  &1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2*SW2) +&
  & (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2&
  &.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 +&
  & SW2)**INT(4.D0)))/PI2))/(MH12 - 1.D0*MH32) + (0.5D0*SA3*((-0.0625D0*CS1S1S1f111*CS1S1S1f121)/PI2 + (0.0625D0*CS1S1S1f111*CS1S&
  &1S1f211)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f212)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f213)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f22&
  &3)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f233)/PI2 - (0.0625D0*CS1S1S1f123*CS1S1S1f333)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f211)/P&
  &I2 + (0.125D0*CS1S3S3f121*CS1S3S3f212)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f221)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f222)/PI2 + (&
  &0.0625D0*CS2S2S1f111*CS2S2S1f112)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f122)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f222)/PI2 - (0.06&
  &25D0*CS2S2S1S1f2212*MA02)/PI2 - (0.0625D0*CS1S1S1f121*CS2S2S1f221*MA02)/(MH12*PI2) - (0.0625D0*CS1S1S1S1f1211*MH12)/PI2 - (0.0&
  &625D0*CS1S1S1f122*CS2S2S1f222*MA02)/(MH22*PI2) - (0.0625D0*CS1S1S1f122*CS1S1S1f211*MH12)/(MH22*PI2) - (0.0625D0*CS1S1S1S1f1222&
  &*MH22)/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f122*MH22)/(MH12*PI2) - (0.0625D0*CS1S1S1f123*CS2S2S1f223*MA02)/(MH32*PI2) - (0.0625&
  &D0*CS1S1S1f123*CS1S1S1f311*MH12)/(MH32*PI2) - (0.0625D0*CS1S1S1f123*CS1S1S1f322*MH22)/(MH32*PI2) - (0.0625D0*CS1S1S1S1f1233*MH&
  &32)/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f133*MH32)/(MH12*PI2) - (0.0625D0*CS1S1S1f122*CS1S1S1f233*MH32)/(MH22*PI2) - (0.125D0*C&
  &S1S1S3S3f1222*MHp2)/PI2 - (0.125D0*CS1S1S1f121*CS1S3S3f122*MHp2)/(MH12*PI2) - (0.125D0*CS1S1S1f122*CS1S3S3f222*MHp2)/(MH22*PI2&
  &) - (0.125D0*CS1S1S1f123*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.125D0*CS1S1S3S3f1211*MW2)/PI2 - (0.125D0*CS1S1S1f121*CS1S3S3f111*MW&
  &2)/(MH12*PI2) - (0.125D0*CS1S1S1f122*CS1S3S3f211*MW2)/(MH22*PI2) - (0.125D0*CS1S1S1f123*CS1S3S3f311*MW2)/(MH32*PI2) - (0.0625D&
  &0*CS2S2S1S1f1112*MZ2)/PI2 - (0.0625D0*CS1S1S1f121*CS2S2S1f111*MZ2)/(MH12*PI2) - (0.0625D0*CS1S1S1f122*CS2S2S1f112*MZ2)/(MH22*P&
  &I2) - (0.0625D0*CS1S1S1f123*CS2S2S1f113*MZ2)/(MH32*PI2) + (0.25D0*EL2*MW2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA&
  &1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2&
  &*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + &
  &2.D0*(MH22 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2&
  &*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) &
  &+ (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3&
  &*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH12)*SA1*(CA&
  &1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))&
  &/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.0&
  &9375D0*CA2*EL2*MT2*(-1.D0*MH22 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0&
  &*MH12 + 6.D0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH22 + 6.D0*MU2)*SA1*(CA&
  &1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) + (0.1875D0*CS1S1S1f121*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB&
  &*SW)/EL))/(MH12*PI2*SW2) + (0.1875D0*CS1S1S1f123*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*C&
  &A3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.1875D0*CS1S1S1f122*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*S&
  &A2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(MH22*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*&
  &YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH22)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0&
  &*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH22 + 6.D0*ML2)*YukS1Lep1*Yuk&
  &S1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1&
  &.D0*MH22 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark2)/(&
  &MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2&
  & - 1.D0*MH22)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH22)*YukS1Quark1*YukS1Quark2)/(MW2&
  &*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH22&
  & + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MH12*MW*PI2*SW)&
  & - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3*DBLE(MB**INT(&
  &4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MC**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f123*EL*(-&
  &1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3&
  &)*DBLE(MC**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D&
  &0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(&
  &MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DB&
  &LE(ME**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1&
  &f121*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MH22*MW*PI2*S&
  &W) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(MM**INT(4.&
  &D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1&
  &Lep3*DBLE(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D&
  &0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(&
  &MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MT**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*&
  &SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT&
  &**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MU**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f&
  &123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*(CA1*CA3 - 1.D0*SA&
  &1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((&
  &2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DB&
  &LE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))&
  &*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*&
  &CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*&
  &SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3)&
  & - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0&
  &*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**IN&
  &T(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*S&
  &A3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f121*EL2*MZ2*((2.D0*CA1&
  &*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*PI2*SW2) + (0.09375D0*CS1S1S1f123*EL2*&
  &MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2&
  &)**INT(2.D0)))/(CW2*MH32*PI2*SW2) + (0.09375D0*CS1S1S1f122*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2&
  &.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2*SW2) + (0.0546875D0*((2.D0*CA1*CA2*&
  &CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(MH12&
  & - 1.D0*MH22)) + 2.D0*MH22*RR23*(-1.D0*SA2*SA3*((0.5D0*CA3*((-0.0625D0*CS1S1S1f111*CS1S1S1f131)/PI2 - (0.0625D0*CS1S1S1f132*CS&
  &1S1S1f222)/PI2 + (0.0625D0*CS1S1S1f111*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1&
  &f313)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f311)&
  &/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f322)/PI2 +&
  & (0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f223)/PI2 - (0.&
  &0625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.0625D0*CS1S1S1f131*CS2S2S1f221*MA02)/(MH12*PI2) - (0.0625D0*CS1S1S1S1f1311*MH12)/PI2 - (0&
  &.0625D0*CS1S1S1f132*CS2S2S1f222*MA02)/(MH22*PI2) - (0.0625D0*CS1S1S1f132*CS1S1S1f211*MH12)/(MH22*PI2) - (0.0625D0*CS1S1S1S1f13&
  &22*MH22)/PI2 - (0.0625D0*CS1S1S1f122*CS1S1S1f131*MH22)/(MH12*PI2) - (0.0625D0*CS1S1S1f133*CS2S2S1f223*MA02)/(MH32*PI2) - (0.06&
  &25D0*CS1S1S1f133*CS1S1S1f311*MH12)/(MH32*PI2) - (0.0625D0*CS1S1S1f133*CS1S1S1f322*MH22)/(MH32*PI2) - (0.0625D0*CS1S1S1S1f1333*&
  &MH32)/PI2 - (0.0625D0*CS1S1S1f131*CS1S1S1f133*MH32)/(MH12*PI2) - (0.0625D0*CS1S1S1f132*CS1S1S1f233*MH32)/(MH22*PI2) - (0.125D0&
  &*CS1S1S3S3f1322*MHp2)/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f122*MHp2)/(MH12*PI2) - (0.125D0*CS1S1S1f132*CS1S3S3f222*MHp2)/(MH22*P&
  &I2) - (0.125D0*CS1S1S1f133*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.125D0*CS1S1S3S3f1311*MW2)/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f111*&
  &MW2)/(MH12*PI2) - (0.125D0*CS1S1S1f132*CS1S3S3f211*MW2)/(MH22*PI2) - (0.125D0*CS1S1S1f133*CS1S3S3f311*MW2)/(MH32*PI2) - (0.062&
  &5D0*CS2S2S1S1f1113*MZ2)/PI2 - (0.0625D0*CS1S1S1f131*CS2S2S1f111*MZ2)/(MH12*PI2) - (0.0625D0*CS1S1S1f132*CS2S2S1f112*MZ2)/(MH22&
  &*PI2) - (0.0625D0*CS1S1S1f133*CS2S2S1f113*MZ2)/(MH32*PI2) + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*&
  &CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1&
  &*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH32 + 2.D0*MW2)*(CA1*CA2*CB + &
  &CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2&
  & + 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 +&
  & SA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH32 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA&
  &3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH12)*&
  &SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*SA1*(-1.D0*CA3*SA1*S&
  &A2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))&
  &/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH32 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) &
  &- (0.09375D0*CA2*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL&
  &2*MU2*(-1.D0*MH32 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) + (0.1875D0*CS1S1S1f131*EL2*MW2*((2.D0&
  &*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(MH12*PI2*SW2) + (0.1875D0*CS1S1S1f133*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA&
  &3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.1875D0*CS1S1S1f132*EL2*M&
  &W2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(MH22*PI2*SW2) -&
  & (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32)*YukS&
  &1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL&
  &2*ML2*(-1.D0*MH32 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1L&
  &ep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH32 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*&
  &MB2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)/(&
  &MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2&
  & - 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW&
  &2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH32 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.75D0*CS1S1S1f131*EL*YukS1Q&
  &uark1*DBLE(MB**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75&
  &D0*CS1S1S1f133*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MC**INT(4.D0)))/(MH1&
  &2*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0&
  &*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*DBL&
  &E(MD**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S&
  &1f133*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MH12*MW*PI&
  &2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(ME**INT&
  &(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*Yu&
  &kS1Lep2*DBLE(ML**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25&
  &D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MH2&
  &2*MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*DBL&
  &E(MS**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S&
  &1f133*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MT**INT(4.D0)))/(MH12*MW*PI2*&
  &SB*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f&
  &132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MU**INT(4.D&
  &0)))/(MH12*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MH32*MW*PI2*SB*SW) &
  &- (0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*CB&
  &*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D&
  &0*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*&
  &SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*&
  &MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**IN&
  &T(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH32 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) +&
  & (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02&
  & + MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)&
  &*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH32) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*&
  &CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2&
  &*SW2) + (0.09375D0*CS1S1S1f131*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))&
  &/(CW2*MH12*PI2*SW2) + (0.09375D0*CS1S1S1f133*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*S&
  &A1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2) + (0.09375D0*CS1S1S1f132*EL2*MZ2*((2.D0*CB*&
  &MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/&
  &(CW2*MH22*PI2*SW2) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + &
  &SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT&
  &(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(MH12 - 1.D0*MH32) + (0.5D0*SA3*((-0.0625D0*CS1S1S1f111*CS1S1S1f121)/PI2 + (0.062&
  &5D0*CS1S1S1f111*CS1S1S1f211)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f212)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f213)/PI2 + (0.125D0*CS&
  &1S1S1f123*CS1S1S1f223)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f233)/PI2 - (0.0625D0*CS1S1S1f123*CS1S1S1f333)/PI2 + (0.125D0*CS1S3S&
  &3f111*CS1S3S3f211)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f212)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f221)/PI2 + (0.125D0*CS1S3S3f122*&
  &CS1S3S3f222)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f112)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f122)/PI2 + (0.0625D0*CS2S2S1f221*CS2S&
  &2S1f222)/PI2 - (0.0625D0*CS2S2S1S1f2212*MA02)/PI2 - (0.0625D0*CS1S1S1f121*CS2S2S1f221*MA02)/(MH12*PI2) - (0.0625D0*CS1S1S1S1f1&
  &211*MH12)/PI2 - (0.0625D0*CS1S1S1f122*CS2S2S1f222*MA02)/(MH22*PI2) - (0.0625D0*CS1S1S1f122*CS1S1S1f211*MH12)/(MH22*PI2) - (0.0&
  &625D0*CS1S1S1S1f1222*MH22)/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f122*MH22)/(MH12*PI2) - (0.0625D0*CS1S1S1f123*CS2S2S1f223*MA02)/&
  &(MH32*PI2) - (0.0625D0*CS1S1S1f123*CS1S1S1f311*MH12)/(MH32*PI2) - (0.0625D0*CS1S1S1f123*CS1S1S1f322*MH22)/(MH32*PI2) - (0.0625&
  &D0*CS1S1S1S1f1233*MH32)/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f133*MH32)/(MH12*PI2) - (0.0625D0*CS1S1S1f122*CS1S1S1f233*MH32)/(MH&
  &22*PI2) - (0.125D0*CS1S1S3S3f1222*MHp2)/PI2 - (0.125D0*CS1S1S1f121*CS1S3S3f122*MHp2)/(MH12*PI2) - (0.125D0*CS1S1S1f122*CS1S3S3&
  &f222*MHp2)/(MH22*PI2) - (0.125D0*CS1S1S1f123*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.125D0*CS1S1S3S3f1211*MW2)/PI2 - (0.125D0*CS1S1S&
  &1f121*CS1S3S3f111*MW2)/(MH12*PI2) - (0.125D0*CS1S1S1f122*CS1S3S3f211*MW2)/(MH22*PI2) - (0.125D0*CS1S1S1f123*CS1S3S3f311*MW2)/(&
  &MH32*PI2) - (0.0625D0*CS2S2S1S1f1112*MZ2)/PI2 - (0.0625D0*CS1S1S1f121*CS2S2S1f111*MZ2)/(MH12*PI2) - (0.0625D0*CS1S1S1f122*CS2S&
  &2S1f112*MZ2)/(MH22*PI2) - (0.0625D0*CS1S1S1f123*CS2S2S1f113*MZ2)/(MH32*PI2) + (0.25D0*EL2*MW2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*C&
  &A1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp2) + MW2)*(CA2*C&
  &B*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D&
  &0*EL2*(-1.D0*MHp2 + 2.D0*(MH22 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA&
  &3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 &
  &- 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA&
  &1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 &
  &- 1.D0*MH12)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*SA1*(CA1*CA3 &
  &- 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2&
  &*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH22 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D&
  &0*CA2*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH22&
  & + 6.D0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) + (0.1875D0*CS1S1S1f121*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL +&
  & (2.D0*CA2*MW*SA1*SB*SW)/EL))/(MH12*PI2*SW2) + (0.1875D0*CS1S1S1f123*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL&
  & + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.1875D0*CS1S1S1f122*EL2*MW2*((2.D0*CB*MW*(-1.D0*&
  &CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(MH22*PI2*SW2) - (0.03125D0*EL2*ME2*(6.&
  &D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH22)*YukS1Lep1*YukS1Lep2)/(MW2*P&
  &I2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH22 + 6.D&
  &0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0&
  &.03125D0*EL2*MM2*(-1.D0*MH22 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH12)*YukS1Q&
  &uark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375&
  &D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH22)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH22)*YukS1Quar&
  &k1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0&
  &*EL2*MS2*(-1.D0*MH22 + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MB**INT(4.D0&
  &)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*YukS1&
  &Quark3*DBLE(MB**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MC**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.75&
  &D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*(CA1*CA&
  &3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MH12&
  &*MW*PI2*SW) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3*DB&
  &LE(MD**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1&
  &f122*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MH32*MW*PI2*S&
  &W) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(ML**INT(4.&
  &D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1&
  &Lep1*DBLE(MM**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*&
  &CS1S1S1f123*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MH12&
  &*MW*PI2*SW) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3*DB&
  &LE(MS**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MT**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.75D0*CS1S1S&
  &1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*(CA1*CA3 - 1.D0*&
  &SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MU**INT(4.D0)))/(MH12*MW*PI2*SB*SW&
  &) - (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*E&
  &L*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*&
  &MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DB&
  &LE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 &
  &- 1.D0*SA1*SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12) + MZ2)*(CA&
  &2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**I&
  &NT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3&
  & - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*E&
  &L2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*&
  &DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*&
  &SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f12&
  &1*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*PI2*SW2) + (0.0937&
  &5D0*CS1S1S1f123*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)&
  &/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2) + (0.09375D0*CS1S1S1f122*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*&
  &SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2*SW2) + (0.05468&
  &75D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*&
  &MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT&
  &(4.D0)))/PI2))/(MH12 - 1.D0*MH22)) + CA2*CA3*((0.5D0*SA2*SA3*((-0.0625D0*CS1S1S1f111*CS1S1S1f131)/PI2 - (0.0625D0*CS1S1S1f132*&
  &CS1S1S1f222)/PI2 + (0.0625D0*CS1S1S1f111*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f113*CS1S1&
  &S1f313)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f31&
  &1)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f322)/PI2&
  & + (0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f223)/PI2 - (&
  &0.0625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.0625D0*CS1S1S1f131*CS2S2S1f221*MA02)/(MH12*PI2) - (0.0625D0*CS1S1S1S1f1311*MH12)/PI2 - &
  &(0.0625D0*CS1S1S1f132*CS2S2S1f222*MA02)/(MH22*PI2) - (0.0625D0*CS1S1S1f132*CS1S1S1f211*MH12)/(MH22*PI2) - (0.0625D0*CS1S1S1S1f&
  &1322*MH22)/PI2 - (0.0625D0*CS1S1S1f122*CS1S1S1f131*MH22)/(MH12*PI2) - (0.0625D0*CS1S1S1f133*CS2S2S1f223*MA02)/(MH32*PI2) - (0.&
  &0625D0*CS1S1S1f133*CS1S1S1f311*MH12)/(MH32*PI2) - (0.0625D0*CS1S1S1f133*CS1S1S1f322*MH22)/(MH32*PI2) - (0.0625D0*CS1S1S1S1f133&
  &3*MH32)/PI2 - (0.0625D0*CS1S1S1f131*CS1S1S1f133*MH32)/(MH12*PI2) - (0.0625D0*CS1S1S1f132*CS1S1S1f233*MH32)/(MH22*PI2) - (0.125&
  &D0*CS1S1S3S3f1322*MHp2)/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f122*MHp2)/(MH12*PI2) - (0.125D0*CS1S1S1f132*CS1S3S3f222*MHp2)/(MH22&
  &*PI2) - (0.125D0*CS1S1S1f133*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.125D0*CS1S1S3S3f1311*MW2)/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f11&
  &1*MW2)/(MH12*PI2) - (0.125D0*CS1S1S1f132*CS1S3S3f211*MW2)/(MH22*PI2) - (0.125D0*CS1S1S1f133*CS1S3S3f311*MW2)/(MH32*PI2) - (0.0&
  &625D0*CS2S2S1S1f1113*MZ2)/PI2 - (0.0625D0*CS1S1S1f131*CS2S2S1f111*MZ2)/(MH12*PI2) - (0.0625D0*CS1S1S1f132*CS2S2S1f112*MZ2)/(MH&
  &22*PI2) - (0.0625D0*CS1S1S1f133*CS2S2S1f113*MZ2)/(MH32*PI2) + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA&
  &1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*C&
  &A1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH32 + 2.D0*MW2)*(CA1*CA2*CB &
  &+ CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MH&
  &p2 + 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2&
  & + SA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH32 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*&
  &CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH12&
  &)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*SA1*(-1.D0*CA3*SA1&
  &*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3&
  &))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH32 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2&
  &) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*&
  &EL2*MU2*(-1.D0*MH32 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) + (0.1875D0*CS1S1S1f131*EL2*MW2*((2.&
  &D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(MH12*PI2*SW2) + (0.1875D0*CS1S1S1f133*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*&
  &CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.1875D0*CS1S1S1f132*EL2&
  &*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(MH22*PI2*SW2)&
  & - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32)*Yu&
  &kS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*&
  &EL2*ML2*(-1.D0*MH32 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS&
  &1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH32 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D&
  &0*MB2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)&
  &/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*M&
  &D2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(&
  &MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH32 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.75D0*CS1S1S1f131*EL*YukS&
  &1Quark1*DBLE(MB**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.&
  &75D0*CS1S1S1f133*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MC**INT(4.D0)))/(M&
  &H12*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75&
  &D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*D&
  &BLE(MD**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S&
  &1S1f133*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MH12*MW*&
  &PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(ME**I&
  &NT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*&
  &YukS1Lep2*DBLE(ML**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.&
  &25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(M&
  &H22*MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*D&
  &BLE(MS**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S&
  &1S1f133*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MT**INT(4.D0)))/(MH12*MW*PI&
  &2*SB*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S&
  &1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MU**INT(4&
  &.D0)))/(MH12*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MH32*MW*PI2*SB*SW&
  &) - (0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*&
  &CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1&
  &.D0*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA&
  &1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D&
  &0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**&
  &INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH32 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3)&
  & + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA&
  &02 + MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*S&
  &B)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH32) + MZ2)*(CA2*CB*SA1 - 1.D0*CA&
  &1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*P&
  &I2*SW2) + (0.09375D0*CS1S1S1f131*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)&
  &))/(CW2*MH12*PI2*SW2) + (0.09375D0*CS1S1S1f133*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3&
  &*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2) + (0.09375D0*CS1S1S1f132*EL2*MZ2*((2.D0*C&
  &B*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0))&
  &)/(CW2*MH22*PI2*SW2) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 &
  &+ SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**I&
  &NT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(CA2*(MH12 - 1.D0*MH32)) - (0.5D0*CA3*SA2*((-0.0625D0*CS1S1S1f111*CS1S1S1f121)/&
  &PI2 + (0.0625D0*CS1S1S1f111*CS1S1S1f211)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f212)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f213)/PI2 +&
  & (0.125D0*CS1S1S1f123*CS1S1S1f223)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f233)/PI2 - (0.0625D0*CS1S1S1f123*CS1S1S1f333)/PI2 + (0.&
  &125D0*CS1S3S3f111*CS1S3S3f211)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f212)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f221)/PI2 + (0.125D0*&
  &CS1S3S3f122*CS1S3S3f222)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f112)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f122)/PI2 + (0.0625D0*CS2S&
  &2S1f221*CS2S2S1f222)/PI2 - (0.0625D0*CS2S2S1S1f2212*MA02)/PI2 - (0.0625D0*CS1S1S1f121*CS2S2S1f221*MA02)/(MH12*PI2) - (0.0625D0&
  &*CS1S1S1S1f1211*MH12)/PI2 - (0.0625D0*CS1S1S1f122*CS2S2S1f222*MA02)/(MH22*PI2) - (0.0625D0*CS1S1S1f122*CS1S1S1f211*MH12)/(MH22&
  &*PI2) - (0.0625D0*CS1S1S1S1f1222*MH22)/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f122*MH22)/(MH12*PI2) - (0.0625D0*CS1S1S1f123*CS2S2S&
  &1f223*MA02)/(MH32*PI2) - (0.0625D0*CS1S1S1f123*CS1S1S1f311*MH12)/(MH32*PI2) - (0.0625D0*CS1S1S1f123*CS1S1S1f322*MH22)/(MH32*PI&
  &2) - (0.0625D0*CS1S1S1S1f1233*MH32)/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f133*MH32)/(MH12*PI2) - (0.0625D0*CS1S1S1f122*CS1S1S1f2&
  &33*MH32)/(MH22*PI2) - (0.125D0*CS1S1S3S3f1222*MHp2)/PI2 - (0.125D0*CS1S1S1f121*CS1S3S3f122*MHp2)/(MH12*PI2) - (0.125D0*CS1S1S1&
  &f122*CS1S3S3f222*MHp2)/(MH22*PI2) - (0.125D0*CS1S1S1f123*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.125D0*CS1S1S3S3f1211*MW2)/PI2 - (0.&
  &125D0*CS1S1S1f121*CS1S3S3f111*MW2)/(MH12*PI2) - (0.125D0*CS1S1S1f122*CS1S3S3f211*MW2)/(MH22*PI2) - (0.125D0*CS1S1S1f123*CS1S3S&
  &3f311*MW2)/(MH32*PI2) - (0.0625D0*CS2S2S1S1f1112*MZ2)/PI2 - (0.0625D0*CS1S1S1f121*CS2S2S1f111*MZ2)/(MH12*PI2) - (0.0625D0*CS1S&
  &1S1f122*CS2S2S1f112*MZ2)/(MH22*PI2) - (0.0625D0*CS1S1S1f123*CS2S2S1f113*MZ2)/(MH32*PI2) + (0.25D0*EL2*MW2*(CA1*CA2*(-1.D0*CA3*&
  &SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp2) +&
  & MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2)&
  & - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH22 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.&
  &D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1&
  &.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 + 2.D0*MW2)*(CA1*CA2&
  &*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC&
  &2*(6.D0*MC2 - 1.D0*MH12)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*S&
  &A1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA&
  &2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH22 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2)&
  & - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2&
  &*(-1.D0*MH22 + 6.D0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) + (0.1875D0*CS1S1S1f121*EL2*MW2*((2.D0*CA1*CA2*CB&
  &*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(MH12*PI2*SW2) + (0.1875D0*CS1S1S1f123*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1&
  &*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.1875D0*CS1S1S1f122*EL2*MW2*((2.D0*C&
  &B*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(MH22*PI2*SW2) - (0.03125D0&
  &*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH22)*YukS1Lep1*YukS1&
  &Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D&
  &0*MH22 + 6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(MW2*P&
  &I2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH22 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*&
  &MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2&
  &) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH22)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH2&
  &2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) &
  &- (0.09375D0*EL2*MS2*(-1.D0*MH22 + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(&
  &MB**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f&
  &123*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MC**INT(4.D0)))/(MH12*MW*PI2*SB&
  &*SW) - (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f12&
  &2*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MD**INT(4&
  &.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*Yu&
  &kS1Quark3*DBLE(MD**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.&
  &25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(M&
  &H32*MW*PI2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBL&
  &E(ML**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f&
  &121*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MH22*MW*PI2*SW&
  &) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MS**INT(4&
  &.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*Yu&
  &kS1Quark3*DBLE(MS**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MT**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0&
  &.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*(CA1&
  &*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MU**INT(4.D0)))/(MH12*&
  &MW*PI2*SB*SW) - (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*C&
  &S1S1S1f122*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL &
  &+ (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*S&
  &B*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*S&
  &A1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12&
  &) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((C&
  &W2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*&
  &(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (&
  &0.015625D0*EL2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*S&
  &A2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB&
  &*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D&
  &0*CS1S1S1f121*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*PI2*SW&
  &2) + (0.09375D0*CS1S1S1f123*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1&
  &*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2) + (0.09375D0*CS1S1S1f122*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1&
  & - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2*SW2&
  &) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)&
  &/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2&
  & + SW2)**INT(4.D0)))/PI2))/(CA2*(MH12 - 1.D0*MH22)) + (0.5D0*((-0.0625D0*CS1S1S1f111*CS1S1S1f231)/PI2 - (0.0625D0*CS1S1S1f222*&
  &CS1S1S1f232)/PI2 + (0.0625D0*CS1S1S1f211*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f212*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f213*CS1S1&
  &S1f313)/PI2 + (0.0625D0*CS1S1S1f222*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f223*CS1S1S1f323)/PI2 + (0.125D0*CS1S3S3f211*CS1S3S3f31&
  &1)/PI2 + (0.125D0*CS1S3S3f221*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f212*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f222*CS1S3S3f322)/PI2&
  & + (0.0625D0*CS2S2S1f112*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f122*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f222*CS2S2S1f223)/PI2 - (&
  &0.0625D0*CS2S2S1S1f2223*MA02)/PI2 - (0.0625D0*CS1S1S1f231*CS2S2S1f221*MA02)/(MH12*PI2) - (0.0625D0*CS1S1S1S1f2311*MH12)/PI2 - &
  &(0.0625D0*CS1S1S1f232*CS2S2S1f222*MA02)/(MH22*PI2) - (0.0625D0*CS1S1S1f211*CS1S1S1f232*MH12)/(MH22*PI2) - (0.0625D0*CS1S1S1S1f&
  &2322*MH22)/PI2 - (0.0625D0*CS1S1S1f122*CS1S1S1f231*MH22)/(MH12*PI2) - (0.0625D0*CS1S1S1f233*CS2S2S1f223*MA02)/(MH32*PI2) - (0.&
  &0625D0*CS1S1S1f233*CS1S1S1f311*MH12)/(MH32*PI2) - (0.0625D0*CS1S1S1f233*CS1S1S1f322*MH22)/(MH32*PI2) - (0.0625D0*CS1S1S1S1f233&
  &3*MH32)/PI2 - (0.0625D0*CS1S1S1f133*CS1S1S1f231*MH32)/(MH12*PI2) - (0.0625D0*CS1S1S1f232*CS1S1S1f233*MH32)/(MH22*PI2) - (0.125&
  &D0*CS1S1S3S3f2322*MHp2)/PI2 - (0.125D0*CS1S1S1f231*CS1S3S3f122*MHp2)/(MH12*PI2) - (0.125D0*CS1S1S1f232*CS1S3S3f222*MHp2)/(MH22&
  &*PI2) - (0.125D0*CS1S1S1f233*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.125D0*CS1S1S3S3f2311*MW2)/PI2 - (0.125D0*CS1S1S1f231*CS1S3S3f11&
  &1*MW2)/(MH12*PI2) - (0.125D0*CS1S1S1f232*CS1S3S3f211*MW2)/(MH22*PI2) - (0.125D0*CS1S1S1f233*CS1S3S3f311*MW2)/(MH32*PI2) - (0.0&
  &625D0*CS2S2S1S1f1123*MZ2)/PI2 - (0.0625D0*CS1S1S1f231*CS2S2S1f111*MZ2)/(MH12*PI2) - (0.0625D0*CS1S1S1f232*CS2S2S1f112*MZ2)/(MH&
  &22*PI2) - (0.0625D0*CS1S1S1f233*CS2S2S1f113*MZ2)/(MH32*PI2) + (0.25D0*EL2*MW2*((-1.D0*CA1*CA3*SA2 + SA1*SA3)*(-1.D0*CA3*SA1 - &
  &1.D0*CA1*SA2*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 +&
  & 2.D0*(MH22 + MHp2) + MW2)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0&
  &*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH32 + MHp2) + MW2&
  &)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.&
  &D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 + 2.D0*MW2)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.&
  &D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.0&
  &3125D0*EL2*(2.D0*MH32 + 2.D0*MW2)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA&
  &1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.09375D0*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*(-1.D0*CA3*SA1&
  &*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*(-1.D0*CA3*SA&
  &1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MT2*(-1.D0*MH22 + 6.D0*MT2)*(-1.D0*CA3*&
  &SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MT2*(-1.D0*MH32 + 6.D0*MT2)*(-1.D0*CA&
  &3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MU2*(-1.D0*MH22 + 6.D0*MU2)*(-1.D0*&
  &CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MU2*(-1.D0*MH32 + 6.D0*MU2)*(-1.D&
  &0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) + (0.1875D0*CS1S1S1f231*EL2*MW2*((2.D0*CA1*CA2*C&
  &B*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(MH12*PI2*SW2) + (0.1875D0*CS1S1S1f233*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA&
  &1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.1875D0*CS1S1S1f232*EL2*MW2*((2.D0*&
  &CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(MH22*PI2*SW2) - (0.03125D&
  &0*EL2*ME2*(6.D0*ME2 - 1.D0*MH22)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32)*YukS1Lep2*YukS&
  &1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH22 + 6.D0*ML2)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.&
  &D0*MH32 + 6.D0*ML2)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH22 + 6.D0*MM2)*YukS1Lep2*YukS1Lep3)/(MW2*&
  &PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH32 + 6.D0*MM2)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0&
  &*MH22)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH22)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW&
  &2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH32)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH&
  &32)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH22 + 6.D0*MS2)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2)&
  & - (0.09375D0*EL2*MS2*(-1.D0*MH32 + 6.D0*MS2)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.75D0*CS1S1S1f231*EL*YukS1Quark1*DBLE&
  &(MB**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f232*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1&
  &f233*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f231*EL*SA1*DBLE(MC**INT(4.D0)))/(MH12*MW*PI2*S&
  &B*SW) - (0.75D0*CS1S1S1f233*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f2&
  &32*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f231*EL*YukS1Quark1*DBLE(MD**INT(&
  &4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f232*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f233*EL*Y&
  &ukS1Quark3*DBLE(MD**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f231*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MH12*MW*PI2*SW) - (0&
  &.25D0*CS1S1S1f232*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f233*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(&
  &MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f231*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f232*EL*YukS1Lep2*DB&
  &LE(ML**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f233*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1&
  &f231*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f232*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MH22*MW*PI2*S&
  &W) - (0.25D0*CS1S1S1f233*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CS1S1S1f231*EL*YukS1Quark1*DBLE(MS**INT(&
  &4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f232*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f233*EL*Y&
  &ukS1Quark3*DBLE(MS**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f231*EL*SA1*DBLE(MT**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (&
  &0.75D0*CS1S1S1f233*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f232*EL*(CA&
  &1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CA2*CS1S1S1f231*EL*SA1*DBLE(MU**INT(4.D0)))/(MH12&
  &*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f233*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*&
  &CS1S1S1f232*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*SW) + (0.109375D0*((2.D0*CB*MW*(-1.D0*CA1*CA3&
  &*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3&
  &)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(&
  &(-1.D0*CA1*CA3*SA2 + SA1*SA3)*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*&
  &SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*(CB*(-1.D0*CA3*SA1&
  &*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA&
  &1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH32) + MZ2)*(CB*(-1.D&
  &0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 &
  &- 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*(CB*(-1.D0*CA1*CA&
  &3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3&
  &)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH32 + 2.D0*MZ2)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) +&
  & (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + &
  &SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f231*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBL&
  &E((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*PI2*SW2) + (0.09375D0*CS1S1S1f233*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/&
  &EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2) + (0.09375D0*CS1S&
  &1S1f232*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE&
  &((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2*SW2) + (0.0546875D0*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D&
  &0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(MH22 -&
  & 1.D0*MH32))) + 2.D0*MH32*RR33*(-1.D0*CA3*SA2*((0.5D0*CA3*((-0.0625D0*CS1S1S1f111*CS1S1S1f131)/PI2 - (0.0625D0*CS1S1S1f132*CS1&
  &S1S1f222)/PI2 + (0.0625D0*CS1S1S1f111*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f&
  &313)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f311)/&
  &PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f322)/PI2 + &
  &(0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f223)/PI2 - (0.0&
  &625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.0625D0*CS1S1S1f131*CS2S2S1f221*MA02)/(MH12*PI2) - (0.0625D0*CS1S1S1S1f1311*MH12)/PI2 - (0.&
  &0625D0*CS1S1S1f132*CS2S2S1f222*MA02)/(MH22*PI2) - (0.0625D0*CS1S1S1f132*CS1S1S1f211*MH12)/(MH22*PI2) - (0.0625D0*CS1S1S1S1f132&
  &2*MH22)/PI2 - (0.0625D0*CS1S1S1f122*CS1S1S1f131*MH22)/(MH12*PI2) - (0.0625D0*CS1S1S1f133*CS2S2S1f223*MA02)/(MH32*PI2) - (0.062&
  &5D0*CS1S1S1f133*CS1S1S1f311*MH12)/(MH32*PI2) - (0.0625D0*CS1S1S1f133*CS1S1S1f322*MH22)/(MH32*PI2) - (0.0625D0*CS1S1S1S1f1333*M&
  &H32)/PI2 - (0.0625D0*CS1S1S1f131*CS1S1S1f133*MH32)/(MH12*PI2) - (0.0625D0*CS1S1S1f132*CS1S1S1f233*MH32)/(MH22*PI2) - (0.125D0*&
  &CS1S1S3S3f1322*MHp2)/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f122*MHp2)/(MH12*PI2) - (0.125D0*CS1S1S1f132*CS1S3S3f222*MHp2)/(MH22*PI&
  &2) - (0.125D0*CS1S1S1f133*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.125D0*CS1S1S3S3f1311*MW2)/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f111*M&
  &W2)/(MH12*PI2) - (0.125D0*CS1S1S1f132*CS1S3S3f211*MW2)/(MH22*PI2) - (0.125D0*CS1S1S1f133*CS1S3S3f311*MW2)/(MH32*PI2) - (0.0625&
  &D0*CS2S2S1S1f1113*MZ2)/PI2 - (0.0625D0*CS1S1S1f131*CS2S2S1f111*MZ2)/(MH12*PI2) - (0.0625D0*CS1S1S1f132*CS2S2S1f112*MZ2)/(MH22*&
  &PI2) - (0.0625D0*CS1S1S1f133*CS2S2S1f113*MZ2)/(MH32*PI2) + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*C&
  &A2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*&
  &CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH32 + 2.D0*MW2)*(CA1*CA2*CB + C&
  &A2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 &
  &+ 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + &
  &SA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH32 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3&
  &*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH12)*S&
  &A1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*SA1*(-1.D0*CA3*SA1*SA&
  &2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/&
  &(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH32 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) -&
  & (0.09375D0*CA2*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2&
  &*MU2*(-1.D0*MH32 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) + (0.1875D0*CS1S1S1f131*EL2*MW2*((2.D0*&
  &CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(MH12*PI2*SW2) + (0.1875D0*CS1S1S1f133*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3&
  &*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.1875D0*CS1S1S1f132*EL2*MW&
  &2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(MH22*PI2*SW2) - &
  &(0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32)*YukS1&
  &Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2&
  &*ML2*(-1.D0*MH32 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Le&
  &p3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH32 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*M&
  &B2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)/(M&
  &W2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 &
  &- 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2&
  &*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH32 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.75D0*CS1S1S1f131*EL*YukS1Qu&
  &ark1*DBLE(MB**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D&
  &0*CS1S1S1f133*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MC**INT(4.D0)))/(MH12&
  &*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*&
  &CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*DBLE&
  &(MD**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1&
  &f133*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MH12*MW*PI2&
  &*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(ME**INT(&
  &4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*Yuk&
  &S1Lep2*DBLE(ML**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D&
  &0*CS1S1S1f131*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MH22&
  &*MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quark1*DBLE&
  &(MS**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1&
  &f133*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MT**INT(4.D0)))/(MH12*MW*PI2*S&
  &B*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f1&
  &32*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MU**INT(4.D0&
  &)))/(MH12*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MH32*MW*PI2*SB*SW) -&
  & (0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*CB*&
  &MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0&
  &*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*S&
  &A3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*M&
  &Z2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**INT&
  &(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH32 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + &
  &(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 &
  &+ MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*&
  &DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH32) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*C&
  &A2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*&
  &SW2) + (0.09375D0*CS1S1S1f131*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/&
  &(CW2*MH12*PI2*SW2) + (0.09375D0*CS1S1S1f133*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA&
  &1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2) + (0.09375D0*CS1S1S1f132*EL2*MZ2*((2.D0*CB*M&
  &W*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(&
  &CW2*MH22*PI2*SW2) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + S&
  &A1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(&
  &-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(MH12 - 1.D0*MH32) + (0.5D0*SA3*((-0.0625D0*CS1S1S1f111*CS1S1S1f121)/PI2 + (0.0625&
  &D0*CS1S1S1f111*CS1S1S1f211)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f212)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f213)/PI2 + (0.125D0*CS1&
  &S1S1f123*CS1S1S1f223)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f233)/PI2 - (0.0625D0*CS1S1S1f123*CS1S1S1f333)/PI2 + (0.125D0*CS1S3S3&
  &f111*CS1S3S3f211)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f212)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f221)/PI2 + (0.125D0*CS1S3S3f122*C&
  &S1S3S3f222)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f112)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f122)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2&
  &S1f222)/PI2 - (0.0625D0*CS2S2S1S1f2212*MA02)/PI2 - (0.0625D0*CS1S1S1f121*CS2S2S1f221*MA02)/(MH12*PI2) - (0.0625D0*CS1S1S1S1f12&
  &11*MH12)/PI2 - (0.0625D0*CS1S1S1f122*CS2S2S1f222*MA02)/(MH22*PI2) - (0.0625D0*CS1S1S1f122*CS1S1S1f211*MH12)/(MH22*PI2) - (0.06&
  &25D0*CS1S1S1S1f1222*MH22)/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f122*MH22)/(MH12*PI2) - (0.0625D0*CS1S1S1f123*CS2S2S1f223*MA02)/(&
  &MH32*PI2) - (0.0625D0*CS1S1S1f123*CS1S1S1f311*MH12)/(MH32*PI2) - (0.0625D0*CS1S1S1f123*CS1S1S1f322*MH22)/(MH32*PI2) - (0.0625D&
  &0*CS1S1S1S1f1233*MH32)/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f133*MH32)/(MH12*PI2) - (0.0625D0*CS1S1S1f122*CS1S1S1f233*MH32)/(MH2&
  &2*PI2) - (0.125D0*CS1S1S3S3f1222*MHp2)/PI2 - (0.125D0*CS1S1S1f121*CS1S3S3f122*MHp2)/(MH12*PI2) - (0.125D0*CS1S1S1f122*CS1S3S3f&
  &222*MHp2)/(MH22*PI2) - (0.125D0*CS1S1S1f123*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.125D0*CS1S1S3S3f1211*MW2)/PI2 - (0.125D0*CS1S1S1&
  &f121*CS1S3S3f111*MW2)/(MH12*PI2) - (0.125D0*CS1S1S1f122*CS1S3S3f211*MW2)/(MH22*PI2) - (0.125D0*CS1S1S1f123*CS1S3S3f311*MW2)/(M&
  &H32*PI2) - (0.0625D0*CS2S2S1S1f1112*MZ2)/PI2 - (0.0625D0*CS1S1S1f121*CS2S2S1f111*MZ2)/(MH12*PI2) - (0.0625D0*CS1S1S1f122*CS2S2&
  &S1f112*MZ2)/(MH22*PI2) - (0.0625D0*CS1S1S1f123*CS2S2S1f113*MZ2)/(MH32*PI2) + (0.25D0*EL2*MW2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA&
  &1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB&
  &*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0&
  &*EL2*(-1.D0*MHp2 + 2.D0*(MH22 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3&
  &*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 -&
  & 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1&
  &*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 -&
  & 1.D0*MH12)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*SA1*(CA1*CA3 -&
  & 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*&
  &PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH22 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0&
  &*CA2*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH22 &
  &+ 6.D0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) + (0.1875D0*CS1S1S1f121*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + &
  &(2.D0*CA2*MW*SA1*SB*SW)/EL))/(MH12*PI2*SW2) + (0.1875D0*CS1S1S1f123*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL &
  &+ (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.1875D0*CS1S1S1f122*EL2*MW2*((2.D0*CB*MW*(-1.D0*C&
  &A3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(MH22*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D&
  &0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH22)*YukS1Lep1*YukS1Lep2)/(MW2*PI&
  &2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH22 + 6.D0&
  &*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.&
  &03125D0*EL2*MM2*(-1.D0*MH22 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH12)*YukS1Qu&
  &ark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D&
  &0*EL2*MB2*(6.D0*MB2 - 1.D0*MH22)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH22)*YukS1Quark&
  &1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*&
  &EL2*MS2*(-1.D0*MH22 + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MB**INT(4.D0)&
  &))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*YukS1Q&
  &uark3*DBLE(MB**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MC**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.75D&
  &0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*(CA1*CA3&
  & - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MH12*&
  &MW*PI2*SW) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3*DBL&
  &E(MD**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f&
  &122*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MH32*MW*PI2*SW&
  &) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(ML**INT(4.D&
  &0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1L&
  &ep1*DBLE(MM**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*C&
  &S1S1S1f123*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MH12*&
  &MW*PI2*SW) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f123*EL*YukS1Quark3*DBL&
  &E(MS**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MT**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.75D0*CS1S1S1&
  &f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*(CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MU**INT(4.D0)))/(MH12*MW*PI2*SB*SW)&
  & - (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL&
  &*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*M&
  &W*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBL&
  &E(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 -&
  & 1.D0*SA1*SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12) + MZ2)*(CA2&
  &*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**IN&
  &T(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 &
  &- 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL&
  &2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*D&
  &BLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*S&
  &A1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f121&
  &*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*PI2*SW2) + (0.09375&
  &D0*CS1S1S1f123*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/&
  &EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2) + (0.09375D0*CS1S1S1f122*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*S&
  &A2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2*SW2) + (0.054687&
  &5D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*M&
  &W*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(&
  &4.D0)))/PI2))/(MH12 - 1.D0*MH22)) - 1.D0*CA2*SA3*((0.5D0*SA2*SA3*((-0.0625D0*CS1S1S1f111*CS1S1S1f131)/PI2 - (0.0625D0*CS1S1S1f&
  &132*CS1S1S1f222)/PI2 + (0.0625D0*CS1S1S1f111*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f113*C&
  &S1S1S1f313)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S&
  &3f311)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f322)&
  &/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f223)/PI2&
  & - (0.0625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.0625D0*CS1S1S1f131*CS2S2S1f221*MA02)/(MH12*PI2) - (0.0625D0*CS1S1S1S1f1311*MH12)/PI&
  &2 - (0.0625D0*CS1S1S1f132*CS2S2S1f222*MA02)/(MH22*PI2) - (0.0625D0*CS1S1S1f132*CS1S1S1f211*MH12)/(MH22*PI2) - (0.0625D0*CS1S1S&
  &1S1f1322*MH22)/PI2 - (0.0625D0*CS1S1S1f122*CS1S1S1f131*MH22)/(MH12*PI2) - (0.0625D0*CS1S1S1f133*CS2S2S1f223*MA02)/(MH32*PI2) -&
  & (0.0625D0*CS1S1S1f133*CS1S1S1f311*MH12)/(MH32*PI2) - (0.0625D0*CS1S1S1f133*CS1S1S1f322*MH22)/(MH32*PI2) - (0.0625D0*CS1S1S1S1&
  &f1333*MH32)/PI2 - (0.0625D0*CS1S1S1f131*CS1S1S1f133*MH32)/(MH12*PI2) - (0.0625D0*CS1S1S1f132*CS1S1S1f233*MH32)/(MH22*PI2) - (0&
  &.125D0*CS1S1S3S3f1322*MHp2)/PI2 - (0.125D0*CS1S1S1f131*CS1S3S3f122*MHp2)/(MH12*PI2) - (0.125D0*CS1S1S1f132*CS1S3S3f222*MHp2)/(&
  &MH22*PI2) - (0.125D0*CS1S1S1f133*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.125D0*CS1S1S3S3f1311*MW2)/PI2 - (0.125D0*CS1S1S1f131*CS1S3S&
  &3f111*MW2)/(MH12*PI2) - (0.125D0*CS1S1S1f132*CS1S3S3f211*MW2)/(MH22*PI2) - (0.125D0*CS1S1S1f133*CS1S3S3f311*MW2)/(MH32*PI2) - &
  &(0.0625D0*CS2S2S1S1f1113*MZ2)/PI2 - (0.0625D0*CS1S1S1f131*CS2S2S1f111*MZ2)/(MH12*PI2) - (0.0625D0*CS1S1S1f132*CS2S2S1f112*MZ2)&
  &/(MH22*PI2) - (0.0625D0*CS1S1S1f133*CS2S2S1f113*MZ2)/(MH32*PI2) + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) &
  &+ CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.&
  &D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH32 + 2.D0*MW2)*(CA1*CA2&
  &*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D&
  &0*MHp2 + 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3&
  &*SA2 + SA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH32 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1&
  &.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*&
  &MH12)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*SA1*(-1.D0*CA3&
  &*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1&
  &*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH32 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2&
  &*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*&
  &CA2*EL2*MU2*(-1.D0*MH32 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) + (0.1875D0*CS1S1S1f131*EL2*MW2*&
  &((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(MH12*PI2*SW2) + (0.1875D0*CS1S1S1f133*EL2*MW2*((2.D0*CB*MW*(-1.D0*&
  &CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.1875D0*CS1S1S1f132&
  &*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(MH22*PI2*&
  &SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32&
  &)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.0312&
  &5D0*EL2*ML2*(-1.D0*MH32 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*&
  &YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH32 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*&
  &(6.D0*MB2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Qua&
  &rk3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.&
  &D0*MD2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark&
  &3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH32 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.75D0*CS1S1S1f131*EL*&
  &YukS1Quark1*DBLE(MB**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW) -&
  & (0.75D0*CS1S1S1f133*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MC**INT(4.D0))&
  &)/(MH12*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (&
  &0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quar&
  &k1*DBLE(MD**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*&
  &CS1S1S1f133*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MH12&
  &*MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(M&
  &E**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f132&
  &*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MH32*MW*PI2*SW) -&
  & (0.25D0*CS1S1S1f131*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f132*EL*YukS1Lep2*DBLE(MM**INT(4.D0))&
  &)/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f133*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CS1S1S1f131*EL*YukS1Quar&
  &k1*DBLE(MS**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f132*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*&
  &CS1S1S1f133*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MT**INT(4.D0)))/(MH12*M&
  &W*PI2*SB*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS&
  &1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CA2*CS1S1S1f131*EL*SA1*DBLE(MU**I&
  &NT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f133*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MH32*MW*PI2*S&
  &B*SW) - (0.75D0*CS1S1S1f132*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*&
  &CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2&
  & - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D&
  &0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 +&
  & 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW&
  &2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH32 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*&
  &SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0&
  &*(MA02 + MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA&
  &3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH32) + MZ2)*(CA2*CB*SA1 - 1.D&
  &0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(C&
  &W2*PI2*SW2) + (0.09375D0*CS1S1S1f131*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2&
  &.D0)))/(CW2*MH12*PI2*SW2) + (0.09375D0*CS1S1S1f133*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0&
  &*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2) + (0.09375D0*CS1S1S1f132*EL2*MZ2*((2.&
  &D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.&
  &D0)))/(CW2*MH22*PI2*SW2) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*&
  &SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(S&
  &W**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(CA2*(MH12 - 1.D0*MH32)) - (0.5D0*CA3*SA2*((-0.0625D0*CS1S1S1f111*CS1S1S1f1&
  &21)/PI2 + (0.0625D0*CS1S1S1f111*CS1S1S1f211)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f212)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f213)/P&
  &I2 + (0.125D0*CS1S1S1f123*CS1S1S1f223)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f233)/PI2 - (0.0625D0*CS1S1S1f123*CS1S1S1f333)/PI2 +&
  & (0.125D0*CS1S3S3f111*CS1S3S3f211)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f212)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f221)/PI2 + (0.12&
  &5D0*CS1S3S3f122*CS1S3S3f222)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f112)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f122)/PI2 + (0.0625D0*&
  &CS2S2S1f221*CS2S2S1f222)/PI2 - (0.0625D0*CS2S2S1S1f2212*MA02)/PI2 - (0.0625D0*CS1S1S1f121*CS2S2S1f221*MA02)/(MH12*PI2) - (0.06&
  &25D0*CS1S1S1S1f1211*MH12)/PI2 - (0.0625D0*CS1S1S1f122*CS2S2S1f222*MA02)/(MH22*PI2) - (0.0625D0*CS1S1S1f122*CS1S1S1f211*MH12)/(&
  &MH22*PI2) - (0.0625D0*CS1S1S1S1f1222*MH22)/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f122*MH22)/(MH12*PI2) - (0.0625D0*CS1S1S1f123*CS&
  &2S2S1f223*MA02)/(MH32*PI2) - (0.0625D0*CS1S1S1f123*CS1S1S1f311*MH12)/(MH32*PI2) - (0.0625D0*CS1S1S1f123*CS1S1S1f322*MH22)/(MH3&
  &2*PI2) - (0.0625D0*CS1S1S1S1f1233*MH32)/PI2 - (0.0625D0*CS1S1S1f121*CS1S1S1f133*MH32)/(MH12*PI2) - (0.0625D0*CS1S1S1f122*CS1S1&
  &S1f233*MH32)/(MH22*PI2) - (0.125D0*CS1S1S3S3f1222*MHp2)/PI2 - (0.125D0*CS1S1S1f121*CS1S3S3f122*MHp2)/(MH12*PI2) - (0.125D0*CS1&
  &S1S1f122*CS1S3S3f222*MHp2)/(MH22*PI2) - (0.125D0*CS1S1S1f123*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.125D0*CS1S1S3S3f1211*MW2)/PI2 -&
  & (0.125D0*CS1S1S1f121*CS1S3S3f111*MW2)/(MH12*PI2) - (0.125D0*CS1S1S1f122*CS1S3S3f211*MW2)/(MH22*PI2) - (0.125D0*CS1S1S1f123*CS&
  &1S3S3f311*MW2)/(MH32*PI2) - (0.0625D0*CS2S2S1S1f1112*MZ2)/PI2 - (0.0625D0*CS1S1S1f121*CS2S2S1f111*MZ2)/(MH12*PI2) - (0.0625D0*&
  &CS1S1S1f122*CS2S2S1f112*MZ2)/(MH22*PI2) - (0.0625D0*CS1S1S1f123*CS2S2S1f113*MZ2)/(MH32*PI2) + (0.25D0*EL2*MW2*(CA1*CA2*(-1.D0*&
  &CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp&
  &2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*&
  &SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH22 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) &
  &- 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB&
  &*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 + 2.D0*MW2)*(CA1&
  &*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL&
  &2*MC2*(6.D0*MC2 - 1.D0*MH12)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH2&
  &2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA&
  &1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH22 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*&
  &SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2&
  &*MU2*(-1.D0*MH22 + 6.D0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) + (0.1875D0*CS1S1S1f121*EL2*MW2*((2.D0*CA1*CA&
  &2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(MH12*PI2*SW2) + (0.1875D0*CS1S1S1f123*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 +&
  & SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.1875D0*CS1S1S1f122*EL2*MW2*((2.&
  &D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(MH22*PI2*SW2) - (0.031&
  &25D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH22)*YukS1Lep1*Y&
  &ukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(&
  &-1.D0*MH22 + 6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(M&
  &W2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH22 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1&
  &.D0*MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2&
  &*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH22)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0&
  &*MH22)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*S&
  &W2) - (0.09375D0*EL2*MS2*(-1.D0*MH22 + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*D&
  &BLE(MB**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S&
  &1S1f123*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MC**INT(4.D0)))/(MH12*MW*PI&
  &2*SB*SW) - (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S&
  &1f122*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MD**I&
  &NT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f123*E&
  &L*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MH12*MW*PI2*SW) -&
  & (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(ME**INT(4.D0))&
  &)/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f121*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2&
  &*DBLE(ML**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S&
  &1S1f121*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f122*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MH22*MW*PI&
  &2*SW) - (0.25D0*CS1S1S1f123*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CS1S1S1f121*EL*YukS1Quark1*DBLE(MS**I&
  &NT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f122*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f123*E&
  &L*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MT**INT(4.D0)))/(MH12*MW*PI2*SB*SW) &
  &- (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f122*EL*&
  &(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CA2*CS1S1S1f121*EL*SA1*DBLE(MU**INT(4.D0)))/(M&
  &H12*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f123*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75&
  &D0*CS1S1S1f122*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*SW) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)&
  &/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA&
  &3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + C&
  &A2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + &
  &MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBL&
  &E((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*&
  &SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)&
  & - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)&
  &*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09&
  &375D0*CS1S1S1f121*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*PI&
  &2*SW2) + (0.09375D0*CS1S1S1f123*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0&
  &*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2) + (0.09375D0*CS1S1S1f122*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3&
  &*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2&
  &*SW2) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)&
  &*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(&
  &(CW2 + SW2)**INT(4.D0)))/PI2))/(CA2*(MH12 - 1.D0*MH22)) + (0.5D0*((-0.0625D0*CS1S1S1f111*CS1S1S1f231)/PI2 - (0.0625D0*CS1S1S1f&
  &222*CS1S1S1f232)/PI2 + (0.0625D0*CS1S1S1f211*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f212*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f213*C&
  &S1S1S1f313)/PI2 + (0.0625D0*CS1S1S1f222*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f223*CS1S1S1f323)/PI2 + (0.125D0*CS1S3S3f211*CS1S3S&
  &3f311)/PI2 + (0.125D0*CS1S3S3f221*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f212*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f222*CS1S3S3f322)&
  &/PI2 + (0.0625D0*CS2S2S1f112*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f122*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f222*CS2S2S1f223)/PI2&
  & - (0.0625D0*CS2S2S1S1f2223*MA02)/PI2 - (0.0625D0*CS1S1S1f231*CS2S2S1f221*MA02)/(MH12*PI2) - (0.0625D0*CS1S1S1S1f2311*MH12)/PI&
  &2 - (0.0625D0*CS1S1S1f232*CS2S2S1f222*MA02)/(MH22*PI2) - (0.0625D0*CS1S1S1f211*CS1S1S1f232*MH12)/(MH22*PI2) - (0.0625D0*CS1S1S&
  &1S1f2322*MH22)/PI2 - (0.0625D0*CS1S1S1f122*CS1S1S1f231*MH22)/(MH12*PI2) - (0.0625D0*CS1S1S1f233*CS2S2S1f223*MA02)/(MH32*PI2) -&
  & (0.0625D0*CS1S1S1f233*CS1S1S1f311*MH12)/(MH32*PI2) - (0.0625D0*CS1S1S1f233*CS1S1S1f322*MH22)/(MH32*PI2) - (0.0625D0*CS1S1S1S1&
  &f2333*MH32)/PI2 - (0.0625D0*CS1S1S1f133*CS1S1S1f231*MH32)/(MH12*PI2) - (0.0625D0*CS1S1S1f232*CS1S1S1f233*MH32)/(MH22*PI2) - (0&
  &.125D0*CS1S1S3S3f2322*MHp2)/PI2 - (0.125D0*CS1S1S1f231*CS1S3S3f122*MHp2)/(MH12*PI2) - (0.125D0*CS1S1S1f232*CS1S3S3f222*MHp2)/(&
  &MH22*PI2) - (0.125D0*CS1S1S1f233*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.125D0*CS1S1S3S3f2311*MW2)/PI2 - (0.125D0*CS1S1S1f231*CS1S3S&
  &3f111*MW2)/(MH12*PI2) - (0.125D0*CS1S1S1f232*CS1S3S3f211*MW2)/(MH22*PI2) - (0.125D0*CS1S1S1f233*CS1S3S3f311*MW2)/(MH32*PI2) - &
  &(0.0625D0*CS2S2S1S1f1123*MZ2)/PI2 - (0.0625D0*CS1S1S1f231*CS2S2S1f111*MZ2)/(MH12*PI2) - (0.0625D0*CS1S1S1f232*CS2S2S1f112*MZ2)&
  &/(MH22*PI2) - (0.0625D0*CS1S1S1f233*CS2S2S1f113*MZ2)/(MH32*PI2) + (0.25D0*EL2*MW2*((-1.D0*CA1*CA3*SA2 + SA1*SA3)*(-1.D0*CA3*SA&
  &1 - 1.D0*CA1*SA2*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MH&
  &p2 + 2.D0*(MH22 + MHp2) + MW2)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - &
  &1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH32 + MHp2) +&
  & MW2)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*&
  &(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 + 2.D0*MW2)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + &
  &(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - &
  &(0.03125D0*EL2*(2.D0*MH32 + 2.D0*MW2)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA&
  &3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.09375D0*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*(-1.D0*CA3&
  &*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*(-1.D0*CA&
  &3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MT2*(-1.D0*MH22 + 6.D0*MT2)*(-1.D0*&
  &CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MT2*(-1.D0*MH32 + 6.D0*MT2)*(-1.D&
  &0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MU2*(-1.D0*MH22 + 6.D0*MU2)*(-1&
  &.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MU2*(-1.D0*MH32 + 6.D0*MU2)*(&
  &-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) + (0.1875D0*CS1S1S1f231*EL2*MW2*((2.D0*CA1*C&
  &A2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(MH12*PI2*SW2) + (0.1875D0*CS1S1S1f233*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 &
  &+ SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.1875D0*CS1S1S1f232*EL2*MW2*((2&
  &.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(MH22*PI2*SW2) - (0.03&
  &125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH22)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32)*YukS1Lep2*&
  &YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH22 + 6.D0*ML2)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*&
  &(-1.D0*MH32 + 6.D0*ML2)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH22 + 6.D0*MM2)*YukS1Lep2*YukS1Lep3)/(&
  &MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH32 + 6.D0*MM2)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - &
  &1.D0*MH22)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH22)*YukS1Quark2*YukS1Quark3)/(MW2*PI&
  &2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH32)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D&
  &0*MH32)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH22 + 6.D0*MS2)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*&
  &SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH32 + 6.D0*MS2)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.75D0*CS1S1S1f231*EL*YukS1Quark1*&
  &DBLE(MB**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f232*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1&
  &S1S1f233*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f231*EL*SA1*DBLE(MC**INT(4.D0)))/(MH12*MW*P&
  &I2*SB*SW) - (0.75D0*CS1S1S1f233*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1&
  &S1f232*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f231*EL*YukS1Quark1*DBLE(MD**&
  &INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f232*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f233*&
  &EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f231*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MH12*MW*PI2*SW) &
  &- (0.25D0*CS1S1S1f232*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f233*EL*YukS1Lep3*DBLE(ME**INT(4.D0)&
  &))/(MH32*MW*PI2*SW) - (0.25D0*CS1S1S1f231*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f232*EL*YukS1Lep&
  &2*DBLE(ML**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.25D0*CS1S1S1f233*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.25D0*CS1&
  &S1S1f231*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.25D0*CS1S1S1f232*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MH22*MW*P&
  &I2*SW) - (0.25D0*CS1S1S1f233*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CS1S1S1f231*EL*YukS1Quark1*DBLE(MS**&
  &INT(4.D0)))/(MH12*MW*PI2*SW) - (0.75D0*CS1S1S1f232*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.75D0*CS1S1S1f233*&
  &EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.75D0*CA2*CS1S1S1f231*EL*SA1*DBLE(MT**INT(4.D0)))/(MH12*MW*PI2*SB*SW)&
  & - (0.75D0*CS1S1S1f233*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f232*EL&
  &*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.75D0*CA2*CS1S1S1f231*EL*SA1*DBLE(MU**INT(4.D0)))/(&
  &MH12*MW*PI2*SB*SW) - (0.75D0*CS1S1S1f233*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.7&
  &5D0*CS1S1S1f232*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*SW) + (0.109375D0*((2.D0*CB*MW*(-1.D0*CA1&
  &*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2&
  &*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*M&
  &Z2*((-1.D0*CA1*CA3*SA2 + SA1*SA3)*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*&
  &SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*(CB*(-1.D0*CA3&
  &*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D&
  &0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH32) + MZ2)*(CB*(&
  &-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*&
  &SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*(CB*(-1.D0*CA&
  &1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2&
  &*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH32 + 2.D0*MZ2)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA&
  &3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW&
  &2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.09375D0*CS1S1S1f231*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)&
  &*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*PI2*SW2) + (0.09375D0*CS1S1S1f233*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*&
  &SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2) + (0.09375D0*&
  &CS1S1S1f232*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*&
  &DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2*SW2) + (0.0546875D0*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(&
  &-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0&
  &*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(MH&
  &22 - 1.D0*MH32))) + DBLE(RR13**INT(2.D0))*((-0.03125D0*CS1S1S1f112*CS1S1S1f222)/PI2 - (0.03125D0*CS1S1S1f113*CS1S1S1f333)/PI2 &
  &+ (0.125D0*CS1S3S3f112*CS1S3S3f121)/PI2 - (0.03125D0*CS2S2S1S1f2211*MA02)/PI2 - (0.03125D0*CS1S1S1f111*CS2S2S1f221*MA02)/(MH12&
  &*PI2) - (0.03125D0*CS1S1S1S1f1111*MH12)/PI2 - (0.03125D0*CS1S1S1f112*CS2S2S1f222*MA02)/(MH22*PI2) - (0.03125D0*CS1S1S1f112*CS1&
  &S1S1f211*MH12)/(MH22*PI2) - (0.03125D0*CS1S1S1S1f1122*MH22)/PI2 - (0.03125D0*CS1S1S1f111*CS1S1S1f122*MH22)/(MH12*PI2) - (0.031&
  &25D0*CS1S1S1f113*CS2S2S1f223*MA02)/(MH32*PI2) - (0.03125D0*CS1S1S1f113*CS1S1S1f311*MH12)/(MH32*PI2) - (0.03125D0*CS1S1S1f113*C&
  &S1S1S1f322*MH22)/(MH32*PI2) - (0.03125D0*CS1S1S1S1f1133*MH32)/PI2 - (0.03125D0*CS1S1S1f111*CS1S1S1f133*MH32)/(MH12*PI2) - (0.0&
  &3125D0*CS1S1S1f112*CS1S1S1f233*MH32)/(MH22*PI2) - (0.0625D0*CS1S1S3S3f1122*MHp2)/PI2 - (0.0625D0*CS1S1S1f111*CS1S3S3f122*MHp2)&
  &/(MH12*PI2) - (0.0625D0*CS1S1S1f112*CS1S3S3f222*MHp2)/(MH22*PI2) - (0.0625D0*CS1S1S1f113*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.062&
  &5D0*CS1S1S3S3f1111*MW2)/PI2 - (0.0625D0*CS1S1S1f111*CS1S3S3f111*MW2)/(MH12*PI2) - (0.0625D0*CS1S1S1f112*CS1S3S3f211*MW2)/(MH22&
  &*PI2) - (0.0625D0*CS1S1S1f113*CS1S3S3f311*MW2)/(MH32*PI2) - (0.03125D0*CS2S2S1S1f1111*MZ2)/PI2 - (0.03125D0*CS1S1S1f111*CS2S2S&
  &1f111*MZ2)/(MH12*PI2) - (0.03125D0*CS1S1S1f112*CS2S2S1f112*MZ2)/(MH22*PI2) - (0.03125D0*CS1S1S1f113*CS2S2S1f113*MZ2)/(MH32*PI2&
  &) + (0.125D0*EL2*MW2*(CA12*CA22 + CA22*SA12))/(PI2*SW2) - (0.09375D0*CA22*EL2*MC2*(6.D0*MC2 - 1.D0*MH12)*SA12)/(MW2*PI2*SB2*SW&
  &2) - (0.09375D0*CA22*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA12)/(MW2*PI2*SB2*SW2) - (0.09375D0*CA22*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)&
  &*SA12)/(MW2*PI2*SB2*SW2) + (0.09375D0*CS1S1S1f111*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(MH12*PI2&
  &*SW2) + (0.09375D0*CS1S1S1f113*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*&
  &CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.09375D0*CS1S1S1f112*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + &
  &(2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(MH22*PI2*SW2) + (0.0625D0*DBLE(CS1S1S1f112**INT(2.D0)))/PI2 + (0.0625D0*DBL&
  &E(CS1S1S1f113**INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS1S1S1f122**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S1S1f123**INT(2.D0)))/PI2 + &
  &(0.03125D0*DBLE(CS1S1S1f133**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S3S3f111**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S3S3f122**INT(2&
  &.D0)))/PI2 + (0.03125D0*DBLE(CS2S2S1f111**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS2S2S1f121**INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS2S&
  &2S1f221**INT(2.D0)))/PI2 - (0.375D0*CS1S1S1f111*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.375D0*CS1S1S1f112*EL&
  &*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.375D0*CS1S1S1f113*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MH32*MW*PI2*SW)&
  & - (0.375D0*CA2*CS1S1S1f111*EL*SA1*DBLE(MC**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f113*EL*(-1.D0*CA3*SA1*SA2 - 1.D&
  &0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f112*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0))&
  &)/(MH22*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f111*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.375D0*CS1S1S1f112*EL*Yu&
  &kS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.375D0*CS1S1S1f113*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MH32*MW*PI2*SW) - &
  &(0.125D0*CS1S1S1f111*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.125D0*CS1S1S1f112*EL*YukS1Lep2*DBLE(ME**INT(4.D0)&
  &))/(MH22*MW*PI2*SW) - (0.125D0*CS1S1S1f113*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.125D0*CS1S1S1f111*EL*YukS1L&
  &ep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.125D0*CS1S1S1f112*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.125D0&
  &*CS1S1S1f113*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.125D0*CS1S1S1f111*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MH12&
  &*MW*PI2*SW) - (0.125D0*CS1S1S1f112*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.125D0*CS1S1S1f113*EL*YukS1Lep3*DBLE&
  &(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.375D0*CS1S1S1f111*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.375D0*CS1S1&
  &S1f112*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.375D0*CS1S1S1f113*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MH32*M&
  &W*PI2*SW) - (0.375D0*CA2*CS1S1S1f111*EL*SA1*DBLE(MT**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f113*EL*(-1.D0*CA3*SA1*&
  &SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f112*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**I&
  &NT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.375D0*CA2*CS1S1S1f111*EL*SA1*DBLE(MU**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f1&
  &13*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f112*EL*(CA1*CA3 - 1.D0*SA&
  &1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp2) + MW2)*DBLE((CA2*CB*SA1 &
  &- 1.D0*CA1*CA2*SB)**INT(2.D0)))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*DBLE((CA1*CA2*CB + CA2*SA1*SB)**INT(2.D0)))/&
  &(PI2*SW2) + (0.0546875D0*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/E&
  &L)**INT(2.D0)))/PI2 + (0.0625D0*EL2*MZ2*(CA12*CA22 + CA22*SA12)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.046875D0*CS1S&
  &1S1f111*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*PI2*SW2) + (&
  &0.046875D0*CS1S1S1f113*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)&
  &*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2) + (0.046875D0*CS1S1S1f112*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1&
  &.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2*SW2) - &
  &(0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12) + MZ2)*DBLE((CA2*CB*SA1 - 1.D0*CA1*CA2*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(&
  &2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*MZ2)*DBLE((CA1*CA2*CB + CA2*SA1*SB)**INT(2.D0))*DBLE((CW2 + SW2)**IN&
  &T(2.D0)))/(CW2*PI2*SW2) + (0.02734375D0*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CA1*CA2*CB*M&
  &W*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)**INT(2.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2 - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH1&
  &2)*DBLE(YukS1Lep1**INT(2.D0)))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*DBLE(YukS1Lep1**INT(2.D0)))/(MW2*PI2&
  &*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*DBLE(YukS1Lep1**INT(2.D0)))/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - &
  &1.D0*MH12)*DBLE(YukS1Quark1**INT(2.D0)))/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*DBLE(YukS1Quark1**INT(2.D0)&
  &))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*DBLE(YukS1Quark1**INT(2.D0)))/(MW2*PI2*SW2)) + DBLE(RR23**INT(2.&
  &D0))*((-0.03125D0*CS1S1S1f111*CS1S1S1f221)/PI2 - (0.03125D0*CS1S1S1f223*CS1S1S1f333)/PI2 + (0.125D0*CS1S3S3f212*CS1S3S3f221)/P&
  &I2 - (0.03125D0*CS2S2S1S1f2222*MA02)/PI2 - (0.03125D0*CS1S1S1f221*CS2S2S1f221*MA02)/(MH12*PI2) - (0.03125D0*CS1S1S1S1f2211*MH1&
  &2)/PI2 - (0.03125D0*CS1S1S1f222*CS2S2S1f222*MA02)/(MH22*PI2) - (0.03125D0*CS1S1S1f211*CS1S1S1f222*MH12)/(MH22*PI2) - (0.03125D&
  &0*CS1S1S1S1f2222*MH22)/PI2 - (0.03125D0*CS1S1S1f122*CS1S1S1f221*MH22)/(MH12*PI2) - (0.03125D0*CS1S1S1f223*CS2S2S1f223*MA02)/(M&
  &H32*PI2) - (0.03125D0*CS1S1S1f223*CS1S1S1f311*MH12)/(MH32*PI2) - (0.03125D0*CS1S1S1f223*CS1S1S1f322*MH22)/(MH32*PI2) - (0.0312&
  &5D0*CS1S1S1S1f2233*MH32)/PI2 - (0.03125D0*CS1S1S1f133*CS1S1S1f221*MH32)/(MH12*PI2) - (0.03125D0*CS1S1S1f222*CS1S1S1f233*MH32)/&
  &(MH22*PI2) - (0.0625D0*CS1S1S3S3f2222*MHp2)/PI2 - (0.0625D0*CS1S1S1f221*CS1S3S3f122*MHp2)/(MH12*PI2) - (0.0625D0*CS1S1S1f222*C&
  &S1S3S3f222*MHp2)/(MH22*PI2) - (0.0625D0*CS1S1S1f223*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.0625D0*CS1S1S3S3f2211*MW2)/PI2 - (0.0625&
  &D0*CS1S1S1f221*CS1S3S3f111*MW2)/(MH12*PI2) - (0.0625D0*CS1S1S1f222*CS1S3S3f211*MW2)/(MH22*PI2) - (0.0625D0*CS1S1S1f223*CS1S3S3&
  &f311*MW2)/(MH32*PI2) - (0.03125D0*CS2S2S1S1f1122*MZ2)/PI2 - (0.03125D0*CS1S1S1f221*CS2S2S1f111*MZ2)/(MH12*PI2) - (0.03125D0*CS&
  &1S1S1f222*CS2S2S1f112*MZ2)/(MH22*PI2) - (0.03125D0*CS1S1S1f223*CS2S2S1f113*MZ2)/(MH32*PI2) + (0.09375D0*CS1S1S1f221*EL2*MW2*((&
  &2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(MH12*PI2*SW2) + (0.09375D0*CS1S1S1f223*EL2*MW2*((2.D0*CB*MW*(-1.D0*C&
  &A1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.09375D0*CS1S1S1f222&
  &*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(MH22*PI2*&
  &SW2) + (0.03125D0*DBLE(CS1S1S1f211**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S1S1f212**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S1S1f213&
  &**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S1S1f223**INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS1S1S1f233**INT(2.D0)))/PI2 + (0.0625D0*DBL&
  &E(CS1S3S3f211**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S3S3f222**INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS2S2S1f112**INT(2.D0)))/PI2 + &
  &(0.0625D0*DBLE(CS2S2S1f122**INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS2S2S1f222**INT(2.D0)))/PI2 - (0.375D0*CS1S1S1f221*EL*YukS1Quar&
  &k1*DBLE(MB**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.375D0*CS1S1S1f222*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.375D&
  &0*CS1S1S1f223*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.375D0*CA2*CS1S1S1f221*EL*SA1*DBLE(MC**INT(4.D0)))/(MH1&
  &2*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f223*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.375&
  &D0*CS1S1S1f222*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f221*EL*YukS1Quark1*&
  &DBLE(MD**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.375D0*CS1S1S1f222*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.375D0*C&
  &S1S1S1f223*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.125D0*CS1S1S1f221*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MH12&
  &*MW*PI2*SW) - (0.125D0*CS1S1S1f222*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.125D0*CS1S1S1f223*EL*YukS1Lep3*DBLE&
  &(ME**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.125D0*CS1S1S1f221*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.125D0*CS1S1S1&
  &f222*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.125D0*CS1S1S1f223*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MH32*MW*PI2*&
  &SW) - (0.125D0*CS1S1S1f221*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.125D0*CS1S1S1f222*EL*YukS1Lep2*DBLE(MM**INT&
  &(4.D0)))/(MH22*MW*PI2*SW) - (0.125D0*CS1S1S1f223*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.375D0*CS1S1S1f221*EL*&
  &YukS1Quark1*DBLE(MS**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.375D0*CS1S1S1f222*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*MW*PI2*SW) &
  &- (0.375D0*CS1S1S1f223*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.375D0*CA2*CS1S1S1f221*EL*SA1*DBLE(MT**INT(4.D&
  &0)))/(MH12*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f223*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW*PI2*SB*SW)&
  & - (0.375D0*CS1S1S1f222*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.375D0*CA2*CS1S1S1f221*EL&
  &*SA1*DBLE(MU**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f223*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0))&
  &)/(MH32*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f222*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.093&
  &75D0*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MT2*(-1.&
  &D0*MH22 + 6.D0*MT2)*DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MU2*(-1.D0*MH22 + 6.D0*M&
  &U2)*DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2) + (0.125D0*EL2*MW2*(DBLE((-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA&
  &3)**INT(2.D0)) + DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3)**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH22 + MHp2) +&
  & MW2)*DBLE((CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)**INT(2.D0)))/(PI2*SW2) - (0.03125D0*&
  &EL2*(2.D0*MH22 + 2.D0*MW2)*DBLE((CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)**INT(2.D0)))/(PI2*SW&
  &2) + (0.0546875D0*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0&
  &*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)**INT(2.D0)))/PI2 + (0.046875D0*CS1S1S1f221*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (&
  &2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*PI2*SW2) + (0.046875D0*CS1S1S1f223*EL2*MZ2*((2.D0*CB*MW*(-1&
  &.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*&
  &MH32*PI2*SW2) + (0.046875D0*CS1S1S1f222*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1&
  &.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2*SW2) + (0.0625D0*EL2*MZ2*(DBLE((-1.D0*CA3*SA1 - 1.D0*C&
  &A1*SA2*SA3)**INT(2.D0)) + DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3)**INT(2.D0)))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.0156&
  &25D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*DBLE((CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA&
  &3)*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*DBLE((CB*(-1.D0*CA3*SA&
  &1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) + (0.02734375&
  &D0*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL +&
  & (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)**INT(2.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2 - (0.03125D0*EL2*ME2*(6.D0*ME2&
  & - 1.D0*MH22)*DBLE(YukS1Lep2**INT(2.D0)))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH22 + 6.D0*ML2)*DBLE(YukS1Lep2**INT(2.D0)&
  &))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH22 + 6.D0*MM2)*DBLE(YukS1Lep2**INT(2.D0)))/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(&
  &6.D0*MB2 - 1.D0*MH22)*DBLE(YukS1Quark2**INT(2.D0)))/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH22)*DBLE(YukS1Quark2&
  &**INT(2.D0)))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH22 + 6.D0*MS2)*DBLE(YukS1Quark2**INT(2.D0)))/(MW2*PI2*SW2)) + DBLE(R&
  &R33**INT(2.D0))*((-0.03125D0*CS1S1S1f111*CS1S1S1f331)/PI2 - (0.03125D0*CS1S1S1f222*CS1S1S1f332)/PI2 + (0.125D0*CS1S3S3f312*CS1&
  &S3S3f321)/PI2 - (0.03125D0*CS2S2S1S1f2233*MA02)/PI2 - (0.03125D0*CS1S1S1f331*CS2S2S1f221*MA02)/(MH12*PI2) - (0.03125D0*CS1S1S1&
  &S1f3311*MH12)/PI2 - (0.03125D0*CS1S1S1f332*CS2S2S1f222*MA02)/(MH22*PI2) - (0.03125D0*CS1S1S1f211*CS1S1S1f332*MH12)/(MH22*PI2) &
  &- (0.03125D0*CS1S1S1S1f3322*MH22)/PI2 - (0.03125D0*CS1S1S1f122*CS1S1S1f331*MH22)/(MH12*PI2) - (0.03125D0*CS1S1S1f333*CS2S2S1f2&
  &23*MA02)/(MH32*PI2) - (0.03125D0*CS1S1S1f311*CS1S1S1f333*MH12)/(MH32*PI2) - (0.03125D0*CS1S1S1f322*CS1S1S1f333*MH22)/(MH32*PI2&
  &) - (0.03125D0*CS1S1S1S1f3333*MH32)/PI2 - (0.03125D0*CS1S1S1f133*CS1S1S1f331*MH32)/(MH12*PI2) - (0.03125D0*CS1S1S1f233*CS1S1S1&
  &f332*MH32)/(MH22*PI2) - (0.0625D0*CS1S1S3S3f3322*MHp2)/PI2 - (0.0625D0*CS1S1S1f331*CS1S3S3f122*MHp2)/(MH12*PI2) - (0.0625D0*CS&
  &1S1S1f332*CS1S3S3f222*MHp2)/(MH22*PI2) - (0.0625D0*CS1S1S1f333*CS1S3S3f322*MHp2)/(MH32*PI2) - (0.0625D0*CS1S1S3S3f3311*MW2)/PI&
  &2 - (0.0625D0*CS1S1S1f331*CS1S3S3f111*MW2)/(MH12*PI2) - (0.0625D0*CS1S1S1f332*CS1S3S3f211*MW2)/(MH22*PI2) - (0.0625D0*CS1S1S1f&
  &333*CS1S3S3f311*MW2)/(MH32*PI2) - (0.03125D0*CS2S2S1S1f1133*MZ2)/PI2 - (0.03125D0*CS1S1S1f331*CS2S2S1f111*MZ2)/(MH12*PI2) - (0&
  &.03125D0*CS1S1S1f332*CS2S2S1f112*MZ2)/(MH22*PI2) - (0.03125D0*CS1S1S1f333*CS2S2S1f113*MZ2)/(MH32*PI2) + (0.09375D0*CS1S1S1f331&
  &*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(MH12*PI2*SW2) + (0.09375D0*CS1S1S1f333*EL2*MW2*((2.D0*CB*&
  &MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(MH32*PI2*SW2) + (0.09375D0*&
  &CS1S1S1f332*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))&
  &/(MH22*PI2*SW2) + (0.03125D0*DBLE(CS1S1S1f311**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S1S1f312**INT(2.D0)))/PI2 + (0.0625D0*DBLE(&
  &CS1S1S1f313**INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS1S1S1f322**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S1S1f323**INT(2.D0)))/PI2 + (0&
  &.0625D0*DBLE(CS1S3S3f311**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S3S3f322**INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS2S2S1f113**INT(2.D&
  &0)))/PI2 + (0.0625D0*DBLE(CS2S2S1f123**INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS2S2S1f223**INT(2.D0)))/PI2 - (0.375D0*CS1S1S1f331*E&
  &L*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.375D0*CS1S1S1f332*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MH22*MW*PI2*SW&
  &) - (0.375D0*CS1S1S1f333*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.375D0*CA2*CS1S1S1f331*EL*SA1*DBLE(MC**INT(4&
  &.D0)))/(MH12*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f333*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MH32*MW*PI2*SB*S&
  &W) - (0.375D0*CS1S1S1f332*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f331*EL*Y&
  &ukS1Quark1*DBLE(MD**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.375D0*CS1S1S1f332*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MH22*MW*PI2*SW) -&
  & (0.375D0*CS1S1S1f333*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.125D0*CS1S1S1f331*EL*YukS1Lep1*DBLE(ME**INT(4.&
  &D0)))/(MH12*MW*PI2*SW) - (0.125D0*CS1S1S1f332*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.125D0*CS1S1S1f333*EL*Yuk&
  &S1Lep3*DBLE(ME**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.125D0*CS1S1S1f331*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.12&
  &5D0*CS1S1S1f332*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.125D0*CS1S1S1f333*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(M&
  &H32*MW*PI2*SW) - (0.125D0*CS1S1S1f331*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.125D0*CS1S1S1f332*EL*YukS1Lep2*D&
  &BLE(MM**INT(4.D0)))/(MH22*MW*PI2*SW) - (0.125D0*CS1S1S1f333*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.375D0*CS1S&
  &1S1f331*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MH12*MW*PI2*SW) - (0.375D0*CS1S1S1f332*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MH22*&
  &MW*PI2*SW) - (0.375D0*CS1S1S1f333*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MH32*MW*PI2*SW) - (0.375D0*CA2*CS1S1S1f331*EL*SA1*DBLE(&
  &MT**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f333*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MH32*MW&
  &*PI2*SB*SW) - (0.375D0*CS1S1S1f332*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MH22*MW*PI2*SB*SW) - (0.375D0*CA2*CS1&
  &S1S1f331*EL*SA1*DBLE(MU**INT(4.D0)))/(MH12*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f333*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU*&
  &*INT(4.D0)))/(MH32*MW*PI2*SB*SW) - (0.375D0*CS1S1S1f332*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MH22*MW*PI2*SB*S&
  &W) - (0.09375D0*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2) - (0.093&
  &75D0*EL2*MT2*(-1.D0*MH32 + 6.D0*MT2)*DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*M&
  &U2*(-1.D0*MH32 + 6.D0*MU2)*DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2) + (0.125D0*EL2*MW2*(DBLE((-1&
  &.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0)) + DBLE((-1.D0*CA1*CA3*SA2 + SA1*SA3)**INT(2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(2.D&
  &0*MH32 + 2.D0*MW2)*DBLE((CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)**INT(2.D0)))/(PI2*SW2) - (0&
  &.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH32 + MHp2) + MW2)*DBLE((CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + S&
  &A1*SA3)*SB)**INT(2.D0)))/(PI2*SW2) + (0.0546875D0*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CB*MW*(-1.D0*CA1*CA3*SA&
  &2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)**INT(2.D0)))/PI2 + (0.046875D0*CS1S1S1f331*EL2*MZ&
  &2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*PI2*SW2) + (0.046875D0*CS1&
  &S1S1f333*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DB&
  &LE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2) + (0.046875D0*CS1S1S1f332*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA&
  &3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2*SW2) + (0.0625D0*EL2*&
  &MZ2*(DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0)) + DBLE((-1.D0*CA1*CA3*SA2 + SA1*SA3)**INT(2.D0)))*DBLE((CW2 + SW2)**I&
  &NT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH32 + 2.D0*MZ2)*DBLE((CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 &
  &- 1.D0*CA1*SA3)*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH32)&
  & + MZ2)*DBLE((CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT&
  &(2.D0)))/(CW2*PI2*SW2) + (0.02734375D0*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CB*MW*(-1.D0*&
  &CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)**INT(2.D0))*DBLE((CW2 + SW2)**INT(4.D0)&
  &))/PI2 - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32)*DBLE(YukS1Lep3**INT(2.D0)))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH32 &
  &+ 6.D0*ML2)*DBLE(YukS1Lep3**INT(2.D0)))/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH32 + 6.D0*MM2)*DBLE(YukS1Lep3**INT(2.D0)))&
  &/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH32)*DBLE(YukS1Quark3**INT(2.D0)))/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6&
  &.D0*MD2 - 1.D0*MH32)*DBLE(YukS1Quark3**INT(2.D0)))/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH32 + 6.D0*MS2)*DBLE(YukS1Quark3&
  &**INT(2.D0)))/(MW2*PI2*SW2))))/(MH12*DBLE(RR13**INT(2.D0)) + MH22*DBLE(RR23**INT(2.D0)) + MH32*DBLE(RR33**INT(2.D0))) - (0.5D0&
  &*vS*(2.D0*CA2*MH12*RR13*((0.5D0*CA3*((0.0625D0*CS1S1S1f111*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0&
  &*CS1S1S1f113*CS1S1S1f313)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.0625D0*CS1&
  &S1S1f133*CS1S1S1f333)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f311)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f1&
  &12*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f322)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f121*CS&
  &2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f223)/PI2 - (0.0625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.0625D0*CS1S1S1S1f1311*MH12)/&
  &PI2 - (0.0625D0*CS1S1S1S1f1322*MH22)/PI2 - (0.0625D0*CS1S1S1S1f1333*MH32)/PI2 - (0.125D0*CS1S1S3S3f1322*MHp2)/PI2 - (0.125D0*C&
  &S1S1S3S3f1311*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1113*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1&
  &*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA&
  &1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH32 + 2.D0*MW2)*(CA1*CA2*CB +&
  & CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp&
  &2 + 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 &
  &+ SA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH32 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*C&
  &A3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH12)&
  &*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*SA1*(-1.D0*CA3*SA1*&
  &SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)&
  &)/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH32 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2)&
  & - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*E&
  &L2*MU2*(-1.D0*MH32 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0&
  &*MH12)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.&
  &03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH32 + 6.D0*ML2)*YukS1L&
  &ep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*&
  &MM2*(-1.D0*MH32 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH12)*YukS1Quark1*YukS1Qu&
  &ark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6&
  &.D0*MB2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark&
  &3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.&
  &D0*MH32 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)&
  &/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT(4&
  &.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + &
  &SA1*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(&
  &-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D&
  &0*EL2*(2.D0*MH32 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*&
  &SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*C&
  &A1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*&
  &PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH32) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D&
  &0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR31*(R&
  &R11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S&
  &1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09&
  &375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0&
  &)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*P&
  &I2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.&
  &125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*&
  &EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2&
  &.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S&
  &1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.&
  &0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB&
  &*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*Yuk&
  &S1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW)&
  & - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D&
  &0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Qu&
  &ark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (&
  &0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA&
  &1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW&
  &2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125&
  &D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 &
  &+ (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)&
  &)/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT&
  &(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0))&
  &)/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW&
  &) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(&
  &MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*M&
  &W*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(&
  &CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR12*RR32*(RR12*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 -&
  & (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111&
  &*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(P&
  &I2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) -&
  & (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*&
  &EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quar&
  &k1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**I&
  &NT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**IN&
  &T(2.D0)))/(CW2*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322&
  &*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*&
  &CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.&
  &D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 &
  &- 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*Y&
  &ukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBL&
  &E(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.&
  &D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*&
  &SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*S&
  &B*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12&
  &)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S&
  &3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL&
  & + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0&
  &.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*&
  &PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0&
  &.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*&
  &(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)&
  &))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*&
  &SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR13*RR33*(RR13*((-0.03125D0*CS2S2S1f221*MA02&
  &)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1&
  &S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*M&
  &W*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL&
  &*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DB&
  &LE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4&
  &.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*P&
  &I2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*&
  &CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S&
  &1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0&
  &.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)&
  &*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW&
  &*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(M&
  &D**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)&
  &))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2&
  &*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1&
  &.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.&
  &D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f2&
  &22*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.062&
  &5D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW&
  &*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Qu&
  &ark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D&
  &0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1&
  &Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(&
  &MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*&
  &CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*S&
  &A3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D&
  &0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D&
  &0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4&
  &.D0)))/PI2))/(MH12 - 1.D0*MH32) + (0.5D0*SA3*((0.0625D0*CS1S1S1f111*CS1S1S1f211)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f212)/PI2 +&
  & (0.125D0*CS1S1S1f113*CS1S1S1f213)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f222)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f223)/PI2 + (0.0&
  &625D0*CS1S1S1f133*CS1S1S1f233)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f211)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f212)/PI2 + (0.125D0*&
  &CS1S3S3f112*CS1S3S3f221)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f222)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f112)/PI2 + (0.125D0*CS2S2&
  &S1f121*CS2S2S1f122)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f222)/PI2 - (0.0625D0*CS2S2S1S1f2212*MA02)/PI2 - (0.0625D0*CS1S1S1S1f12&
  &11*MH12)/PI2 - (0.0625D0*CS1S1S1S1f1222*MH22)/PI2 - (0.0625D0*CS1S1S1S1f1233*MH32)/PI2 - (0.125D0*CS1S1S3S3f1222*MHp2)/PI2 - (&
  &0.125D0*CS1S1S3S3f1211*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1112*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*S&
  &A3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB*SA1 - &
  &1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-&
  &1.D0*MHp2 + 2.D0*(MH22 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - &
  &1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*C&
  &A1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(C&
  &B*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*M&
  &H12)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*SA1*(CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2&
  &*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH22 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL&
  &2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH22 + 6.D0*&
  &MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep2)/(MW2&
  &*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH22)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.&
  &D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH22 + 6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (&
  &0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH22 + 6.D0*MM2)*YukS&
  &1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0&
  &*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH22)*YukS1Quark1&
  &*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH22)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL&
  &2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH22 + 6.D0*MS2)*YukS1Quark1*&
  &YukS1Quark2)/(MW2*PI2*SW2) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1&
  & - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 +&
  & (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*DBLE((CW2 + SW2)**INT(2.&
  &D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.&
  &D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-&
  &1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - &
  &1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*S&
  &A1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) -&
  & (0.015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR21*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2&
  & - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f&
  &122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)&
  &/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*&
  &DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME&
  &**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0))&
  &)/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB&
  &*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*M&
  &W*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f3&
  &11*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625&
  &D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/&
  &EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*&
  &SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(M&
  &W*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*C&
  &A1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW&
  &*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA&
  &02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*C&
  &S1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.&
  &D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*&
  &DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*&
  &YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*&
  &DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**I&
  &NT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 -&
  & 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*S&
  &W)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL&
  &*RR12*RR22*(RR12*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0&
  &.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2&
  &)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE&
  &(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(&
  &4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*&
  &PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - &
  &(0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D&
  &0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR32*((-0.0&
  &3125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH&
  &32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*&
  &MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0&
  &.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(&
  &MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*&
  &SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375&
  &D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*P&
  &I2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB&
  &*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0))&
  &)/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/P&
  &I2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f&
  &112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA&
  &3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)&
  &*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(M&
  &E**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)&
  &))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**&
  &INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ&
  &2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**&
  &INT(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR13*RR23*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/P&
  &I2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3&
  &f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)&
  &)/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*S&
  &W) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.12&
  &5D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1&
  &Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(M&
  &U**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)&
  &**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1&
  &f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.0312&
  &5D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 &
  &- 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*&
  &SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*&
  &EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3&
  &*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 &
  &- 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*&
  &PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA&
  &3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*&
  &MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*&
  &CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW&
  &)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/&
  &(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0&
  &*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4&
  &.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*&
  &SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*&
  &CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL&
  &)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(MH12 - 1.D0*MH22)) + 2.D0&
  &*MH22*RR23*(-1.D0*SA2*SA3*((0.5D0*CA3*((0.0625D0*CS1S1S1f111*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125&
  &D0*CS1S1S1f113*CS1S1S1f313)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.0625D0*C&
  &S1S1S1f133*CS1S1S1f333)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f311)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3&
  &f112*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f322)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f121*&
  &CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f223)/PI2 - (0.0625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.0625D0*CS1S1S1S1f1311*MH12&
  &)/PI2 - (0.0625D0*CS1S1S1S1f1322*MH22)/PI2 - (0.0625D0*CS1S1S1S1f1333*MH32)/PI2 - (0.125D0*CS1S1S3S3f1322*MHp2)/PI2 - (0.125D0&
  &*CS1S1S3S3f1311*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1113*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + C&
  &A1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*&
  &CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH32 + 2.D0*MW2)*(CA1*CA2*CB&
  & + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*M&
  &Hp2 + 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA&
  &2 + SA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH32 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0&
  &*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH1&
  &2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*SA1*(-1.D0*CA3*SA&
  &1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA&
  &3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH32 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW&
  &2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2&
  &*EL2*MU2*(-1.D0*MH32 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.&
  &D0*MH12)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (&
  &0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH32 + 6.D0*ML2)*YukS&
  &1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL&
  &2*MM2*(-1.D0*MH32 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH12)*YukS1Quark1*YukS1&
  &Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*&
  &(6.D0*MB2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH32)*YukS1Quark1*YukS1Qua&
  &rk3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-&
  &1.D0*MH32 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*S&
  &W)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT&
  &(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 &
  &+ SA1*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB&
  &*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.01562&
  &5D0*EL2*(2.D0*MH32 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3&
  &)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12) + MZ2)*(CA2*CB*SA1 - 1.D0&
  &*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW&
  &2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH32) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1&
  &.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR31*&
  &(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS&
  &1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.&
  &09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.&
  &D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW&
  &*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (&
  &0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA&
  &2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*(&
  &(2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S&
  &2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (&
  &0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*&
  &CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*Y&
  &ukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*S&
  &W) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.12&
  &5D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1&
  &Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) -&
  & (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*&
  &CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*&
  &SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.031&
  &25D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI&
  &2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/E&
  &L))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**I&
  &NT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0&
  &)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*&
  &SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))&
  &/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB&
  &*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))&
  &/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR12*RR32*(RR12*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2&
  & - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f1&
  &11*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/&
  &(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW)&
  & - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D&
  &0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Qu&
  &ark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU*&
  &*INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**&
  &INT(2.D0)))/(CW2*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f3&
  &22*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D&
  &0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - &
  &1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA&
  &2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL&
  &*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*D&
  &BLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - &
  &1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI&
  &2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)&
  &*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH&
  &12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS&
  &1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/&
  &EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - &
  &(0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(M&
  &W*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - &
  &(0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*E&
  &L*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D&
  &0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA&
  &1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR13*RR33*(RR13*((-0.03125D0*CS2S2S1f221*MA&
  &02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*C&
  &S1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB&
  &*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*&
  &EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*&
  &DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT&
  &(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW&
  &*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D&
  &0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS&
  &1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - &
  &(0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA&
  &3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(&
  &MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE&
  &(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D&
  &0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*P&
  &I2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 -&
  & 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (&
  &2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1&
  &f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0&
  &625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*&
  &MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1&
  &Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.37&
  &5D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*Yuk&
  &S1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBL&
  &E(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA&
  &1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2&
  &*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.054687&
  &5D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1&
  &.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT&
  &(4.D0)))/PI2))/(MH12 - 1.D0*MH32) + (0.5D0*SA3*((0.0625D0*CS1S1S1f111*CS1S1S1f211)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f212)/PI2&
  & + (0.125D0*CS1S1S1f113*CS1S1S1f213)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f222)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f223)/PI2 + (0&
  &.0625D0*CS1S1S1f133*CS1S1S1f233)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f211)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f212)/PI2 + (0.125D&
  &0*CS1S3S3f112*CS1S3S3f221)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f222)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f112)/PI2 + (0.125D0*CS2&
  &S2S1f121*CS2S2S1f122)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f222)/PI2 - (0.0625D0*CS2S2S1S1f2212*MA02)/PI2 - (0.0625D0*CS1S1S1S1f&
  &1211*MH12)/PI2 - (0.0625D0*CS1S1S1S1f1222*MH22)/PI2 - (0.0625D0*CS1S1S1S1f1233*MH32)/PI2 - (0.125D0*CS1S1S3S3f1222*MHp2)/PI2 -&
  & (0.125D0*CS1S1S3S3f1211*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1112*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2&
  &*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB*SA1 &
  &- 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*&
  &(-1.D0*MHp2 + 2.D0*(MH22 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 &
  &- 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0&
  &*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*&
  &(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0&
  &*MH12)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*SA1*(CA1*CA3 - 1.D0&
  &*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*S&
  &B2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH22 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*&
  &EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH22 + 6.D&
  &0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep2)/(M&
  &W2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH22)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + &
  &6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH22 + 6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) -&
  & (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH22 + 6.D0*MM2)*Yu&
  &kS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375&
  &D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH22)*YukS1Quar&
  &k1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH22)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*&
  &EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH22 + 6.D0*MS2)*YukS1Quark&
  &1*YukS1Quark2)/(MW2*PI2*SW2) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*S&
  &A1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2&
  & + (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*DBLE((CW2 + SW2)**INT(&
  &2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - &
  &1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*&
  &(-1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 &
  &- 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + CA2&
  &*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)&
  & - (0.015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR21*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/P&
  &I2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S&
  &3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*S&
  &W)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA&
  &1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(&
  &ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0&
  &)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*&
  &SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2&
  &*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1&
  &f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.06&
  &25D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW&
  &)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI&
  &2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**&
  &INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/&
  &(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW&
  &) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0&
  &*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*&
  &MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*&
  &MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0&
  &*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-&
  &1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark&
  &2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*E&
  &L*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep&
  &2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS*&
  &*INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3&
  & - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)&
  &*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*&
  &EL*RR12*RR22*(RR12*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - &
  &(0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*M&
  &Z2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DB&
  &LE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(M&
  &W*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.04687&
  &5D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR32*((-0&
  &.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*&
  &MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL&
  &2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - &
  &(0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))&
  &/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI&
  &2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.3&
  &75D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW&
  &*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*&
  &CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0&
  &)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)&
  &/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S&
  &1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*&
  &SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA&
  &3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE&
  &(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D&
  &0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT&
  &**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*&
  &MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)&
  &**INT(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR13*RR23*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)&
  &/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3&
  &S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/E&
  &L))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB&
  &*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.&
  &125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*Yuk&
  &S1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE&
  &(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW&
  &2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1&
  &S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03&
  &125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA&
  &2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA&
  &1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D&
  &0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Le&
  &p3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA&
  &2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(M&
  &W*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*&
  &SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f21&
  &1*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D&
  &0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*&
  &SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW&
  &) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0))&
  &)/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW&
  &) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375&
  &D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT&
  &(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D&
  &0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D&
  &0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/&
  &EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(MH12 - 1.D0*MH22)) + CA&
  &2*CA3*((0.5D0*SA2*SA3*((0.0625D0*CS1S1S1f111*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f113*C&
  &S1S1S1f313)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1&
  &S1f333)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f311)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f321&
  &)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f322)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f123)/PI2&
  & + (0.0625D0*CS2S2S1f221*CS2S2S1f223)/PI2 - (0.0625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.0625D0*CS1S1S1S1f1311*MH12)/PI2 - (0.0625D&
  &0*CS1S1S1S1f1322*MH22)/PI2 - (0.0625D0*CS1S1S1S1f1333*MH32)/PI2 - (0.125D0*CS1S1S3S3f1322*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1311*&
  &MW2)/PI2 - (0.0625D0*CS2S2S1S1f1113*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA&
  &1*CA3*SA2 + SA1*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA&
  &1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH32 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(&
  &CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12&
  & + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)&
  &)/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH32 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1&
  &.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH12)*SA1*(-1.D0*CA&
  &3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1&
  &*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2&
  &*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH32 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*&
  &CA2*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*&
  &MH32 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*YukS1Le&
  &p1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML&
  &2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH32 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)&
  &/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH3&
  &2 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)/(MW2*PI2&
  &*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0&
  &*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW&
  &2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH32 + 6.D0&
  &*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB&
  &*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW*&
  &*INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*DBLE&
  &((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*&
  &SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH&
  &32 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 &
  &+ SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB&
  &*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.&
  &015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH32) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1&
  &.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR31*(RR11*((-0.03125&
  &D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/&
  &PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*&
  &((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW&
  &) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.12&
  &5D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1&
  &Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT&
  &**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB&
  &*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI&
  &2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3&
  &f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1&
  &*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(&
  &MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL&
  &*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3&
  &*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**&
  &INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1&
  &.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA&
  &1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-&
  &0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233&
  &*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*E&
  &L2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - &
  &(0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*P&
  &I2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*E&
  &L*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) &
  &- (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*S&
  &A1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)))&
  &)/(CB*MW*SW) + (0.5D0*EL*RR12*RR32*(RR12*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS&
  &1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0&
  &.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.3&
  &75D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*Y&
  &ukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*D&
  &BLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW&
  &*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2&
  &*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (&
  &0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ&
  &2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB&
  &*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3&
  &)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(&
  &ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0&
  &)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DB&
  &LE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.04&
  &6875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE&
  &((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.031&
  &25D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/P&
  &I2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(C&
  &A1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1&
  &*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.1&
  &25D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS&
  &1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D&
  &0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*&
  &SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW&
  &)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR13*RR33*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.031&
  &25D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)&
  &/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.&
  &D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**&
  &INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D&
  &0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2&
  &*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0&
  &.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*&
  &SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/&
  &PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S&
  &3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D&
  &0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.&
  &375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))&
  &/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW)&
  & - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D&
  &0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*D&
  &BLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*C&
  &A3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 -&
  & (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f22&
  &2*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA&
  &1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**&
  &INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quar&
  &k2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**&
  &INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0))&
  &)/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (&
  &2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CA1*C&
  &A2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 &
  &- 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(&
  &CA2*(MH12 - 1.D0*MH32)) - (0.5D0*CA3*SA2*((0.0625D0*CS1S1S1f111*CS1S1S1f211)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f212)/PI2 + (0.&
  &125D0*CS1S1S1f113*CS1S1S1f213)/PI2 + (0.0625D0*CS1S1S1f122*CS1S1S1f222)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f223)/PI2 + (0.0625D&
  &0*CS1S1S1f133*CS1S1S1f233)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S3f211)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f212)/PI2 + (0.125D0*CS1S&
  &3S3f112*CS1S3S3f221)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f222)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f112)/PI2 + (0.125D0*CS2S2S1f1&
  &21*CS2S2S1f122)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f222)/PI2 - (0.0625D0*CS2S2S1S1f2212*MA02)/PI2 - (0.0625D0*CS1S1S1S1f1211*M&
  &H12)/PI2 - (0.0625D0*CS1S1S1S1f1222*MH22)/PI2 - (0.0625D0*CS1S1S1S1f1233*MH32)/PI2 - (0.125D0*CS1S1S3S3f1222*MHp2)/PI2 - (0.12&
  &5D0*CS1S1S3S3f1211*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1112*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) &
  &+ CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0&
  &*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0&
  &*MHp2 + 2.D0*(MH22 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0&
  &*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*S&
  &A2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-&
  &1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH12)&
  &*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*SA1*(CA1*CA3 - 1.D0*SA1*S&
  &A2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2&
  &) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH22 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU&
  &2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH22 + 6.D0*MU2)&
  &*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep2)/(MW2*PI2&
  &*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH22)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*M&
  &L2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH22 + 6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03&
  &125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH22 + 6.D0*MM2)*YukS1Lep&
  &1*YukS1Lep2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2&
  &*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH22)*YukS1Quark1*Yuk&
  &S1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH22)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS&
  &2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH22 + 6.D0*MS2)*YukS1Quark1*YukS&
  &1Quark2)/(MW2*PI2*SW2) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1&
  &.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.&
  &125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0))&
  &)/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0&
  &*MA02 + 2.D0*(MA02 + MH22) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0&
  &*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*S&
  &B)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.&
  &015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2&
  &*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR21*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (&
  &0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*&
  &MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL &
  &+ (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE&
  &(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(M&
  &W*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW)&
  & - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA&
  &1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*M&
  &H12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*C&
  &S1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL +&
  & (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.&
  &D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI&
  &2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0&
  &.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*S&
  &A3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1&
  &.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/&
  &PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3&
  &S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*C&
  &A3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE&
  &(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS&
  &1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE&
  &(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4&
  &.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D&
  &0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/E&
  &L + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR1&
  &2*RR22*(RR12*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.031&
  &25D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI&
  &2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB*&
  &*INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0&
  &)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*&
  &SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.3&
  &75D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL&
  &2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR32*((-0.03125&
  &D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/&
  &PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*&
  &((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375&
  &D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*P&
  &I2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*E&
  &L*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*S&
  &B*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*&
  &(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(C&
  &W2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 -&
  & (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*&
  &MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*S&
  &B*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBL&
  &E(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**I&
  &NT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(&
  &MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(&
  &4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((&
  &2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(&
  &2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR13*RR23*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 -&
  & (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111&
  &*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(P&
  &I2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) -&
  & (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*&
  &EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quar&
  &k1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**I&
  &NT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**IN&
  &T(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322&
  &*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*&
  &CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.&
  &D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 &
  &- 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*Y&
  &ukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBL&
  &E(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.&
  &D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*&
  &SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*S&
  &B*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12&
  &)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S&
  &3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL&
  & + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0&
  &.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*&
  &PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0&
  &.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*&
  &(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)&
  &))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*&
  &SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*&
  &MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DB&
  &LE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(CA2*(MH12 - 1.D0*MH22)) + (0.&
  &5D0*((0.0625D0*CS1S1S1f211*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f212*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f213*CS1S1S1f313)/PI2 + &
  &(0.0625D0*CS1S1S1f222*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f223*CS1S1S1f323)/PI2 + (0.0625D0*CS1S1S1f233*CS1S1S1f333)/PI2 + (0.1&
  &25D0*CS1S3S3f211*CS1S3S3f311)/PI2 + (0.125D0*CS1S3S3f221*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f212*CS1S3S3f321)/PI2 + (0.125D0*C&
  &S1S3S3f222*CS1S3S3f322)/PI2 + (0.0625D0*CS2S2S1f112*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f122*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2&
  &S1f222*CS2S2S1f223)/PI2 - (0.0625D0*CS2S2S1S1f2223*MA02)/PI2 - (0.0625D0*CS1S1S1S1f2311*MH12)/PI2 - (0.0625D0*CS1S1S1S1f2322*M&
  &H22)/PI2 - (0.0625D0*CS1S1S1S1f2333*MH32)/PI2 - (0.125D0*CS1S1S3S3f2322*MHp2)/PI2 - (0.125D0*CS1S1S3S3f2311*MW2)/PI2 - (0.0625&
  &D0*CS2S2S1S1f1123*MZ2)/PI2 + (0.25D0*EL2*MW2*((-1.D0*CA1*CA3*SA2 + SA1*SA3)*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (-1.D0*CA3*SA&
  &1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH22 + MHp2) + MW2)*(CB*(-&
  &1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*S&
  &A1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH32 + MHp2) + MW2)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*C&
  &A1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB&
  &))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 + 2.D0*MW2)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB&
  &)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH32 + 2.D0*MW2)&
  &*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 &
  &- 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.09375D0*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1&
  &.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - &
  &1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MT2*(-1.D0*MH22 + 6.D0*MT2)*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 &
  &- 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MT2*(-1.D0*MH32 + 6.D0*MT2)*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA&
  &3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MU2*(-1.D0*MH22 + 6.D0*MU2)*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*&
  &CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MU2*(-1.D0*MH32 + 6.D0*MU2)*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA&
  &1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH22)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - &
  &(0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH22 + 6.D0*ML2)*YukS&
  &1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH32 + 6.D0*ML2)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL&
  &2*MM2*(-1.D0*MH22 + 6.D0*MM2)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH32 + 6.D0*MM2)*YukS1Lep2*YukS1L&
  &ep3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH22)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.&
  &D0*MD2 - 1.D0*MH22)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH32)*YukS1Quark2*YukS1Quark3&
  &)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH32)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0&
  &*MH22 + 6.D0*MS2)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH32 + 6.D0*MS2)*YukS1Quark2*YukS1Quark3)&
  &/(MW2*PI2*SW2) + (0.109375D0*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*S&
  &B*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**I&
  &NT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*((-1.D0*CA1*CA3*SA2 + SA1*SA3)*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (-1&
  &.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-&
  &1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1&
  &*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625&
  &D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH32) + MZ2)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)&
  &*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - &
  &(0.015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*C&
  &A3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2&
  &.D0*MH32 + 2.D0*MZ2)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*&
  &SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR21*RR31*(RR11*((-0&
  &.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*&
  &MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL&
  &2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*&
  &PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) -&
  & (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL&
  &*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*D&
  &BLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*&
  &CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA&
  &02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*C&
  &S1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.&
  &D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3&
  &*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.37&
  &5D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*Yuk&
  &S1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBL&
  &E(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*&
  &EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA&
  &2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR&
  &21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1&
  &S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.093&
  &75D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*S&
  &W2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))&
  &/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI&
  &2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.3&
  &75D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*S&
  &B*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0&
  &*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*&
  &SW2))))/(CB*MW*SW) + (0.5D0*EL*RR22*RR32*(RR12*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.0312&
  &5D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI&
  &2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) &
  &- (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D&
  &0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1&
  &Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(&
  &MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)&
  &))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0))&
  &)/(CW2*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/P&
  &I2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f&
  &113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*S&
  &A3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*C&
  &A1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3&
  &*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*S&
  &A3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) +&
  & (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL&
  &)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - &
  &(0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*&
  &MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0&
  &*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*E&
  &L*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*E&
  &L*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3&
  & - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*P&
  &I2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)&
  &*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR23*RR33*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - &
  &(0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122&
  &*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL&
  & + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBL&
  &E(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**I&
  &NT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(&
  &MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW&
  &) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*S&
  &A1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*&
  &MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*&
  &CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL &
  &+ (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW)&
  & - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4&
  &.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*P&
  &I2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (&
  &0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*&
  &SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-&
  &1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)&
  &/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S&
  &3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*&
  &CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBL&
  &E(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*Yuk&
  &S1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBL&
  &E(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(&
  &4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.&
  &D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/&
  &EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0&
  &*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*S&
  &A1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE&
  &(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(MH22 - 1.D0*MH32))) + 2.D0*MH32*RR33*(-1.D0*CA3*SA2*((0.5D0*CA3*((0.0625&
  &D0*CS1S1S1f111*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f313)/PI2 + (0.0625D0*CS&
  &1S1S1f122*CS1S1S1f322)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f333)/PI2 + (0.125D0*CS1S3S3&
  &f111*CS1S3S3f311)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f122*C&
  &S1S3S3f322)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2&
  &S1f223)/PI2 - (0.0625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.0625D0*CS1S1S1S1f1311*MH12)/PI2 - (0.0625D0*CS1S1S1S1f1322*MH22)/PI2 - (&
  &0.0625D0*CS1S1S1S1f1333*MH32)/PI2 - (0.125D0*CS1S1S3S3f1322*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1311*MW2)/PI2 - (0.0625D0*CS2S2S1S1&
  &f1113*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)))/(PI2*SW&
  &2) - (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - &
  &1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH32 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA&
  &3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB*SA1 - &
  &1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(&
  &-1.D0*MHp2 + 2.D0*(MH32 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1&
  &*CA3*SA2 + SA1*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH12)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW&
  &2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.&
  &09375D0*CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2&
  &*(-1.D0*MH32 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH12 + 6.D0&
  &*MU2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH32 + 6.D0*MU2)*SA1*(-1.D0*CA&
  &3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - &
  &(0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS&
  &1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH32 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL&
  &2*MM2*(-1.D0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH32 + 6.D0*MM2)*YukS1Lep1*YukS1L&
  &ep3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.&
  &D0*MD2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark3&
  &)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0&
  &*MH12 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH32 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)&
  &/(MW2*PI2*SW2) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*S&
  &A3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*E&
  &L2*MZ2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2&
  &*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA&
  &1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH32 + 2.D0*MZ2)*(CA1*CA2*CB + C&
  &A2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*S&
  &W2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1&
  &*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D&
  &0*(MA02 + MH32) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*S&
  &A3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR31*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.&
  &03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MH&
  &p2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + &
  &(2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(M&
  &C**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(&
  &4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*&
  &PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) -&
  & (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*&
  &SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH1&
  &2)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1&
  &S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (&
  &2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - &
  &(0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0&
  &)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*&
  &SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.3&
  &75D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3&
  &)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D&
  &0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI&
  &2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3&
  &f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3&
  &*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(M&
  &B**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Q&
  &uark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(M&
  &L**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D&
  &0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*&
  &SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL &
  &+ (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR12*&
  &RR32*(RR12*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125&
  &D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 &
  &+ (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**I&
  &NT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0))&
  &)/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW&
  &) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375&
  &D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*&
  &MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR32*((-0.03125D0&
  &*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI&
  &2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((&
  &2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0&
  &*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2&
  &*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - &
  &(0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*&
  &YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*&
  &SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-&
  &1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2&
  &*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (&
  &0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ&
  &2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*&
  &SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(&
  &MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT&
  &(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW&
  &*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.&
  &D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.&
  &D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.&
  &D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR13*RR33*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (&
  &0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*M&
  &W2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2&
  &*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (&
  &0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL&
  &*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1&
  &*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT&
  &(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(&
  &2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*M&
  &H22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS&
  &2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0&
  &*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - &
  &1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*Yuk&
  &S1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(&
  &MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0&
  &*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB&
  &*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*&
  &SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/&
  &PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S&
  &3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL +&
  & (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.3&
  &75D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI&
  &2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.1&
  &25D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(C&
  &A1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))&
  &/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA&
  &2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW&
  &*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBL&
  &E(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(MH12 - 1.D0*MH32) + (0.5D0*SA3&
  &*((0.0625D0*CS1S1S1f111*CS1S1S1f211)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f212)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f213)/PI2 + (0.&
  &0625D0*CS1S1S1f122*CS1S1S1f222)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f223)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f233)/PI2 + (0.125D&
  &0*CS1S3S3f111*CS1S3S3f211)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f212)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f221)/PI2 + (0.125D0*CS1S&
  &3S3f122*CS1S3S3f222)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f112)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f122)/PI2 + (0.0625D0*CS2S2S1f&
  &221*CS2S2S1f222)/PI2 - (0.0625D0*CS2S2S1S1f2212*MA02)/PI2 - (0.0625D0*CS1S1S1S1f1211*MH12)/PI2 - (0.0625D0*CS1S1S1S1f1222*MH22&
  &)/PI2 - (0.0625D0*CS1S1S1S1f1233*MH32)/PI2 - (0.125D0*CS1S1S3S3f1222*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1211*MW2)/PI2 - (0.0625D0*&
  &CS2S2S1S1f1112*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))&
  &/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2&
  &*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH22 + MHp2) + MW2)*(CA2*C&
  &B*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D&
  &0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*S&
  &B))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*&
  &CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH12)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*&
  &PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*&
  &CA2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH22 +&
  & 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(CA1*CA3 -&
  & 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH22 + 6.D0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*&
  &PI2*SB2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1&
  &.D0*MH22)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) -&
  & (0.03125D0*EL2*ML2*(-1.D0*MH22 + 6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*Yu&
  &kS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH22 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.09375D0*&
  &EL2*MB2*(6.D0*MB2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*&
  &YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH22)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2&
  &*MD2*(6.D0*MD2 - 1.D0*MH22)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*Yu&
  &kS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH22 + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) + (0.109375D0*((2&
  &.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*&
  &CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - &
  &1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0&
  &*MA02 + 2.D0*(MA02 + MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0&
  &*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*(CA2*C&
  &B*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(&
  &2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3&
  &) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*(CA1&
  &*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/&
  &(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR21*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.031&
  &25D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/P&
  &I2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2)&
  & - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375&
  &D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS&
  &1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE&
  &(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0&
  &)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)&
  &))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/&
  &PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1&
  &f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*&
  &SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*&
  &CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep&
  &3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**I&
  &NT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*&
  &SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) &
  &+ (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/E&
  &L)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 -&
  & (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211&
  &*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D&
  &0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*&
  &EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW)&
  & - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*&
  &EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA&
  &3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*&
  &PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3&
  &)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR12*RR22*(RR12*((-0.03125D0*CS2S2S1f221*MA&
  &02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*C&
  &S1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB&
  &*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*&
  &EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*&
  &DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT&
  &(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW&
  &*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D&
  &0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS&
  &1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - &
  &(0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA&
  &3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(&
  &MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE&
  &(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D&
  &0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*P&
  &I2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 -&
  & 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (&
  &2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1&
  &f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0&
  &625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*&
  &MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1&
  &Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.37&
  &5D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*Yuk&
  &S1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBL&
  &E(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA&
  &1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2&
  &*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (R&
  &R13*RR23*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.0&
  &3125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/&
  &PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(M&
  &B**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.&
  &D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI&
  &2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0&
  &.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*&
  &EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.031&
  &25D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32&
  &)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW&
  &2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.3&
  &75D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW&
  &*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW&
  &) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0&
  &*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2&
  &*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*M&
  &W*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/&
  &(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2&
  & - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f11&
  &2*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)&
  &*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*D&
  &BLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME*&
  &*INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))&
  &/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**IN&
  &T(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*&
  &((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**IN&
  &T(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA&
  &3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*D&
  &BLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(MH12 - 1.D0*MH22)) - 1.D0*CA2*SA3*((0.5D0*SA2*SA3*((0.0625D0*CS1S1S1f&
  &111*CS1S1S1f311)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f313)/PI2 + (0.0625D0*CS1S1S1f122*C&
  &S1S1S1f322)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f323)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f333)/PI2 + (0.125D0*CS1S3S3f111*CS1S3S&
  &3f311)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f122*CS1S3S3f322)&
  &/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f221*CS2S2S1f223)/PI2&
  & - (0.0625D0*CS2S2S1S1f2213*MA02)/PI2 - (0.0625D0*CS1S1S1S1f1311*MH12)/PI2 - (0.0625D0*CS1S1S1S1f1322*MH22)/PI2 - (0.0625D0*CS&
  &1S1S1S1f1333*MH32)/PI2 - (0.125D0*CS1S1S3S3f1322*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1311*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1113*MZ2)/&
  &PI2 + (0.25D0*EL2*MW2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)))/(PI2*SW2) - (0.031&
  &25D0*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA&
  &3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH32 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0&
  &*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA&
  &2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 &
  &+ 2.D0*(MH32 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + &
  &SA1*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH12)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*S&
  &W2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2&
  &*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH3&
  &2 + 6.D0*MT2)*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(&
  &-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH32 + 6.D0*MU2)*SA1*(-1.D0*CA3*SA1*SA2 -&
  & 1.D0*CA1*SA3))/(MW2*PI2*SB2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*&
  &EL2*ME2*(6.D0*ME2 - 1.D0*MH32)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1&
  &Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH32 + 6.D0*ML2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D&
  &0*MH12 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH32 + 6.D0*MM2)*YukS1Lep1*YukS1Lep3)/(MW2*P&
  &I2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.&
  &D0*MH12)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*&
  &SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH32)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D&
  &0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH32 + 6.D0*MS2)*YukS1Quark1*YukS1Quark3)/(MW2*PI2*S&
  &W2) + (0.109375D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL &
  &+ (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA2&
  &*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) -&
  & (0.015625D0*EL2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D&
  &0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH32 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*&
  &(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.01&
  &5625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D&
  &0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + M&
  &H32) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*DBL&
  &E((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR31*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1&
  &S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (&
  &0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*M&
  &W*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0&
  &)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW&
  &*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (&
  &0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*C&
  &A2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*&
  &DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0&
  &.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW&
  &2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1&
  &.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL&
  &*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2&
  &*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.12&
  &5D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1&
  &.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**&
  &INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*S&
  &A2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.0312&
  &5D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/&
  &PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0&
  &*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0&
  &)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(&
  &MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0&
  &)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI&
  &2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3&
  &)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*&
  &(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR12*RR32*(RR12*&
  &((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f&
  &133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D&
  &0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/&
  &(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*S&
  &W) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D&
  &0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*S&
  &A1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*&
  &CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f22&
  &3*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625&
  &D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*&
  &(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Qu&
  &ark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (&
  &0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL&
  &*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3&
  &*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.37&
  &5D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA&
  &3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) &
  &+ RR22*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*C&
  &S1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0&
  &.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(P&
  &I2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D&
  &0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(M&
  &W*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - &
  &(0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*P&
  &I2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-&
  &1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*&
  &PI2*SW2))))/(MW*SB*SW) + (RR13*RR33*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*C&
  &S1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (&
  &0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.&
  &375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*&
  &YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*&
  &DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**I&
  &NT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(M&
  &W*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW&
  &2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - &
  &(0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*M&
  &Z2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*S&
  &B*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA&
  &3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE&
  &(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D&
  &0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*D&
  &BLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.0&
  &46875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBL&
  &E((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03&
  &125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/&
  &PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(&
  &CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA&
  &1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.&
  &125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*Yuk&
  &S1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.&
  &D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB&
  &*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*S&
  &W)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)&
  &/EL)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE(CW**INT(-&
  &4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(CA2*(MH12 - 1.D0*MH32)) - (0.5D0*CA3*SA2*&
  &((0.0625D0*CS1S1S1f111*CS1S1S1f211)/PI2 + (0.125D0*CS1S1S1f112*CS1S1S1f212)/PI2 + (0.125D0*CS1S1S1f113*CS1S1S1f213)/PI2 + (0.0&
  &625D0*CS1S1S1f122*CS1S1S1f222)/PI2 + (0.125D0*CS1S1S1f123*CS1S1S1f223)/PI2 + (0.0625D0*CS1S1S1f133*CS1S1S1f233)/PI2 + (0.125D0&
  &*CS1S3S3f111*CS1S3S3f211)/PI2 + (0.125D0*CS1S3S3f121*CS1S3S3f212)/PI2 + (0.125D0*CS1S3S3f112*CS1S3S3f221)/PI2 + (0.125D0*CS1S3&
  &S3f122*CS1S3S3f222)/PI2 + (0.0625D0*CS2S2S1f111*CS2S2S1f112)/PI2 + (0.125D0*CS2S2S1f121*CS2S2S1f122)/PI2 + (0.0625D0*CS2S2S1f2&
  &21*CS2S2S1f222)/PI2 - (0.0625D0*CS2S2S1S1f2212*MA02)/PI2 - (0.0625D0*CS1S1S1S1f1211*MH12)/PI2 - (0.0625D0*CS1S1S1S1f1222*MH22)&
  &/PI2 - (0.0625D0*CS1S1S1S1f1233*MH32)/PI2 - (0.125D0*CS1S1S3S3f1222*MHp2)/PI2 - (0.125D0*CS1S1S3S3f1211*MW2)/PI2 - (0.0625D0*C&
  &S2S2S1S1f1112*MZ2)/PI2 + (0.25D0*EL2*MW2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3)))/&
  &(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp2) + MW2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*&
  &SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH22 + MHp2) + MW2)*(CA2*CB&
  &*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0&
  &*EL2*(2.D0*MH12 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB&
  &))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 + 2.D0*MW2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*C&
  &A3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH12)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*P&
  &I2*SB2*SW2) - (0.09375D0*CA2*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*C&
  &A2*EL2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MT2*(-1.D0*MH22 + &
  &6.D0*MT2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA1*(CA1*CA3 - &
  &1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.09375D0*CA2*EL2*MU2*(-1.D0*MH22 + 6.D0*MU2)*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*P&
  &I2*SB2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.&
  &D0*MH22)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - &
  &(0.03125D0*EL2*ML2*(-1.D0*MH22 + 6.D0*ML2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*Yuk&
  &S1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH22 + 6.D0*MM2)*YukS1Lep1*YukS1Lep2)/(MW2*PI2*SW2) - (0.09375D0*E&
  &L2*MB2*(6.D0*MB2 - 1.D0*MH12)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*YukS1Quark1*Y&
  &ukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH22)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*&
  &MD2*(6.D0*MD2 - 1.D0*MH22)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*YukS1Quark1*Yuk&
  &S1Quark2)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH22 + 6.D0*MS2)*YukS1Quark1*YukS1Quark2)/(MW2*PI2*SW2) + (0.109375D0*((2.&
  &D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*C&
  &A3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.125D0*EL2*MZ2*(CA1*CA2*(-1.D0*CA3*SA1 - 1&
  &.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*&
  &MA02 + 2.D0*(MA02 + MH12) + MZ2)*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*&
  &CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*(CA2*CB&
  &*SA1 - 1.D0*CA1*CA2*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2&
  &.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*MZ2)*(CA1*CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)&
  & + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*(CA1*&
  &CA2*CB + CA2*SA1*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(&
  &CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR11*RR21*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.0312&
  &5D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI&
  &2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) &
  &- (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D&
  &0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1&
  &Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(&
  &MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)&
  &))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0))&
  &)/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/P&
  &I2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f&
  &113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*S&
  &A3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*C&
  &A1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3&
  &*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*S&
  &A3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) +&
  & (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL&
  &)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - &
  &(0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*&
  &MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0&
  &*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*E&
  &L*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*E&
  &L*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3&
  & - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*P&
  &I2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)&
  &*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR12*RR22*(RR12*((-0.03125D0*CS2S2S1f221*MA0&
  &2)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS&
  &1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*&
  &MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*E&
  &L*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*D&
  &BLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(&
  &4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*&
  &PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0&
  &*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1&
  &S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (&
  &0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3&
  &)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(M&
  &W*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(&
  &MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0&
  &)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI&
  &2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - &
  &1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2&
  &.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f&
  &222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.06&
  &25D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*M&
  &W*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Q&
  &uark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375&
  &D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS&
  &1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE&
  &(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1&
  &*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*&
  &SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR&
  &13*RR23*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03&
  &125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/P&
  &I2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB&
  &**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D&
  &0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2&
  &*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.&
  &375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*E&
  &L2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.0312&
  &5D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)&
  &/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2&
  &*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.37&
  &5D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*&
  &PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW)&
  & - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*&
  &EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*&
  &SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW&
  &*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(&
  &CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 &
  &- (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112&
  &*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*&
  &SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DB&
  &LE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**&
  &INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/&
  &(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT&
  &(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*(&
  &(2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT&
  &(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3&
  &*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DB&
  &LE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2))/(CA2*(MH12 - 1.D0*MH22)) + (0.5D0*((0.0625D0*CS1S1S1f211*CS1S1S1f311)/P&
  &I2 + (0.125D0*CS1S1S1f212*CS1S1S1f312)/PI2 + (0.125D0*CS1S1S1f213*CS1S1S1f313)/PI2 + (0.0625D0*CS1S1S1f222*CS1S1S1f322)/PI2 + &
  &(0.125D0*CS1S1S1f223*CS1S1S1f323)/PI2 + (0.0625D0*CS1S1S1f233*CS1S1S1f333)/PI2 + (0.125D0*CS1S3S3f211*CS1S3S3f311)/PI2 + (0.12&
  &5D0*CS1S3S3f221*CS1S3S3f312)/PI2 + (0.125D0*CS1S3S3f212*CS1S3S3f321)/PI2 + (0.125D0*CS1S3S3f222*CS1S3S3f322)/PI2 + (0.0625D0*C&
  &S2S2S1f112*CS2S2S1f113)/PI2 + (0.125D0*CS2S2S1f122*CS2S2S1f123)/PI2 + (0.0625D0*CS2S2S1f222*CS2S2S1f223)/PI2 - (0.0625D0*CS2S2&
  &S1S1f2223*MA02)/PI2 - (0.0625D0*CS1S1S1S1f2311*MH12)/PI2 - (0.0625D0*CS1S1S1S1f2322*MH22)/PI2 - (0.0625D0*CS1S1S1S1f2333*MH32)&
  &/PI2 - (0.125D0*CS1S1S3S3f2322*MHp2)/PI2 - (0.125D0*CS1S1S3S3f2311*MW2)/PI2 - (0.0625D0*CS2S2S1S1f1123*MZ2)/PI2 + (0.25D0*EL2*&
  &MW2*((-1.D0*CA1*CA3*SA2 + SA1*SA3)*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3)))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH22 + MHp2) + MW2)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(&
  &-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (&
  &0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH32 + MHp2) + MW2)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*S&
  &A3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 &
  &+ 2.D0*MW2)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) &
  &+ (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH32 + 2.D0*MW2)*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.&
  &D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB))/(PI2*SW2) - (0.0&
  &9375D0*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0.&
  &09375D0*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - (0&
  &.09375D0*EL2*MT2*(-1.D0*MH22 + 6.D0*MT2)*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) - &
  &(0.09375D0*EL2*MT2*(-1.D0*MH32 + 6.D0*MT2)*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2) &
  &- (0.09375D0*EL2*MU2*(-1.D0*MH22 + 6.D0*MU2)*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*SW2&
  &) - (0.09375D0*EL2*MU2*(-1.D0*MH32 + 6.D0*MU2)*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 - 1.D0*SA1*SA2*SA3))/(MW2*PI2*SB2*S&
  &W2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH22)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32)&
  &*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH22 + 6.D0*ML2)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125&
  &D0*EL2*ML2*(-1.D0*MH32 + 6.D0*ML2)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH22 + 6.D0*MM2)*YukS1Lep2*Y&
  &ukS1Lep3)/(MW2*PI2*SW2) - (0.03125D0*EL2*MM2*(-1.D0*MH32 + 6.D0*MM2)*YukS1Lep2*YukS1Lep3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(&
  &6.D0*MB2 - 1.D0*MH22)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH22)*YukS1Quark2*YukS1Quar&
  &k3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH32)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D&
  &0*MD2 - 1.D0*MH32)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH22 + 6.D0*MS2)*YukS1Quark2*YukS1Quark3&
  &)/(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH32 + 6.D0*MS2)*YukS1Quark2*YukS1Quark3)/(MW2*PI2*SW2) + (0.109375D0*((2.D0*CB*MW&
  &*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1&
  &.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0)))/PI2 + (0.&
  &125D0*EL2*MZ2*((-1.D0*CA1*CA3*SA2 + SA1*SA3)*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA1*CA3 &
  &- 1.D0*SA1*SA2*SA3))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*(CB&
  &*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA&
  &3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH32) +&
  & MZ2)*(CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*&
  &(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH22 + 2.D0*MZ2)*(C&
  &B*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1&
  &.D0*SA1*SA2*SA3)*SB)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH32 + 2.D0*MZ2)*(CB*(-1.D0*CA1*CA3*S&
  &A2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)*(CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*S&
  &B)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - 2.D0*((0.5D0*EL*RR21*RR31*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125&
  &D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/P&
  &I2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0&
  &*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**IN&
  &T(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)&
  &))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*S&
  &W) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.3&
  &75D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW&
  &)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI&
  &2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f&
  &311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*&
  &MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.37&
  &5D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(&
  &MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) -&
  & (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*&
  &EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBL&
  &E(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3&
  &*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (&
  &0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*&
  &MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 &
  &- 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2&
  &*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/&
  &(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*S&
  &A2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.&
  &D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) + (0.5D0*EL*RR22*RR32*&
  &(RR12*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS&
  &1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.&
  &09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.&
  &D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW&
  &*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (&
  &0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA&
  &2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*(&
  &(2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR32*((-0.03125D0*CS2S&
  &2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (&
  &0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*&
  &CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*Y&
  &ukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*S&
  &W) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.12&
  &5D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1&
  &Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) -&
  & (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*&
  &CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*&
  &SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.031&
  &25D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI&
  &2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/E&
  &L))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**I&
  &NT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0&
  &)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*&
  &SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))&
  &/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB&
  &*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))&
  &/(CW2*PI2*SW2))))/(MW*SB*SW) + (RR23*RR33*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.031&
  &25D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/P&
  &I2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2)&
  & - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375&
  &D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS&
  &1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE&
  &(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0&
  &)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)&
  &))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/&
  &PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1&
  &f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*&
  &SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*&
  &CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep&
  &3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**I&
  &NT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*&
  &SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) &
  &+ (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/E&
  &L)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 -&
  & (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211&
  &*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D&
  &0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*&
  &EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW)&
  & - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*&
  &EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA&
  &3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*&
  &PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3&
  &)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS) + (0.0546875D0*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/&
  &EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*&
  &(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE((CW2 + SW2)**INT(4.&
  &D0)))/PI2))/(MH22 - 1.D0*MH32))) + DBLE(RR13**INT(2.D0))*((0.125D0*CS1S3S3f112*CS1S3S3f121)/PI2 - (0.03125D0*CS2S2S1S1f2211*MA&
  &02)/PI2 - (0.03125D0*CS1S1S1S1f1111*MH12)/PI2 - (0.03125D0*CS1S1S1S1f1122*MH22)/PI2 - (0.03125D0*CS1S1S1S1f1133*MH32)/PI2 - (0&
  &.0625D0*CS1S1S3S3f1122*MHp2)/PI2 - (0.0625D0*CS1S1S3S3f1111*MW2)/PI2 - (0.03125D0*CS2S2S1S1f1111*MZ2)/PI2 + (0.125D0*EL2*MW2*(&
  &CA12*CA22 + CA22*SA12))/(PI2*SW2) - (0.09375D0*CA22*EL2*MC2*(6.D0*MC2 - 1.D0*MH12)*SA12)/(MW2*PI2*SB2*SW2) - (0.09375D0*CA22*E&
  &L2*MT2*(-1.D0*MH12 + 6.D0*MT2)*SA12)/(MW2*PI2*SB2*SW2) - (0.09375D0*CA22*EL2*MU2*(-1.D0*MH12 + 6.D0*MU2)*SA12)/(MW2*PI2*SB2*SW&
  &2) + (0.03125D0*DBLE(CS1S1S1f111**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S1S1f112**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S1S1f113**&
  &INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS1S1S1f122**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S1S1f123**INT(2.D0)))/PI2 + (0.03125D0*DBLE&
  &(CS1S1S1f133**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S3S3f111**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S3S3f122**INT(2.D0)))/PI2 + (0&
  &.03125D0*DBLE(CS2S2S1f111**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS2S2S1f121**INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS2S2S1f221**INT(2.&
  &D0)))/PI2 - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH12 + MHp2) + MW2)*DBLE((CA2*CB*SA1 - 1.D0*CA1*CA2*SB)**INT(2.D0)))/(PI2*SW2) &
  &- (0.03125D0*EL2*(2.D0*MH12 + 2.D0*MW2)*DBLE((CA1*CA2*CB + CA2*SA1*SB)**INT(2.D0)))/(PI2*SW2) + (0.0546875D0*DBLE(EL**INT(4.D0&
  &))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)**INT(2.D0)))/PI2 + (0.0625D0*EL2*MZ2*(C&
  &A12*CA22 + CA22*SA12)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH12) + MZ2)*DB&
  &LE((CA2*CB*SA1 - 1.D0*CA1*CA2*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH12 + 2.D0*&
  &MZ2)*DBLE((CA1*CA2*CB + CA2*SA1*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.5D0*EL*DBLE(RR11**INT(2.D0))*&
  &(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS&
  &1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.&
  &09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.&
  &D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW&
  &*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (&
  &0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA&
  &2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*(&
  &(2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S&
  &2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (&
  &0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*&
  &CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*Y&
  &ukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*S&
  &W) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.12&
  &5D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1&
  &Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) -&
  & (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*&
  &CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*&
  &SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.031&
  &25D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI&
  &2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/E&
  &L))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**I&
  &NT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0&
  &)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*&
  &SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))&
  &/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB&
  &*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))&
  &/(CW2*PI2*SW2))))/(CB*MW*SW) - (0.5D0*EL*DBLE(RR12**INT(2.D0))*(RR12*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f1&
  &11*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625&
  &D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*&
  &SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(M&
  &W*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*S&
  &W) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D&
  &0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*&
  &SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((&
  &CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR32*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125&
  &D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2&
  & - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA&
  &3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D&
  &0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) -&
  & (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL&
  &*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA&
  &3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.&
  &D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1&
  &.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS&
  &1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - &
  &(0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*S&
  &A2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(M&
  &W*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(M&
  &W*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE&
  &(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*C&
  &A3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) - (1.D0*DBLE(RR13**INT(2.D0))*(RR1&
  &3*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S&
  &1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.0937&
  &5D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0))&
  &)/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2&
  &*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.12&
  &5D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL&
  &*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D&
  &0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f&
  &223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.06&
  &25D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*M&
  &W*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1&
  &Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) -&
  & (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*&
  &EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quar&
  &k3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.&
  &375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*&
  &CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)&
  &) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0&
  &*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + &
  &(0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/&
  &(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4&
  &.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/&
  &(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW&
  &*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*&
  &(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW&
  &2*PI2*SW2))))/vS + (0.02734375D0*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CA1*CA2*CB*MW*SW)/E&
  &L + (2.D0*CA2*MW*SA1*SB*SW)/EL)**INT(2.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2 - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH12)*DBLE&
  &(YukS1Lep1**INT(2.D0)))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH12 + 6.D0*ML2)*DBLE(YukS1Lep1**INT(2.D0)))/(MW2*PI2*SW2) -&
  & (0.03125D0*EL2*MM2*(-1.D0*MH12 + 6.D0*MM2)*DBLE(YukS1Lep1**INT(2.D0)))/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH&
  &12)*DBLE(YukS1Quark1**INT(2.D0)))/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH12)*DBLE(YukS1Quark1**INT(2.D0)))/(MW2&
  &*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH12 + 6.D0*MS2)*DBLE(YukS1Quark1**INT(2.D0)))/(MW2*PI2*SW2)) + DBLE(RR23**INT(2.D0))*((&
  &0.125D0*CS1S3S3f212*CS1S3S3f221)/PI2 - (0.03125D0*CS2S2S1S1f2222*MA02)/PI2 - (0.03125D0*CS1S1S1S1f2211*MH12)/PI2 - (0.03125D0*&
  &CS1S1S1S1f2222*MH22)/PI2 - (0.03125D0*CS1S1S1S1f2233*MH32)/PI2 - (0.0625D0*CS1S1S3S3f2222*MHp2)/PI2 - (0.0625D0*CS1S1S3S3f2211&
  &*MW2)/PI2 - (0.03125D0*CS2S2S1S1f1122*MZ2)/PI2 + (0.03125D0*DBLE(CS1S1S1f211**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S1S1f212**IN&
  &T(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S1S1f213**INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS1S1S1f222**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS&
  &1S1S1f223**INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS1S1S1f233**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S3S3f211**INT(2.D0)))/PI2 + (0.0&
  &625D0*DBLE(CS1S3S3f222**INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS2S2S1f112**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS2S2S1f122**INT(2.D0)&
  &))/PI2 + (0.03125D0*DBLE(CS2S2S1f222**INT(2.D0)))/PI2 - (0.09375D0*EL2*MC2*(6.D0*MC2 - 1.D0*MH22)*DBLE((CA1*CA3 - 1.D0*SA1*SA2&
  &*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MT2*(-1.D0*MH22 + 6.D0*MT2)*DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3)**INT(2.D0)&
  &))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MU2*(-1.D0*MH22 + 6.D0*MU2)*DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3)**INT(2.D0)))/(MW2*PI2*SB2*&
  &SW2) + (0.125D0*EL2*MW2*(DBLE((-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)**INT(2.D0)) + DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3)**INT(2.D0))))&
  &/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH22 + MHp2) + MW2)*DBLE((CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA&
  &1 - 1.D0*CA1*SA2*SA3)*SB)**INT(2.D0)))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH22 + 2.D0*MW2)*DBLE((CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA&
  &2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)**INT(2.D0)))/(PI2*SW2) + (0.0546875D0*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(&
  &((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)**INT(2.D0)))/PI2 + (&
  &0.0625D0*EL2*MZ2*(DBLE((-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)**INT(2.D0)) + DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3)**INT(2.D0)))*DBLE((C&
  &W2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*MA02 + 2.D0*(MA02 + MH22) + MZ2)*DBLE((CB*(CA1*CA3 - 1.D0*SA1*SA&
  &2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2&
  &*(2.D0*MH22 + 2.D0*MZ2)*DBLE((CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)**INT(2.D0))*DBLE((CW2 +&
  & SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.5D0*EL*DBLE(RR21**INT(2.D0))*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1&
  &S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.&
  &0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*&
  &SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0))&
  &)/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*P&
  &I2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.&
  &375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2&
  &*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DB&
  &LE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.0&
  &3125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)&
  &/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D&
  &0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(&
  &-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*S&
  &W) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D&
  &0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D&
  &0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**IN&
  &T(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2&
  & - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D&
  &0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI&
  &2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*C&
  &A1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0))&
  &)/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD&
  &**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0))&
  &)/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*&
  &SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*&
  &DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(C&
  &A1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) - (0.5D0*EL*DBLE(RR22**INT(2.D&
  &0))*(RR12*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D&
  &0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 +&
  & (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))&
  &/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW)&
  & - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D&
  &0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*M&
  &Z2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR32*((-0.03125D0*&
  &CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2&
  & - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2&
  &.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*&
  &EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*&
  &SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (&
  &0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*Y&
  &ukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*S&
  &W) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1&
  &.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*&
  &PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0&
  &.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2&
  &)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*S&
  &W)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(M&
  &C**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(&
  &4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*&
  &PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D&
  &0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D&
  &0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D&
  &0)))/(CW2*PI2*SW2))))/(MW*SB*SW) - (1.D0*DBLE(RR23**INT(2.D0))*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f1&
  &11*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625&
  &D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*&
  &SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(M&
  &W*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*S&
  &W) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D&
  &0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*&
  &SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((&
  &CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125&
  &D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2&
  & - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA&
  &3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D&
  &0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) -&
  & (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL&
  &*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA&
  &3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.&
  &D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1&
  &.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS&
  &1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - &
  &(0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*S&
  &A2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(M&
  &W*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(M&
  &W*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) &
  &- (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE&
  &(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*C&
  &A3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS + (0.02734375D0*DBLE(CW**INT(-4.D0))*DBLE(&
  &EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3)*SB*SW)/EL)**INT(2.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2 - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH22)*DBLE(YukS1Lep2*&
  &*INT(2.D0)))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH22 + 6.D0*ML2)*DBLE(YukS1Lep2**INT(2.D0)))/(MW2*PI2*SW2) - (0.03125D0&
  &*EL2*MM2*(-1.D0*MH22 + 6.D0*MM2)*DBLE(YukS1Lep2**INT(2.D0)))/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D0*MH22)*DBLE(Yu&
  &kS1Quark2**INT(2.D0)))/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH22)*DBLE(YukS1Quark2**INT(2.D0)))/(MW2*PI2*SW2) -&
  & (0.09375D0*EL2*MS2*(-1.D0*MH22 + 6.D0*MS2)*DBLE(YukS1Quark2**INT(2.D0)))/(MW2*PI2*SW2)) + DBLE(RR33**INT(2.D0))*((0.125D0*CS1&
  &S3S3f312*CS1S3S3f321)/PI2 - (0.03125D0*CS2S2S1S1f2233*MA02)/PI2 - (0.03125D0*CS1S1S1S1f3311*MH12)/PI2 - (0.03125D0*CS1S1S1S1f3&
  &322*MH22)/PI2 - (0.03125D0*CS1S1S1S1f3333*MH32)/PI2 - (0.0625D0*CS1S1S3S3f3322*MHp2)/PI2 - (0.0625D0*CS1S1S3S3f3311*MW2)/PI2 -&
  & (0.03125D0*CS2S2S1S1f1133*MZ2)/PI2 + (0.03125D0*DBLE(CS1S1S1f311**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S1S1f312**INT(2.D0)))/P&
  &I2 + (0.0625D0*DBLE(CS1S1S1f313**INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS1S1S1f322**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S1S1f323**&
  &INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS1S1S1f333**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS1S3S3f311**INT(2.D0)))/PI2 + (0.0625D0*DBLE(&
  &CS1S3S3f322**INT(2.D0)))/PI2 + (0.03125D0*DBLE(CS2S2S1f113**INT(2.D0)))/PI2 + (0.0625D0*DBLE(CS2S2S1f123**INT(2.D0)))/PI2 + (0&
  &.03125D0*DBLE(CS2S2S1f223**INT(2.D0)))/PI2 - (0.09375D0*EL2*MC2*(6.D0*MC2 - 1.D0*MH32)*DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)&
  &**INT(2.D0)))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MT2*(-1.D0*MH32 + 6.D0*MT2)*DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0&
  &)))/(MW2*PI2*SB2*SW2) - (0.09375D0*EL2*MU2*(-1.D0*MH32 + 6.D0*MU2)*DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0)))/(MW2*P&
  &I2*SB2*SW2) + (0.125D0*EL2*MW2*(DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0)) + DBLE((-1.D0*CA1*CA3*SA2 + SA1*SA3)**INT(&
  &2.D0))))/(PI2*SW2) - (0.03125D0*EL2*(2.D0*MH32 + 2.D0*MW2)*DBLE((CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*&
  &CA1*SA3)*SB)**INT(2.D0)))/(PI2*SW2) - (0.03125D0*EL2*(-1.D0*MHp2 + 2.D0*(MH32 + MHp2) + MW2)*DBLE((CB*(-1.D0*CA3*SA1*SA2 - 1.D&
  &0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)**INT(2.D0)))/(PI2*SW2) + (0.0546875D0*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.&
  &D0))*DBLE(((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)**INT(2.D0&
  &)))/PI2 + (0.0625D0*EL2*MZ2*(DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0)) + DBLE((-1.D0*CA1*CA3*SA2 + SA1*SA3)**INT(2.D&
  &0)))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(2.D0*MH32 + 2.D0*MZ2)*DBLE((CB*(-1.D0*CA1*CA3*SA2 + SA1*SA&
  &3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.015625D0*EL2*(-1.D0*M&
  &A02 + 2.D0*(MA02 + MH32) + MZ2)*DBLE((CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)**INT(2.D0&
  &))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2) - (0.5D0*EL*DBLE(RR31**INT(2.D0))*(RR11*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0&
  &.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*M&
  &Hp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL +&
  & (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(&
  &MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT&
  &(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW&
  &*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) &
  &- (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1&
  &*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR31*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH&
  &12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS&
  &1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + &
  &(2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) -&
  & (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D&
  &0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2&
  &*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.&
  &375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA&
  &3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.&
  &D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR21*((-0.03125D0*CS2S2S1f222*MA02)/P&
  &I2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S&
  &3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA&
  &3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(&
  &MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1&
  &Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(&
  &ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.&
  &D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0&
  &*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL&
  & + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(CB*MW*SW) - (0.5D0*EL*DBLE&
  &(RR32**INT(2.D0))*(RR12*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.03125D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/P&
  &I2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f&
  &111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quar&
  &k1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(M&
  &D**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)&
  &))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2&
  &*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.&
  &046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR32&
  &*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1&
  &f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375&
  &D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW&
  &2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.&
  &D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(&
  &MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) -&
  & (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0))&
  &)/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((&
  &2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT&
  &(2.D0)))/(CW2*PI2*SW2)) + RR22*((-0.03125D0*CS2S2S1f222*MA02)/PI2 - (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*&
  &MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f222*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*C&
  &S2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*S&
  &A2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2&
  &*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**IN&
  &T(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DB&
  &LE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0&
  &*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 +&
  & SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/(MW*SB*SW) - (1.D0*DBLE(RR33**INT(2.D0))*(RR13*((-0.03125D0*CS2S2S1f221*MA02)/PI2 - (0.031&
  &25D0*CS1S1S1f111*MH12)/PI2 - (0.03125D0*CS1S1S1f122*MH22)/PI2 - (0.03125D0*CS1S1S1f133*MH32)/PI2 - (0.0625D0*CS1S3S3f122*MHp2)&
  &/PI2 - (0.0625D0*CS1S3S3f111*MW2)/PI2 - (0.03125D0*CS2S2S1f111*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.&
  &D0*CA2*MW*SA1*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark1*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MC**&
  &INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ME**INT(4.D&
  &0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep1*DBLE(MM**INT(4.D0)))/(MW*PI2&
  &*SW) - (0.375D0*EL*YukS1Quark1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA2*EL*SA1*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0&
  &.375D0*CA2*EL*SA1*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*&
  &SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR33*((-0.03125D0*CS2S2S1f223*MA02)/PI2 - (0.03125D0*CS1S1S1f311*MH12)/&
  &PI2 - (0.03125D0*CS1S1S1f322*MH22)/PI2 - (0.03125D0*CS1S1S1f333*MH32)/PI2 - (0.0625D0*CS1S3S3f322*MHp2)/PI2 - (0.0625D0*CS1S3S&
  &3f311*MW2)/PI2 - (0.03125D0*CS2S2S1f113*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D&
  &0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark3*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.&
  &375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MD**INT(4.D0)))&
  &/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep3*DBLE(ML**INT(4.D0)))/(MW*PI2*SW)&
  & - (0.125D0*EL*YukS1Lep3*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark3*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D&
  &0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*D&
  &BLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*C&
  &A3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)) + RR23*((-0.03125D0*CS2S2S1f222*MA02)/PI2 -&
  & (0.03125D0*CS1S1S1f211*MH12)/PI2 - (0.03125D0*CS1S1S1f222*MH22)/PI2 - (0.03125D0*CS1S1S1f233*MH32)/PI2 - (0.0625D0*CS1S3S3f22&
  &2*MHp2)/PI2 - (0.0625D0*CS1S3S3f211*MW2)/PI2 - (0.03125D0*CS2S2S1f112*MZ2)/PI2 + (0.09375D0*EL2*MW2*((2.D0*CB*MW*(-1.D0*CA3*SA&
  &1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL))/(PI2*SW2) - (0.375D0*EL*YukS1Quark2*DBLE(MB**&
  &INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*YukS1Quar&
  &k2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(ML**&
  &INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*YukS1Lep2*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*YukS1Quark2*DBLE(MS**INT(4.D0))&
  &)/(MW*PI2*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*(CA1*CA3 - 1.D0*SA1&
  &*SA2*SA3)*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.046875D0*EL2*MZ2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (&
  &2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2))))/vS + (0.02734375D0*DBLE(CW**INT&
  &(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3&
  &*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)**INT(2.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2 - (0.03125D0*EL2*ME2*(6.D0*ME2 - 1.D0*MH32)*&
  &DBLE(YukS1Lep3**INT(2.D0)))/(MW2*PI2*SW2) - (0.03125D0*EL2*ML2*(-1.D0*MH32 + 6.D0*ML2)*DBLE(YukS1Lep3**INT(2.D0)))/(MW2*PI2*SW&
  &2) - (0.03125D0*EL2*MM2*(-1.D0*MH32 + 6.D0*MM2)*DBLE(YukS1Lep3**INT(2.D0)))/(MW2*PI2*SW2) - (0.09375D0*EL2*MB2*(6.D0*MB2 - 1.D&
  &0*MH32)*DBLE(YukS1Quark3**INT(2.D0)))/(MW2*PI2*SW2) - (0.09375D0*EL2*MD2*(6.D0*MD2 - 1.D0*MH32)*DBLE(YukS1Quark3**INT(2.D0)))/&
  &(MW2*PI2*SW2) - (0.09375D0*EL2*MS2*(-1.D0*MH32 + 6.D0*MS2)*DBLE(YukS1Quark3**INT(2.D0)))/(MW2*PI2*SW2))))/(MH12*DBLE(RR13**INT&
  &(2.D0)) + MH22*DBLE(RR23**INT(2.D0)) + MH32*DBLE(RR33**INT(2.D0)))&
                    &)*DLOG(1D0/EvalScale**2)
                end function dvSMSBarAlter

end module counterterms
