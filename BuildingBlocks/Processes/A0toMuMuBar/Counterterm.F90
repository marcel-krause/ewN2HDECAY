double precision function A0toMuMuBarCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) - (1 + YukS2Lep2**2)/YukS2Lep2*&
			&dBeta1KanUsual() + dZA0A0OS()/2D0 + 1D0/YukS2Lep2 * dZG0A0OSUsual()/2D0 + dZMuMuOSLeft()/2D0 + dZMuMuOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*MM2*DBLE(YukS2Lep2**INT(2.D0)))/(MW2*SW2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) - (1 + YukS2Lep2**2)/YukS2Lep2*&
			&dBeta2KanUsual() + dZA0A0OS()/2D0 + 1D0/YukS2Lep2 * dZG0A0OSUsual()/2D0 + dZMuMuOSLeft()/2D0 + dZMuMuOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*MM2*DBLE(YukS2Lep2**INT(2.D0)))/(MW2*SW2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) - (1 + YukS2Lep2**2)/YukS2Lep2*&
			&dBeta1KanAlter() + dZA0A0OS()/2D0 + 1D0/YukS2Lep2 * dZG0A0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + dZMuMuOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*MM2*DBLE(YukS2Lep2**INT(2.D0)))/(MW2*SW2)
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) - (1 + YukS2Lep2**2)/YukS2Lep2*&
			&dBeta2KanAlter() + dZA0A0OS()/2D0 + 1D0/YukS2Lep2 * dZG0A0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + dZMuMuOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*MM2*DBLE(YukS2Lep2**INT(2.D0)))/(MW2*SW2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) - (1 + YukS2Lep2**2)/YukS2Lep2*&
			&dBeta1PinchPStar() + dZA0A0OS()/2D0 + 1D0/YukS2Lep2 * dZG0A0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + dZMuMuOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*MM2*DBLE(YukS2Lep2**INT(2.D0)))/(MW2*SW2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) - (1 + YukS2Lep2**2)/YukS2Lep2*&
			&dBeta2PinchPStar() + dZA0A0OS()/2D0 + 1D0/YukS2Lep2 * dZG0A0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + dZMuMuOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*MM2*DBLE(YukS2Lep2**INT(2.D0)))/(MW2*SW2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) - (1 + YukS2Lep2**2)/YukS2Lep2*&
			&dBeta1PinchOS() + dZA0A0OS()/2D0 + 1D0/YukS2Lep2 * dZG0A0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + dZMuMuOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*MM2*DBLE(YukS2Lep2**INT(2.D0)))/(MW2*SW2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) - (1 + YukS2Lep2**2)/YukS2Lep2*&
			&dBeta2PinchOS() + dZA0A0OS()/2D0 + 1D0/YukS2Lep2 * dZG0A0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + dZMuMuOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*MM2*DBLE(YukS2Lep2**INT(2.D0)))/(MW2*SW2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) - (1 + YukS2Lep2**2)/YukS2Lep2*&
			&dBetaMSBarUsual() + dZA0A0OS()/2D0 + 1D0/YukS2Lep2 * dZG0A0OSUsual()/2D0 + dZMuMuOSLeft()/2D0 + dZMuMuOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*MM2*DBLE(YukS2Lep2**INT(2.D0)))/(MW2*SW2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) - (1 + YukS2Lep2**2)/YukS2Lep2*&
			&dBetaMSBarAlter() + dZA0A0OS()/2D0 + 1D0/YukS2Lep2 * dZG0A0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + dZMuMuOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*MM2*DBLE(YukS2Lep2**INT(2.D0)))/(MW2*SW2)
 end select

 A0toMuMuBarCT = totalAmplitude
end function A0toMuMuBarCT
