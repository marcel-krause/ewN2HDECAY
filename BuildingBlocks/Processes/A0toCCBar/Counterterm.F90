double precision function A0toCCBarCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMCOSUsual()/MC - dMW2Usual()/(2D0*MW2) - 1D0/(SB*CB)*dBeta1KanUsual() + &
			& dZA0A0OS()/2D0 + TB*dZG0A0OSUsual()/2D0 + dZCCOSLeft()/2D0 + dZCCOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MC2*CB2)/(MW2*SW2*SB2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMCOSUsual()/MC - dMW2Usual()/(2D0*MW2) - 1D0/(SB*CB)*dBeta2KanUsual() + &
			& dZA0A0OS()/2D0 + TB*dZG0A0OSUsual()/2D0 + dZCCOSLeft()/2D0 + dZCCOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MC2*CB2)/(MW2*SW2*SB2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMCOSAlter()/MC - dMW2Alter()/(2D0*MW2) - 1D0/(SB*CB)*dBeta1KanAlter() + &
			& dZA0A0OS()/2D0 + TB*dZG0A0OSAlter()/2D0 + dZCCOSLeft()/2D0 + dZCCOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MC2*CB2)/(MW2*SW2*SB2)
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMCOSAlter()/MC - dMW2Alter()/(2D0*MW2) - 1D0/(SB*CB)*dBeta2KanAlter() + &
			& dZA0A0OS()/2D0 + TB*dZG0A0OSAlter()/2D0 + dZCCOSLeft()/2D0 + dZCCOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MC2*CB2)/(MW2*SW2*SB2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMCOSAlter()/MC - dMW2Alter()/(2D0*MW2) - 1D0/(SB*CB)*dBeta1PinchPStar() + &
			& dZA0A0OS()/2D0 + TB*dZG0A0OSAlter()/2D0 + dZCCOSLeft()/2D0 + dZCCOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MC2*CB2)/(MW2*SW2*SB2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMCOSAlter()/MC - dMW2Alter()/(2D0*MW2) - 1D0/(SB*CB)*dBeta2PinchPStar() + &
			& dZA0A0OS()/2D0 + TB*dZG0A0OSAlter()/2D0 + dZCCOSLeft()/2D0 + dZCCOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MC2*CB2)/(MW2*SW2*SB2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMCOSAlter()/MC - dMW2Alter()/(2D0*MW2) - 1D0/(SB*CB)*dBeta1PinchOS() + &
			& dZA0A0OS()/2D0 + TB*dZG0A0OSAlter()/2D0 + dZCCOSLeft()/2D0 + dZCCOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MC2*CB2)/(MW2*SW2*SB2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMCOSAlter()/MC - dMW2Alter()/(2D0*MW2) - 1D0/(SB*CB)*dBeta2PinchOS() + &
			& dZA0A0OS()/2D0 + TB*dZG0A0OSAlter()/2D0 + dZCCOSLeft()/2D0 + dZCCOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MC2*CB2)/(MW2*SW2*SB2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMCOSUsual()/MC - dMW2Usual()/(2D0*MW2) - 1D0/(SB*CB)*dBetaMSBarUsual() + &
			& dZA0A0OS()/2D0 + TB*dZG0A0OSUsual()/2D0 + dZCCOSLeft()/2D0 + dZCCOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MC2*CB2)/(MW2*SW2*SB2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMCOSAlter()/MC - dMW2Alter()/(2D0*MW2) - 1D0/(SB*CB)*dBetaMSBarAlter() + &
			& dZA0A0OS()/2D0 + TB*dZG0A0OSAlter()/2D0 + dZCCOSLeft()/2D0 + dZCCOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MC2*CB2)/(MW2*SW2*SB2)
	case default
		totalAmplitude = 0D0
 end select

 A0toCCBarCT = totalAmplitude
end function A0toCCBarCT
