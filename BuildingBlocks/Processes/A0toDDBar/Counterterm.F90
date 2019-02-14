double precision function A0toDDBarCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMDOSUsual()/MD - dMW2Usual()/(2D0*MW2) - (1+YukS2Quark2**2)/YukS2Quark2*&
			&dBeta1KanUsual() + dZA0A0OS()/2D0 + 1D0/YukS2Quark2*dZG0A0OSUsual()/2D0 + dZDDOSLeft()/2D0 + dZDDOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MD2*YukS2Quark2**2)/(MW2*SW2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMDOSUsual()/MD - dMW2Usual()/(2D0*MW2) - (1+YukS2Quark2**2)/YukS2Quark2*&
			&dBeta2KanUsual() + dZA0A0OS()/2D0 + 1D0/YukS2Quark2*dZG0A0OSUsual()/2D0 + dZDDOSLeft()/2D0 + dZDDOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MD2*YukS2Quark2**2)/(MW2*SW2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMDOSAlter()/MD - dMW2Alter()/(2D0*MW2) - (1+YukS2Quark2**2)/YukS2Quark2*&
			&dBeta1KanAlter() + dZA0A0OS()/2D0 + 1D0/YukS2Quark2*dZG0A0OSAlter()/2D0 + dZDDOSLeft()/2D0 + dZDDOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MD2*YukS2Quark2**2)/(MW2*SW2)
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMDOSAlter()/MD - dMW2Alter()/(2D0*MW2) - (1+YukS2Quark2**2)/YukS2Quark2*&
			&dBeta2KanAlter() + dZA0A0OS()/2D0 + 1D0/YukS2Quark2*dZG0A0OSAlter()/2D0 + dZDDOSLeft()/2D0 + dZDDOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MD2*YukS2Quark2**2)/(MW2*SW2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMDOSAlter()/MD - dMW2Alter()/(2D0*MW2) - (1+YukS2Quark2**2)/YukS2Quark2*&
			&dBeta1PinchPStar() + dZA0A0OS()/2D0 + 1D0/YukS2Quark2*dZG0A0OSAlter()/2D0 + dZDDOSLeft()/2D0 + dZDDOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MD2*YukS2Quark2**2)/(MW2*SW2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMDOSAlter()/MD - dMW2Alter()/(2D0*MW2) - (1+YukS2Quark2**2)/YukS2Quark2*&
			&dBeta2PinchPStar() + dZA0A0OS()/2D0 + 1D0/YukS2Quark2*dZG0A0OSAlter()/2D0 + dZDDOSLeft()/2D0 + dZDDOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MD2*YukS2Quark2**2)/(MW2*SW2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMDOSAlter()/MD - dMW2Alter()/(2D0*MW2) - (1+YukS2Quark2**2)/YukS2Quark2*&
			&dBeta1PinchOS() + dZA0A0OS()/2D0 + 1D0/YukS2Quark2*dZG0A0OSAlter()/2D0 + dZDDOSLeft()/2D0 + dZDDOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MD2*YukS2Quark2**2)/(MW2*SW2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMDOSAlter()/MD - dMW2Alter()/(2D0*MW2) - (1+YukS2Quark2**2)/YukS2Quark2*&
			&dBeta2PinchOS() + dZA0A0OS()/2D0 + 1D0/YukS2Quark2*dZG0A0OSAlter()/2D0 + dZDDOSLeft()/2D0 + dZDDOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MD2*YukS2Quark2**2)/(MW2*SW2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMDOSUsual()/MD - dMW2Usual()/(2D0*MW2) - (1+YukS2Quark2**2)/YukS2Quark2*&
			&dBetaMSBarUsual() + dZA0A0OS()/2D0 + 1D0/YukS2Quark2*dZG0A0OSUsual()/2D0 + dZDDOSLeft()/2D0 + dZDDOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MD2*YukS2Quark2**2)/(MW2*SW2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMDOSAlter()/MD - dMW2Alter()/(2D0*MW2) - (1+YukS2Quark2**2)/YukS2Quark2*&
			&dBetaMSBarAlter() + dZA0A0OS()/2D0 + 1D0/YukS2Quark2*dZG0A0OSAlter()/2D0 + dZDDOSLeft()/2D0 + dZDDOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MD2*YukS2Quark2**2)/(MW2*SW2)
	case default
		totalAmplitude = 0D0
 end select

 A0toDDBarCT = totalAmplitude
end function A0toDDBarCT
