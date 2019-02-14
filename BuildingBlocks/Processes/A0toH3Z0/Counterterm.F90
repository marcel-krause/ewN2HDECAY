double precision function A0toH3Z0CT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude
 double precision :: dRR11, dRR12, dRR13, dRR21, dRR22, dRR23, dRR31, dRR32, dRR33

 select case (x)
	case (1)
		dRR11 = -CA2*SA1*dAlpha1KanUsual() - CA1*SA2*dAlpha2KanUsual()
		dRR12 = CA1*CA2*dAlpha1KanUsual() - SA1*SA2*dAlpha2KanUsual()
		dRR13 = CA2*dAlpha2KanUsual()
		dRR21 = -CA1*CA3*dAlpha1KanUsual() - CA1*CA3*SA2*dAlpha3KanUsual() - CA1*CA2*SA3*dAlpha2KanUsual() + &
			& SA1*SA3*dAlpha3KanUsual() + SA1*SA2*SA3*dAlpha1KanUsual()
		dRR22 = -CA3*SA1*dAlpha1KanUsual() - CA3*SA1*SA2*dAlpha3KanUsual() - CA1*SA3*dAlpha3KanUsual() - &
			& CA2*SA1*SA3*dAlpha2KanUsual() - CA1*SA2*SA3*dAlpha1KanUsual()
		dRR23 = CA2*CA3*dAlpha3KanUsual() - SA2*SA3*dAlpha2KanUsual()
		dRR31 = -CA1*CA2*CA3*dAlpha2KanUsual() + CA3*SA1*dAlpha3KanUsual() + CA3*SA1*SA2*dAlpha1KanUsual() + &
			& CA1*SA3*dAlpha1KanUsual() + CA1*SA2*SA3*dAlpha3KanUsual()
		dRR32 = -CA1*CA3*dAlpha3KanUsual() - CA2*CA3*SA1*dAlpha2KanUsual() - CA1*CA3*SA2*dAlpha1KanUsual() + &
			& SA1*SA3*dAlpha1KanUsual() + SA1*SA2*SA3*dAlpha3KanUsual()
		dRR33 = -CA3*SA2*dAlpha2KanUsual() - CA2*SA3*dAlpha3KanUsual()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Usual()/MZ2 - dMW2Usual()/MW2)/2D0 + dZA0A0OS()/2D0 + dZZ0Z0OS()/2D0 + &
			& 1D0/(CB*RR32 - RR31*SB)*( CB*dRR32 - dRR31*SB - SB*RR32*dBeta1KanUsual() - RR31*CB*dBeta1KanUsual() ) + &
			& (SB*RR32 + RR31*CB)/(CB*RR32 - RR31*SB)*( dZG0A0OSUsual()/2D0 ) + &
			& (CB*RR12 - RR11*SB)/(CB*RR32 - RR31*SB)*( dZH1H3OSUsual()/2D0 ) + &
			& (CB*RR22 - RR21*SB)/(CB*RR32 - RR31*SB)*( dZH2H3OSUsual()/2D0 ) + &
			& (CB*RR32 - RR31*SB)/(CB*RR32 - RR31*SB)*( dZH3H3OS()/2D0 ) ) &
            & * (EL2/SW2)*(CB*RR32 - RR31*SB)**2/CW2 &
			& * ( (MH32**2 + MZ2**2 + MA02**2 - 2D0*MH32*MZ2 - 2D0*MH32*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
	case (2)
		dRR11 = -CA2*SA1*dAlpha1KanUsual() - CA1*SA2*dAlpha2KanUsual()
		dRR12 = CA1*CA2*dAlpha1KanUsual() - SA1*SA2*dAlpha2KanUsual()
		dRR13 = CA2*dAlpha2KanUsual()
		dRR21 = -CA1*CA3*dAlpha1KanUsual() - CA1*CA3*SA2*dAlpha3KanUsual() - CA1*CA2*SA3*dAlpha2KanUsual() + &
			& SA1*SA3*dAlpha3KanUsual() + SA1*SA2*SA3*dAlpha1KanUsual()
		dRR22 = -CA3*SA1*dAlpha1KanUsual() - CA3*SA1*SA2*dAlpha3KanUsual() - CA1*SA3*dAlpha3KanUsual() - &
			& CA2*SA1*SA3*dAlpha2KanUsual() - CA1*SA2*SA3*dAlpha1KanUsual()
		dRR23 = CA2*CA3*dAlpha3KanUsual() - SA2*SA3*dAlpha2KanUsual()
		dRR31 = -CA1*CA2*CA3*dAlpha2KanUsual() + CA3*SA1*dAlpha3KanUsual() + CA3*SA1*SA2*dAlpha1KanUsual() + &
			& CA1*SA3*dAlpha1KanUsual() + CA1*SA2*SA3*dAlpha3KanUsual()
		dRR32 = -CA1*CA3*dAlpha3KanUsual() - CA2*CA3*SA1*dAlpha2KanUsual() - CA1*CA3*SA2*dAlpha1KanUsual() + &
			& SA1*SA3*dAlpha1KanUsual() + SA1*SA2*SA3*dAlpha3KanUsual()
		dRR33 = -CA3*SA2*dAlpha2KanUsual() - CA2*SA3*dAlpha3KanUsual()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Usual()/MZ2 - dMW2Usual()/MW2)/2D0 + dZA0A0OS()/2D0 + dZZ0Z0OS()/2D0 + &
			& 1D0/(CB*RR32 - RR31*SB)*( CB*dRR32 - dRR31*SB - SB*RR32*dBeta2KanUsual() - RR31*CB*dBeta2KanUsual() ) + &
			& (SB*RR32 + RR31*CB)/(CB*RR32 - RR31*SB)*( dZG0A0OSUsual()/2D0 ) + &
			& (CB*RR12 - RR11*SB)/(CB*RR32 - RR31*SB)*( dZH1H3OSUsual()/2D0 ) + &
			& (CB*RR22 - RR21*SB)/(CB*RR32 - RR31*SB)*( dZH2H3OSUsual()/2D0 ) + &
			& (CB*RR32 - RR31*SB)/(CB*RR32 - RR31*SB)*( dZH3H3OS()/2D0 ) ) &
            & * (EL2/SW2)*(CB*RR32 - RR31*SB)**2/CW2 &
			& * ( (MH32**2 + MZ2**2 + MA02**2 - 2D0*MH32*MZ2 - 2D0*MH32*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
	case (3)
		dRR11 = -CA2*SA1*dAlpha1KanAlter() - CA1*SA2*dAlpha2KanAlter()
		dRR12 = CA1*CA2*dAlpha1KanAlter() - SA1*SA2*dAlpha2KanAlter()
		dRR13 = CA2*dAlpha2KanAlter()
		dRR21 = -CA1*CA3*dAlpha1KanAlter() - CA1*CA3*SA2*dAlpha3KanAlter() - CA1*CA2*SA3*dAlpha2KanAlter() + &
			& SA1*SA3*dAlpha3KanAlter() + SA1*SA2*SA3*dAlpha1KanAlter()
		dRR22 = -CA3*SA1*dAlpha1KanAlter() - CA3*SA1*SA2*dAlpha3KanAlter() - CA1*SA3*dAlpha3KanAlter() - &
			& CA2*SA1*SA3*dAlpha2KanAlter() - CA1*SA2*SA3*dAlpha1KanAlter()
		dRR23 = CA2*CA3*dAlpha3KanAlter() - SA2*SA3*dAlpha2KanAlter()
		dRR31 = -CA1*CA2*CA3*dAlpha2KanAlter() + CA3*SA1*dAlpha3KanAlter() + CA3*SA1*SA2*dAlpha1KanAlter() + &
			& CA1*SA3*dAlpha1KanAlter() + CA1*SA2*SA3*dAlpha3KanAlter()
		dRR32 = -CA1*CA3*dAlpha3KanAlter() - CA2*CA3*SA1*dAlpha2KanAlter() - CA1*CA3*SA2*dAlpha1KanAlter() + &
			& SA1*SA3*dAlpha1KanAlter() + SA1*SA2*SA3*dAlpha3KanAlter()
		dRR33 = -CA3*SA2*dAlpha2KanAlter() - CA2*SA3*dAlpha3KanAlter()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZA0A0OS()/2D0 + dZZ0Z0OS()/2D0 + &
			& 1D0/(CB*RR32 - RR31*SB)*( CB*dRR32 - dRR31*SB - SB*RR32*dBeta1KanAlter() - RR31*CB*dBeta1KanAlter() ) + &
			& (SB*RR32 + RR31*CB)/(CB*RR32 - RR31*SB)*( dZG0A0OSAlter()/2D0 ) + &
			& (CB*RR12 - RR11*SB)/(CB*RR32 - RR31*SB)*( dZH1H3OSAlter()/2D0 ) + &
			& (CB*RR22 - RR21*SB)/(CB*RR32 - RR31*SB)*( dZH2H3OSAlter()/2D0 ) + &
			& (CB*RR32 - RR31*SB)/(CB*RR32 - RR31*SB)*( dZH3H3OS()/2D0 ) ) &
            & * (EL2/SW2)*(CB*RR32 - RR31*SB)**2/CW2 &
			& * ( (MH32**2 + MZ2**2 + MA02**2 - 2D0*MH32*MZ2 - 2D0*MH32*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
	case (4)
		dRR11 = -CA2*SA1*dAlpha1KanAlter() - CA1*SA2*dAlpha2KanAlter()
		dRR12 = CA1*CA2*dAlpha1KanAlter() - SA1*SA2*dAlpha2KanAlter()
		dRR13 = CA2*dAlpha2KanAlter()
		dRR21 = -CA1*CA3*dAlpha1KanAlter() - CA1*CA3*SA2*dAlpha3KanAlter() - CA1*CA2*SA3*dAlpha2KanAlter() + &
			& SA1*SA3*dAlpha3KanAlter() + SA1*SA2*SA3*dAlpha1KanAlter()
		dRR22 = -CA3*SA1*dAlpha1KanAlter() - CA3*SA1*SA2*dAlpha3KanAlter() - CA1*SA3*dAlpha3KanAlter() - &
			& CA2*SA1*SA3*dAlpha2KanAlter() - CA1*SA2*SA3*dAlpha1KanAlter()
		dRR23 = CA2*CA3*dAlpha3KanAlter() - SA2*SA3*dAlpha2KanAlter()
		dRR31 = -CA1*CA2*CA3*dAlpha2KanAlter() + CA3*SA1*dAlpha3KanAlter() + CA3*SA1*SA2*dAlpha1KanAlter() + &
			& CA1*SA3*dAlpha1KanAlter() + CA1*SA2*SA3*dAlpha3KanAlter()
		dRR32 = -CA1*CA3*dAlpha3KanAlter() - CA2*CA3*SA1*dAlpha2KanAlter() - CA1*CA3*SA2*dAlpha1KanAlter() + &
			& SA1*SA3*dAlpha1KanAlter() + SA1*SA2*SA3*dAlpha3KanAlter()
		dRR33 = -CA3*SA2*dAlpha2KanAlter() - CA2*SA3*dAlpha3KanAlter()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZA0A0OS()/2D0 + dZZ0Z0OS()/2D0 + &
			& 1D0/(CB*RR32 - RR31*SB)*( CB*dRR32 - dRR31*SB - SB*RR32*dBeta2KanAlter() - RR31*CB*dBeta2KanAlter() ) + &
			& (SB*RR32 + RR31*CB)/(CB*RR32 - RR31*SB)*( dZG0A0OSAlter()/2D0 ) + &
			& (CB*RR12 - RR11*SB)/(CB*RR32 - RR31*SB)*( dZH1H3OSAlter()/2D0 ) + &
			& (CB*RR22 - RR21*SB)/(CB*RR32 - RR31*SB)*( dZH2H3OSAlter()/2D0 ) + &
			& (CB*RR32 - RR31*SB)/(CB*RR32 - RR31*SB)*( dZH3H3OS()/2D0 ) ) &
            & * (EL2/SW2)*(CB*RR32 - RR31*SB)**2/CW2 &
			& * ( (MH32**2 + MZ2**2 + MA02**2 - 2D0*MH32*MZ2 - 2D0*MH32*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
	case (5)
		dRR11 = -CA2*SA1*dAlpha1PinchPStar() - CA1*SA2*dAlpha2PinchPStar()
		dRR12 = CA1*CA2*dAlpha1PinchPStar() - SA1*SA2*dAlpha2PinchPStar()
		dRR13 = CA2*dAlpha2PinchPStar()
		dRR21 = -CA1*CA3*dAlpha1PinchPStar() - CA1*CA3*SA2*dAlpha3PinchPStar() - CA1*CA2*SA3*dAlpha2PinchPStar() + &
			& SA1*SA3*dAlpha3PinchPStar() + SA1*SA2*SA3*dAlpha1PinchPStar()
		dRR22 = -CA3*SA1*dAlpha1PinchPStar() - CA3*SA1*SA2*dAlpha3PinchPStar() - CA1*SA3*dAlpha3PinchPStar() - &
			& CA2*SA1*SA3*dAlpha2PinchPStar() - CA1*SA2*SA3*dAlpha1PinchPStar()
		dRR23 = CA2*CA3*dAlpha3PinchPStar() - SA2*SA3*dAlpha2PinchPStar()
		dRR31 = -CA1*CA2*CA3*dAlpha2PinchPStar() + CA3*SA1*dAlpha3PinchPStar() + CA3*SA1*SA2*dAlpha1PinchPStar() + &
			& CA1*SA3*dAlpha1PinchPStar() + CA1*SA2*SA3*dAlpha3PinchPStar()
		dRR32 = -CA1*CA3*dAlpha3PinchPStar() - CA2*CA3*SA1*dAlpha2PinchPStar() - CA1*CA3*SA2*dAlpha1PinchPStar() + &
			& SA1*SA3*dAlpha1PinchPStar() + SA1*SA2*SA3*dAlpha3PinchPStar()
		dRR33 = -CA3*SA2*dAlpha2PinchPStar() - CA2*SA3*dAlpha3PinchPStar()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZA0A0OS()/2D0 + dZZ0Z0OS()/2D0 + &
			& 1D0/(CB*RR32 - RR31*SB)*( CB*dRR32 - dRR31*SB - SB*RR32*dBeta1PinchPStar() - RR31*CB*dBeta1PinchPStar() ) + &
			& (SB*RR32 + RR31*CB)/(CB*RR32 - RR31*SB)*( dZG0A0OSAlter()/2D0 ) + &
			& (CB*RR12 - RR11*SB)/(CB*RR32 - RR31*SB)*( dZH1H3OSAlter()/2D0 ) + &
			& (CB*RR22 - RR21*SB)/(CB*RR32 - RR31*SB)*( dZH2H3OSAlter()/2D0 ) + &
			& (CB*RR32 - RR31*SB)/(CB*RR32 - RR31*SB)*( dZH3H3OS()/2D0 ) ) &
            & * (EL2/SW2)*(CB*RR32 - RR31*SB)**2/CW2 &
			& * ( (MH32**2 + MZ2**2 + MA02**2 - 2D0*MH32*MZ2 - 2D0*MH32*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
	case (6)
		dRR11 = -CA2*SA1*dAlpha1PinchPStar() - CA1*SA2*dAlpha2PinchPStar()
		dRR12 = CA1*CA2*dAlpha1PinchPStar() - SA1*SA2*dAlpha2PinchPStar()
		dRR13 = CA2*dAlpha2PinchPStar()
		dRR21 = -CA1*CA3*dAlpha1PinchPStar() - CA1*CA3*SA2*dAlpha3PinchPStar() - CA1*CA2*SA3*dAlpha2PinchPStar() + &
			& SA1*SA3*dAlpha3PinchPStar() + SA1*SA2*SA3*dAlpha1PinchPStar()
		dRR22 = -CA3*SA1*dAlpha1PinchPStar() - CA3*SA1*SA2*dAlpha3PinchPStar() - CA1*SA3*dAlpha3PinchPStar() - &
			& CA2*SA1*SA3*dAlpha2PinchPStar() - CA1*SA2*SA3*dAlpha1PinchPStar()
		dRR23 = CA2*CA3*dAlpha3PinchPStar() - SA2*SA3*dAlpha2PinchPStar()
		dRR31 = -CA1*CA2*CA3*dAlpha2PinchPStar() + CA3*SA1*dAlpha3PinchPStar() + CA3*SA1*SA2*dAlpha1PinchPStar() + &
			& CA1*SA3*dAlpha1PinchPStar() + CA1*SA2*SA3*dAlpha3PinchPStar()
		dRR32 = -CA1*CA3*dAlpha3PinchPStar() - CA2*CA3*SA1*dAlpha2PinchPStar() - CA1*CA3*SA2*dAlpha1PinchPStar() + &
			& SA1*SA3*dAlpha1PinchPStar() + SA1*SA2*SA3*dAlpha3PinchPStar()
		dRR33 = -CA3*SA2*dAlpha2PinchPStar() - CA2*SA3*dAlpha3PinchPStar()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZA0A0OS()/2D0 + dZZ0Z0OS()/2D0 + &
			& 1D0/(CB*RR32 - RR31*SB)*( CB*dRR32 - dRR31*SB - SB*RR32*dBeta2PinchPStar() - RR31*CB*dBeta2PinchPStar() ) + &
			& (SB*RR32 + RR31*CB)/(CB*RR32 - RR31*SB)*( dZG0A0OSAlter()/2D0 ) + &
			& (CB*RR12 - RR11*SB)/(CB*RR32 - RR31*SB)*( dZH1H3OSAlter()/2D0 ) + &
			& (CB*RR22 - RR21*SB)/(CB*RR32 - RR31*SB)*( dZH2H3OSAlter()/2D0 ) + &
			& (CB*RR32 - RR31*SB)/(CB*RR32 - RR31*SB)*( dZH3H3OS()/2D0 ) ) &
            & * (EL2/SW2)*(CB*RR32 - RR31*SB)**2/CW2 &
			& * ( (MH32**2 + MZ2**2 + MA02**2 - 2D0*MH32*MZ2 - 2D0*MH32*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
	case (7)
		dRR11 = -CA2*SA1*dAlpha1PinchOS() - CA1*SA2*dAlpha2PinchOS()
		dRR12 = CA1*CA2*dAlpha1PinchOS() - SA1*SA2*dAlpha2PinchOS()
		dRR13 = CA2*dAlpha2PinchOS()
		dRR21 = -CA1*CA3*dAlpha1PinchOS() - CA1*CA3*SA2*dAlpha3PinchOS() - CA1*CA2*SA3*dAlpha2PinchOS() + &
			& SA1*SA3*dAlpha3PinchOS() + SA1*SA2*SA3*dAlpha1PinchOS()
		dRR22 = -CA3*SA1*dAlpha1PinchOS() - CA3*SA1*SA2*dAlpha3PinchOS() - CA1*SA3*dAlpha3PinchOS() - &
			& CA2*SA1*SA3*dAlpha2PinchOS() - CA1*SA2*SA3*dAlpha1PinchOS()
		dRR23 = CA2*CA3*dAlpha3PinchOS() - SA2*SA3*dAlpha2PinchOS()
		dRR31 = -CA1*CA2*CA3*dAlpha2PinchOS() + CA3*SA1*dAlpha3PinchOS() + CA3*SA1*SA2*dAlpha1PinchOS() + &
			& CA1*SA3*dAlpha1PinchOS() + CA1*SA2*SA3*dAlpha3PinchOS()
		dRR32 = -CA1*CA3*dAlpha3PinchOS() - CA2*CA3*SA1*dAlpha2PinchOS() - CA1*CA3*SA2*dAlpha1PinchOS() + &
			& SA1*SA3*dAlpha1PinchOS() + SA1*SA2*SA3*dAlpha3PinchOS()
		dRR33 = -CA3*SA2*dAlpha2PinchOS() - CA2*SA3*dAlpha3PinchOS()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZA0A0OS()/2D0 + dZZ0Z0OS()/2D0 + &
			& 1D0/(CB*RR32 - RR31*SB)*( CB*dRR32 - dRR31*SB - SB*RR32*dBeta1PinchOS() - RR31*CB*dBeta1PinchOS() ) + &
			& (SB*RR32 + RR31*CB)/(CB*RR32 - RR31*SB)*( dZG0A0OSAlter()/2D0 ) + &
			& (CB*RR12 - RR11*SB)/(CB*RR32 - RR31*SB)*( dZH1H3OSAlter()/2D0 ) + &
			& (CB*RR22 - RR21*SB)/(CB*RR32 - RR31*SB)*( dZH2H3OSAlter()/2D0 ) + &
			& (CB*RR32 - RR31*SB)/(CB*RR32 - RR31*SB)*( dZH3H3OS()/2D0 ) ) &
            & * (EL2/SW2)*(CB*RR32 - RR31*SB)**2/CW2 &
			& * ( (MH32**2 + MZ2**2 + MA02**2 - 2D0*MH32*MZ2 - 2D0*MH32*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
	case (8)
		dRR11 = -CA2*SA1*dAlpha1PinchOS() - CA1*SA2*dAlpha2PinchOS()
		dRR12 = CA1*CA2*dAlpha1PinchOS() - SA1*SA2*dAlpha2PinchOS()
		dRR13 = CA2*dAlpha2PinchOS()
		dRR21 = -CA1*CA3*dAlpha1PinchOS() - CA1*CA3*SA2*dAlpha3PinchOS() - CA1*CA2*SA3*dAlpha2PinchOS() + &
			& SA1*SA3*dAlpha3PinchOS() + SA1*SA2*SA3*dAlpha1PinchOS()
		dRR22 = -CA3*SA1*dAlpha1PinchOS() - CA3*SA1*SA2*dAlpha3PinchOS() - CA1*SA3*dAlpha3PinchOS() - &
			& CA2*SA1*SA3*dAlpha2PinchOS() - CA1*SA2*SA3*dAlpha1PinchOS()
		dRR23 = CA2*CA3*dAlpha3PinchOS() - SA2*SA3*dAlpha2PinchOS()
		dRR31 = -CA1*CA2*CA3*dAlpha2PinchOS() + CA3*SA1*dAlpha3PinchOS() + CA3*SA1*SA2*dAlpha1PinchOS() + &
			& CA1*SA3*dAlpha1PinchOS() + CA1*SA2*SA3*dAlpha3PinchOS()
		dRR32 = -CA1*CA3*dAlpha3PinchOS() - CA2*CA3*SA1*dAlpha2PinchOS() - CA1*CA3*SA2*dAlpha1PinchOS() + &
			& SA1*SA3*dAlpha1PinchOS() + SA1*SA2*SA3*dAlpha3PinchOS()
		dRR33 = -CA3*SA2*dAlpha2PinchOS() - CA2*SA3*dAlpha3PinchOS()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZA0A0OS()/2D0 + dZZ0Z0OS()/2D0 + &
			& 1D0/(CB*RR32 - RR31*SB)*( CB*dRR32 - dRR31*SB - SB*RR32*dBeta2PinchOS() - RR31*CB*dBeta2PinchOS() ) + &
			& (SB*RR32 + RR31*CB)/(CB*RR32 - RR31*SB)*( dZG0A0OSAlter()/2D0 ) + &
			& (CB*RR12 - RR11*SB)/(CB*RR32 - RR31*SB)*( dZH1H3OSAlter()/2D0 ) + &
			& (CB*RR22 - RR21*SB)/(CB*RR32 - RR31*SB)*( dZH2H3OSAlter()/2D0 ) + &
			& (CB*RR32 - RR31*SB)/(CB*RR32 - RR31*SB)*( dZH3H3OS()/2D0 ) ) &
            & * (EL2/SW2)*(CB*RR32 - RR31*SB)**2/CW2 &
			& * ( (MH32**2 + MZ2**2 + MA02**2 - 2D0*MH32*MZ2 - 2D0*MH32*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
	case (9)
		dRR11 = -CA2*SA1*dAlpha1MSBarUsual() - CA1*SA2*dAlpha2MSBarUsual()
		dRR12 = CA1*CA2*dAlpha1MSBarUsual() - SA1*SA2*dAlpha2MSBarUsual()
		dRR13 = CA2*dAlpha2MSBarUsual()
		dRR21 = -CA1*CA3*dAlpha1MSBarUsual() - CA1*CA3*SA2*dAlpha3MSBarUsual() - CA1*CA2*SA3*dAlpha2MSBarUsual() + &
			& SA1*SA3*dAlpha3MSBarUsual() + SA1*SA2*SA3*dAlpha1MSBarUsual()
		dRR22 = -CA3*SA1*dAlpha1MSBarUsual() - CA3*SA1*SA2*dAlpha3MSBarUsual() - CA1*SA3*dAlpha3MSBarUsual() - &
			& CA2*SA1*SA3*dAlpha2MSBarUsual() - CA1*SA2*SA3*dAlpha1MSBarUsual()
		dRR23 = CA2*CA3*dAlpha3MSBarUsual() - SA2*SA3*dAlpha2MSBarUsual()
		dRR31 = -CA1*CA2*CA3*dAlpha2MSBarUsual() + CA3*SA1*dAlpha3MSBarUsual() + CA3*SA1*SA2*dAlpha1MSBarUsual() + &
			& CA1*SA3*dAlpha1MSBarUsual() + CA1*SA2*SA3*dAlpha3MSBarUsual()
		dRR32 = -CA1*CA3*dAlpha3MSBarUsual() - CA2*CA3*SA1*dAlpha2MSBarUsual() - CA1*CA3*SA2*dAlpha1MSBarUsual() + &
			& SA1*SA3*dAlpha1MSBarUsual() + SA1*SA2*SA3*dAlpha3MSBarUsual()
		dRR33 = -CA3*SA2*dAlpha2MSBarUsual() - CA2*SA3*dAlpha3MSBarUsual()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Usual()/MZ2 - dMW2Usual()/MW2)/2D0 + dZA0A0OS()/2D0 + dZZ0Z0OS()/2D0 + &
			& 1D0/(CB*RR32 - RR31*SB)*( CB*dRR32 - dRR31*SB - SB*RR32*dBetaMSBarUsual() - RR31*CB*dBetaMSBarUsual() ) + &
			& (SB*RR32 + RR31*CB)/(CB*RR32 - RR31*SB)*( dZG0A0OSUsual()/2D0 ) + &
			& (CB*RR12 - RR11*SB)/(CB*RR32 - RR31*SB)*( dZH1H3OSUsual()/2D0 ) + &
			& (CB*RR22 - RR21*SB)/(CB*RR32 - RR31*SB)*( dZH2H3OSUsual()/2D0 ) + &
			& (CB*RR32 - RR31*SB)/(CB*RR32 - RR31*SB)*( dZH3H3OS()/2D0 ) ) &
            & * (EL2/SW2)*(CB*RR32 - RR31*SB)**2/CW2 &
			& * ( (MH32**2 + MZ2**2 + MA02**2 - 2D0*MH32*MZ2 - 2D0*MH32*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
	case (10)
		dRR11 = -CA2*SA1*dAlpha1MSBarAlter() - CA1*SA2*dAlpha2MSBarAlter()
		dRR12 = CA1*CA2*dAlpha1MSBarAlter() - SA1*SA2*dAlpha2MSBarAlter()
		dRR13 = CA2*dAlpha2MSBarAlter()
		dRR21 = -CA1*CA3*dAlpha1MSBarAlter() - CA1*CA3*SA2*dAlpha3MSBarAlter() - CA1*CA2*SA3*dAlpha2MSBarAlter() + &
			& SA1*SA3*dAlpha3MSBarAlter() + SA1*SA2*SA3*dAlpha1MSBarAlter()
		dRR22 = -CA3*SA1*dAlpha1MSBarAlter() - CA3*SA1*SA2*dAlpha3MSBarAlter() - CA1*SA3*dAlpha3MSBarAlter() - &
			& CA2*SA1*SA3*dAlpha2MSBarAlter() - CA1*SA2*SA3*dAlpha1MSBarAlter()
		dRR23 = CA2*CA3*dAlpha3MSBarAlter() - SA2*SA3*dAlpha2MSBarAlter()
		dRR31 = -CA1*CA2*CA3*dAlpha2MSBarAlter() + CA3*SA1*dAlpha3MSBarAlter() + CA3*SA1*SA2*dAlpha1MSBarAlter() + &
			& CA1*SA3*dAlpha1MSBarAlter() + CA1*SA2*SA3*dAlpha3MSBarAlter()
		dRR32 = -CA1*CA3*dAlpha3MSBarAlter() - CA2*CA3*SA1*dAlpha2MSBarAlter() - CA1*CA3*SA2*dAlpha1MSBarAlter() + &
			& SA1*SA3*dAlpha1MSBarAlter() + SA1*SA2*SA3*dAlpha3MSBarAlter()
		dRR33 = -CA3*SA2*dAlpha2MSBarAlter() - CA2*SA3*dAlpha3MSBarAlter()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZA0A0OS()/2D0 + dZZ0Z0OS()/2D0 + &
			& 1D0/(CB*RR32 - RR31*SB)*( CB*dRR32 - dRR31*SB - SB*RR32*dBetaMSBarAlter() - RR31*CB*dBetaMSBarAlter() ) + &
			& (SB*RR32 + RR31*CB)/(CB*RR32 - RR31*SB)*( dZG0A0OSAlter()/2D0 ) + &
			& (CB*RR12 - RR11*SB)/(CB*RR32 - RR31*SB)*( dZH1H3OSAlter()/2D0 ) + &
			& (CB*RR22 - RR21*SB)/(CB*RR32 - RR31*SB)*( dZH2H3OSAlter()/2D0 ) + &
			& (CB*RR32 - RR31*SB)/(CB*RR32 - RR31*SB)*( dZH3H3OS()/2D0 ) ) &
            & * (EL2/SW2)*(CB*RR32 - RR31*SB)**2/CW2 &
			& * ( (MH32**2 + MZ2**2 + MA02**2 - 2D0*MH32*MZ2 - 2D0*MH32*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
	case default
		totalAmplitude = 0D0
 end select

 A0toH3Z0CT = totalAmplitude
end function A0toH3Z0CT
