double precision function H1toTauTauBarCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude
 double precision :: dRR11, dRR12, dRR13, dRR21, dRR22, dRR23, dRR31, dRR32, dRR33, dYukS1Lep1

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

		dYukS1Lep1 = ( CA2*CA3*YukS1Lep2 - CA2*SA3*YukS1Lep3 )*dAlpha1KanUsual() - &
						& YukS1Lep1*YukS3Lep2*dBeta1KanUsual() - SA2/CA2*YukS1Lep1*dAlpha2KanUsual()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSUsual()/ML - dMW2Usual()/(2D0*MW2) + dYukS1Lep1/YukS1Lep1 + &
			& dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 + &
			& YukS1Lep1/YukS1Lep1*( dZH1H1OS()/2D0 ) + YukS1Lep2/YukS1Lep1*( dZH2H1OSUsual()/2D0 ) + &
			& YukS1Lep3/YukS1Lep1*( dZH3H1OSUsual()/2D0 ) &
			& )*(0.5D0*EL2*(MH12 - 4.D0*ML2)*ML2*YukS1Lep1**2)/(MW2*SW2)
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

		dYukS1Lep1 = ( CA2*CA3*YukS1Lep2 - CA2*SA3*YukS1Lep3 )*dAlpha1KanUsual() - &
						& YukS1Lep1*YukS3Lep2*dBeta2KanUsual() - SA2/CA2*YukS1Lep1*dAlpha2KanUsual()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSUsual()/ML - dMW2Usual()/(2D0*MW2) + dYukS1Lep1/YukS1Lep1 + &
			& dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 + &
			& YukS1Lep1/YukS1Lep1*( dZH1H1OS()/2D0 ) + YukS1Lep2/YukS1Lep1*( dZH2H1OSUsual()/2D0 ) + &
			& YukS1Lep3/YukS1Lep1*( dZH3H1OSUsual()/2D0 ) &
			& )*(0.5D0*EL2*(MH12 - 4.D0*ML2)*ML2*YukS1Lep1**2)/(MW2*SW2)
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

		dYukS1Lep1 = ( CA2*CA3*YukS1Lep2 - CA2*SA3*YukS1Lep3 )*dAlpha1KanAlter() - &
						& YukS1Lep1*YukS3Lep2*dBeta1KanAlter() - SA2/CA2*YukS1Lep1*dAlpha2KanAlter()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + dYukS1Lep1/YukS1Lep1 + &
			& dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 + &
			& YukS1Lep1/YukS1Lep1*( dZH1H1OS()/2D0 ) + YukS1Lep2/YukS1Lep1*( dZH2H1OSAlter()/2D0 ) + &
			& YukS1Lep3/YukS1Lep1*( dZH3H1OSAlter()/2D0 ) &
			& )*(0.5D0*EL2*(MH12 - 4.D0*ML2)*ML2*YukS1Lep1**2)/(MW2*SW2)
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

		dYukS1Lep1 = ( CA2*CA3*YukS1Lep2 - CA2*SA3*YukS1Lep3 )*dAlpha1KanAlter() - &
						& YukS1Lep1*YukS3Lep2*dBeta2KanAlter() - SA2/CA2*YukS1Lep1*dAlpha2KanAlter()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + dYukS1Lep1/YukS1Lep1 + &
			& dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 + &
			& YukS1Lep1/YukS1Lep1*( dZH1H1OS()/2D0 ) + YukS1Lep2/YukS1Lep1*( dZH2H1OSAlter()/2D0 ) + &
			& YukS1Lep3/YukS1Lep1*( dZH3H1OSAlter()/2D0 ) &
			& )*(0.5D0*EL2*(MH12 - 4.D0*ML2)*ML2*YukS1Lep1**2)/(MW2*SW2)
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

		dYukS1Lep1 = ( CA2*CA3*YukS1Lep2 - CA2*SA3*YukS1Lep3 )*dAlpha1PinchPStar() - &
						& YukS1Lep1*YukS3Lep2*dBeta1PinchPStar() - SA2/CA2*YukS1Lep1*dAlpha2PinchPStar()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + dYukS1Lep1/YukS1Lep1 + &
			& dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 + &
			& YukS1Lep1/YukS1Lep1*( dZH1H1OS()/2D0 ) + YukS1Lep2/YukS1Lep1*( dZH2H1OSAlter()/2D0 ) + &
			& YukS1Lep3/YukS1Lep1*( dZH3H1OSAlter()/2D0 ) &
			& )*(0.5D0*EL2*(MH12 - 4.D0*ML2)*ML2*YukS1Lep1**2)/(MW2*SW2)
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

		dYukS1Lep1 = ( CA2*CA3*YukS1Lep2 - CA2*SA3*YukS1Lep3 )*dAlpha1PinchPStar() - &
						& YukS1Lep1*YukS3Lep2*dBeta2PinchPStar() - SA2/CA2*YukS1Lep1*dAlpha2PinchPStar()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + dYukS1Lep1/YukS1Lep1 + &
			& dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 + &
			& YukS1Lep1/YukS1Lep1*( dZH1H1OS()/2D0 ) + YukS1Lep2/YukS1Lep1*( dZH2H1OSAlter()/2D0 ) + &
			& YukS1Lep3/YukS1Lep1*( dZH3H1OSAlter()/2D0 ) &
			& )*(0.5D0*EL2*(MH12 - 4.D0*ML2)*ML2*YukS1Lep1**2)/(MW2*SW2)
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

		dYukS1Lep1 = ( CA2*CA3*YukS1Lep2 - CA2*SA3*YukS1Lep3 )*dAlpha1PinchOS() - &
						& YukS1Lep1*YukS3Lep2*dBeta1PinchOS() - SA2/CA2*YukS1Lep1*dAlpha2PinchOS()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + dYukS1Lep1/YukS1Lep1 + &
			& dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 + &
			& YukS1Lep1/YukS1Lep1*( dZH1H1OS()/2D0 ) + YukS1Lep2/YukS1Lep1*( dZH2H1OSAlter()/2D0 ) + &
			& YukS1Lep3/YukS1Lep1*( dZH3H1OSAlter()/2D0 ) &
			& )*(0.5D0*EL2*(MH12 - 4.D0*ML2)*ML2*YukS1Lep1**2)/(MW2*SW2)
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

		dYukS1Lep1 = ( CA2*CA3*YukS1Lep2 - CA2*SA3*YukS1Lep3 )*dAlpha1PinchOS() - &
						& YukS1Lep1*YukS3Lep2*dBeta2PinchOS() - SA2/CA2*YukS1Lep1*dAlpha2PinchOS()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + dYukS1Lep1/YukS1Lep1 + &
			& dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 + &
			& YukS1Lep1/YukS1Lep1*( dZH1H1OS()/2D0 ) + YukS1Lep2/YukS1Lep1*( dZH2H1OSAlter()/2D0 ) + &
			& YukS1Lep3/YukS1Lep1*( dZH3H1OSAlter()/2D0 ) &
			& )*(0.5D0*EL2*(MH12 - 4.D0*ML2)*ML2*YukS1Lep1**2)/(MW2*SW2)
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

		dYukS1Lep1 = ( CA2*CA3*YukS1Lep2 - CA2*SA3*YukS1Lep3 )*dAlpha1MSBarUsual() - &
						& YukS1Lep1*YukS3Lep2*dBetaMSBarUsual() - SA2/CA2*YukS1Lep1*dAlpha2MSBarUsual()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSUsual()/ML - dMW2Usual()/(2D0*MW2) + dYukS1Lep1/YukS1Lep1 + &
			& dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 + &
			& YukS1Lep1/YukS1Lep1*( dZH1H1OS()/2D0 ) + YukS1Lep2/YukS1Lep1*( dZH2H1OSUsual()/2D0 ) + &
			& YukS1Lep3/YukS1Lep1*( dZH3H1OSUsual()/2D0 ) &
			& )*(0.5D0*EL2*(MH12 - 4.D0*ML2)*ML2*YukS1Lep1**2)/(MW2*SW2)
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

		dYukS1Lep1 = ( CA2*CA3*YukS1Lep2 - CA2*SA3*YukS1Lep3 )*dAlpha1MSBarAlter() - &
						& YukS1Lep1*YukS3Lep2*dBetaMSBarAlter() - SA2/CA2*YukS1Lep1*dAlpha2MSBarAlter()

		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + dYukS1Lep1/YukS1Lep1 + &
			& dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 + &
			& YukS1Lep1/YukS1Lep1*( dZH1H1OS()/2D0 ) + YukS1Lep2/YukS1Lep1*( dZH2H1OSAlter()/2D0 ) + &
			& YukS1Lep3/YukS1Lep1*( dZH3H1OSAlter()/2D0 ) &
			& )*(0.5D0*EL2*(MH12 - 4.D0*ML2)*ML2*YukS1Lep1**2)/(MW2*SW2)
	case default
		totalAmplitude = 0D0
 end select

 H1toTauTauBarCT = totalAmplitude
end function H1toTauTauBarCT
