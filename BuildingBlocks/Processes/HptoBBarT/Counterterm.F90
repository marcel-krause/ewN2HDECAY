double precision function HptoBBarTCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude
 double precision :: dcLeft, dcRight

 select case (x)
	case (1)
		dcLeft = EL*CKM33/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSUsual()/2D0 + &
			& (CKM13/CKM33*MU/MT*dZUTOSRight() + CKM31/CKM33*dZDBOSLeft())/2D0 + &
			& (CKM23/CKM33*MC/MT*dZCTOSRight() + CKM32/CKM33*dZSBOSLeft())/2D0 + &
			& (CKM33/CKM33*MT/MT*dZTTOSRight() + CKM33/CKM33*dZBBOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSUsual()/MT + dCKM33Yamada()/CKM33 - dMW2Usual()/(2D0*MW2) - &
			& dBeta1KanUsual()/(SB*CB) )
		dcRight = -EL*CKM33/(DSQRT(2D0)*MW*SW)*MB*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSUsual()/(2D0*YukS3Quark2) + &
			& (CKM13/CKM33*dZUTOSLeft() + CKM31/CKM33*MD/MB*dZDBOSRight())/2D0 + &
			& (CKM23/CKM33*dZCTOSLeft() + CKM32/CKM33*MS/MB*dZSBOSRight())/2D0 + &
			& (CKM33/CKM33*dZTTOSLeft() + CKM33/CKM33*MB/MB*dZBBOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMBOSUsual()/MB + dCKM33Yamada()/CKM33 - dMW2Usual()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta1KanUsual() )

		totalAmplitude = 3D0*(MHp2 - (MT + MB)**2)*(MT/TB - MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MB)**2)*(MT/TB + MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (2)
		dcLeft = EL*CKM33/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSUsual()/2D0 + &
			& (CKM13/CKM33*MU/MT*dZUTOSRight() + CKM31/CKM33*dZDBOSLeft())/2D0 + &
			& (CKM23/CKM33*MC/MT*dZCTOSRight() + CKM32/CKM33*dZSBOSLeft())/2D0 + &
			& (CKM33/CKM33*MT/MT*dZTTOSRight() + CKM33/CKM33*dZBBOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSUsual()/MT + dCKM33Yamada()/CKM33 - dMW2Usual()/(2D0*MW2) - &
			& dBeta2KanUsual()/(SB*CB) )
		dcRight = -EL*CKM33/(DSQRT(2D0)*MW*SW)*MB*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSUsual()/(2D0*YukS3Quark2) + &
			& (CKM13/CKM33*dZUTOSLeft() + CKM31/CKM33*MD/MB*dZDBOSRight())/2D0 + &
			& (CKM23/CKM33*dZCTOSLeft() + CKM32/CKM33*MS/MB*dZSBOSRight())/2D0 + &
			& (CKM33/CKM33*dZTTOSLeft() + CKM33/CKM33*MB/MB*dZBBOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMBOSUsual()/MB + dCKM33Yamada()/CKM33 - dMW2Usual()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta2KanUsual() )

		totalAmplitude = 3D0*(MHp2 - (MT + MB)**2)*(MT/TB - MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MB)**2)*(MT/TB + MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (3)
		dcLeft = EL*CKM33/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM13/CKM33*MU/MT*dZUTOSRight() + CKM31/CKM33*dZDBOSLeft())/2D0 + &
			& (CKM23/CKM33*MC/MT*dZCTOSRight() + CKM32/CKM33*dZSBOSLeft())/2D0 + &
			& (CKM33/CKM33*MT/MT*dZTTOSRight() + CKM33/CKM33*dZBBOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSAlter()/MT + dCKM33Yamada()/CKM33 - dMW2Alter()/(2D0*MW2) - &
			& dBeta1KanAlter()/(SB*CB) )
		dcRight = -EL*CKM33/(DSQRT(2D0)*MW*SW)*MB*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM13/CKM33*dZUTOSLeft() + CKM31/CKM33*MD/MB*dZDBOSRight())/2D0 + &
			& (CKM23/CKM33*dZCTOSLeft() + CKM32/CKM33*MS/MB*dZSBOSRight())/2D0 + &
			& (CKM33/CKM33*dZTTOSLeft() + CKM33/CKM33*MB/MB*dZBBOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMBOSAlter()/MB + dCKM33Yamada()/CKM33 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta1KanAlter() )

		totalAmplitude = 3D0*(MHp2 - (MT + MB)**2)*(MT/TB - MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MB)**2)*(MT/TB + MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (4)
		dcLeft = EL*CKM33/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM13/CKM33*MU/MT*dZUTOSRight() + CKM31/CKM33*dZDBOSLeft())/2D0 + &
			& (CKM23/CKM33*MC/MT*dZCTOSRight() + CKM32/CKM33*dZSBOSLeft())/2D0 + &
			& (CKM33/CKM33*MT/MT*dZTTOSRight() + CKM33/CKM33*dZBBOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSAlter()/MT + dCKM33Yamada()/CKM33 - dMW2Alter()/(2D0*MW2) - &
			& dBeta2KanAlter()/(SB*CB) )
		dcRight = -EL*CKM33/(DSQRT(2D0)*MW*SW)*MB*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM13/CKM33*dZUTOSLeft() + CKM31/CKM33*MD/MB*dZDBOSRight())/2D0 + &
			& (CKM23/CKM33*dZCTOSLeft() + CKM32/CKM33*MS/MB*dZSBOSRight())/2D0 + &
			& (CKM33/CKM33*dZTTOSLeft() + CKM33/CKM33*MB/MB*dZBBOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMBOSAlter()/MB + dCKM33Yamada()/CKM33 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta2KanAlter() )

		totalAmplitude = 3D0*(MHp2 - (MT + MB)**2)*(MT/TB - MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MB)**2)*(MT/TB + MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (5)
		dcLeft = EL*CKM33/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM13/CKM33*MU/MT*dZUTOSRight() + CKM31/CKM33*dZDBOSLeft())/2D0 + &
			& (CKM23/CKM33*MC/MT*dZCTOSRight() + CKM32/CKM33*dZSBOSLeft())/2D0 + &
			& (CKM33/CKM33*MT/MT*dZTTOSRight() + CKM33/CKM33*dZBBOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSAlter()/MT + dCKM33Yamada()/CKM33 - dMW2Alter()/(2D0*MW2) - &
			& dBeta1PinchPStar()/(SB*CB) )
		dcRight = -EL*CKM33/(DSQRT(2D0)*MW*SW)*MB*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM13/CKM33*dZUTOSLeft() + CKM31/CKM33*MD/MB*dZDBOSRight())/2D0 + &
			& (CKM23/CKM33*dZCTOSLeft() + CKM32/CKM33*MS/MB*dZSBOSRight())/2D0 + &
			& (CKM33/CKM33*dZTTOSLeft() + CKM33/CKM33*MB/MB*dZBBOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMBOSAlter()/MB + dCKM33Yamada()/CKM33 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta1PinchPStar() )

		totalAmplitude = 3D0*(MHp2 - (MT + MB)**2)*(MT/TB - MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MB)**2)*(MT/TB + MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (6)
		dcLeft = EL*CKM33/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM13/CKM33*MU/MT*dZUTOSRight() + CKM31/CKM33*dZDBOSLeft())/2D0 + &
			& (CKM23/CKM33*MC/MT*dZCTOSRight() + CKM32/CKM33*dZSBOSLeft())/2D0 + &
			& (CKM33/CKM33*MT/MT*dZTTOSRight() + CKM33/CKM33*dZBBOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSAlter()/MT + dCKM33Yamada()/CKM33 - dMW2Alter()/(2D0*MW2) - &
			& dBeta2PinchPStar()/(SB*CB) )
		dcRight = -EL*CKM33/(DSQRT(2D0)*MW*SW)*MB*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM13/CKM33*dZUTOSLeft() + CKM31/CKM33*MD/MB*dZDBOSRight())/2D0 + &
			& (CKM23/CKM33*dZCTOSLeft() + CKM32/CKM33*MS/MB*dZSBOSRight())/2D0 + &
			& (CKM33/CKM33*dZTTOSLeft() + CKM33/CKM33*MB/MB*dZBBOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMBOSAlter()/MB + dCKM33Yamada()/CKM33 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta2PinchPStar() )

		totalAmplitude = 3D0*(MHp2 - (MT + MB)**2)*(MT/TB - MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MB)**2)*(MT/TB + MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (7)
		dcLeft = EL*CKM33/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM13/CKM33*MU/MT*dZUTOSRight() + CKM31/CKM33*dZDBOSLeft())/2D0 + &
			& (CKM23/CKM33*MC/MT*dZCTOSRight() + CKM32/CKM33*dZSBOSLeft())/2D0 + &
			& (CKM33/CKM33*MT/MT*dZTTOSRight() + CKM33/CKM33*dZBBOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSAlter()/MT + dCKM33Yamada()/CKM33 - dMW2Alter()/(2D0*MW2) - &
			& dBeta1PinchOS()/(SB*CB) )
		dcRight = -EL*CKM33/(DSQRT(2D0)*MW*SW)*MB*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM13/CKM33*dZUTOSLeft() + CKM31/CKM33*MD/MB*dZDBOSRight())/2D0 + &
			& (CKM23/CKM33*dZCTOSLeft() + CKM32/CKM33*MS/MB*dZSBOSRight())/2D0 + &
			& (CKM33/CKM33*dZTTOSLeft() + CKM33/CKM33*MB/MB*dZBBOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMBOSAlter()/MB + dCKM33Yamada()/CKM33 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta1PinchOS() )

		totalAmplitude = 3D0*(MHp2 - (MT + MB)**2)*(MT/TB - MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MB)**2)*(MT/TB + MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (8)
		dcLeft = EL*CKM33/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM13/CKM33*MU/MT*dZUTOSRight() + CKM31/CKM33*dZDBOSLeft())/2D0 + &
			& (CKM23/CKM33*MC/MT*dZCTOSRight() + CKM32/CKM33*dZSBOSLeft())/2D0 + &
			& (CKM33/CKM33*MT/MT*dZTTOSRight() + CKM33/CKM33*dZBBOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSAlter()/MT + dCKM33Yamada()/CKM33 - dMW2Alter()/(2D0*MW2) - &
			& dBeta2PinchOS()/(SB*CB) )
		dcRight = -EL*CKM33/(DSQRT(2D0)*MW*SW)*MB*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM13/CKM33*dZUTOSLeft() + CKM31/CKM33*MD/MB*dZDBOSRight())/2D0 + &
			& (CKM23/CKM33*dZCTOSLeft() + CKM32/CKM33*MS/MB*dZSBOSRight())/2D0 + &
			& (CKM33/CKM33*dZTTOSLeft() + CKM33/CKM33*MB/MB*dZBBOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMBOSAlter()/MB + dCKM33Yamada()/CKM33 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta2PinchOS() )

		totalAmplitude = 3D0*(MHp2 - (MT + MB)**2)*(MT/TB - MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MB)**2)*(MT/TB + MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (9)
		dcLeft = EL*CKM33/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSUsual()/2D0 + &
			& (CKM13/CKM33*MU/MT*dZUTOSRight() + CKM31/CKM33*dZDBOSLeft())/2D0 + &
			& (CKM23/CKM33*MC/MT*dZCTOSRight() + CKM32/CKM33*dZSBOSLeft())/2D0 + &
			& (CKM33/CKM33*MT/MT*dZTTOSRight() + CKM33/CKM33*dZBBOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSUsual()/MT + dCKM33Yamada()/CKM33 - dMW2Usual()/(2D0*MW2) - &
			& dBetaMSBarUsual()/(SB*CB) )
		dcRight = -EL*CKM33/(DSQRT(2D0)*MW*SW)*MB*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSUsual()/(2D0*YukS3Quark2) + &
			& (CKM13/CKM33*dZUTOSLeft() + CKM31/CKM33*MD/MB*dZDBOSRight())/2D0 + &
			& (CKM23/CKM33*dZCTOSLeft() + CKM32/CKM33*MS/MB*dZSBOSRight())/2D0 + &
			& (CKM33/CKM33*dZTTOSLeft() + CKM33/CKM33*MB/MB*dZBBOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMBOSUsual()/MB + dCKM33Yamada()/CKM33 - dMW2Usual()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBetaMSBarUsual() )

		totalAmplitude = 3D0*(MHp2 - (MT + MB)**2)*(MT/TB - MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MB)**2)*(MT/TB + MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (10)
		dcLeft = EL*CKM33/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM13/CKM33*MU/MT*dZUTOSRight() + CKM31/CKM33*dZDBOSLeft())/2D0 + &
			& (CKM23/CKM33*MC/MT*dZCTOSRight() + CKM32/CKM33*dZSBOSLeft())/2D0 + &
			& (CKM33/CKM33*MT/MT*dZTTOSRight() + CKM33/CKM33*dZBBOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSAlter()/MT + dCKM33Yamada()/CKM33 - dMW2Alter()/(2D0*MW2) - &
			& dBetaMSBarAlter()/(SB*CB) )
		dcRight = -EL*CKM33/(DSQRT(2D0)*MW*SW)*MB*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM13/CKM33*dZUTOSLeft() + CKM31/CKM33*MD/MB*dZDBOSRight())/2D0 + &
			& (CKM23/CKM33*dZCTOSLeft() + CKM32/CKM33*MS/MB*dZSBOSRight())/2D0 + &
			& (CKM33/CKM33*dZTTOSLeft() + CKM33/CKM33*MB/MB*dZBBOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMBOSAlter()/MB + dCKM33Yamada()/CKM33 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBetaMSBarAlter() )

		totalAmplitude = 3D0*(MHp2 - (MT + MB)**2)*(MT/TB - MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MB)**2)*(MT/TB + MB*YukS3Quark2)*EL*CKM33/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case default
		totalAmplitude = 0D0
 end select

 HptoBBarTCT = totalAmplitude
end function HptoBBarTCT
