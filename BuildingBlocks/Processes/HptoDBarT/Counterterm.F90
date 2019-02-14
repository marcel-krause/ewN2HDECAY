double precision function HptoDBarTCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude
 double precision :: dcLeft, dcRight

 select case (x)
	case (1)
		dcLeft = EL*CKM31/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSUsual()/2D0 + &
			& (CKM11/CKM31*MU/MT*dZUTOSRight() + CKM31/CKM31*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM31*MC/MT*dZCTOSRight() + CKM32/CKM31*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM31*MT/MT*dZTTOSRight() + CKM33/CKM31*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSUsual()/MT + dCKM31Yamada()/CKM31 - dMW2Usual()/(2D0*MW2) - &
			& dBeta1KanUsual()/(SB*CB) )
		dcRight = -EL*CKM32/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSUsual()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM31*dZUTOSLeft() + CKM31/CKM31*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM31*dZCTOSLeft() + CKM32/CKM31*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM31*dZTTOSLeft() + CKM33/CKM31*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSUsual()/MD + dCKM31Yamada()/CKM31 - dMW2Usual()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta1KanUsual() )

		totalAmplitude = 3D0*(MHp2 - (MT + MD)**2)*(MT/TB - MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MD)**2)*(MT/TB + MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (2)
		dcLeft = EL*CKM31/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSUsual()/2D0 + &
			& (CKM11/CKM31*MU/MT*dZUTOSRight() + CKM31/CKM31*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM31*MC/MT*dZCTOSRight() + CKM32/CKM31*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM31*MT/MT*dZTTOSRight() + CKM33/CKM31*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSUsual()/MT + dCKM31Yamada()/CKM31 - dMW2Usual()/(2D0*MW2) - &
			& dBeta2KanUsual()/(SB*CB) )
		dcRight = -EL*CKM32/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSUsual()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM31*dZUTOSLeft() + CKM31/CKM31*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM31*dZCTOSLeft() + CKM32/CKM31*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM31*dZTTOSLeft() + CKM33/CKM31*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSUsual()/MD + dCKM31Yamada()/CKM31 - dMW2Usual()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta2KanUsual() )

		totalAmplitude = 3D0*(MHp2 - (MT + MD)**2)*(MT/TB - MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MD)**2)*(MT/TB + MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (3)
		dcLeft = EL*CKM31/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM11/CKM31*MU/MT*dZUTOSRight() + CKM31/CKM31*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM31*MC/MT*dZCTOSRight() + CKM32/CKM31*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM31*MT/MT*dZTTOSRight() + CKM33/CKM31*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSAlter()/MT + dCKM31Yamada()/CKM31 - dMW2Alter()/(2D0*MW2) - &
			& dBeta1KanAlter()/(SB*CB) )
		dcRight = -EL*CKM32/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM31*dZUTOSLeft() + CKM31/CKM31*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM31*dZCTOSLeft() + CKM32/CKM31*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM31*dZTTOSLeft() + CKM33/CKM31*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSAlter()/MD + dCKM31Yamada()/CKM31 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta1KanAlter() )

		totalAmplitude = 3D0*(MHp2 - (MT + MD)**2)*(MT/TB - MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MD)**2)*(MT/TB + MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (4)
		dcLeft = EL*CKM31/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM11/CKM31*MU/MT*dZUTOSRight() + CKM31/CKM31*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM31*MC/MT*dZCTOSRight() + CKM32/CKM31*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM31*MT/MT*dZTTOSRight() + CKM33/CKM31*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSAlter()/MT + dCKM31Yamada()/CKM31 - dMW2Alter()/(2D0*MW2) - &
			& dBeta2KanAlter()/(SB*CB) )
		dcRight = -EL*CKM32/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM31*dZUTOSLeft() + CKM31/CKM31*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM31*dZCTOSLeft() + CKM32/CKM31*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM31*dZTTOSLeft() + CKM33/CKM31*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSAlter()/MD + dCKM31Yamada()/CKM31 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta2KanAlter() )

		totalAmplitude = 3D0*(MHp2 - (MT + MD)**2)*(MT/TB - MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MD)**2)*(MT/TB + MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (5)
		dcLeft = EL*CKM31/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM11/CKM31*MU/MT*dZUTOSRight() + CKM31/CKM31*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM31*MC/MT*dZCTOSRight() + CKM32/CKM31*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM31*MT/MT*dZTTOSRight() + CKM33/CKM31*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSAlter()/MT + dCKM31Yamada()/CKM31 - dMW2Alter()/(2D0*MW2) - &
			& dBeta1PinchPStar()/(SB*CB) )
		dcRight = -EL*CKM32/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM31*dZUTOSLeft() + CKM31/CKM31*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM31*dZCTOSLeft() + CKM32/CKM31*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM31*dZTTOSLeft() + CKM33/CKM31*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSAlter()/MD + dCKM31Yamada()/CKM31 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta1PinchPStar() )

		totalAmplitude = 3D0*(MHp2 - (MT + MD)**2)*(MT/TB - MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MD)**2)*(MT/TB + MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (6)
		dcLeft = EL*CKM31/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM11/CKM31*MU/MT*dZUTOSRight() + CKM31/CKM31*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM31*MC/MT*dZCTOSRight() + CKM32/CKM31*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM31*MT/MT*dZTTOSRight() + CKM33/CKM31*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSAlter()/MT + dCKM31Yamada()/CKM31 - dMW2Alter()/(2D0*MW2) - &
			& dBeta2PinchPStar()/(SB*CB) )
		dcRight = -EL*CKM32/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM31*dZUTOSLeft() + CKM31/CKM31*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM31*dZCTOSLeft() + CKM32/CKM31*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM31*dZTTOSLeft() + CKM33/CKM31*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSAlter()/MD + dCKM31Yamada()/CKM31 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta2PinchPStar() )

		totalAmplitude = 3D0*(MHp2 - (MT + MD)**2)*(MT/TB - MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MD)**2)*(MT/TB + MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (7)
		dcLeft = EL*CKM31/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM11/CKM31*MU/MT*dZUTOSRight() + CKM31/CKM31*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM31*MC/MT*dZCTOSRight() + CKM32/CKM31*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM31*MT/MT*dZTTOSRight() + CKM33/CKM31*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSAlter()/MT + dCKM31Yamada()/CKM31 - dMW2Alter()/(2D0*MW2) - &
			& dBeta1PinchOS()/(SB*CB) )
		dcRight = -EL*CKM32/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM31*dZUTOSLeft() + CKM31/CKM31*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM31*dZCTOSLeft() + CKM32/CKM31*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM31*dZTTOSLeft() + CKM33/CKM31*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSAlter()/MD + dCKM31Yamada()/CKM31 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta1PinchOS() )

		totalAmplitude = 3D0*(MHp2 - (MT + MD)**2)*(MT/TB - MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MD)**2)*(MT/TB + MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (8)
		dcLeft = EL*CKM31/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM11/CKM31*MU/MT*dZUTOSRight() + CKM31/CKM31*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM31*MC/MT*dZCTOSRight() + CKM32/CKM31*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM31*MT/MT*dZTTOSRight() + CKM33/CKM31*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSAlter()/MT + dCKM31Yamada()/CKM31 - dMW2Alter()/(2D0*MW2) - &
			& dBeta2PinchOS()/(SB*CB) )
		dcRight = -EL*CKM32/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM31*dZUTOSLeft() + CKM31/CKM31*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM31*dZCTOSLeft() + CKM32/CKM31*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM31*dZTTOSLeft() + CKM33/CKM31*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSAlter()/MD + dCKM31Yamada()/CKM31 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta2PinchOS() )

		totalAmplitude = 3D0*(MHp2 - (MT + MD)**2)*(MT/TB - MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MD)**2)*(MT/TB + MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (9)
		dcLeft = EL*CKM31/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSUsual()/2D0 + &
			& (CKM11/CKM31*MU/MT*dZUTOSRight() + CKM31/CKM31*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM31*MC/MT*dZCTOSRight() + CKM32/CKM31*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM31*MT/MT*dZTTOSRight() + CKM33/CKM31*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSUsual()/MT + dCKM31Yamada()/CKM31 - dMW2Usual()/(2D0*MW2) - &
			& dBetaMSBarUsual()/(SB*CB) )
		dcRight = -EL*CKM32/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSUsual()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM31*dZUTOSLeft() + CKM31/CKM31*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM31*dZCTOSLeft() + CKM32/CKM31*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM31*dZTTOSLeft() + CKM33/CKM31*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSUsual()/MD + dCKM31Yamada()/CKM31 - dMW2Usual()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBetaMSBarUsual() )

		totalAmplitude = 3D0*(MHp2 - (MT + MD)**2)*(MT/TB - MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MD)**2)*(MT/TB + MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (10)
		dcLeft = EL*CKM31/(DSQRT(2D0)*MW*SW)*MT/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM11/CKM31*MU/MT*dZUTOSRight() + CKM31/CKM31*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM31*MC/MT*dZCTOSRight() + CKM32/CKM31*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM31*MT/MT*dZTTOSRight() + CKM33/CKM31*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMTOSAlter()/MT + dCKM31Yamada()/CKM31 - dMW2Alter()/(2D0*MW2) - &
			& dBetaMSBarAlter()/(SB*CB) )
		dcRight = -EL*CKM32/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM31*dZUTOSLeft() + CKM31/CKM31*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM31*dZCTOSLeft() + CKM32/CKM31*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM31*dZTTOSLeft() + CKM33/CKM31*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSAlter()/MD + dCKM31Yamada()/CKM31 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBetaMSBarAlter() )

		totalAmplitude = 3D0*(MHp2 - (MT + MD)**2)*(MT/TB - MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MT - MD)**2)*(MT/TB + MD*YukS3Quark2)*EL*CKM31/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case default
		totalAmplitude = 0D0
 end select

 HptoDBarTCT = totalAmplitude
end function HptoDBarTCT
