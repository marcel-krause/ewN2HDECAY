double precision function HptoCDBarCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude
 double precision :: dcLeft, dcRight

 select case (x)
	case (1)
		dcLeft = EL*CKM21/(DSQRT(2D0)*MW*SW)*MC/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSUsual()/2D0 + &
			& (CKM11/CKM21*MU/MC*dZUCOSRight() + CKM21/CKM21*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM21*MC/MC*dZCCOSRight() + CKM22/CKM21*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM21*MT/MC*dZTCOSRight() + CKM23/CKM21*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMCOSUsual()/MC + dCKM21Yamada()/CKM21 - dMW2Usual()/(2D0*MW2) - &
			& dBeta1KanUsual()/(SB*CB) )
		dcRight = -EL*CKM21/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSUsual()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM21*dZUCOSLeft() + CKM21/CKM21*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM21*dZCCOSLeft() + CKM22/CKM21*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM21*dZTCOSLeft() + CKM23/CKM21*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSUsual()/MD + dCKM21Yamada()/CKM21 - dMW2Usual()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta1KanUsual() )

		totalAmplitude = 3D0*(MHp2 - (MC + MD)**2)*(MC/TB - MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MC - MD)**2)*(MC/TB + MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (2)
		dcLeft = EL*CKM21/(DSQRT(2D0)*MW*SW)*MC/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSUsual()/2D0 + &
			& (CKM11/CKM21*MU/MC*dZUCOSRight() + CKM21/CKM21*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM21*MC/MC*dZCCOSRight() + CKM22/CKM21*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM21*MT/MC*dZTCOSRight() + CKM23/CKM21*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMCOSUsual()/MC + dCKM21Yamada()/CKM21 - dMW2Usual()/(2D0*MW2) - &
			& dBeta2KanUsual()/(SB*CB) )
		dcRight = -EL*CKM21/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSUsual()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM21*dZUCOSLeft() + CKM21/CKM21*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM21*dZCCOSLeft() + CKM22/CKM21*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM21*dZTCOSLeft() + CKM23/CKM21*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSUsual()/MD + dCKM21Yamada()/CKM21 - dMW2Usual()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta2KanUsual() )

		totalAmplitude = 3D0*(MHp2 - (MC + MD)**2)*(MC/TB - MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MC - MD)**2)*(MC/TB + MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (3)
		dcLeft = EL*CKM21/(DSQRT(2D0)*MW*SW)*MC/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM11/CKM21*MU/MC*dZUCOSRight() + CKM21/CKM21*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM21*MC/MC*dZCCOSRight() + CKM22/CKM21*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM21*MT/MC*dZTCOSRight() + CKM23/CKM21*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMCOSAlter()/MC + dCKM21Yamada()/CKM21 - dMW2Alter()/(2D0*MW2) - &
			& dBeta1KanAlter()/(SB*CB) )
		dcRight = -EL*CKM21/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM21*dZUCOSLeft() + CKM21/CKM21*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM21*dZCCOSLeft() + CKM22/CKM21*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM21*dZTCOSLeft() + CKM23/CKM21*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSAlter()/MD + dCKM21Yamada()/CKM21 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta1KanAlter() )

		totalAmplitude = 3D0*(MHp2 - (MC + MD)**2)*(MC/TB - MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MC - MD)**2)*(MC/TB + MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (4)
		dcLeft = EL*CKM21/(DSQRT(2D0)*MW*SW)*MC/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM11/CKM21*MU/MC*dZUCOSRight() + CKM21/CKM21*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM21*MC/MC*dZCCOSRight() + CKM22/CKM21*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM21*MT/MC*dZTCOSRight() + CKM23/CKM21*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMCOSAlter()/MC + dCKM21Yamada()/CKM21 - dMW2Alter()/(2D0*MW2) - &
			& dBeta2KanAlter()/(SB*CB) )
		dcRight = -EL*CKM21/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM21*dZUCOSLeft() + CKM21/CKM21*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM21*dZCCOSLeft() + CKM22/CKM21*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM21*dZTCOSLeft() + CKM23/CKM21*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSAlter()/MD + dCKM21Yamada()/CKM21 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta2KanAlter() )

		totalAmplitude = 3D0*(MHp2 - (MC + MD)**2)*(MC/TB - MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MC - MD)**2)*(MC/TB + MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (5)
		dcLeft = EL*CKM21/(DSQRT(2D0)*MW*SW)*MC/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM11/CKM21*MU/MC*dZUCOSRight() + CKM21/CKM21*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM21*MC/MC*dZCCOSRight() + CKM22/CKM21*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM21*MT/MC*dZTCOSRight() + CKM23/CKM21*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMCOSAlter()/MC + dCKM21Yamada()/CKM21 - dMW2Alter()/(2D0*MW2) - &
			& dBeta1PinchPStar()/(SB*CB) )
		dcRight = -EL*CKM21/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM21*dZUCOSLeft() + CKM21/CKM21*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM21*dZCCOSLeft() + CKM22/CKM21*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM21*dZTCOSLeft() + CKM23/CKM21*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSAlter()/MD + dCKM21Yamada()/CKM21 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta1PinchPStar() )

		totalAmplitude = 3D0*(MHp2 - (MC + MD)**2)*(MC/TB - MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MC - MD)**2)*(MC/TB + MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (6)
		dcLeft = EL*CKM21/(DSQRT(2D0)*MW*SW)*MC/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM11/CKM21*MU/MC*dZUCOSRight() + CKM21/CKM21*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM21*MC/MC*dZCCOSRight() + CKM22/CKM21*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM21*MT/MC*dZTCOSRight() + CKM23/CKM21*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMCOSAlter()/MC + dCKM21Yamada()/CKM21 - dMW2Alter()/(2D0*MW2) - &
			& dBeta2PinchPStar()/(SB*CB) )
		dcRight = -EL*CKM21/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM21*dZUCOSLeft() + CKM21/CKM21*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM21*dZCCOSLeft() + CKM22/CKM21*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM21*dZTCOSLeft() + CKM23/CKM21*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSAlter()/MD + dCKM21Yamada()/CKM21 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta2PinchPStar() )

		totalAmplitude = 3D0*(MHp2 - (MC + MD)**2)*(MC/TB - MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MC - MD)**2)*(MC/TB + MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (7)
		dcLeft = EL*CKM21/(DSQRT(2D0)*MW*SW)*MC/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM11/CKM21*MU/MC*dZUCOSRight() + CKM21/CKM21*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM21*MC/MC*dZCCOSRight() + CKM22/CKM21*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM21*MT/MC*dZTCOSRight() + CKM23/CKM21*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMCOSAlter()/MC + dCKM21Yamada()/CKM21 - dMW2Alter()/(2D0*MW2) - &
			& dBeta1PinchOS()/(SB*CB) )
		dcRight = -EL*CKM21/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM21*dZUCOSLeft() + CKM21/CKM21*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM21*dZCCOSLeft() + CKM22/CKM21*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM21*dZTCOSLeft() + CKM23/CKM21*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSAlter()/MD + dCKM21Yamada()/CKM21 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta1PinchOS() )

		totalAmplitude = 3D0*(MHp2 - (MC + MD)**2)*(MC/TB - MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MC - MD)**2)*(MC/TB + MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (8)
		dcLeft = EL*CKM21/(DSQRT(2D0)*MW*SW)*MC/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM11/CKM21*MU/MC*dZUCOSRight() + CKM21/CKM21*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM21*MC/MC*dZCCOSRight() + CKM22/CKM21*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM21*MT/MC*dZTCOSRight() + CKM23/CKM21*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMCOSAlter()/MC + dCKM21Yamada()/CKM21 - dMW2Alter()/(2D0*MW2) - &
			& dBeta2PinchOS()/(SB*CB) )
		dcRight = -EL*CKM21/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM21*dZUCOSLeft() + CKM21/CKM21*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM21*dZCCOSLeft() + CKM22/CKM21*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM21*dZTCOSLeft() + CKM23/CKM21*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSAlter()/MD + dCKM21Yamada()/CKM21 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBeta2PinchOS() )

		totalAmplitude = 3D0*(MHp2 - (MC + MD)**2)*(MC/TB - MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MC - MD)**2)*(MC/TB + MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (9)
		dcLeft = EL*CKM21/(DSQRT(2D0)*MW*SW)*MC/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSUsual()/2D0 + &
			& (CKM11/CKM21*MU/MC*dZUCOSRight() + CKM21/CKM21*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM21*MC/MC*dZCCOSRight() + CKM22/CKM21*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM21*MT/MC*dZTCOSRight() + CKM23/CKM21*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMCOSUsual()/MC + dCKM21Yamada()/CKM21 - dMW2Usual()/(2D0*MW2) - &
			& dBetaMSBarUsual()/(SB*CB) )
		dcRight = -EL*CKM21/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSUsual()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM21*dZUCOSLeft() + CKM21/CKM21*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM21*dZCCOSLeft() + CKM22/CKM21*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM21*dZTCOSLeft() + CKM23/CKM21*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSUsual()/MD + dCKM21Yamada()/CKM21 - dMW2Usual()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBetaMSBarUsual() )

		totalAmplitude = 3D0*(MHp2 - (MC + MD)**2)*(MC/TB - MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MC - MD)**2)*(MC/TB + MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (10)
		dcLeft = EL*CKM21/(DSQRT(2D0)*MW*SW)*MC/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM11/CKM21*MU/MC*dZUCOSRight() + CKM21/CKM21*dZDDOSLeft())/2D0 + &
			& (CKM21/CKM21*MC/MC*dZCCOSRight() + CKM22/CKM21*dZSDOSLeft())/2D0 + &
			& (CKM31/CKM21*MT/MC*dZTCOSRight() + CKM23/CKM21*dZBDOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMCOSAlter()/MC + dCKM21Yamada()/CKM21 - dMW2Alter()/(2D0*MW2) - &
			& dBetaMSBarAlter()/(SB*CB) )
		dcRight = -EL*CKM21/(DSQRT(2D0)*MW*SW)*MD*YukS3Quark2*( &
			& dZHpHpOS()/2D0 + dZGpHpOSAlter()/(2D0*YukS3Quark2) + &
			& (CKM11/CKM21*dZUCOSLeft() + CKM21/CKM21*MD/MD*dZDDOSRight())/2D0 + &
			& (CKM21/CKM21*dZCCOSLeft() + CKM22/CKM21*MS/MD*dZSDOSRight())/2D0 + &
			& (CKM31/CKM21*dZTCOSLeft() + CKM23/CKM21*MB/MD*dZBDOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMDOSAlter()/MD + dCKM21Yamada()/CKM21 - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Quark2**2)/YukS3Quark2*dBetaMSBarAlter() )

		totalAmplitude = 3D0*(MHp2 - (MC + MD)**2)*(MC/TB - MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MC - MD)**2)*(MC/TB + MD*YukS3Quark2)*EL*CKM21/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case default
		totalAmplitude = 0D0
 end select

 HptoCDBarCT = totalAmplitude
end function HptoCDBarCT
