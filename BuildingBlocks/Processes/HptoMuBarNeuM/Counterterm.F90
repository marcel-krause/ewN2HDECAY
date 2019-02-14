double precision function HptoMuBarNeuMCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) - &
			& (1D0 + YukS3Lep2**2)/YukS3Lep2*dBeta1KanUsual() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 + 1D0/YukS3Lep2*dZGpHpOSUsual()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*SW2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) - &
			& (1D0 + YukS3Lep2**2)/YukS3Lep2*dBeta2KanUsual() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 + 1D0/YukS3Lep2*dZGpHpOSUsual()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*SW2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Lep2**2)/YukS3Lep2*dBeta1KanAlter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 + 1D0/YukS3Lep2*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*SW2)
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Lep2**2)/YukS3Lep2*dBeta2KanAlter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 + 1D0/YukS3Lep2*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*SW2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Lep2**2)/YukS3Lep2*dBeta1PinchPStar() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 + 1D0/YukS3Lep2*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*SW2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Lep2**2)/YukS3Lep2*dBeta2PinchPStar() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 + 1D0/YukS3Lep2*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*SW2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Lep2**2)/YukS3Lep2*dBeta1PinchOS() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 + 1D0/YukS3Lep2*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*SW2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Lep2**2)/YukS3Lep2*dBeta2PinchOS() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 + 1D0/YukS3Lep2*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*SW2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) - &
			& (1D0 + YukS3Lep2**2)/YukS3Lep2*dBetaMSBarUsual() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 + 1D0/YukS3Lep2*dZGpHpOSUsual()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*SW2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) - &
			& (1D0 + YukS3Lep2**2)/YukS3Lep2*dBetaMSBarAlter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 + 1D0/YukS3Lep2*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*SW2)
	case default
		totalAmplitude = 0D0
 end select

 HptoMuBarNeuMCT = totalAmplitude
end function HptoMuBarNeuMCT
