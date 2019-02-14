double precision function HptoSBarTTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.75D0*EL2*TB2*DBLE(CKM32**INT(2.D0))*((MHp2 - 1.D0*DBLE((MS + MT)**INT(2.D0)))*DBLE((MT/TB - 1.D0*MS*Y&
  &ukS3Quark2)**INT(2.D0)) + (MHp2 - 1.D0*DBLE((MS - 1.D0*MT)**INT(2.D0)))*DBLE((MT/TB + MS*YukS3Quark2)**INT(2.D0)))* DBL&
  &E((-1.D0*CB*MT + MS*SB*YukS3Quark2)**INT(2.D0))*DBLE((MT - 1.D0*MS*TB*YukS3Quark2)**INT(-2.D0)))/(MW2*SB2*SW2)

 HptoSBarTTree = totalAmplitude
end function HptoSBarTTree