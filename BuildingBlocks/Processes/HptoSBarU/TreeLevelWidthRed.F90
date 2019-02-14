double precision function HptoSBarUTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.75D0*EL2*TB2*DBLE(CKM12**INT(2.D0))*((MHp2 - 1.D0*DBLE((MS + MU)**INT(2.D0)))*DBLE((MU/TB - 1.D0*MS*YukS2Qua&
  &rk2)**INT(2.D0)) + (MHp2 - 1.D0*DBLE((MS - 1.D0*MU)**INT(2.D0)))*DBLE((MU/TB + MS*YukS2Quark2)**INT(2.D0)))*DBLE((MU - 1.D0*MS&
  &*TB*YukS2Quark2)**INT(-2.D0))* DBLE((-1.D0*CB*MU + MS*SB*YukS3Quark2)**INT(2.D0)))/(MW2*SB2*SW2)

 HptoSBarUTree = totalAmplitude
end function HptoSBarUTree